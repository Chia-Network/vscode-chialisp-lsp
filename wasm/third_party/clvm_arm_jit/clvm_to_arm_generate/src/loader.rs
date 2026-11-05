use crate::mem::{TargetMemory, read_i32, read_u16, read_u32};
use elf_rs::{Elf, ElfFile, Error, SectionHeaderFlags, SectionType};
use std::collections::{BTreeMap, HashMap};

#[derive(Debug, Clone)]
#[allow(dead_code)]
struct ElfSym {
    st_name: u32,
    st_value: u32,
    st_size: u32,
    st_info: u8,
    st_other: u8,
    st_shndx: u16,
}

#[derive(Debug, Clone)]
struct ElfRela {
    offset: u32,
    info: u32,
    addend: i32,
}

#[derive(Debug, Clone)]
enum ElfRelaType {
    RArmAbs32,
    RArmJmp,
    RArmLdrPCG0,
}

impl ElfRela {
    fn sym(&self) -> usize {
        (self.info >> 8) as usize
    }
}

pub struct ElfLoader<'a> {
    elf: Elf<'a>,
    upper_addr: u32,
    symbol_string_table: Vec<u8>,
    sections: Vec<u32>,
    relocs: ElfRelocations,
    symbols: Vec<ElfSym>,
}

/// Information that can be used to associate a symbol as emitted by the jit with
/// a symbol in the clvm code.
#[derive(Debug, Clone)]
pub struct EmuSymbolInfo {
    pub stripped: String,
    pub raw: String,
    pub address: u32,
}

#[derive(Debug, Clone)]
struct ElfRelaSection {
    content: Vec<ElfRela>,
}

#[derive(Debug, Clone)]
struct ElfRelocations {
    pub rela: BTreeMap<usize, ElfRelaSection>,
}

impl<'a> ElfLoader<'a> {
    pub fn new(elf_bytes: &'a [u8], target_addr: u32) -> Result<Self, Error> {
        let mut loader = ElfLoader {
            elf: Elf::from_bytes(&elf_bytes)?,
            upper_addr: target_addr,
            symbol_string_table: Vec::new(),
            sections: Vec::new(),
            relocs: ElfRelocations {
                rela: BTreeMap::default(),
            },
            symbols: Vec::new(),
        };

        let mut section_addr = target_addr;
        for (i, s) in loader.elf.section_header_iter().enumerate() {
            if s.flags().contains(SectionHeaderFlags::SHF_ALLOC) {
                let align_mask = (s.addralign() - 1) as u32;
                eprintln!("align mask for {s:?}: {align_mask:x} {:x}", s.size());
                section_addr = (section_addr + align_mask) & !align_mask;
                loader.sections.push(section_addr);
                eprintln!("{i} {s:?}");
                eprintln!("load section {} at {section_addr:08x}", i);
                section_addr += s.size() as u32;
            } else {
                loader.sections.push(0);
            }

            if matches!(s.sh_type(), SectionType::SHT_RELA) {
                if let Some(content) = s.content() {
                    let content = read_reloc_content(content, s.entsize() as usize);
                    let target_usize = s.info() as usize;
                    loader
                        .relocs
                        .rela
                        .insert(target_usize, ElfRelaSection { content });
                }
            } else if matches!(s.sh_type(), SectionType::SHT_SYMTAB) {
                if let Some(content) = s.content() {
                    if !loader.symbols.is_empty() {
                        todo!();
                    }
                    loader.symbols = read_sym_content(content, s.entsize() as usize);
                    if let Some(strtab_section) = loader.elf.section_header_nth(s.link() as usize) {
                        if let Some(strtab_content) = strtab_section.content() {
                            loader.symbol_string_table = strtab_content.to_vec();
                        }
                    }
                }
            }
        }
        loader.upper_addr = section_addr;

        Ok(loader)
    }

    pub fn next_free_addr(&self) -> u32 {
        self.upper_addr
    }

    // Set all section addresses to those computed at load time and set the
    // type to executable.
    pub fn patch_sections(&self) -> Vec<(usize, u32)> {
        // Get the location of the section headers
        let shoff = self.elf.elf_header().section_header_offset() as usize;
        let shent = self.elf.elf_header().section_header_entry_size() as usize;

        // 12 bytes into each section header is the section address.
        self.sections
            .iter()
            .enumerate()
            .map(|(i, s)| {
                let sh_addr = shoff + i * shent + 12;
                (sh_addr, *s)
            })
            .collect()
    }

    fn apply_reloc<M>(
        &self,
        memory: &mut M,
        sections: &[u32],
        symbols: &[ElfSym],
        in_section: usize,
        r: &ElfRela,
    ) where
        M: TargetMemory,
    {
        let reloc_at_addr = (sections[in_section] as u32) + r.offset;
        let existing_data = memory.read_u32(reloc_at_addr);

        // Hack: determine how faerie decides on a relocation type.
        let kind = if existing_data == 0 {
            Some(ElfRelaType::RArmAbs32)
        } else if existing_data == 0xea000000 || existing_data == 0xeb000000 {
            Some(ElfRelaType::RArmJmp)
        } else {
            Some(ElfRelaType::RArmLdrPCG0)
        };

        let symbol = &symbols[r.sym()];
        eprintln!(
            "R {kind:?} {symbol:?} {in_section} {reloc_at_addr:08x} reloc {r:?} = {existing_data:08x}"
        );

        match kind {
            Some(ElfRelaType::RArmJmp) => {
                // Straight signed 24.
                let val_s = (symbol.st_value + sections[symbol.st_shndx as usize]) as i32;
                eprintln!(
                    "relocate jmp targeting section at {:x}",
                    sections[symbol.st_shndx as usize]
                );
                let val_p = (sections[in_section] + r.offset) as i32;
                let val_a = r.addend;
                let final_value =
                    (((((val_s - val_p + val_a) - 4) >> 2) & 0xffffff) as u32) | existing_data;
                eprintln!("S {val_s:08x} P {val_p:08x} A {val_a:08x} => {final_value:08x}");
                memory.write_u32(reloc_at_addr, existing_data | final_value);
            }
            Some(_) => {
                // R_ARM_ABS32 = S + A
                let val_s = (symbol.st_value + sections[symbol.st_shndx as usize]) as i32;
                let val_a = r.addend;
                let final_value = if val_a < 0 {
                    val_s - -val_a
                } else {
                    val_s + val_a
                };
                eprintln!("S {val_s:08x} A {val_a:08x} => {final_value:08x}");
                memory.write_i32(reloc_at_addr, final_value + existing_data as i32);
            }
            _ => todo!(),
        }
    }

    // Return the symbol list from the elf executable.
    pub fn get_symbols(&self) -> HashMap<String, EmuSymbolInfo> {
        if !self.symbol_string_table.is_empty() {
            let get_string = |idx: u32| -> Vec<u8> {
                self.symbol_string_table
                    .iter()
                    .skip(idx as usize)
                    .take_while(|b| *b != &b'\0')
                    .copied()
                    .collect()
            };
            let mut result = HashMap::new();
            for es in self.symbols.iter() {
                let sym_string = get_string(es.st_name);
                let raw = String::from_utf8_lossy(&sym_string).to_string();
                if raw.is_empty() {
                    continue;
                }
                if let Some(target_section) = self.elf.section_header_nth(es.st_shndx as usize) {
                    let sym_info = EmuSymbolInfo {
                        stripped: raw.clone(),
                        raw: raw.clone(),
                        address: self.sections[es.st_shndx as usize] + es.st_value,
                    };
                    if target_section
                        .flags()
                        .contains(SectionHeaderFlags::SHF_EXECINSTR)
                    {
                        let stripped = if let Some(stripped) =
                            raw.strip_prefix('_').and_then(|s| s.split('_').next())
                        {
                            stripped.to_string()
                        } else {
                            raw.clone()
                        };
                        let mut function_sym_info = sym_info.clone();
                        function_sym_info.stripped = stripped.clone();
                        result.insert(raw, function_sym_info.clone());
                        result.insert(stripped, function_sym_info);
                    } else if raw.starts_with("_$_") {
                        // Hash -> renamed function alias data entries.
                        result.insert(raw, sym_info);
                    }
                }
            }
            return result;
        }

        HashMap::default()
    }

    pub fn load<M>(&self, memory: &mut M)
    where
        M: TargetMemory,
    {
        // Collect relocation sections and set loaded data.
        for (i, s) in self.elf.section_header_iter().enumerate() {
            let section_addr = self.sections[i];
            if s.flags().contains(SectionHeaderFlags::SHF_ALLOC) {
                if let Some(content) = s.content() {
                    memory.write_data(content, section_addr);
                }
                eprintln!("{i} {s:?}");
                eprintln!("load section {} at {section_addr:08x}", i);
            }
        }

        for (rs, rd) in self.relocs.rela.iter() {
            for r in rd.content.iter() {
                self.apply_reloc(memory, &self.sections, &self.symbols, *rs, r);
            }
        }
    }
}

fn read_rela(content: &[u8], offset: usize) -> ElfRela {
    ElfRela {
        offset: read_u32(content, offset),
        info: read_u32(content, offset + 4),
        addend: read_i32(content, offset + 8),
    }
}

fn read_reloc_content(content: &[u8], entry_size: usize) -> Vec<ElfRela> {
    let mut result = Vec::new();
    for i in 0..(content.len() / entry_size) {
        result.push(read_rela(content, i * entry_size));
    }

    result
}

fn read_sym(content: &[u8], offset: usize) -> ElfSym {
    ElfSym {
        st_name: read_u32(content, offset),
        st_value: read_u32(content, offset + 4),
        st_size: read_u32(content, offset + 8),
        st_info: content[offset + 12],
        st_other: content[offset + 13],
        st_shndx: read_u16(content, offset + 14),
    }
}

fn read_sym_content(content: &[u8], entry_size: usize) -> Vec<ElfSym> {
    let mut result = Vec::new();
    for i in 0..(content.len() / entry_size) {
        result.push(read_sym(content, i * entry_size));
    }

    result
}
