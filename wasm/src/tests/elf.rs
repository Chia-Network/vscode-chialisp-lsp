use std::fs;
use std::path::PathBuf;
use std::rc::Rc;

use crate::arm_gdb::{build_elf, ArmGdbCompilerOpts};
use crate::interfaces::FSFileReader;
use chialisp::compiler::comptypes::HasCompilerOptsDelegation;

const PROGRAM: &str = "\
(mod (X)\n\
  (defun F (X Y)\n\
    (if X\n\
      (F (- X 1) Y)\n\
      (+ X Y)))\n\
  (F X 3)\n\
)\n";

/// ARM `add r0, r0, #0`, the named-function NOP anchor.
const NOP_ANCHOR: [u8; 4] = [0x00, 0x00, 0x80, 0xe2];

struct TempWork {
    path: PathBuf,
}

impl TempWork {
    fn new(label: &str) -> Self {
        let path = std::env::temp_dir().join(format!(
            "chialisp-arm-gdb-{label}-{}-{}",
            std::process::id(),
            label.len()
        ));
        fs::create_dir_all(&path).expect("create temp dir");
        TempWork { path }
    }
}

impl Drop for TempWork {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.path);
    }
}

fn opts_for(dir: &PathBuf, include_paths: Vec<String>) -> ArmGdbCompilerOpts {
    ArmGdbCompilerOpts::new(
        dir.join("prog.clsp").to_str().expect("utf8 path"),
        include_paths,
        Rc::new(FSFileReader::new()),
    )
}

#[test]
fn file_reader_resolves_absolute_include_path_and_missing_include() {
    let work = TempWork::new("reader");
    let absolute = work.path.join("abs.clib");
    let relative_name = "rel.clib";
    let relative = work.path.join(relative_name);
    fs::write(&absolute, "(defconstant ABS 1)\n").unwrap();
    fs::write(&relative, "(defconstant REL 2)\n").unwrap();

    let opts = opts_for(&work.path, vec![work.path.to_str().unwrap().to_string()]);
    let absolute_text = absolute.to_str().unwrap().to_string();

    let (resolved, bytes) = opts
        .override_read_new_file("prog.clsp".to_string(), absolute_text.clone())
        .expect("absolute include");
    assert_eq!(resolved, absolute_text);
    assert_eq!(bytes, b"(defconstant ABS 1)\n");

    let (resolved, bytes) = opts
        .override_read_new_file("prog.clsp".to_string(), relative_name.to_string())
        .expect("include-path resolution");
    assert_eq!(resolved, relative.to_str().unwrap());
    assert_eq!(bytes, b"(defconstant REL 2)\n");

    let missing = opts
        .override_read_new_file("prog.clsp".to_string(), "missing.clib".to_string())
        .expect_err("missing include");
    let missing_text = format!("{missing:?}");
    assert!(
        missing_text.contains("could not find missing.clib"),
        "{missing_text}"
    );
}

#[test]
fn missing_include_fails_during_elf_build() {
    let work = TempWork::new("missing");
    let program_path = work.path.join("prog.clsp");
    let err = build_elf(
        program_path.to_str().unwrap(),
        "(mod () (include \"missing.clib\") ())",
        "()",
        vec![work.path.to_str().unwrap().to_string()],
        Rc::new(FSFileReader::new()),
        "prog.arm.elf",
    )
    .expect_err("include failure");
    assert!(
        err.contains("missing.clib"),
        "compiler error should name the missing include: {err}"
    );
}

#[derive(Clone, Debug)]
struct LineRow {
    address: u64,
    file: String,
    line: u64,
    is_stmt: bool,
}

struct ElfSection {
    name: String,
    address: u64,
    data: Vec<u8>,
}

fn read_u16(elf: &[u8], offset: usize) -> u16 {
    u16::from_le_bytes([elf[offset], elf[offset + 1]])
}

fn read_u32(elf: &[u8], offset: usize) -> u32 {
    u32::from_le_bytes([
        elf[offset],
        elf[offset + 1],
        elf[offset + 2],
        elf[offset + 3],
    ])
}

fn elf_sections(elf: &[u8]) -> Vec<ElfSection> {
    let section_offset = read_u32(elf, 32) as usize;
    let section_size = read_u16(elf, 46) as usize;
    let section_count = read_u16(elf, 48) as usize;
    let name_index = read_u16(elf, 50) as usize;
    let name_offset = read_u32(elf, section_offset + name_index * section_size + 16) as usize;
    let section_name = |offset: usize| {
        let start = name_offset + offset;
        let end = start + elf[start..].iter().position(|byte| *byte == 0).unwrap();
        String::from_utf8_lossy(&elf[start..end]).into_owned()
    };
    (0..section_count)
        .map(|index| {
            let base = section_offset + index * section_size;
            let size = read_u32(elf, base + 20) as usize;
            let offset = read_u32(elf, base + 16) as usize;
            ElfSection {
                name: section_name(read_u32(elf, base) as usize),
                address: read_u32(elf, base + 12) as u64,
                data: elf.get(offset..offset + size).unwrap_or(&[]).to_vec(),
            }
        })
        .collect()
}

fn section_bytes<'a>(sections: &'a [ElfSection], name: &str) -> &'a [u8] {
    sections
        .iter()
        .find(|section| section.name == name)
        .map(|section| section.data.as_slice())
        .unwrap_or(&[])
}

fn load_dwarf<'a>(
    sections: &'a [ElfSection],
) -> gimli::Dwarf<gimli::EndianSlice<'a, gimli::LittleEndian>> {
    gimli::Dwarf::load(|id: gimli::SectionId| -> Result<_, gimli::Error> {
        Ok(gimli::EndianSlice::new(
            section_bytes(sections, id.name()),
            gimli::LittleEndian,
        ))
    })
    .expect("load dwarf")
}

fn dwarf_rows(elf: &[u8]) -> Vec<LineRow> {
    let sections = elf_sections(elf);
    let dwarf = load_dwarf(&sections);

    let mut rows = Vec::new();
    let mut units = dwarf.units();
    while let Some(header) = units.next().expect("unit") {
        let unit = dwarf.unit(header).expect("debug unit");
        let Some(program) = unit.line_program.clone() else {
            continue;
        };
        let mut program_rows = program.rows();
        while let Some((header, row)) = program_rows.next_row().expect("line row") {
            if row.end_sequence() {
                continue;
            }
            let Some(file) = row.file(header) else {
                continue;
            };
            let path = dwarf
                .attr_string(&unit, file.path_name())
                .map(|value| value.to_string_lossy().into_owned())
                .unwrap_or_default();
            let line = row.line().map(|value| value.get()).unwrap_or(0);
            rows.push(LineRow {
                address: row.address(),
                file: path,
                line,
                is_stmt: row.is_stmt(),
            });
        }
    }
    rows
}

#[test]
fn test_generate_classic_program_ensure_function_breakable() {
    let work = TempWork::new("break");
    let program_path = work.path.join("prog.clsp");
    fs::write(&program_path, PROGRAM).unwrap();
    let elf_name = work.path.join("prog.arm.elf");
    let (elf, synthetic_source, symbols) = build_elf(
        program_path.to_str().unwrap(),
        PROGRAM,
        "(5)",
        vec![work.path.to_str().unwrap().to_string()],
        Rc::new(FSFileReader::new()),
        elf_name.to_str().unwrap(),
    )
    .expect("build elf");

    assert!(
        elf.len() > 64,
        "elf is unexpectedly small: {} bytes",
        elf.len()
    );
    assert_eq!(&elf[0..4], b"\x7fELF");
    assert!(
        synthetic_source.contains("prog.clsp"),
        "synthetic source should retain original locations:\n{synthetic_source}"
    );
    assert!(
        symbols
            .values()
            .any(|value| value.contains(':') || value == "F"),
        "symbol table should contain source locations or a function name, got {symbols:?}"
    );
    assert!(
        !work.path.join("test.elf").exists(),
        "ELF generation must not write a workspace test.elf"
    );

    let rows = dwarf_rows(&elf);
    assert!(
        subprograms(&elf).iter().any(|program| program.name == "F"),
        "DWARF should name helper F"
    );
    let sections = elf_sections(&elf);
    let section = sections
        .iter()
        .find(|section| section.name == ".text.F")
        .expect("section .text.F");
    let code = section.data.as_slice();
    let entry = section.address;
    assert_eq!(
        &code[0..4],
        &[0x00, 0x48, 0x2d, 0xe9],
        "function F should open with push {{fp, lr}}"
    );
    let anchor_index = code
        .chunks_exact(4)
        .take(12)
        .position(|word| word == NOP_ANCHOR)
        .expect("NOP anchor in the function prologue");
    let anchor = entry + (anchor_index as u64) * 4;
    assert!(
        rows.iter().any(|row| {
            row.is_stmt
                && row.address >= entry
                && row.address <= anchor
                && (row.file == "prog.clsp" || row.file.ends_with("/prog.clsp"))
        }),
        "function entry should have a breakpointable source statement at or before {anchor:#x}"
    );
    let section_end = entry + code.len() as u64;
    assert!(
        rows.iter().any(|row| {
            row.address > anchor
                && row.address < section_end
                && row.file != "prog.clsp"
                && !row.file.ends_with("/prog.clsp")
        }),
        "function F should have a later line after the NOP anchor"
    );
}

struct Subprogram {
    name: String,
    low_pc: u64,
    high_pc: u64,
}

fn subprograms(elf: &[u8]) -> Vec<Subprogram> {
    let sections = elf_sections(elf);
    let dwarf = load_dwarf(&sections);
    let mut found = Vec::new();
    let mut units = dwarf.units();
    while let Some(header) = units.next().expect("unit") {
        let unit = dwarf.unit(header).expect("debug unit");
        let mut entries = unit.entries();
        while let Some((_, entry)) = entries.next_dfs().expect("die") {
            if entry.tag() != gimli::DW_TAG_subprogram {
                continue;
            }
            let Some(name_attr) = entry.attr(gimli::DW_AT_name).expect("name attr") else {
                continue;
            };
            let name = dwarf
                .attr_string(&unit, name_attr.value())
                .map(|value| value.to_string_lossy().into_owned())
                .unwrap_or_default();
            let Some(low_attr) = entry.attr(gimli::DW_AT_low_pc).expect("low attr") else {
                continue;
            };
            let Some(high_attr) = entry.attr(gimli::DW_AT_high_pc).expect("high attr") else {
                continue;
            };
            let gimli::AttributeValue::Addr(low_pc) = low_attr.value() else {
                continue;
            };
            let high_pc = match high_attr.value() {
                gimli::AttributeValue::Addr(addr) => addr,
                gimli::AttributeValue::Udata(offset) => low_pc + offset,
                _ => continue,
            };
            found.push(Subprogram {
                name,
                low_pc,
                high_pc,
            });
        }
    }
    found
}
