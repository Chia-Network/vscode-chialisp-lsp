use std::fs;
use std::rc::Rc;
use crate::arm_gdb::build_elf;
use crate::interfaces::IFileReader;
use crate::tests::FSFileReader;

#[test]
fn test_generate_classic_program_ensure_function_breakable() {
    let elf = build_elf(
        "test.clsp",
        "(mod (X)\n  (defun F (X Y) (if X (F (- X 1)) (+ X Y)))\n  (F X 3)\n)",
        "(5)",
        vec![],
        Rc::new(FSFileReader::new()),
        "test.elf",
    ).unwrap();
    fs::write("test.elf", elf.0).unwrap();
    todo!();
}
