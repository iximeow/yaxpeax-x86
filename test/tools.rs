// for masm testing:
// * `dumpbin` is a "bytes to masm-like text" function and,
// * `masm` is a "masm-like text to bytes" function.
pub use imp::{dumpbin, masm};

#[cfg(not(any(target_os="linux", target_os="windows")))]
mod imp {
    // stub impls to at least run tests on other platforms, but some
    // test-specific features will of course fail at runtime..
    pub fn dumpbin(bytes: &[u8]) -> Result<String, String> {
        panic!("no impl of dumpbin on this target");
    }

    pub fn masm(text: &str) -> Result<Vec<u8>, String> {
        panic!("no impl of masm on this target");
    }
}

#[cfg(target_os="linux")]
mod imp {
    pub fn dumpbin(bytes: &[u8]) -> Result<String, String> {
        // how very sad:
        // > wibo: call reached missing import GetModuleHandleExA from kernel32
        panic!("wibo can't run dumpbin right now");
    }

    pub fn masm(text: &str) -> Result<Vec<u8>, String> {
        panic!("have not implemented wibo/masm on linux yet");
    }
}

#[cfg(target_os="windows")]
mod imp {
    use std::fmt::{Write as FmtWrite};
    use std::io::Write;
    use std::process::Command;
    use crate::tools::carve_dumpbin_stdout;

    use tempfile::NamedTempFile;

    pub fn dumpbin(bytes: &[u8]) -> Result<String, String> {
        let mut source = String::new();

        source.push_str(".code\n");
        source.push_str("\n");
        source.push_str("start::\n");
        source.push_str("    db ");
        let mut printed = false;
        for byte in bytes {
            if printed {
                source.push_str(", ");
            }
            write!(source, "0{:02x}h", byte).expect("can write");
            printed = true;
        }
        source.push_str("\nEND\n");
        eprintln!("SOURCE FOLLOWS: {source}");

        let mut tempfile = NamedTempFile::new().unwrap();
        tempfile.write_all(source.as_bytes()).expect("can write source");
        let sourcepath = tempfile.into_temp_path();
        let mut objpath = sourcepath.to_path_buf();
        objpath.add_extension(".o");

        let out = Command::new("..\\..\\tools\\ml64.exe")
            .args(&["/c", "/Fo", &objpath.display().to_string(), &sourcepath.display().to_string()])
            .output()
            .expect("can run");
        if !out.status.success() {
            eprintln!("failed to assemble {bytes:x?}:");
            eprintln!("stdout: {}", std::str::from_utf8(out.stdout.as_slice()).expect("valid utf8"));
            eprintln!("stderr: {}", std::str::from_utf8(out.stderr.as_slice()).expect("valid utf8"));
            panic!("failed to ml64.exe");
        }

        let out = Command::new("..\\..\\tools\\dumpbin.exe")
            .args(&["/disasm:wide", &objpath.display().to_string()])
            .output()
            .expect("can run");
        if !out.status.success() {
            eprintln!("failed to dumpbin {bytes:x?}:");
            eprintln!("stdout: {}", std::str::from_utf8(out.stdout.as_slice()).expect("valid utf8"));
            eprintln!("stderr: {}", std::str::from_utf8(out.stderr.as_slice()).expect("valid utf8"));
            panic!("failed to dumpbin.exe");
        }


        let dumpbin_out = std::str::from_utf8(out.stdout.as_slice()).expect("valid utf8");

        let dumpbin_interesting = carve_dumpbin_stdout(dumpbin_out).expect("works");
        let dumpbin_interesting = dumpbin_interesting[0];

        let end =   "  0000000000000000: 0F C7 0F                                     ".len();
        if dumpbin_interesting.len() <= end {
            return Err("no instruction".to_string());
        }

        let asm_line = dumpbin_interesting[end..].trim();
        let text = if let Some(idx) = asm_line.find("  ") {
            let opcode = &asm_line[..idx];
            let operands = &asm_line[idx..].trim();
            format!("{opcode} {operands}")
        } else {
            asm_line.to_string()
        };
        let text = text.replace(",", ", ")
            .replace("+", " + ")
            .replace("-", " - ")
            .replace("*", " * ")
            .replace(" + FFFFFFFFCCBBAA34h", " - 334455CCh") // with apologies to future-me, replace common negative displacements into more normal values...
            .replace("rn - sae", "rn-sae")
            .replace("rd - sae", "rd-sae")
            .replace("ru - sae", "ru-sae")
            .replace("rz - sae", "rz-sae")
            .replace(" oword ", " xmmword ");

        eprintln!("testcase bytes {:x?} -> dumpbin -> text {}", bytes, text);

        Ok(text)
    }

    pub fn masm(text: &str) -> Result<Vec<u8>, String> {
        let mut source = String::new();

        source.push_str(".code\n");
        source.push_str("\n");
        source.push_str("start::\n");
        writeln!(source, "    {text}").expect("ok");
        source.push_str("\nEND\n");
/*
        eprintln!("assembling SOURCE:");
        eprintln!("{source}");
        eprintln!("-----");
*/
        let mut tempfile = NamedTempFile::new().unwrap();
        tempfile.write_all(source.as_bytes()).expect("can write source");
        tempfile.as_file().sync_data().expect("can sync");
        let sourcepath = tempfile.into_temp_path();
        let mut objpath = sourcepath.to_path_buf();
        objpath.add_extension(".o");

        let out = Command::new("..\\..\\tools\\ml64.exe")
            .args(&["/c", "/Fo", &objpath.display().to_string(), &sourcepath.display().to_string()])
            .output()
            .expect("can run");
        if !out.status.success() {
            eprintln!("failed to assemble {text:x?}:");
            eprintln!("stdout: {}", std::str::from_utf8(out.stdout.as_slice()).expect("valid utf8"));
            eprintln!("stderr: {}", std::str::from_utf8(out.stderr.as_slice()).expect("valid utf8"));
            panic!("failed to ml64.exe as part of masm()");
        }

        let out = Command::new("..\\..\\tools\\dumpbin.exe")
            .args(&["/disasm:wide", &objpath.display().to_string()])
            .output()
            .expect("can run");
        if !out.status.success() {
            eprintln!("failed to dumpbin {text:x?}:");
            eprintln!("stdout: {}", std::str::from_utf8(out.stdout.as_slice()).expect("valid utf8"));
            eprintln!("stderr: {}", std::str::from_utf8(out.stderr.as_slice()).expect("valid utf8"));
            panic!("failed to dumpbin.exe");
        }

        let dumpbin_out = std::str::from_utf8(out.stdout.as_slice()).expect("valid utf8");

        let dumpbin_interesting = carve_dumpbin_stdout(dumpbin_out).expect("works");

        let end =   "  0000000000000000: 0F C7 0F                                     ".len();
        let start = "  0000000000000000: ".len();
        let hex_text = dumpbin_interesting[0][start..end].trim();
        let mut bytes = Vec::new();
        for f in hex_text.split(" ") {
            let b = u8::from_str_radix(f, 16).expect("should be able to parse");
            bytes.push(b);
        }

        eprintln!("testcase \"{}\" -> masm -> dumpbin -> bytes {:x?}", text, bytes);

        Ok(bytes)
    }
}

fn carve_dumpbin_stdout(stdout: &str) -> Result<Vec<&str>, String> {
    let lines = stdout.split("\n").collect::<Vec<_>>();

    let mut disasm_start = match lines.iter().enumerate().find_map(|(idx, line)| {
        if line.starts_with("File Type: COFF OBJECT") {
            Some(idx)
        } else {
            None
        }
    }) {
        Some(start) => start,
        None => {
            eprintln!("failed to find COFF OBJECT line in dumpbin output:");
            eprintln!("{}", stdout);
            return Err("failed to find disassembly start in dumpbin output".to_string());
        }
    };

    let disasm_end = match lines.iter().enumerate().find_map(|(idx, line)| {
        if line.starts_with("  Summary") {
            Some(idx)
        } else {
            None
        }
    }) {
        Some(end) => end,
        None => {
            eprintln!("failed to find Summary line in dumpbin output:");
            eprintln!("{}", stdout);
            return Err("failed to find disassembly end in dumpbin output".to_string());
        }
    };

    if lines[disasm_start + 2].starts_with("$$00") {
        // the line is probably an invented label for rip-relative addressing.
        disasm_start += 1;
    }

    let disasm_lines = &lines[disasm_start + 2..disasm_end - 2 + 1];

    if disasm_lines.len() > 1 {
        eprintln!("disassembly is too complex");
        eprintln!("{}", stdout);
        return Err("got multiple lines of disassembly".to_string());
    }

    // eprintln!("dumpbin returns: {:?}", disasm_lines);

    Ok(disasm_lines.to_vec())
}
