// for masm testing:
// * `dumpbin` is a "bytes to masm-like text" function and,
// * `masm` is a "masm-like text to bytes" function.
pub use imp::{dumpbin, masm};

/// configure the various test tools for a desired bitness.
// some tools (dumpbin) do not require any particular configuration as they take their cues from
// object file headers. other tools (masm) not only need different source directives, but are
// entirely different executables for different modes.
#[derive(Copy, Clone, Debug)]
pub enum CodeModel {
    // nothing even tries to run masm in 16-bit mode (yet..?)
    #[allow(dead_code)]
    Bits16,
    Bits32,
    Bits64,
}

#[cfg(not(any(target_os="linux", target_os="windows")))]
mod imp {
    use super::CodeModel;

    // stub impls to at least run tests on other platforms, but some
    // test-specific features will of course fail at runtime..
    pub fn dumpbin(_bytes: &[u8], _codeness: CodeModel) -> Result<String, String> {
        panic!("no impl of dumpbin on this target");
    }

    pub fn masm(_text: &str, _codeness: CodeModel) -> Result<Vec<u8>, String> {
        panic!("no impl of masm on this target");
    }
}

#[cfg(target_os="linux")]
mod imp {
    use super::CodeModel;

    pub fn dumpbin(_bytes: &[u8], _codeness: CodeModel) -> Result<String, String> {
        // how very sad:
        // > wibo: call reached missing import GetModuleHandleExA from kernel32
        panic!("wibo can't run dumpbin right now");
    }

    pub fn masm(_text: &str, _codeness: CodeModel) -> Result<Vec<u8>, String> {
        panic!("have not implemented wibo/masm on linux yet");
    }
}

#[cfg(target_os="windows")]
mod imp {
    use super::CodeModel;

    use std::fmt::{Write as FmtWrite};
    use std::io::Write;
    use std::process::Command;
    use crate::tools::carve_dumpbin_stdout;

    use tempfile::NamedTempFile;

    pub fn dumpbin(bytes: &[u8], codeness: CodeModel) -> Result<String, String> {
        let replacement = match codeness {
            CodeModel::Bits16 => {
                // no replacements for 16-bit yet, because masm is little-tested in 16-bit mode..
                None
            }
            CodeModel::Bits32 => {
                match bytes {
                    &[0xf1] => Some("int 1"),  // dumpbin does not know how to decode f1...
                    &[0xe5, 0x99] => Some("in eax, 99h"), // this is a MASM/dumpbin bug. see notes on testcase.
                    &[0xe7, 0x99] => Some("out 99h, eax"), // this is a MASM/dumpbin bug. see notes on testcase.
                    // dumpbin prints the instruction as if it was encoded in 32-bit form regardless of object file, so overrule it.
                    &[0xf3, 0x0f, 0xc7, 0xfd] => Some("rdpid ebp"),
                    &[0x0f, 0x18, 0xc0] => Some("nop eax"), // dumpbin would love to call this "prefetchnta eax" ???
                    &[0x0f, 0x18, 0xcc] => Some("nop esp"), // dumpbin would love to call this "prefetchnta esp" ???
                    &[0x0f, 0x18, 0x20] => Some("nop zmmword ptr [eax]"), // getting around dumpbin knowing about prefetchrst2..
                    &[0x0f, 0x19, 0x20] => Some("nop dword ptr [eax]"), // dumpbin doesn't know about 0f19..
                    &[0x0f, 0x1a, 0x20] => Some("nop dword ptr [eax]"), // dumpbin wants to call this bndldx, yax doesn't do MPX yet
                    &[0x0f, 0x1b, 0x20] => Some("nop dword ptr [eax]"), // dumpbin wants to call this bndstx, yax doesn't do MPX yet
                    &[0x0f, 0x1c, 0x20] => Some("nop dword ptr [eax]"), // dumpbin doesn't know about 0f1c..
                    &[0x0f, 0x1d, 0x20] => Some("nop dword ptr [eax]"), // dumpbin doesn't know about 0f1d..
                    &[0x0f, 0x1e, 0x20] => Some("nop dword ptr [eax]"), // dumpbin doesn't know about 0f1e..
                    &[0xf2, 0x66, 0x66, 0x0f, 0x10, 0xc0] => Some("movsd xmm0, xmm0"), // dumpbin does not love the prefixes
                    &[0xf3, 0x0f, 0x1e, 0xfc] => Some("nop"), // dumpbin does not tolerate this at all, redirect into a boring nop.
                    &[0x0f, 0x43, 0xec] => Some("cmovnb ebp, esp"), // dumpbin writes it "cmovae" instead of yax's cmovnb.
                    &[0x2e, 0x36, 0x0f, 0x18, 0xe7] => Some("nop edi"), // dumpbin reports a mildly-confused prefetchrst2 rdi (even in 32-bit mode!)
                    &[0x0f, 0xbe, 0x83, 0xb4, 0x00, 0x00, 0x00] => {
                        Some("movsx eax, byte ptr [ebx + 0B4h]") // dumpbin uses %016 formatting, masm happily accepts shorter.
                    },
                    &[0x62, 0xd2, 0x7e, 0x28, 0x3a, 0xca] => {
                        Some("vpbroadcastmw2d ymm1, k2") // dumpbin inexplicably uses "bnd2" as the source register??? MSVC 14.52.36328.
                    },
                    &[0x62, 0xd2, 0x7e, 0x08, 0x28, 0xc2] => {
                        Some("vpmovm2b xmm0, k2") // dumpbin inexplicably uses "bnd2" as the source register??? MSVC 14.52.36328.
                    },
                    &[0x0f, 0x0d, 0x00] => {
                        // dumpbin interprets this as the 3DNow!-style PREFETCH instruction, but we're definitely not 3dnow..
                        Some("nop zmmword ptr [eax]")
                    }
                    &[0xc4, 0x03, 0x3d, 0x0a, 0xca, 0x77] => {
                        // dumpbin can't deal with this instruction..
                        Some("vroundss xmm9, xmm8, xmm10, 77h")
                    }
                    &[0xc4, 0x03, 0x3d, 0x0b, 0xca, 0x77] => {
                        // dumpbin can't deal with this instruction..
                        Some("vroundsd xmm9, xmm8, xmm10, 77h")
                    }
                    &[0x66, 0x0f, 0xd6, 0x01] => {
                        // dumpbin really wants to use mmword here, but i really don't.
                        Some("movq qword ptr [ecx], xmm0")
                    }
                    // dumpbin doesn't know how to decode, and masm doesn't know how to *en*code, ud0.
                    &[0x66, 0x0f, 0xff, 0xc1] => Some("ud0 eax, ecx"),
                    &[0xf2, 0x0f, 0xff, 0xc1] => Some("ud0 eax, ecx"),
                    &[0xf3, 0x0f, 0xff, 0xc1] => Some("ud0 eax, ecx"),
                    &[0x66, 0x0f, 0xff, 0x01] => Some("ud0 eax, dword ptr [ecx]"),
                    &[0x0f, 0xff, 0x6b, 0xac] => Some("ud0 ebp, dword ptr [ebx - 54h]"),
                    // dumpbin does not tolerate the pointless prefixes.
                    &[0x36, 0x36, 0x2e, 0x0f, 0x38, 0xf9, 0x55, 0x3e] => Some("movdiri dword ptr cs:[ebp + 3Eh], edx"),
                    // dumpbin does not tolerate the pointless prefixes.
                    &[0x36, 0x26, 0x66, 0x0f, 0x38, 0xf8, 0xad, 0x0b, 0x08, 0x29, 0x07] => Some("movdir64b ebp, zmmword ptr es:[ebp + 729080Bh]"),
                    // dumpbin does not tolerate the pointless prefixes.
                    &[0x36, 0x26, 0x66, 0x67, 0x0f, 0x38, 0xf8, 0xad, 0x0b, 0x08] => Some("movdir64b bp, zmmword ptr es:[di + 80Bh]"),
                    // and again
                     &[0xf2, 0xf2, 0x2e, 0x36, 0x0f, 0x38, 0xf8, 0x83, 0x09, 0x1c, 0x9d, 0x3f] => Some("enqcmd eax, zmmword ptr ss:[ebx + 3F9D1C09h]"),
                    // and again.
                    &[0x3e, 0x64, 0xf3, 0x64, 0x0f, 0x38, 0xf8, 0x72, 0x54] => Some("enqcmds esi, zmmword ptr fs:[edx + 54h]"),
                    // prefixes confuse dumpbin again
                    &[0x66, 0xf3, 0x0f, 0x01, 0xe8] => Some("setssbsy"),
                    // prefixes confuse dumpbin again
                    &[0x66, 0xf3, 0x0f, 0x01, 0xea] => Some("saveprevssp"),
                    // prefixes confuse dumpbin again
                    &[0xf3, 0x66, 0x0f, 0x01, 0xe8] => Some("setssbsy"), // TODO: yax does not support `serialize` (yet)
                    // prefixes confuse dumpbin again
                    &[0xf3, 0x66, 0x0f, 0x01, 0xea] => Some("saveprevssp"),
                    // prefixes confuse dumpbin again
                    &[0xf3, 0x66, 0x0f, 0x01, 0x29] => Some("rstorssp qword ptr [ecx]"),
                    // dumpbin writes the repne, but it doesn't do anything..
                    &[0xf2, 0x0f, 0x21, 0xc8] => Some("mov eax, dr1"),
                    // dumpbin writes the rep, but it doesn't do anything..
                    &[0xf3, 0x0f, 0x21, 0xc8] => Some("mov eax, dr1"),
                    // dumpbin prints out an xacquire when there is no lock prefix, which causes the instruction to grow a lock prefix in round-tripping. no!
                    &[0xf2, 0x0f, 0xc0, 0xcc] => Some("xadd ah, cl"),
                    // dumpbin prints out an rep when one is not allowed, which fails round-tripping. yax doesn't.
                    &[0xf3, 0x0f, 0xc0, 0xcc] => Some("xadd ah, cl"),
                    // dumpbin prints out an xacquire when there is no lock prefix, which causes the instruction to grow a lock prefix in round-tripping. no!
                    &[0xf2, 0x0f, 0xc1, 0xcc] => Some("xadd esp, ecx"),
                    // dumpbin prints out an rep when one is not allowed, which fails round-tripping. yax doesn't.
                    &[0xf3, 0x0f, 0xc1, 0xcc] => Some("xadd esp, ecx"),
                    // dumpbin prints out an xacquire when there is no lock prefix, which causes the instruction to grow a lock prefix in round-tripping. no!
                    &[0xf2, 0x0f, 0xc7, 0x0f] => Some("cmpxchg8b qword ptr [edi]"),
                    // dumpbin prints out an rep when one is not allowed, which fails round-tripping. yax doesn't.
                    &[0xf3, 0x0f, 0xc7, 0x0f] => Some("cmpxchg8b qword ptr [edi]"),
                    // prefixes again..
                    &[0x66, 0x36, 0x0f, 0x3a, 0xce, 0x8c, 0x56, 0x9e, 0x82, 0xd1, 0xbe, 0xad] => Some("gf2p8affineqb xmm1, xmmword ptr ss:[esi + edx * 2 - 412E7D62h], 0ADh"),
                    &[0x3e, 0x64, 0x64, 0x66, 0x0f, 0x3a, 0xcf, 0xba, 0x13, 0x23, 0x04, 0xba, 0x6b] => Some("gf2p8affineinvqb xmm7, xmmword ptr fs:[edx - 45FBDCEDh], 6Bh"),
                    &[0xf3, 0x64, 0x2e, 0x65, 0x0f, 0x38, 0xdc, 0xe8] => Some("loadiwkey xmm5, xmm0"),
                    // dumpbin prints out the memory size as "oword", but yax uses "xmmword". masm accepts either.
                    &[0x66, 0x0f, 0x38, 0x80, 0x01] => Some("invept eax, xmmword ptr [ecx]"),
                    // dumpbin prints out the memory size as "oword", but yax uses "xmmword". masm accepts either.
                    &[0x66, 0x0f, 0x38, 0x81, 0x01] => Some("invvpid eax, xmmword ptr [ecx]"),
                    // dumpbin uses absolute branch destinations, but yax uses relative.
                    // (and we print jnb instead of jae)
                    &[0x73, 0x31] => Some("jnb $+33h"),
                    // dumpbin uses absolute branch destinations, but yax uses relative.
                    &[0x72, 0x5a] => Some("jb $+5Ch"),
                    // dumpbin uses absolute branch destinations, but yax uses relative.
                    &[0x72, 0xf0] => Some("jb $-0Eh"),
                    // dumpbin uses absolute branch destinations, but yax uses relative.
                    &[0xe8, 0x01, 0x00, 0x00, 0x00] => Some("call $+6"),
                    // dumpbin uses absolute branch destinations, but yax uses relative.
                    &[0xe8, 0x80, 0x00, 0x00, 0x00] => Some("call near ptr $+85h"),
                    // dumpbin uses absolute branch destinations, but yax uses relative.
                    &[0xe8, 0xff, 0xff, 0xff, 0xff] => Some("call $+4"),
                    // dumpbin uses absolute branch destinations, but yax uses relative.
                    &[0xe9, 0x01, 0x00, 0x00, 0x00] => Some("jmp $+6"),
                    // dumpbin uses absolute branch destinations, but yax uses relative. there's also the near ptr nonsense..
                    &[0xe9, 0x80, 0x00, 0x00, 0x00] => Some("jmp near ptr $+85h"),
                    // dumpbin uses absolute branch destinations, but yax uses relative.
                    &[0xe9, 0xff, 0xff, 0xff, 0xff] => Some("jmp $+4"),
                    // dumpbin uses absolute branch destinations, but yax uses relative.
                    &[0x0f, 0x86, 0x8b, 0x01, 0x00, 0x00] => Some("jna $+191h"),
                    // dumpbin uses absolute branch destinations, but yax uses relative.
                    &[0x0f, 0x85, 0x3b, 0x25, 0x00, 0x00] => Some("jnz $+2541h"),
                    // dumpbin uses absolute branch destinations, but yax uses relative.
                    &[0x74, 0x47] => Some("jz $+49h"),
                    // dumpbin prints a ds: since this is an absolute address..
                    &[0xff, 0x15, 0x7e, 0x72, 0x24, 0x00] => Some("call dword ptr [0024727Eh]"),
                    // dumpbin uses a really wide displacement .. for laughs..
                    &[0xff, 0x24, 0xcd, 0x70, 0xa0, 0xbc, 0x01] => Some("jmp dword ptr [ecx * 8 + 1BCA070h]"),
                    // dumpbin uses a really wide displacement .. for laughs..
                    &[0xff, 0x14, 0xcd, 0x70, 0xa0, 0xbc, 0x01] => Some("call dword ptr [ecx * 8 + 1BCA070h]"),
                    // dumpbin uses absolute branch destinations, but yax uses relative.
                    &[0xe0, 0x12] => Some("loopnz $+14h"),
                    // dumpbin uses absolute branch destinations, but yax uses relative.
                    &[0xe1, 0x12] => Some("loopz $+14h"),
                    // dumpbin uses absolute branch destinations, but yax uses relative.
                    &[0xe2, 0x12] => Some("loop $+14h"),
                    // dumpbin uses absolute branch destinations, but yax uses relative.
                    &[0xe3, 0x12] => Some("jecxz $+14h"),
                    // dumpbin uses absolute branch destinations, but yax uses relative.
                    &[0xe3, 0xf0] => Some("jecxz $-0Eh"),
                    // dumpbin uses absolute branch destinations, but yax uses relative.
                    &[0x67, 0xe3, 0x12] => Some("jcxz $+15h"),
                    // dumpbin uses absolute branch destinations, but yax uses relative.
                    &[0x67, 0xe3, 0xf0] => Some("jcxz $-0Dh"),
                    // dumpbin dislikes prefixes.
                    &[0x66, 0xf2, 0x0f, 0x79, 0xcf] => Some("insertq xmm1, xmm7"),
                    &[0xf6, 0x05, 0x2c, 0x9b, 0xff, 0xff, 0x01] => Some("test byte ptr [0FFFF9B2Ch], 1"),
                    // yax uses wider immediates
                    &[0x3d, 0x01, 0xf0, 0xff, 0xff] => Some("cmp eax, 0FFFFF001h"),
                    // dumpbin gets the size wrong
                    &[0x62, 0xf2, 0xfd, 0x0f, 0x8a, 0x62, 0xf2] => Some("vcompresspd xmmword ptr [edx - 70h]{k7}, xmm4"),
                    // TODO: yax doesn't know about rdssp{d,q}?
                    &[0xf3, 0x0f, 0x1e, 0x0f] => Some("nop"),
                    // yax won't mention the pointless repne prefix
                    &[0xf2, 0x0f, 0x06] => Some("clts"),
                    // yax won't mention the pointless repne prefix
                    &[0xf2, 0x0f, 0x07] => Some("sysret"),
                    // dumpbin spells this mmword
                    &[0x0f, 0x6f, 0x00] => Some("movq mm0, qword ptr [eax]"),
                    &[0x66, 0x2e, 0xf2, 0xf0, 0x0f, 0xbb, 0x13] => Some("xacquire lock btc word ptr cs:[ebx], dx"),
                    // dumpbin prints with more.. flourish
                    &[0x66, 0x2e, 0x0f, 0x1f, 0x84, 0x00, 0x00, 0x00, 0x00, 0x00] => Some("nop word ptr cs:[eax + eax]"),
                    // disp is wider from dumpbin
                    &[0x0f, 0xfc, 0xaf, 0x40, 0x38, 0x25, 0xbf] => Some("paddb mm5, mmword ptr [edi - 40DAC7C0h]"),
                    &[0xc7, 0xf8, 0x10, 0x12, 0x34, 0x56] => Some("xbegin $+56341216h"),
                    &[0x66, 0xc7, 0xf8, 0x10, 0x12] => Some("xbegin $+1215h"),
                    &[0x26, 0x36, 0x0f, 0x0f, 0x70, 0xfb, 0x0c] => Some("pi2fw mm6, qword ptr ss:[eax - 5]"), // more prefix confusion..
                    // prefixes confuse dumpbin, and dumpbin says "qword" where we use mmword. masm accepts either
                    &[0x3e, 0xf3, 0x2e, 0xf2, 0x0f, 0x0f, 0x64, 0x93, 0x93, 0xa4] => Some("pfmax mm4, mmword ptr cs:[ebx + edx * 4 - 6Dh]"),
                    // dumpbin shows this as a non-rip-rel offset :(
                    &[0x0f, 0xe5, 0x3d, 0xaa, 0xbb, 0xcc, 0x77] => Some("pmulhw mm7, qword ptr [77CCBBAAh]"),
                    // dumpbin confused about prefixes once again
                    &[0x66, 0x3e, 0x26, 0x2e, 0x2e, 0x0f, 0x38, 0x2a, 0x2b] => Some("movntdqa xmm5, xmmword ptr cs:[ebx]"),
                    // prefixes.. cs: isn't real in 64-bit mode
                    &[0x66, 0x2e, 0x67, 0x0f, 0x3a, 0x0d, 0xb8, 0xf0, 0x2f, 0x7c] => Some("blendpd xmm7, xmmword ptr cs:[bx + si + 2FF0h], 7Ch"),
                    // prefixes confuse dumpbin
                    &[0x66, 0x66, 0x64, 0x3e, 0x0f, 0x38, 0x23, 0x9d, 0x69, 0x0f, 0xa8, 0x2d] => Some("pmovsxwd xmm3, qword ptr [ebp + 2DA80F69h]"),
                    // prefixes confuse dumpbin
                    &[0x2e, 0x66, 0x26, 0x64, 0x0f, 0x3a, 0x21, 0x0b, 0xb1] => Some("insertps xmm1, dword ptr fs:[ebx], 0B1h"),
                    // prefixes confuse dumpbin
                    &[0x66, 0x26, 0x0f, 0x3a, 0x42, 0x96, 0x74, 0x29, 0x96, 0xf9, 0x6a] => Some("mpsadbw xmm2, xmmword ptr es:[esi - 669D68Ch], 6Ah"),
                    // prefixes confuse dumpbin
                    &[0x67, 0x26, 0x66, 0x65, 0x0f, 0x38, 0x3f, 0x9d, 0xcc, 0x03] => Some("pmaxud xmm3, xmmword ptr gs:[di + 3CCh]"),
                    // prefixes confuse dumpbin
                    &[0x67, 0x66, 0x65, 0x3e, 0x0f, 0x6d, 0xd1] => Some("punpckhqdq xmm2, xmm1"),
                    // prefixes confuse dumpbin
                    &[0xf2, 0x3e, 0x26, 0x67, 0x0f, 0xf0, 0xa0, 0x1b, 0x5f] => Some("lddqu xmm4, xmmword ptr es:[bx + si + 5F1Bh]"),
                    // prefixes confuse dumpbin
                    &[0x2e, 0x3e, 0x66, 0x3e, 0x0f, 0x3a, 0x41, 0x30, 0x48] => Some("dppd xmm6, xmmword ptr [eax], 48h"),
                    // again prefixes confuse dumpbin
                    &[0x65, 0x66, 0x66, 0x64, 0x0f, 0x38, 0xdb, 0x0f] => Some("aesimc xmm1, xmmword ptr fs:[edi]"),
                    // dumpbin prints the order backwards =|
                    &[0x65, 0xf0, 0x87, 0x0f] => Some("lock xchg dword ptr gs:[edi], ecx"),
                    // dumpbin knows about "fstpnce" as "fstp1", but masm does not.
                    // since this is an undocumented instruction anyway, decode it ourselves..
                    &[0xd9, 0xdb] => Some("fstpnce st(3), st(0)"),
                    // dumpbin calls this "fcom2", but it's just an undocumented fcom alias. this round-trips to a different instruction but it's at least.. kinda right.
                    &[0xdc, 0xd3] => Some("fcom st(3)"),
                    // dumpbin calls this "fcomp3", but it's just an undocumented fcomp alias. this round-trips to a different instruction but it's at least.. kinda right.
                    &[0xdc, 0xdb] => Some("fcomp st(3)"),
                    // dumpbin calls this "fxch4", but it's just an undocumented fxch alias. this round-trips to a different instruction but it's at least.. kinda right.
                    &[0xdd, 0xcb] => Some("fxch st(3)"),
                    // dumpbin calls this "fcomp5", but it's just an undocumented fcomp alias. this round-trips to a different instruction but it's at least.. kinda right.
                    &[0xde, 0xd3] => Some("fcomp st(3)"),
                    // dumpbin calls this "fxch7", but it's just an undocumented fxch alias. this round-trips to a different instruction but it's at least.. kinda right.
                    &[0xdf, 0xcb] => Some("fxch st(3)"),
                    // dumpbin calls this "fstp8", but it's just an undocumented fstp alias. this round-trips to a different instruction but it's at least.. kinda right.
                    &[0xdf, 0xd3] => Some("fstp st(3)"),
                    // dumpbin calls this "fstp9", but it's just an undocumented fstp alias. this round-trips to a different instruction but it's at least.. kinda right.
                    &[0xdf, 0xdb] => Some("fstp st(3)"),
                    &[0xf2, 0x0f, 0xbc, 0xd3] => Some("bsf edx, ebx"),
                    // mov abs in 32-bit mode gets a ds: prefix even though that's the default. masm does not need this prefix, so we round-trip fine without it.
                    &[0xa0, 0x93, 0x62, 0xc4, 0x00] => Some("mov al, byte ptr [00C46293h]"),
                    &[0x67, 0xa0, 0x93, 0x62] => Some("mov al, byte ptr [00006293h]"),
                    &[0xa1, 0x93, 0x62, 0xc4, 0x00] => Some("mov eax, dword ptr [00C46293h]"),
                    &[0x67, 0xa1, 0x93, 0x62] => Some("mov eax, dword ptr [00006293h]"),
                    &[0xa2, 0x93, 0x62, 0xc4, 0x00] => Some("mov byte ptr [00C46293h], al"),
                    &[0x67, 0xa2, 0x93, 0x62] => Some("mov byte ptr [00006293h], al"),
                    &[0xa3, 0x93, 0x62, 0xc4, 0x00] => Some("mov dword ptr [00C46293h], eax"),
                    &[0x67, 0xa3, 0x93, 0x62] => Some("mov dword ptr [00006293h], eax"),
                    &[0x33, 0x05, 0x78, 0x56, 0x34, 0x12] => Some("xor eax, dword ptr [12345678h]"),
                    &[0x33, 0x04, 0x25, 0x11, 0x22, 0x33, 0x44] => Some("xor eax, dword ptr [44332211h]"),
                    &[0x33, 0x04, 0xe5, 0x11, 0x22, 0x33, 0x44] => Some("xor eax, dword ptr [44332211h]"),
                    &[0x33, 0x34, 0x25, 0x20, 0x30, 0x40, 0x50] => Some("xor esi, dword ptr [50403020h]"),
                    &[0xa0, 0xc0, 0xb0, 0xa0, 0x90] => Some("mov al, byte ptr [90A0B0C0h]"),
                    &[0x67, 0xa0, 0xc0, 0xb0] => Some("mov al, byte ptr [0B0C0h]"),
                    &[0x67, 0xa1, 0xc0, 0xb0] => Some("mov eax, dword ptr [0000B0C0h]"),
                    &[0x66, 0x67, 0xa1, 0xc0, 0xb0] => Some("mov ax, word ptr [0000B0C0h]"),
                    // same for wrssd
                    &[0x3e, 0x0f, 0x38, 0xf6, 0x23] => Some("wrssd dword ptr [ebx], esp"),
                    // dumpbin believes that rex.w works even in 32-bit code, thus prints `rorx rax, ..`. haha what a dingus
                    &[0xc4, 0xe3, 0xfb, 0xf0, 0x01, 0x05] => Some("rorx eax, dword ptr [ecx], 5"),
                    &[0xc4, 0xe2, 0xe3, 0xf5, 0x07] => Some("pdep eax, ebx, dword ptr [edi]"),
                    &[0xc4, 0xe2, 0xe3, 0xf6, 0x07] => Some("mulx eax, ebx, dword ptr [edi]"),
                    &[0xc4, 0xe2, 0xe3, 0xf7, 0x01] => Some("shrx eax, dword ptr [ecx], ebx"),
                    &[0xc4, 0xe2, 0xe2, 0xf5, 0x07] => Some("pext eax, ebx, dword ptr [edi]"),
                    &[0xc4, 0xe2, 0xe2, 0xf7, 0x01] => Some("sarx eax, dword ptr [ecx], ebx"),
                    &[0xc4, 0xe2, 0xe0, 0xf5, 0x07] => Some("bzhi eax, dword ptr [edi], ebx"),
                    &[0xc4, 0xe2, 0xe1, 0xf7, 0x01] => Some("shlx eax, dword ptr [ecx], ebx"),
                    &[0xc4, 0xe2, 0xe0, 0xf2, 0x01] => Some("andn eax, ebx, dword ptr [ecx]"),
                    &[0xc4, 0xe2, 0xf8, 0xf3, 0x09] => Some("blsr eax, dword ptr [ecx]"),
                    &[0xc4, 0xe2, 0xf8, 0xf3, 0x11] => Some("blsmsk eax, dword ptr [ecx]"),
                    &[0xc4, 0xe2, 0xf8, 0xf3, 0x19] => Some("blsi eax, dword ptr [ecx]"),
                    &[0xc4, 0xe2, 0xe0, 0xf7, 0x01] => Some("bextr eax, dword ptr [ecx], ebx"),
                    &[0xc4, 0xc3, 0x39, 0x0c, 0xca, 0x77] => Some("vblendps xmm1, xmm0, xmm2, 77h"),
                    // just have to decide we know better than dumpbin: masm does not accept an absolute far call/far jump destination,
                    // so we definitely can't round-trip by following dumpbin. dumpbin doesn't use hex suffixes here, instead printing
                    // "6655:44332211" as the destination. this is technically not ambiguous since `:` is a hint that this is a absolute
                    // far address and that both numbers are base 16, but that's ... subtle and easy to miss. so add some h's.
                    &[0x9a, 0x11, 0x22, 0x33, 0x44, 0x55, 0x66] => Some("call 6655h:44332211h"),
                    &[0x66, 0x9a, 0x11, 0x22, 0x33, 0x44] => Some("call 4433h:2211h"),
                    // terribly unfortunate: masm reasonably encodes this instruction as a 32-bit offset, which causes yax to spell the offset
                    // as 0000AA55 instead of AA55. override dumpbin to use the (worse) encoding for the sake of matching with the test.
                    &[0x66, 0x67, 0x8b, 0x0e, 0x55, 0xaa] => Some("mov cx, word ptr [0AA55h]"),
                    // inexplicably, dumpbin spells this "aamb", for .. ascii adjust after multiplcation (byte) ???
                    // additionally, masm does not accept an integer operand: it only supports `aam 10` as in d4 0a. so.. bummer.
                    &[0xd4, 0x01] => Some("aam 1"),
                    // same as above
                    &[0xd5, 0x01] => Some("aad 1"),
                    // dunno why dumpbin doesn't like this one..
                    &[0xc5, 0b1_1111_100, 0x2e, 0b00_001_010] => Some("vucomiss xmm1, dword ptr [edx]"),
                    &[0xc5, 0b1_1111_100, 0x2f, 0b00_001_010] => Some("vcomiss xmm1, dword ptr [edx]"),
                    _ => None,
                }
            }
            CodeModel::Bits64 => {
                match bytes {
                    &[0xf1] => Some("int 1"),  // dumpbin does not know how to decode f1...
                    &[0x4f, 0xe5, 0x99] => Some("in eax, 99h"), // this is a MASM/dumpbin bug. see notes on testcase.
                    &[0x4f, 0xe7, 0x99] => Some("out 99h, eax"), // this is a MASM/dumpbin bug. see notes on testcase.
                    // dumpbin prints the instruction as if it was encoded in 32-bit form regardless of object file, so overrule it.
                    &[0xf3, 0x0f, 0xc7, 0xfd] => Some("rdpid rbp"),
                    &[0x0f, 0x18, 0xc0] => Some("nop eax"), // dumpbin would love to call this "prefetchnta rax" ???
                    &[0x0f, 0x18, 0xcc] => Some("nop esp"), // dumpbin would love to call this "prefetchnta rsp" ???
                    &[0x0f, 0x18, 0x20] => Some("nop zmmword ptr [rax]"), // getting around dumpbin knowing about prefetchrst2..
                    &[0x4f, 0x0f, 0x18, 0x20] => Some("nop zmmword ptr [r8]"), // getting around dumpbin knowing about prefetchrst2..
                    &[0x2e, 0x36, 0x47, 0x0f, 0x18, 0xe7] => Some("nop r15d"), // getting around dumpbin knowing about prefetchrst2..
                    &[0x0f, 0x19, 0x20] => Some("nop dword ptr [rax]"), // dumpbin doesn't know about 0f19..
                    &[0x0f, 0x1a, 0x20] => Some("nop dword ptr [rax]"), // dumpbin wants to call this bndldx, yax doesn't do MPX yet
                    &[0x0f, 0x1b, 0x20] => Some("nop dword ptr [rax]"), // dumpbin wants to call this bndstx, yax doesn't do MPX yet
                    &[0x0f, 0x1c, 0x20] => Some("nop dword ptr [rax]"), // dumpbin doesn't know about 0f1c..
                    &[0x0f, 0x1d, 0x20] => Some("nop dword ptr [rax]"), // dumpbin doesn't know about 0f1d..
                    &[0x0f, 0x1e, 0x20] => Some("nop dword ptr [rax]"), // dumpbin doesn't know about 0f1e..
                    &[0xf2, 0x66, 0x66, 0x4d, 0x0f, 0x10, 0xc0] => Some("movsd xmm8, xmm8"), // dumpbin does not love the prefixes
                    &[0x4f, 0x66, 0x0f, 0x28, 0x00] => Some("movapd xmm0, xmmword ptr [rax]"), // dumpbin does not love the prefixes
                    &[0x67, 0x4f, 0x66, 0x0f, 0x28, 0x00] => Some("movapd xmm0, xmmword ptr [eax]"), // dumpbin does not love the prefixes
                    &[0xf3, 0x0f, 0x1e, 0xfc] => Some("nop"), // dumpbin does not tolerate this at all, redirect into a boring nop.
                    &[0x4d, 0x0f, 0x43, 0xec] => Some("cmovnb r13, r12"), // dumpbin writes it "cmovae" instead of yax's cmovnb.
                    &[0x65, 0x4c, 0x89, 0x04, 0x25, 0xa8, 0x01, 0x00, 0x00] => {
                        Some("mov qword ptr gs:[000001A8h], r8") // dumpbin uses %016 formatting, masm happily accepts shorter.
                    },
                    &[0x0f, 0xbe, 0x83, 0xb4, 0x00, 0x00, 0x00] => {
                        Some("movsx eax, byte ptr [rbx + 0B4h]") // dumpbin uses %016 formatting, masm happily accepts shorter.
                    },
                    &[0x46, 0x63, 0xc1] => Some("movsxd r8, ecx"), // dumpbin writes 32-bit destinations for this, but masm accepts either?
                    &[0x62, 0xd2, 0x7e, 0x28, 0x3a, 0xca] => {
                        Some("vpbroadcastmw2d ymm1, k2") // dumpbin inexplicably uses "bnd2" as the source register??? MSVC 14.52.36328.
                    },
                    &[0x62, 0xd2, 0x7e, 0x08, 0x28, 0xc2] => {
                        Some("vpmovm2b xmm0, k2") // dumpbin inexplicably uses "bnd2" as the source register??? MSVC 14.52.36328.
                    },
                    &[0x0f, 0x01, 0x51, 0xff] => {
                        Some("lgdt fword ptr [rcx - 1]") // dumpbin prints this as "tbyte", which masm does not accept.
                    },
                    &[0x0f, 0x01, 0x59, 0xff] => {
                        Some("lidt fword ptr [rcx - 1]") // dumpbin prints this as "tbyte", which masm does not accept.
                    },
                    &[0x2e, 0x67, 0x65, 0x2e, 0x46, 0x0f, 0x01, 0xff] => {
                        Some("tlbsync") // dumpbin does not exactly tolerate the extra prefixes.
                    },
                    &[0x0f, 0x0d, 0x00] => {
                        // dumpbin interprets this as the 3DNow!-style PREFETCH instruction, but we're definitely not 3dnow..
                        Some("nop zmmword ptr [rax]")
                    }
                    &[0xf2, 0x41, 0x0f, 0xbc, 0xd3] => {
                        // masm doesn't like the extra prefix
                        Some("bsf edx, r11d")
                    }
                    &[0x4f, 0x4e, 0x00, 0xcc] => {
                        // masm doesn't like the extra prefix
                        Some("add spl, r9b")
                    }
                    &[0xc4, 0x03, 0x3d, 0x0a, 0xca, 0x77] => {
                        // dumpbin can't deal with this instruction..
                        Some("vroundss xmm9, xmm8, xmm10, 77h")
                    }
                    &[0xc4, 0x03, 0x3d, 0x0b, 0xca, 0x77] => {
                        // dumpbin can't deal with this instruction..
                        Some("vroundsd xmm9, xmm8, xmm10, 77h")
                    }
                    &[0x66, 0x4f, 0x0f, 0x6e, 0x9c, 0x9c, 0x34, 0xaa, 0xbb, 0xcc] => {
                        // dumpbin really wants to use mmword here, but i really don't.
                        Some("movq xmm11, qword ptr [r12 + r11 * 4 - 334455CCh]")
                    }
                    &[0x66, 0x0f, 0xd6, 0x01] => {
                        // dumpbin really wants to use mmword here, but i really don't.
                        Some("movq qword ptr [rcx], xmm0")
                    }
                    &[0x66, 0x4f, 0x0f, 0xd7, 0xc1] => {
                        // yax bug? default operand size is 64-bit in 64-bit mode, so the register should be r8?
                        Some("pmovmskb r8d, xmm9")
                    }
                    // dumpbin doesn't know how to decode, and masm doesn't know how to *en*code, ud0.
                    &[0x66, 0x0f, 0xff, 0xc1] => Some("ud0 eax, ecx"),
                    &[0xf2, 0x0f, 0xff, 0xc1] => Some("ud0 eax, ecx"),
                    &[0xf3, 0x0f, 0xff, 0xc1] => Some("ud0 eax, ecx"),
                    &[0x66, 0x0f, 0xff, 0x01] => Some("ud0 eax, dword ptr [rcx]"),
                    &[0x66, 0x4f, 0x0f, 0xff, 0xc1] => Some("ud0 r8d, r9d"),
                    &[0x4c, 0x0f, 0xff, 0x6b, 0xac] => Some("ud0 r13d, dword ptr [rbx - 54h]"),
                    // dumpbin does not tolerate the pointless rex prefix.
                    &[0x4f, 0x66, 0x0f, 0x2a, 0xcf] => Some("cvtpi2pd xmm1, mm7"),
                    // dumpbin does not tolerate the pointless rex prefix.
                    &[0x4f, 0xf3, 0x0f, 0x2a, 0xcf] => Some("cvtsi2ss xmm1, edi"),
                    // dumpbin does not tolerate the pointless rex prefix.
                    &[0x4f, 0xf2, 0x0f, 0x2a, 0xcf] => Some("cvtsi2sd xmm1, edi"),
                    // dumpbin does not tolerate the pointless rex prefix.
                    &[0x4f, 0xf2, 0x0f, 0x2a, 0x00] => Some("cvtsi2sd xmm0, dword ptr [rax]"),
                    // dumpbin does not tolerate the pointless rex prefix.
                    &[0x4f, 0xf3, 0x0f, 0x2a, 0x00] => Some("cvtsi2ss xmm0, dword ptr [rax]"),
                    // dumpbin does not tolerate the pointless rex prefix.
                    &[0x4f, 0x66, 0x0f, 0x2a, 0x00] => Some("cvtpi2pd xmm0, mmword ptr [rax]"),
                    // dumpbin does not tolerate the pointless prefixes.
                    &[0x36, 0x36, 0x2e, 0x0f, 0x38, 0xf9, 0x55, 0x3e] => Some("movdiri dword ptr [rbp + 3Eh], edx"),
                    // dumpbin does not tolerate the pointless prefixes.
                    &[0x36, 0x26, 0x66, 0x0f, 0x38, 0xf8, 0xad, 0x0b, 0x08, 0x29, 0x07] => Some("movdir64b rbp, zmmword ptr [rbp + 729080Bh]"),
                    // dumpbin does not tolerate the pointless prefixes.
                    &[0x36, 0x26, 0x66, 0x67, 0x0f, 0x38, 0xf8, 0xad, 0x0b, 0x08, 0x29, 0x07] => Some("movdir64b ebp, zmmword ptr [ebp + 729080Bh]"),
                    // dumpbin is super confused about the prefixing.
                    &[0xf2, 0xf2, 0x2e, 0x36, 0x47, 0x0f, 0x38, 0xf8, 0x83, 0x09, 0x1c, 0x9d, 0x3f] => Some("enqcmd r8, zmmword ptr [r11 + 3F9D1C09h]"),
                    // and again.
                    &[0x3e, 0x64, 0xf3, 0x64, 0x0f, 0x38, 0xf8, 0x72, 0x54] => Some("enqcmds rsi, zmmword ptr fs:[rdx + 54h]"),
                    // dumpbin shows a ds prefix; this is tolerated by masm but is kinda incorrect in x86_64. either way masm accepts it though.
                    &[0x3e, 0x4f, 0x0f, 0x38, 0xf6, 0x23] => Some("wrssq qword ptr [r11], r12"),
                    // prefixes confuse dumpbin again
                    &[0x66, 0xf3, 0x0f, 0x01, 0xe8] => Some("setssbsy"),
                    // prefixes confuse dumpbin again
                    &[0x66, 0xf3, 0x0f, 0x01, 0xea] => Some("saveprevssp"),
                    // prefixes confuse dumpbin again
                    &[0xf3, 0x66, 0x0f, 0x01, 0xe8] => Some("setssbsy"), // TODO: yax does not support `serialize` (yet)
                    // prefixes confuse dumpbin again
                    &[0xf3, 0x66, 0x0f, 0x01, 0xea] => Some("saveprevssp"),
                    // prefixes confuse dumpbin again
                    &[0xf3, 0x66, 0x0f, 0x01, 0x29] => Some("rstorssp qword ptr [rcx]"),
                    // dumpbin prints out an xacquire when there is no lock prefix, which causes the instruction to grow a lock prefix in round-tripping. no!
                    &[0xf2, 0x0f, 0xc0, 0xcc] => Some("xadd ah, cl"),
                    // dumpbin prints out an rep when one is not allowed, which fails round-tripping. yax doesn't.
                    &[0xf3, 0x0f, 0xc0, 0xcc] => Some("xadd ah, cl"),
                    // dumpbin prints out an xacquire when there is no lock prefix, which causes the instruction to grow a lock prefix in round-tripping. no!
                    &[0xf2, 0x0f, 0xc1, 0xcc] => Some("xadd esp, ecx"),
                    // dumpbin prints out an rep when one is not allowed, which fails round-tripping. yax doesn't.
                    &[0xf3, 0x0f, 0xc1, 0xcc] => Some("xadd esp, ecx"),
                    // dumpbin prints out an xacquire when there is no lock prefix, which causes the instruction to grow a lock prefix in round-tripping. no!
                    &[0xf2, 0x0f, 0xc7, 0x0f] => Some("cmpxchg8b qword ptr [rdi]"),
                    // dumpbin prints out an rep when one is not allowed, which fails round-tripping. yax doesn't.
                    &[0xf3, 0x0f, 0xc7, 0x0f] => Some("cmpxchg8b qword ptr [rdi]"),
                    // dumpbin prints out the memory size as "oword", but yax uses "xmmword". masm accepts either.
                    &[0x4f, 0x0f, 0xc7, 0x0f] => Some("cmpxchg16b xmmword ptr [r15]"),
                    // dumpbin prints out the memory size as "oword", but yax uses "xmmword". masm accepts either.
                    &[0x66, 0x4f, 0x0f, 0xc7, 0x0f] => Some("cmpxchg16b xmmword ptr [r15]"),
                    // dumpbin prints out repne prefix, which does not round-trip.
                    &[0xf2, 0x4f, 0x0f, 0xc7, 0x0f] => Some("cmpxchg16b xmmword ptr [r15]"),
                    // dumpbin prints out rep prefix, which does not round-trip.
                    &[0xf3, 0x4f, 0x0f, 0xc7, 0x0f] => Some("cmpxchg16b xmmword ptr [r15]"),
                    // prefixes again..
                    &[0x3e, 0x64, 0x64, 0x66, 0x4e, 0x0f, 0x3a, 0xcf, 0xba, 0x13, 0x23, 0x04, 0xba, 0x6b] => Some("gf2p8affineinvqb xmm15, xmmword ptr fs:[rdx - 45FBDCEDh], 6Bh"),
                    &[0x66, 0x36, 0x0f, 0x3a, 0xce, 0x8c, 0x56, 0x9e, 0x82, 0xd1, 0xbe, 0xad] => Some("gf2p8affineqb xmm1, xmmword ptr [rsi + rdx * 2 - 412E7D62h], 0ADh"),
                    &[0xf3, 0x64, 0x2e, 0x65, 0x0f, 0x38, 0xdc, 0xe8] => Some("loadiwkey xmm5, xmm0"),
                    // dumpbin prints out the memory size as "oword", but yax uses "xmmword". masm accepts either.
                    &[0x66, 0x0f, 0x38, 0x80, 0x01] => Some("invept rax, xmmword ptr [rcx]"),
                    // dumpbin prints out the memory size as "oword", but yax uses "xmmword". masm accepts either.
                    &[0x66, 0x0f, 0x38, 0x81, 0x01] => Some("invvpid rax, xmmword ptr [rcx]"),
                    // dumpbin uses absolute branch destinations, but yax uses relative.
                    // (and we print jnb instead of jae)
                    &[0x73, 0x31] => Some("jnb $+33h"),
                    // dumpbin uses absolute branch destinations, but yax uses relative.
                    &[0x72, 0x5a] => Some("jb $+5Ch"),
                    // dumpbin uses absolute branch destinations, but yax uses relative.
                    &[0x72, 0xf0] => Some("jb $-0Eh"),
                    // dumpbin uses absolute branch destinations, but yax uses relative.
                    &[0xe8, 0x01, 0x00, 0x00, 0x00] => Some("call $+6"),
                    // dumpbin uses absolute branch destinations, but yax uses relative.
                    &[0xe8, 0x80, 0x00, 0x00, 0x00] => Some("call near ptr $+85h"),
                    // dumpbin uses absolute branch destinations, but yax uses relative.
                    &[0xe8, 0xff, 0xff, 0xff, 0xff] => Some("call $+4"),
                    // dumpbin uses absolute branch destinations, but yax uses relative.
                    &[0xe9, 0x01, 0x00, 0x00, 0x00] => Some("jmp $+6"),
                    // dumpbin uses absolute branch destinations, but yax uses relative. there's also the near ptr nonsense..
                    &[0xe9, 0x80, 0x00, 0x00, 0x00] => Some("jmp near ptr $+85h"),
                    // dumpbin uses absolute branch destinations, but yax uses relative.
                    &[0xe9, 0xff, 0xff, 0xff, 0xff] => Some("jmp $+4"),
                    // dumpbin uses absolute branch destinations, but yax uses relative.
                    &[0x0f, 0x86, 0x8b, 0x01, 0x00, 0x00] => Some("jna $+191h"),
                    // dumpbin uses absolute branch destinations, but yax uses relative.
                    &[0x0f, 0x85, 0x3b, 0x25, 0x00, 0x00] => Some("jnz $+2541h"),
                    // dumpbin uses absolute branch destinations, but yax uses relative.
                    &[0x74, 0x47] => Some("jz $+49h"),
                    // dumpbin invents a label for laughs.
                    &[0xff, 0x15, 0x7e, 0x72, 0x24, 0x00] => Some("call qword ptr [$ + 24727Eh]"),
                    // dumpbin uses a really wide displacement .. for laughs..
                    &[0xff, 0x24, 0xcd, 0x70, 0xa0, 0xbc, 0x01] => Some("jmp qword ptr [rcx * 8 + 1BCA070h]"),
                    // dumpbin uses a really wide displacement .. for laughs..
                    &[0xff, 0x14, 0xcd, 0x70, 0xa0, 0xbc, 0x01] => Some("call qword ptr [rcx * 8 + 1BCA070h]"),
                    // dumpbin bug: 66-prefixed jmp/call does not pick 16-bit registers
                    &[0x66, 0xff, 0xe0] => Some("jmp rax"),
                    // dumpbin bug: 66-prefixed jmp/call does not pick 16-bit registers
                    &[0x66, 0xff, 0xd0] => Some("call rax"),
                    // dumpbin uses absolute branch destinations, but yax uses relative.
                    &[0xe0, 0x12] => Some("loopnz $+14h"),
                    // dumpbin uses absolute branch destinations, but yax uses relative.
                    &[0xe1, 0x12] => Some("loopz $+14h"),
                    // dumpbin uses absolute branch destinations, but yax uses relative.
                    &[0xe2, 0x12] => Some("loop $+14h"),
                    // dumpbin uses absolute branch destinations, but yax uses relative.
                    &[0xe3, 0x12] => Some("jrcxz $+14h"),
                    // dumpbin uses absolute branch destinations, but yax uses relative.
                    &[0xe3, 0xf0] => Some("jrcxz $-0Eh"),
                    // dumpbin uses absolute branch destinations, but yax uses relative.
                    &[0x67, 0xe3, 0x12] => Some("jecxz $+15h"),
                    // dumpbin uses absolute branch destinations, but yax uses relative.
                    &[0x67, 0xe3, 0xf0] => Some("jecxz $-0Dh"),
                    // dumpbin dislikes prefixes.
                    &[0x66, 0xf2, 0x0f, 0x79, 0xcf] => Some("insertq xmm1, xmm7"),
                    // rip-rel: oh dear
                    &[0xf6, 0x05, 0x2c, 0x9b, 0xff, 0xff, 0x01] => Some("test byte ptr [$ - 64D4h], 1"),
                    // yax uses wider immediates
                    &[0x3d, 0x01, 0xf0, 0xff, 0xff] => Some("cmp eax, 0FFFFFFFFFFFFF001h"),
                    // dumpbin doesn't print the $ of rip-rel :(
                    &[0x33, 0x05, 0x78, 0x56, 0x34, 0x12] => Some("xor eax, dword ptr [$ + 12345678h]"),
                    &[0x33, 0x81, 0x23, 0x01, 0x65, 0x43] => Some("xor eax, dword ptr [rcx + 43650123h]"),
                    &[0x48, 0x33, 0x05, 0x78, 0x56, 0x34, 0x12] => Some("xor rax, qword ptr [$ + 12345678h]"),
                    &[0x48, 0x33, 0x81, 0x23, 0x01, 0x65, 0x43] => Some("xor rax, qword ptr [rcx + 43650123h]"),
                    &[0x44, 0x33, 0x05, 0x78, 0x56, 0x34, 0x12] => Some("xor r8d, dword ptr [$ + 12345678h]"),
                    &[0x44, 0x33, 0x81, 0x23, 0x01, 0x65, 0x43] => Some("xor r8d, dword ptr [rcx + 43650123h]"),
                    &[0x45, 0x33, 0x05, 0x78, 0x56, 0x34, 0x12] => Some("xor r8d, dword ptr [$ + 12345678h]"),
                    &[0x45, 0x33, 0x81, 0x23, 0x01, 0x65, 0x43] => Some("xor r8d, dword ptr [r9 + 43650123h]"),
                    &[0x33, 0x04, 0x25, 0x11, 0x22, 0x33, 0x44] => Some("xor eax, dword ptr [44332211h]"),
                    &[0x41, 0x33, 0x04, 0x25, 0x11, 0x22, 0x33, 0x44] => Some("xor eax, dword ptr [44332211h]"),
                    &[0x33, 0x84, 0xa5, 0x11, 0x22, 0x33, 0x44] => Some("xor eax, dword ptr [rbp + 44332211h]"),
                    &[0x41, 0x33, 0x84, 0xa5, 0x11, 0x22, 0x33, 0x44] => Some("xor eax, dword ptr [r13 + 44332211h]"),
                    &[0x33, 0x04, 0xe5, 0x11, 0x22, 0x33, 0x44] => Some("xor eax, dword ptr [44332211h]"),
                    &[0x41, 0x33, 0x04, 0xe5, 0x11, 0x22, 0x33, 0x44] => Some("xor eax, dword ptr [44332211h]"),
                    &[0x42, 0x33, 0x34, 0x25, 0x20, 0x30, 0x40, 0x50] => Some("xor esi, dword ptr [r12 + 50403020h]"),
                    &[0x43, 0x33, 0x34, 0x25, 0x20, 0x30, 0x40, 0x50] => Some("xor esi, dword ptr [r12 + 50403020h]"),
                    &[0x42, 0x33, 0xb4, 0x25, 0x20, 0x30, 0x40, 0x50] => Some("xor esi, dword ptr [rbp + r12 + 50403020h]"),
                    &[0x43, 0x33, 0xb4, 0x25, 0x20, 0x30, 0x40, 0x50] => Some("xor esi, dword ptr [r13 + r12 + 50403020h]"),
                    // dumpbin gets the size wrong
                    &[0x62, 0xf2, 0xfd, 0x0f, 0x8a, 0x62, 0xf2] => Some("vcompresspd xmmword ptr [rdx - 70h]{k7}, xmm4"),
                    // TODO: yax doesn't know about rdssp{d,q}?
                    &[0xf3, 0x0f, 0x1e, 0x0f] => Some("nop"),
                    // yax won't mention the pointless repne prefix
                    &[0xf2, 0x0f, 0x06] => Some("clts"),
                    // yax won't mention the pointless repne prefix
                    &[0xf2, 0x0f, 0x07] => Some("sysret"),
                    // dumpbin spells this mmword
                    &[0x0f, 0x6f, 0x00] => Some("movq mm0, qword ptr [rax]"),
                    &[0x66, 0x2e, 0xf2, 0xf0, 0x0f, 0xbb, 0x13] => Some("xacquire lock btc word ptr [rbx], dx"),
                    // dumpbin handles this right (like this!) but the output is weird to parse
                    &[0x45, 0x66, 0x0f, 0x21, 0xc8] => Some("mov rax, dr1"),
                    // dumpbin says repne, but that doesn't round-trip.
                    &[0x45, 0xf2, 0x0f, 0x21, 0xc8] => Some("mov rax, dr1"),
                    // dumpbin says rep, but that doesn't round-trip.
                    &[0x45, 0xf3, 0x0f, 0x21, 0xc8] => Some("mov rax, dr1"),
                    // dumpbin prints with more.. flourish
                    &[0x66, 0x2e, 0x0f, 0x1f, 0x84, 0x00, 0x00, 0x00, 0x00, 0x00] => Some("nop word ptr [rax + rax]"),
                    // disp is wider from dumpbin
                    &[0x48, 0x8d, 0xa4, 0xc7, 0x20, 0x00, 0x00, 0x12] => Some("lea rsp, [rdi + rax * 8 + 12000020h]"),
                    &[0x0f, 0xfc, 0xaf, 0x40, 0x38, 0x25, 0xbf] => Some("paddb mm5, mmword ptr [rdi - 40DAC7C0h]"),
                    &[0xc7, 0xf8, 0x10, 0x12, 0x34, 0x56] => Some("xbegin $+56341216h"),
                    &[0x66, 0xc7, 0xf8, 0x10, 0x12] => Some("xbegin $+1215h"),
                    &[0xf2, 0xf3, 0x66, 0x65, 0x4f, 0x25, 0x9b, 0x5e, 0xda, 0x44] => Some("and rax, 44DA5E9Bh"),
                    &[0x65, 0x66, 0x66, 0x64, 0x48, 0x0f, 0x38, 0xdb, 0x0f] => Some("aesimc xmm1, xmmword ptr fs:[rdi]"),
                    &[0x26, 0x36, 0x0f, 0x0f, 0x70, 0xfb, 0x0c] => Some("pi2fw mm6, qword ptr [rax - 5]"), // more prefix confusion..
                    // prefixes confuse dumpbin, and dumpbin says "qword" where we use mmword. masm accepts either
                    &[0x3e, 0xf3, 0x2e, 0xf2, 0x0f, 0x0f, 0x64, 0x93, 0x93, 0xa4] => Some("pfmax mm4, mmword ptr [rbx + rdx * 4 - 6Dh]"),
                    // dumpbin calls this movq?
                    &[0x4f, 0x0f, 0x7e, 0xcf] => Some("movd r15, mm1"),
                    // dumpbin shows this as a wide register but it doesn't *really* matter and yax uses 32-bit always.
                    &[0x4f, 0x0f, 0xd7, 0xcf] => Some("pmovmskb r9d, mm7"),
                    // dumpbin shows this as a non-rip-rel offset :(
                    &[0x0f, 0xe5, 0x3d, 0xaa, 0xbb, 0xcc, 0x77] => Some("pmulhw mm7, qword ptr [$ + 77CCBBAAh]"),
                    // dumpbin confused about prefixes once again
                    &[0x66, 0x3e, 0x26, 0x2e, 0x2e, 0x0f, 0x38, 0x2a, 0x2b] => Some("movntdqa xmm5, xmmword ptr [rbx]"),
                    // prefixes.. cs: isn't real in 64-bit mode
                    &[0x66, 0x2e, 0x67, 0x0f, 0x3a, 0x0d, 0xb8, 0xf0, 0x2f, 0x7c, 0xf0, 0x63] => Some("blendpd xmm7, xmmword ptr [eax - 0F83D010h], 63h"),
                    // prefixes confuse dumpbin
                    &[0x66, 0x66, 0x64, 0x3e, 0x0f, 0x38, 0x23, 0x9d, 0x69, 0x0f, 0xa8, 0x2d] => Some("pmovsxwd xmm3, qword ptr fs:[rbp + 2DA80F69h]"),
                    // prefixes confuse dumpbin
                    &[0x2e, 0x66, 0x26, 0x64, 0x49, 0x0f, 0x3a, 0x21, 0x0b, 0xb1] => Some("insertps xmm1, dword ptr fs:[r11], 0FFFFFFFFFFFFFFB1h"),
                    // prefixes confuse dumpbin
                    &[0x66, 0x26, 0x45, 0x0f, 0x3a, 0x42, 0x96, 0x74, 0x29, 0x96, 0xf9, 0x6a] => Some("mpsadbw xmm10, xmmword ptr [r14 - 669D68Ch], 6Ah"),
                    // prefixes confuse dumpbin
                    &[0x67, 0x26, 0x66, 0x65, 0x0f, 0x38, 0x3f, 0x9d, 0xcc, 0x03, 0xb3, 0xfa] => Some("pmaxud xmm3, xmmword ptr gs:[ebp - 54CFC34h]"),
                    // prefixes confuse dumpbin
                    &[0x67, 0x66, 0x65, 0x3e, 0x0f, 0x6d, 0xd1] => Some("punpckhqdq xmm2, xmm1"),
                    // prefixes confuse dumpbin
                    &[0x2e, 0x66, 0x40, 0x0f, 0x3a, 0x0d, 0x40, 0x2d, 0x57] => Some("blendpd xmm0, xmmword ptr [rax + 2Dh], 57h"),
                    // prefixes confuse dumpbin
                    &[0xf2, 0x3e, 0x26, 0x67, 0x0f, 0xf0, 0xa0, 0x1b, 0x5f, 0xcd, 0xd7] => Some("lddqu xmm4, xmmword ptr [eax - 2832A0E5h]"),
                    // prefixes confuse dumpbin
                    &[0x2e, 0x3e, 0x66, 0x3e, 0x49, 0x0f, 0x3a, 0x41, 0x30, 0x48] => Some("dppd xmm6, xmmword ptr [r8], 48h"),
                    // dumpbin prints the order backwards =|
                    &[0x65, 0xf0, 0x87, 0x0f] => Some("lock xchg dword ptr gs:[rdi], ecx"),
                    // displacement gets a bunch of extra zeroes
                    &[0x66, 0x4e, 0x0f, 0x3a, 0x44, 0x88, 0xb3, 0xad, 0x26, 0x35, 0x75] => Some("pclmulqdq xmm9, xmmword ptr [rax + 3526ADB3h], 75h"),
                    // dumpbin knows about "fstpnce" as "fstp1", but masm does not.
                    // since this is an undocumented instruction anyway, decode it ourselves..
                    &[0xd9, 0xdb] => Some("fstpnce st(3), st(0)"),
                    // dumpbin calls this "fcom2", but it's just an undocumented fcom alias. this round-trips to a different instruction but it's at least.. kinda right.
                    &[0xdc, 0xd3] => Some("fcom st(3)"),
                    // dumpbin calls this "fcomp3", but it's just an undocumented fcomp alias. this round-trips to a different instruction but it's at least.. kinda right.
                    &[0xdc, 0xdb] => Some("fcomp st(3)"),
                    // dumpbin calls this "fxch4", but it's just an undocumented fxch alias. this round-trips to a different instruction but it's at least.. kinda right.
                    &[0xdd, 0xcb] => Some("fxch st(3)"),
                    // dumpbin calls this "fcomp5", but it's just an undocumented fcomp alias. this round-trips to a different instruction but it's at least.. kinda right.
                    &[0xde, 0xd3] => Some("fcomp st(3)"),
                    // dumpbin calls this "fxch7", but it's just an undocumented fxch alias. this round-trips to a different instruction but it's at least.. kinda right.
                    &[0xdf, 0xcb] => Some("fxch st(3)"),
                    // dumpbin calls this "fstp8", but it's just an undocumented fstp alias. this round-trips to a different instruction but it's at least.. kinda right.
                    &[0xdf, 0xd3] => Some("fstp st(3)"),
                    // dumpbin calls this "fstp9", but it's just an undocumented fstp alias. this round-trips to a different instruction but it's at least.. kinda right.
                    &[0xdf, 0xdb] => Some("fstp st(3)"),
                    // dunno why dumpbin doesn't like this one..
                    &[0xc5, 0b0_1111_100, 0x2e, 0b00_001_010] => Some("vucomiss xmm9, dword ptr [rdx]"),
                    &[0xc5, 0b0_1111_100, 0x2f, 0b00_001_010] => Some("vcomiss xmm9, dword ptr [rdx]"),
                    _other => {
                        None
                    }
                }
            }
        };

        if let Some(replacement) = replacement {
            return Ok(replacement);
        }

        let mut source = String::new();

        match codeness {
            CodeModel::Bits16 => {
                source.push_str(".286\n");
                source.push_str(".model small\n");
                source.push_str("assume nothing\n");
            }
            CodeModel::Bits32 => {
//                do not force masm to limit itself to a 686, though that's an interesting comparison in some cases..
//                source.push_str(".686P\n");
                source.push_str(".model flat\n");
                source.push_str("assume fs:nothing\n");
                source.push_str("assume gs:nothing\n");
            }
            CodeModel::Bits64 => {
                // no special incantations to get 64-bit code out of masm
            }
        }
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

        let exe = match codeness {
            CodeModel::Bits64 => "ml64.exe",
            _other => "ml.exe"
        };

        let out = Command::new(format!("..\\..\\tools\\{}", exe))
            .args(&["/c", "/Fo", &objpath.display().to_string(), &sourcepath.display().to_string()])
            .output()
            .expect("can run");
        if !out.status.success() {
            eprintln!("failed to assemble {bytes:x?}:");
            eprintln!("stdout: {}", std::str::from_utf8(out.stdout.as_slice()).expect("valid utf8"));
            eprintln!("stderr: {}", std::str::from_utf8(out.stderr.as_slice()).expect("valid utf8"));
            panic!("failed to {}", exe);
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

        let dumpbin_interesting = carve_dumpbin_stdout(dumpbin_out)?;
        let dumpbin_interesting = dumpbin_interesting[0];

        let post_addr_len = "0F C7 0F                                     ".len();
        let addr_len = "  ".len() + match codeness {
            CodeModel::Bits64 => 16,
            CodeModel::Bits32 => 8,
            CodeModel::Bits16 => 4,
        } + ":".len();
        let start = addr_len;
        let end = start + post_addr_len;
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
            .replace(" + CCBBAA34h", " - 334455CCh") // with apologies to future-me, replace common negative displacements into more normal values...
            .replace("rn - sae", "rn-sae")
            .replace("rd - sae", "rd-sae")
            .replace("ru - sae", "ru-sae")
            .replace("rz - sae", "rz-sae")
            .replace(" oword ", " xmmword ");

        eprintln!("testcase bytes {:x?} -> dumpbin -> text {}", bytes, text);

        Ok(text)
    }

    pub fn masm(text: &str, codeness: CodeModel) -> Result<Vec<u8>, String> {
        let replacement = match codeness {
            CodeModel::Bits16 => {
                // no replacements for 16-bit yet, because masm is little-tested in 16-bit mode..
                None
            }
            CodeModel::Bits32 => {
                match test {
                    "nop zmmword ptr [eax]" => Some(vec![0x0f, 0x18, 0x20]), // MASM doesn't accept `nop zmmword ..`, no way to round trip 0f1820
                    "sysenter" => Some(vec![0x0f, 0x34]), // MASM doesn't accept sysenter, but dumpbin prints it.
                    "sysexit" => Some(vec![0x0f, 0x35]), // MASM doesn't accept sysexit, but dumpbin prints it.
                    // dumpbin doesn't know how to decode, and masm doesn't know how to *en*code, ud0.
                    "ud0 eax, ecx" => Some(vec![0x66, 0x0f, 0xff, 0xc1]),
                    "ud0 eax, dword ptr [ecx]" => Some(vec![0x66, 0x0f, 0xff, 0x01]),
                    "ud0 ebp, dword ptr [ebx - 54h]" => Some(vec![0x0f, 0xff, 0x6b, 0xac]),
                    // masm seems to not know about fstpnce/fstp1 at all. since this is an undocumented instruction anyway, assemble it ourselves..
                    "fstpnce st(3), st(0)" => Some(vec![0xd9, 0xdb]),
                    // masm inserts a wait prefix here..
                    "feni" => Some(vec![0xdb, 0xe0]),
                    "fdisi" => Some(vec![0xdb, 0xe1]),
                    "fsetpm" => Some(vec![0xdb, 0xe4]),
                    // masm doesn't know how to assemble address-size overrides..?
                    // > cannot use 16-bit register with a 32-bit address
                    "aesimc xmm1, xmmword ptr [bx]" => Some(vec![0x67, 0x66, 0x0f, 0x38, 0xdb, 0x0f]),
                    "aesenc xmm1, xmmword ptr [bx]" => Some(vec![0x67, 0x66, 0x0f, 0x38, 0xdc, 0x0f]),
                    "aesenclast xmm1, xmmword ptr [bx]" => Some(vec![0x67, 0x66, 0x0f, 0x38, 0xdd, 0x0f]),
                    "aesdec xmm1, xmmword ptr [bx]" => Some(vec![0x67, 0x66, 0x0f, 0x38, 0xde, 0x0f]),
                    "aesdeclast xmm1, xmmword ptr [bx]" => Some(vec![0x67, 0x66, 0x0f, 0x38, 0xdf, 0x0f]),
                    "blendpd xmm7, xmmword ptr cs:[bx + si + 2FF0h], 7Ch" => Some(vec![0x66, 0x2e, 0x67, 0x0f, 0x3a, 0x0d, 0xb8, 0xf0, 0x2f, 0x7c]),
                    // more
                    "movdir64b bp, zmmword ptr es:[di + 80Bh]" => Some(vec![0x36, 0x26, 0x66, 0x67, 0x0f, 0x38, 0xf8, 0xad, 0x0b, 0x08]),
                    "lss eax, fword ptr [bx + si]" => Some(vec![0x67, 0x0f, 0xb2, 0x00]),
                    "lddqu xmm4, xmmword ptr es:[bx + si + 5F1Bh]" => Some(vec![0xf2, 0x3e, 0x26, 0x67, 0x0f, 0xf0, 0xa0, 0x1b, 0x5f]),
                    "lods byte ptr [si]" => Some(vec![0x67, 0xac]),
                    "scas byte ptr es:[di]" => Some(vec![0x67, 0xae]),
                    "rep movs byte ptr es:[di], byte ptr [si]" => Some(vec![0x67, 0xf3, 0xa4]),
                    "rep movs dword ptr es:[di], dword ptr [si]" => Some(vec![0x67, 0xf3, 0xa5]),
                    "movapd xmm0, xmmword ptr [bx + si]" => Some(vec![0x67, 0x66, 0x0f, 0x28, 0x00]),
                    "cvtdq2ps xmm0, xmmword ptr [bx + di]" => Some(vec![0x67, 0x0f, 0x5b, 0x01]),
                    // i tried really hard to find a MASM syntax for absolute far call/jump destinations! i turned up a bunch of blanks.
                    // https://mirrors.nycbug.org/pub/The_Unix_Archive/Unix_Usenet/comp.unix.xenix/1989-February/001910.html is the funniest,
                    // given that it is OS hackers experiencing the same issue and concluding they should emit the bytes themselves.
                    // so yax will emit something like bindump would, and we'll just swallow the text as if masm worked like i'd hope..
                    "call 6655h:44332211h" => Some(vec![0x9a, 0x11, 0x22, 0x33, 0x44, 0x55, 0x66]),
                    "call 4433h:2211h" => Some(vec![0x66, 0x9a, 0x11, 0x22, 0x33, 0x44, 0x55, 0x66]),
                    // terribly unfortunate: masm reasonably encodes this instruction as a 32-bit offset, which causes yax to spell the offset
                    // as 0000AA55 instead of AA55. override dumpbin to use the (worse) encoding for the sake of matching with the test.
                    "mov cx, word ptr [0AA55h]" => Some(vec![0x66, 0x67, 0x8b, 0x0e, 0x55, 0xaa]),
                    // same deal, different instruction.
                    "mov al, byte ptr [0B0C0h]" => Some(vec![0x67, 0xa0, 0xc0, 0xb0]),
                    "mov eax, dword ptr [0000B0C0h]" => Some(vec![0x67, 0xa1, 0xc0, 0xb0]),
                    // if you operand-size override pushad/popad you get the 16-bit forms, pusha/popa. dumpbin reflects this, but in 32-bit mode
                    // accepts either as a way of spelling pushad/popad. override it here for tests to match up, but this is an unfortunately
                    // disastrous difference in round-tripping..
                    "pusha" => Some(vec![0x66, 0x60]),
                    "popa" => Some(vec![0x66, 0x61]),
                    // masm does not accept an integer operand: it only supports `aam 10` as in d4 0a. so.. bummer.
                    "aam 1" => Some(vec![0xd4, 0x01]),
                    // same as above
                    "aad 1" => Some(vec![0xd5, 0x01]),
                    _ => None,
                }
            }
            CodeModel::Bits64 => {
                match text {
                    "nop zmmword ptr [rax]" => Some(vec![0x0f, 0x18, 0x20]), // MASM doesn't accept `nop zmmword ..`, no way to round trip 0f1820
                    "nop zmmword ptr [r8]" => Some(vec![0x41, 0x0f, 0x18, 0x20]), // MASM doesn't accept `nop zmmword ..`, no way to round trip 410f1820
                    "sysenter" => Some(vec![0x0f, 0x34]), // MASM doesn't accept sysenter, but dumpbin prints it.
                    "sysexit" => Some(vec![0x0f, 0x35]), // MASM doesn't accept sysexit, but dumpbin prints it.
                    "vpscatterdd dword ptr [r15 + xmm29]{k6}, xmm8" => Some(vec![0x62, 0x12, 0x7d, 0x06, 0xa0, 0x04, 0x2f]), // MASM ...??? assembles vpscatter wrong???
                    "vpscatterdd dword ptr [r15 + xmm25]{k6}, xmm10" => Some(vec![0x62, 0x12, 0x7d, 0x06, 0xa0, 0x14, 0x0f]), // MASM ...??? assembles vpscatter wrong???
                    "vpscatterdd dword ptr [r15 + ymm25]{k6}, ymm10" => Some(vec![0x62, 0x12, 0x7d, 0x26, 0xa0, 0x14, 0x0f]), // MASM ...??? assembles vpscatter wrong???
                    "vpscatterdd dword ptr [r15 + zmm25]{k6}, zmm10" => Some(vec![0x62, 0x12, 0x7d, 0x46, 0xa0, 0x14, 0x0f]), // MASM ...??? assembles vpscatter wrong???
                    "vpscatterdq qword ptr [r15 + xmm25]{k6}, xmm10" => Some(vec![0x62, 0x12, 0xfd, 0x46, 0xa0, 0x14, 0x0f]), // MASM ...??? assembles vpscatter wrong???
                    "vpscatterqd dword ptr [r15 + ymm25]{k6}, ymm10" => Some(vec![0x62, 0x12, 0x7d, 0x46, 0xa1, 0x14, 0x0f]), // MASM ...??? assembles vpscatter wrong???
                    "vpscatterqq qword ptr [r15 + zmm25]{k6}, zmm10" => Some(vec![0x62, 0x12, 0xfd, 0x46, 0xa1, 0x14, 0x0f]), // MASM ...??? assembles vpscatter wrong???
                    // dumpbin doesn't know how to decode, and masm doesn't know how to *en*code, ud0.
                    "ud0 eax, ecx" => Some(vec![0x66, 0x0f, 0xff, 0xc1]),
                    "ud0 eax, dword ptr [rcx]" => Some(vec![0x66, 0x0f, 0xff, 0x01]),
                    "ud0 r8d, r9d" => Some(vec![0x66, 0x4f, 0x0f, 0xff, 0xc1]),
                    "ud0 r13d, dword ptr [rbx - 54h]" => Some(vec![0x4c, 0x0f, 0xff, 0x6b, 0xac]),
                    // masm seems to not know about fstpnce/fstp1 at all. since this is an undocumented instruction anyway, assemble it ourselves..
                    "fstpnce st(3), st(0)" => Some(vec![0xd9, 0xdb]),
                    // masm inserts a wait prefix here..
                    "feni" => Some(vec![0xdb, 0xe0]),
                    "fdisi" => Some(vec![0xdb, 0xe1]),
                    "fsetpm" => Some(vec![0xdb, 0xe4]),
                    _other => None,
                }
            }
        };

        if let Some(replacement) = replacement {
            return Ok(replacement);
        }

        let mut source = String::new();

        match codeness {
            CodeModel::Bits16 => {
                source.push_str(".286\n");
                source.push_str(".model small\n");
                source.push_str("assume nothing\n");
            }
            CodeModel::Bits32 => {
//                do not force masm to limit itself to a 686, though that's an interesting comparison in some cases..
//                source.push_str(".686P\n");
                source.push_str(".model flat\n");
                source.push_str("assume fs:nothing\n");
                source.push_str("assume gs:nothing\n");
            }
            CodeModel::Bits64 => {
                // no special incantations to get 64-bit code out of masm
            }
        }
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

        let exe = match codeness {
            CodeModel::Bits64 => "ml64.exe",
            _other => "ml.exe"
        };

        let out = Command::new(format!("..\\..\\tools\\{}", exe))
            .args(&["/c", "/Fo", &objpath.display().to_string(), &sourcepath.display().to_string()])
            .output()
            .expect("can run");
        if !out.status.success() {
            eprintln!("failed to assemble {text:x?}:");
            eprintln!("stdout: {}", std::str::from_utf8(out.stdout.as_slice()).expect("valid utf8"));
            eprintln!("stderr: {}", std::str::from_utf8(out.stderr.as_slice()).expect("valid utf8"));
            panic!("failed to {} as part of masm()", exe);
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

        let dumpbin_interesting = carve_dumpbin_stdout(dumpbin_out)?;

        let post_addr_len = "0F C7 0F                                     ".len();
        let addr_len = "  ".len() + match codeness {
            CodeModel::Bits64 => 16,
            CodeModel::Bits32 => 8,
            CodeModel::Bits16 => 4,
        } + ":".len();
        let start = addr_len;
        let end = start + post_addr_len;
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

#[allow(unused)]
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

    eprintln!("dumpbin returns: {:?}", disasm_lines);

    Ok(disasm_lines.to_vec())
}
