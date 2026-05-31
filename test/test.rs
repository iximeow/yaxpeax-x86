extern crate yaxpeax_arch;
extern crate yaxpeax_x86;

// target-specific impls for some kinds of testing, such as wrappers for
// "assemble text through {nasm,masm,gas,..}"
mod tools;

mod long_mode;
mod protected_mode;
mod real_mode;
