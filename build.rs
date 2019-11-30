fn main() {
    #[cfg(capstone_bench)]
    {
        println!("cargo:rustc-link-search=/usr/lib/");
        println!("cargo:rustc-link-lib=capstone");
    }
}
