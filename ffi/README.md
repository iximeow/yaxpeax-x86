# `ffi`/

this directory includes (relatively small) repackagings of `yaxpeax-x86` for use from non-Rust callers. these bindings are intended to be expanded on an as-needed basis, so if they are insufficient, please file an issue or contact the maintainers.

# building
for minimal size, ffi crates' `rustflags` includes `-C link-args=-nostdlib`. to avoid conflicts with the native toolchain, this motivates cross-compiling for the native target with `--target x86_64-unknown-linux-gnu`.

then, to remove extra `eh_frame` information from core, rebuild core with panic=abort by `-Z build-std` to cargo. in total, a build command for `yaxpeax_x86_ffi*` looks like:

```
cargo build -Z build-std --release --no-default-features --target x86_64-unknown-linux-gnu
```

where you must replace `x86_64-unknown-linux-gnu` with the target system the build artifacts will be used on. `rustup show` should give a good indication of the local target, and of course you can cross-compile to other architectures and targets if so inclined - `yaxpeax-x86` should have no specific dependencies on any architecture or target.

because even builds for the current host are technically cross-compiles with the above command, build artifacts will be in `target/<build-target>/{release,debug}/`, instead of the typical `target/{release,debug>/`. for example, when i build these crates the artifacts end up like this:

```
target/x86_64-unknown-linux-gnu/
├── [ 177]  CACHEDIR.TAG
└── [4.0K]  release
    ├── [4.0K]  build
    ├── [ 12K]  deps
    ├── [4.0K]  examples
    ├── [4.0K]  incremental
    ├── [730K]  libyaxpeax_x86_ffi_long_mode.a
    ├── [ 909]  libyaxpeax_x86_ffi_long_mode.d
    ├── [143K]  libyaxpeax_x86_ffi_long_mode.so
    ├── [1.2M]  libyaxpeax_x86_ffi_multiarch.a
    ├── [1.0K]  libyaxpeax_x86_ffi_multiarch.d
    ├── [360K]  libyaxpeax_x86_ffi_multiarch.so
    ├── [714K]  libyaxpeax_x86_ffi_protected_mode.a
    ├── [ 929]  libyaxpeax_x86_ffi_protected_mode.d
    ├── [126K]  libyaxpeax_x86_ffi_protected_mode.so
    ├── [715K]  libyaxpeax_x86_ffi_real_mode.a
    └── [ 909]  libyaxpeax_x86_ffi_real_mode.d

5 directories, 12 files
```
