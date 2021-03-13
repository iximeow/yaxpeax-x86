# building
for maximum small, ffi crates' `rustflags` includes `-C link-args=-nostdlib`. to avoid conflicts with the native toolchain, this motivates cross-compiling for the native target with `--target x86_64-unknown-linux-gnu`.

then, to remove extra `eh_frame` information from core, rebuild core with panic=abort by `-Z build-std` to cargo. in total, a build command for `yaxpeax_x86_ffi*` looks like:

```
cargo build -Z build-std --release --no-default-features --verbose --target x86_64-unknown-linux-gnu
``
