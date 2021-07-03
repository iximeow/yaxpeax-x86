## yaxpeax-x86

x86 decoders implemented as part of the yaxpeax project, implementing traits provided by `yaxpeax-arch`.

Rust users of this library will either want to use the [quick and dirty APIs], or more generic decode interfaces from `yaxpeax-arch` - appropriate when mixing `yaxpeax-x86` usage with other `yaxpeax` decoders, such as `yaxpeax-arm`. examples of both styles are provided [in the documentation](link).

the `ffi/` directory provides a repackaging of `yaxpeax-x86` suitable for use by non-Rust callers, such as C or C++. see the `examples` directory for FFI usage of this library.

### features

* `#[no_std]`
* configurable instruction set extensions
* very fast
* pretty small?

### `#[no_std]`
the decoders provided by `yaxpeax-x86` are designed to be usable in a `no_std` setting, and does so by default. to build `yaxpeax_x86` without `std`, add the parameter `default-features = false` to your `yaxpeax-x86` dependency; the [ffi packaging] of `yaxpeax_x86` does this and builds without the Rust standard library as well. serde can be enabled without `std`, but json serialization/deserialization [need some careful attention](https://serde.rs/no-std.html) in that mode. as well as the `colors` feature to render instructions with default (eg terminal-friendly) syntax highlighting.

### instruction set extensions
`yaxpeax-x86` decoders provide the option to specify what [instruction set extensions](http://git.iximeow.net/yaxpeax-x86/tree/src/long_mode/mod.rs#n1297) are eligible when decoding, to support decoding x86 instructions as understood by a particular microarchitecture. the default impls of decoders in `yaxpeax_x86` take an optimistsic approach to decoding and assumes all feature sets are available, as well as accepting both intel-specific and amd-specific quirks around undefined encodings.

yaxpeax-x86 decodes long-mode (`amd64`/`x86_64`), protected-mode (`x86`/`x86_32`), and real-mode (`x86_16`) instructions. the most part, ISA extensions decode equivalently across modes; this is the full list of extensions that are supported:

`3dnow`\*, `sse`\*, `sse2`\*, `sse3`, `ssse3`, `sse4.1`, `sse4.2`, `sse4a`, `avx`, `avx2`, `avx512`\*\*, `syscall`, `cmpxchg16b`, `fma3`, `aesni`, `popcnt`, `rdrand`, `xsave`, `sgx`, `monitor`, `movbe`, `sgx`, `bmi1`, `bmi2`, `invpcid`, `mpx`, `adx`, `clflushopt`, `pcommit`, `sha`, `gfni`, `pclmulqdq`, `rdtscp`, `abm`, `xop`, `skinit`, `tbm`, `svm`, `f16c`, `fma4`, `tsx`, `enqcmd`\*\*\*, `uintr`\*\*\*, `keylocker`\*\*\*, `store_direct`\*\*\*, `cet`\*\*\*

\*: `3dnow`, `sse`, and `sse2` are non-optional in `x86_64`, so it is not permitted to construct a decoder that rejects them. `x86_32` and `x86_16` could have features to reject these instructions for true `8086` and `i386` compatibility, but currently do not.

\*\*: `avx512` is fully supported, but decoders rejecting subgroups of the `avx512` family are not. if you need granular `avx512` compatibility controls, please file an issue.

\*\*\*: i ran out of space for feature bits. `InstDecoder` is currently a `u64` and all 64 bits are used for x86 features mapping to `cpuid` bits. supporting these as optional instructions would require growing this to a pair of `u64`. since the typical case is to decode everything, these are decoded regardless of `InstDecoder` settings. growing `InstDecoder` to an `u128` is likely acceptable, but has not yet been profiled.

### very fast
when hooked up to [`disas-bench`](https://github.com/iximeow/disas-bench#results), `yaxpeax_x86::long_mode` has shown roughly 250mb/s decode throughput and on most hardware is the fastest software x86 decoder available.

while there is an in-repo benchmark, i've decided it's so unrealistic as to be unuseful, and prefer `disas-bench` until it can be made more informative..

### pretty small?
`yaxpeax_x86::long_mode` is expected to be around 20kb of code and data. currently a stripped static build of `ffi/` takes a bit more space - around 130kb. instruction rendering is currently non-optional, and is a significant amount of `.text` size. data tables are larger than anticipated, and it's currently an open question if they can be reduced down, or the size target of `yaxpeax_x86::long_mode` should be raised.

this, however, does not by any means make this library the smallest `x86_64` decoder; [`zydis`](https://github.com/zyantific/zydis) handily beats `yaxpeax-x86` out, taking only 10kb in an -O3 build for benchmarking.

### mirrors

the canonical copy of `yaxpeax-x86` is at [https://git.iximeow.net/yaxpeax-x86/](https://git.iximeow.net/yaxpeax-x86/).

`yaxpeax-x86` is also mirrored on GitHub at [https://www.github.com/iximeow/yaxpeax-x86](https://www.github.com/iximeow/yaxpeax-x86).

### ! user beware !
* `yaxpeax-x86` will, but does not yet, have a decoder for real-mode `x86`. it is strongly recommended to use `<yaxpeax_x86::protected_mode::Arch as Arch>::Instruction` and similar type aliases, rather than using struct and operand types directly. user beware!

### unsafety
`yaxpeax_x86` makes regular use of `unsafe { unreachable_unchecked(); }` and occasional use of `unsafe { _.get_unchecked() }` for purely performance reasons. `yaxpeax_x86` is fuzzed via `mishegos` and has passed multiple days of fuzzing without issue.

### changelog
a changelog across crate versions is maintained in the `CHANGELOG` file located in the repo, as well as [online](https://git.iximeow.net/yaxpeax-x86/tree/CHANGELOG).

### see also

[`iced`](https://github.com/0xd4d/iced) is another very good `x86_64` decoder, also written in rust. it provides additional information about instruction semantics as part of the crate, as well as the ability to re-encode instructions.

[`disas-bench`](https://github.com/athre0z/disas-bench), a handy benchmark of several `x86_64` decoders including `yaxpeax-x86`.
