## yaxpeax-x86

x86 decoders implemented as part of the yaxpeax project.

`yaxpeax-x86` implements traits provided by `yaxpeax-arch`, which are likely how you want to use this library from Rust. included in the `ffi/` directory is a repackaging of `yaxpeax-x86` suitable for use by non-Rust callers, such as C or C++.

### features

* `#[no_std]`
* configurable choice of permitted instruction set extensions
* seems to be pretty fast
* pretty small?

### `#[no_std]`
the decoders provided by `yaxpeax-x86` are designed to be usable in a `no_std` setting, and does so by default. to build `yaxpeax_x86` decoders in `no_std` you'll want to set `default-features = false` as with many other `no_std` Rust crates. serde currently (though it doesn't seem _necessarily_?) relies on `std`, as well as the `colors` feature to render instructions with default (eg terminal-friendly) syntax highlighting.

### instruction set extensions
`yaxpeax-x86` decoders provide the option to specify what [instruction set extensions](http://git.iximeow.net/yaxpeax-x86/tree/src/long_mode/mod.rs#n1297) are eligible when decoding, to support decoding x86 instructions as understood by a particular microarchitecture. the default impls of decoders in `yaxpeax_x86` take an optimistsic approach to decoding and assumes all feature sets are available, as well as accepting both intel-specific and amd-specific quirks around undefined encodings.

### pretty fast
by the in-repo benchmark, `yaxpeax_x86::long_mode` decodes `x86_64` instructions at anywhere between 60 million instructions per second to just shy of 100 million instructions per second, depending on hardware and distribution of instructions being decoded. when hooked up to `disasm-bench`, `yaxpeax_x86::long_mode` has shown roughly 150mb/s decode throughput.

### pretty small?
`yaxpeax_x86::long_mode` is expected to be around 20kb of code and data. currently a stripped static build of `ffi/` takes a bit more space - around 130kb. instruction rendering is currently non-optional, and is a significant amount of `.text` size. data tables are larger than anticipated, and it's currently an open question if they can be reduced down, or the size target of `yaxpeax_x86::long_mode` should be raised.

this, however, does not by any means make this library the smallest `x86_64` decoder; [`zydis`](https://github.com/zyantific/zydis) handily beats `yaxpeax-x86` out, taking only 10kb in an -O3 build for benchmarking.

### mirrors

the canonical copy of `yaxpeax-x86` is at [https://git.iximeow.net/yaxpeax-x86/](https://git.iximeow.net/yaxpeax-x86/).

`yaxpeax-x86` is also mirrored on GitHub at [https://www.github.com/iximeow/yaxpeax-x86](https://www.github.com/iximeow/yaxpeax-x86).

### ! user beware !
* while the decoder has the option to select instruction set extensions, it is entirely likely that some extensions are not yet properly rejected when the corresponding flag is disabled. user beware!
* `yaxpeax-x86` likely has many corners where it does not reject instructions it should. particularly likely is accepting a register operand where the corresponding instruction specifically only allows memory encodings - `lea` is a good example of this. user beware!
* `yaxpeax-x86` will, but does not yet, have a decoder for real-mode `x86`. it is strongly recommended to use `<yaxpeax_x86::protected_mode::Arch as Arch>::Instruction` and similar type aliases, rather than using struct and operand types directly. user beware!
* avx512 is not yet supported. user beware!
* avx256 support is questionable. user beware!
* x87 is not yet supported. user beware!
* ring-0 instructions have questionable support. user beware!
* the only unsafe code in `yaxpeax_x86` is instances of `unsafe { unreachable_unchecked(); }`. while these are, currently, all unreachable, this code should default to a fail-safe assertion fail, with `unreachable_unchecked` being an opt-in feature. user beware!

### changelog
a changelog across crate versions is maintained in the `CHANGELOG` file located in the repo, as well as [online](https://git.iximeow.net/yaxpeax-x86/tree/CHANGELOG).

### see also

[`iced`](https://github.com/0xd4d/iced) looks to be another very good `x86_64` decoder, with an in-progress [translation to rust](https://github.com/0xd4d/iced/tree/rust)

[`disas-bench`](https://github.com/athre0z/disas-bench), a handy benchmark of several `x86_64` decoders, easily extended to compare with `yaxpeax-x86` as well.
