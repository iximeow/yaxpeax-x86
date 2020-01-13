## yaxpeax-x86

x86 decoders implemented as part of the yaxpeax project.

`yaxpeax-x86` implements traits provided by `yaxpeax-arch`, which are likely how you want to use this library from Rust. included in the `ffi/` directory is a repackaging of `yaxpeax-x86` suitable for use by non-Rust callers, such as C or C++.

`yaxpeax-x86` decoders provide the option to specify what [instruction set extensions](http://git.iximeow.net/yaxpeax-x86/tree/src/lib.rs#n1309) are eligible when decoding, to support decoding x86 instructions as understood by a particular microarchitecture. the default `yaxpeax_x86::InstDecoder` takes an optimistsic approach to decoding and assumes all feature sets are available, as well as accepting both intel-specific and amd-specific quirks around undefined encodings.

the decoders provided by `yaxpeax-x86` are designed to be usable in a `no_std` setting, and expected to be less than 20kb of code and data together. this, however, does not make for the smallest `x86_64` decoder! [`zydis`](https://github.com/zyantific/zydis) handily beats this out taking only 10kb in an -O3 build for benchmarking.

### mirrors

the canonical copy of `yaxpeax-x86` is at [https://git.iximeow.net/yaxpeax-x86/](https://git.iximeow.net/yaxpeax-x86/).

`yaxpeax-x86` is also mirrored on GitHub at [https://www.github.com/iximeow/yaxpeax-x86](https://www.github.com/iximeow/yaxpeax-x86).

### ! user beware !
* while `yaxpeax-x86` is designed to support `no_std` use, it currently does not. it should be a small set of changes to get to `no_std`, however. user beware!
* the `libyaxpeax_x86_ffi.a` artifact resulting from a release build of `ffi/` is _huge_. it's almost 2.5mb. it requires linking against `pthread` and `dl`! this largely appears to be a result of depending on the Rust standard library. user beware!
* while the decoder has the option to select instruction set extensions, it is entirely likely that some extensions are not yet properly rejected when the corresponding flag is disabled. user beware!
* `yaxpeax-x86` likely has many corners where it does not reject instructions it should. particularly likely is accepting a register operand where the corresponding instruction specifically only allows memory encodings - `lea` is a good example of this. user beware!
* `yaxpeax-x86` will, but does not yet, have decoders for protected-mode and real-mode `x86`. currently, `yaxpeax-x86` assumes that it is decoding long mode `x86_64` instructions. it is strongly recommended to use `yaxpeax_x86::x86_64::Instruction` and similar type aliases, rather than using struct and operand types directly. user beware!
* avx512 is not yet supported. user beware!
* avx256 support is questionable. user beware!
* ring-0 instructions have questionable support. user beware!
* the only unsafe code in `yaxpeax_x86` is instances of `unsafe { unreachable_unchecked(); }`. while these are, currently, all unreachable, this code should default to a fail-safe assertion fail, with `unreachable_unchecked` being an opt-in feature. user beware!

### see also

[`iced`](https://github.com/0xd4d/iced) looks to be another very good `x86_64` decoder, with an in-progress [translation to rust](https://github.com/0xd4d/iced/tree/rust)
[`disas-bench`](https://github.com/athre0z/disas-bench), a handy benchmark of several `x86_64` decoders, easily extended to compare with `yaxpeax-x86` as well.
