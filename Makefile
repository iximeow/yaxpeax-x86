test: test-fast test-exhaustive

test-fast: test-std test-no-std test-fmt-no-std

test-exhaustive:
	cargo test -- --ignored
	cargo test --no-default-features -- --ignored

test-std:
	cargo test
test-no-std:
	cargo test --no-default-features
test-fmt-no-std:
	cargo test --no-default-features --features "fmt"
