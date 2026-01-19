lint-clippy:
  cargo clippy --all --all-targets -- -Dwarnings

lint-fmt:
  cargo fmt --all -- --check

lint-all: lint-fmt lint-clippy

benchmark-sizes:
  cargo run benchmark_sizes

test-nextest:
  cargo nextest --profile ci run

test-docs:
  cargo test --doc

test-all: test-nextest test-docs

check: lint-all test-all benchmark-sizes
c: check

