lint-clippy:
  cargo clippy --all --all-targets -- -Dwarnings

lint-fmt:
  cargo fmt --all -- --check

lint-all: lint-fmt lint-clippy

test-nextest:
  cargo nextest --profile ci run

test-docs:
  cargo test --doc

test-all: test-nextest test-docs

check: lint-all test-all
c: check

