build:
  cargo build

test: build
  cargo test

snapshot:
  cargo insta test --review

fix:
  clippy --all-features --all-targets --allow-dirty --allow-staged
