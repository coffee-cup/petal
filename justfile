build:
  cargo build

test:
  cargo test --all

snapshot:
  cargo insta test --review -p petal_core

fix:
  cargo fmt --all -- --check
  cargo clippy --fix --all-features --all-targets --allow-dirty --allow-staged

clean:
  cargo clean
  rm -rf build
