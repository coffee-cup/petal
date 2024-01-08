# cargo run -- build test.petal

# Optimize the wasm file with Binaryen
wasm-opt -O build/test.wasm -o build/opt.wasm -O3

# Convert the optimized wasm file to wat so we can compare
wasm2wat build/opt.wasm -o build/opt.wat

echo "Optimized wasm file. See build/opt.wat for the result."

# Get the size of the files in bytes
size_before=$(ls -lh "build/test.wasm" | awk '{print $5}')
size_after=$(ls -lh "build/opt.wasm" | awk '{print $5}')

echo "Original: $size_before"
echo "Optimized: $size_after"
