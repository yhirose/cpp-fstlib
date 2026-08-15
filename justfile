build_dir := "build"

# List available recipes
default:
    @just --list

# Configure and build everything (library, cmd, tests, benchmark)
build:
    cmake -S . -B {{build_dir}}
    cmake --build {{build_dir}}

# Remove the build directory
clean:
    rm -rf {{build_dir}}

# Run the unit test suite
test: build
    ctest --test-dir {{build_dir}} --output-on-failure

# Run the fst CLI, e.g. `just fst compile /usr/share/dict/words words.fst`
fst *args: build
    {{build_dir}}/cmd/fst {{args}}

# Run the full benchmark suite (darts-clone, ux-trie, marisa-trie, cpp-fstlib, BurntSushi/fst) against a dictionary file
benchmark dict="/usr/share/dict/words": build
    {{build_dir}}/benchmark/benchmark {{dict}} -a
    cd benchmark/fst-rust && cargo run --release -- {{dict}}

# Format C++ sources with clang-format
fmt:
    clang-format -i fstlib.h cmd/main.cc test/test.cc benchmark/main.cc
