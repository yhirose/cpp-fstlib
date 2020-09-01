cmake_minimum_required(VERSION 3.1.0)
project("benchmark")

set(CMAKE_CXX_STANDARD 17)
#add_definitions("-Wno-register")

include_directories(
    ..
    marisa-0.2.5/include
    marisa-0.2.5/lib)

add_executable(benchmark
    marisa-0.2.5/lib/marisa/agent.cc
    marisa-0.2.5/lib/marisa/keyset.cc
    marisa-0.2.5/lib/marisa/trie.cc
    marisa-0.2.5/lib/marisa/grimoire/io/mapper.cc
    marisa-0.2.5/lib/marisa/grimoire/io/reader.cc
    marisa-0.2.5/lib/marisa/grimoire/io/writer.cc
    marisa-0.2.5/lib/marisa/grimoire/vector/bit-vector.cc
    marisa-0.2.5/lib/marisa/grimoire/trie/louds-trie.cc
    marisa-0.2.5/lib/marisa/grimoire/trie/tail.cc
    ux-trie/bitVec.cpp
    ux-trie/rsDic.cpp
    ux-trie/uxTrie.cpp
    ux-trie/uxUtil.cpp
    main.cc)
