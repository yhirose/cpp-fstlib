cmake_minimum_required(VERSION 3.14)
project(benchmark)

if(NOT MSVC)
    add_definitions("-Wno-register")
endif()

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
    sqlite3/sqlite3.c
    main.cc)

target_include_directories(
    benchmark PRIVATE
    ..
    /usr/local/Cellar/rocksdb/6.14.6_1/include
    /usr/local/Cellar/rocksdb/6.14.6_1/lib
    marisa-0.2.5/include
    marisa-0.2.5/lib
    sqlite3)

target_link_directories(
    benchmark PRIVATE
    /usr/local/Cellar/rocksdb/6.14.6_1/lib)

target_link_libraries(
    benchmark PRIVATE
    rocksdb)
