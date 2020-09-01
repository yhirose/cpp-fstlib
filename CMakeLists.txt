cmake_minimum_required(VERSION 3.1.0)
project("cpp-fstlib")

add_subdirectory(cmd)

add_subdirectory(test)
enable_testing()

add_subdirectory(benchmark)
