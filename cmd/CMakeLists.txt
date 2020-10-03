cmake_minimum_required(VERSION 3.1.0)
project(cmd)

include_directories(..)
set(CMAKE_CXX_STANDARD 17)

add_executable(fst main.cc)
