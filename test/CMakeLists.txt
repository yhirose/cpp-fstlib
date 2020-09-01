cmake_minimum_required(VERSION 3.1.0)
project("fst")

include_directories(..)
set(CMAKE_CXX_STANDARD 17)

add_executable(test-main test.cc)

enable_testing()
add_test(
  NAME TestMain
  COMMAND test-main
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
