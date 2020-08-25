cmake_minimum_required(VERSION 3.0)
include_directories(..)
add_definitions("-std=c++17")

add_executable(test-main test.cc)

enable_testing()
add_test(
  NAME TestMain
  COMMAND test-main
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
