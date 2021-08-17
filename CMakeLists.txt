cmake_minimum_required(VERSION 3.14)
project(cpp-fstlib)

set(CMAKE_CXX_STANDARD 17)

if(MSVC)
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /Zc:__cplusplus /utf-8")
endif()

enable_testing()

add_subdirectory(test)
add_subdirectory(cmd)
add_subdirectory(benchmark)

install(FILES fstlib.h DESTINATION include)
