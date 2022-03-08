cmake_minimum_required(VERSION 3.14)
project(cpp-fstlib)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_EXTENSIONS OFF)

if(MSVC)
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /Zc:__cplusplus /utf-8")
else()
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wextra -Wno-unused-parameter")
endif()

enable_testing()

add_subdirectory(test)
add_subdirectory(cmd)
add_subdirectory(benchmark)

install(FILES fstlib.h DESTINATION include)
