cmake_minimum_required(VERSION 3.0.0)
project(USFTest VERSION 1.0.0)

set(CMAKE_CXX_FLAGS_RELEASE "-O2 -DNDEBUG" CACHE STRING "" FORCE)
set(CMAKE_CXX_FLAGS_MINSIZEREL "-Os -DNDEBUG" CACHE STRING "" FORCE)
set(CMAKE_CXX_FLAGS_RELWITHDEBINFO "-Og -g -DDEBUG" CACHE STRING "" FORCE)
set(CMAKE_CXX_FLAGS_DEBUG "-O0 -g -DDEBUG" CACHE STRING "" FORCE)

#file(GLOB_RECURSE user_source "source/*.cpp" "../include/*.hpp" "include/*.hpp")

find_program(GCOV_PATH gcov)
if(NOT GCOV_PATH)
    message(FATAL_ERROR "Code coverage analysis requires gcov!")
endif()

set(test_sources
        source/unit_tests_basic_types.cpp
        source/unit_tests_custom_types.cpp
        source/unit_tests_floating_point.cpp
        source/unit_tests_format_spec.cpp
        source/unit_tests_positional_args.cpp
        source/test_usf_translation_array.cpp)

add_executable(USFTest test_main.cpp ${test_sources})

target_include_directories(USFTest PUBLIC ../include) # usflib include directory
target_include_directories(USFTest PUBLIC include)    # USFTest include directory

macro(add_compiler_flags)
    foreach (flag ${ARGV})
        target_compile_options(USFTest PRIVATE ${flag})
    endforeach ()
endmacro()

if (CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    add_compiler_flags(-std=c++20)

    add_compiler_flags(-fstrict-aliasing)
    add_compiler_flags(-fvisibility=hidden)
    add_compiler_flags(-pedantic)
    add_compiler_flags(-pedantic-errors)
    add_compiler_flags(-Werror)
endif ()

if (CMAKE_CXX_COMPILER_ID MATCHES "GNU")
    add_compiler_flags(-Wall)
    add_compiler_flags(-Warray-bounds)
    add_compiler_flags(-Wcast-align)
    add_compiler_flags(-Wcast-qual)
    add_compiler_flags(-Wconversion)
    add_compiler_flags(-Wctor-dtor-privacy)
    add_compiler_flags(-Wdisabled-optimization)
    add_compiler_flags(-Weffc++)
    add_compiler_flags(-Wextra)
    add_compiler_flags(-fdiagnostics-show-option)
    add_compiler_flags(-Wfloat-equal)
    add_compiler_flags(-Wformat=2)
    ##  add_compiler_flags(-Winline)
    add_compiler_flags(-Wlogical-op)
    add_compiler_flags(-Wmissing-declarations)
    add_compiler_flags(-Wmissing-include-dirs)
    add_compiler_flags(-Wnoexcept)
    add_compiler_flags(-Wnon-virtual-dtor)
    add_compiler_flags(-Wold-style-cast)
    add_compiler_flags(-Woverloaded-virtual)
    add_compiler_flags(-Wpointer-arith)
    add_compiler_flags(-Wredundant-decls)
    add_compiler_flags(-Wshadow)
    add_compiler_flags(-Wsign-conversion)
    ##  add_compiler_flags(-Wstrict-overflow=5)
    ##  add_compiler_flags(-Wswitch-default)
    add_compiler_flags(-Wswitch-enum)
    add_compiler_flags(-Wwrite-strings)
    add_compiler_flags(-Wundef)
    add_compiler_flags(-Wunreachable-code)
    add_compiler_flags(-Wunused)

    if (NOT CMAKE_CXX_COMPILER_VERSION VERSION_LESS 5.0)
        add_compiler_flags(-Wdouble-promotion)
        add_compiler_flags(-Wtrampolines)
        add_compiler_flags(-Wuseless-cast)
        add_compiler_flags(-Wvector-operation-performance)
        add_compiler_flags(-Wzero-as-null-pointer-constant)
    endif ()

    if (NOT CMAKE_CXX_COMPILER_VERSION VERSION_LESS 6.0)
        add_compiler_flags(-Wduplicated-cond)
        add_compiler_flags(-Wnull-dereference)
        add_compiler_flags(-Wshift-overflow=2)
    endif ()

    if (NOT CMAKE_CXX_COMPILER_VERSION VERSION_LESS 7.0)
        add_compiler_flags(-Walloc-zero)
        add_compiler_flags(-Walloca)
        add_compiler_flags(-Wduplicated-branches)
    endif ()

    if (NOT CMAKE_CXX_COMPILER_VERSION VERSION_LESS 8.0)
        add_compiler_flags(-Wcast-align=strict)
    endif ()
endif ()

if (CMAKE_CXX_COMPILER_ID MATCHES "Clang")
    add_compiler_flags(-Weverything)
    add_compiler_flags(-Wno-c++98-compat)
    add_compiler_flags(-Wno-c++98-compat-pedantic)
    add_compiler_flags(-Wno-padded)
    add_compiler_flags(-Wno-unused-macros)
endif ()

if (MSVC)
    add_compiler_flags(/std:c++latest)
    add_compiler_flags(/permissive-)
    add_compiler_flags(/Wall)
    add_compiler_flags(/WX)
endif ()

set(BUILD_GMOCK OFF)
add_subdirectory(googletest)
include_directories(${gtest_SOURCE_DIR}/include ${gtest_SOURCE_DIR})
target_link_libraries(USFTest gtest gtest_main)

include(GoogleTest)
gtest_discover_tests(USFTest
        PROPERTIES
        DISCOVERY_TIMEOUT  # How long to wait (in seconds) before crashing
        240)

target_link_options(USFTest PUBLIC --coverage)
target_compile_options(USFTest PUBLIC --coverage)