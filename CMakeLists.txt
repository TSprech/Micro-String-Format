cmake_minimum_required(VERSION 3.22)
project(Micro_String_Format)
set(CMAKE_CXX_STANDARD 20)

enable_testing()
add_compile_definitions(USF_ENABLE_LANG_EN
                        USF_ENABLE_LANG_ES
                        USF_ENABLE_LANG_DA
                        USF_ENABLE_LANG_JA
                        USF_ENABLE_TERR_US
                        USF_ENABLE_TERR_ES
                        USF_ENABLE_TERR_DK
                        USF_ENABLE_TERR_JP)

add_subdirectory(${PROJECT_SOURCE_DIR}/test)