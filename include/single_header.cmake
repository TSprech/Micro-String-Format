set(USF_VERSION:STRING "undefined")

string(TIMESTAMP date "%d %B %Y")
string(TIMESTAMP year "%Y")

set(usf_develop_folder "${CMAKE_CURRENT_LIST_DIR}/usf/develop")
set(usf_release_folder "${CMAKE_CURRENT_LIST_DIR}/usf")

file(READ ${usf_develop_folder}/usf_config.hpp          usf_config_hpp)
file(READ ${usf_develop_folder}/usf_traits.hpp          usf_traits_hpp)
file(READ ${usf_develop_folder}/usf_locale.hpp          usf_locale_hpp)
file(READ ${usf_develop_folder}/usf_integer.hpp         usf_integer_hpp)
file(READ ${usf_develop_folder}/usf_float.hpp           usf_float_hpp)
file(READ ${usf_develop_folder}/usf_arg_format.hpp      usf_arg_format_hpp)
file(READ ${usf_develop_folder}/usf_arg_custom_type.hpp usf_arg_custom_type_hpp)
file(READ ${usf_develop_folder}/usf_argument.hpp        usf_argument_hpp)
file(READ ${usf_develop_folder}/usf_main.hpp            usf_main_hpp)

file(WRITE  ${usf_release_folder}/usf.hpp "// ----------------------------------------------------------------------------\n")
file(APPEND ${usf_release_folder}/usf.hpp "// DO NOT MANUALLY MODIFY THIS FILE, IT IS AUTO GENERATED USING A CMAKE SCRIPT!\n")
file(APPEND ${usf_release_folder}/usf.hpp "// ----------------------------------------------------------------------------\n")
file(APPEND ${usf_release_folder}/usf.hpp "// @file    usf.hpp\n")
file(APPEND ${usf_release_folder}/usf.hpp "// @brief   usflib single header auto generated file.\n")
file(APPEND ${usf_release_folder}/usf.hpp "// @date    ${date}\n")
file(APPEND ${usf_release_folder}/usf.hpp "// ----------------------------------------------------------------------------\n")
file(APPEND ${usf_release_folder}/usf.hpp "//\n")
file(APPEND ${usf_release_folder}/usf.hpp "// μSF - Micro String Format ${USF_VERSION} - https://github.com/hparracho/usflib\n")
file(APPEND ${usf_release_folder}/usf.hpp "// Copyright (c) ${year} Helder Parracho (hparracho@gmail.com)\n")
file(APPEND ${usf_release_folder}/usf.hpp "//\n")
file(APPEND ${usf_release_folder}/usf.hpp "// See README.md file for additional credits and acknowledgments.\n")
file(APPEND ${usf_release_folder}/usf.hpp "//\n")
file(APPEND ${usf_release_folder}/usf.hpp "// Permission is hereby granted, free of charge, to any person obtaining a\n")
file(APPEND ${usf_release_folder}/usf.hpp "// copy of this software and associated documentation files (the \"Software\"),\n")
file(APPEND ${usf_release_folder}/usf.hpp "// to deal in the Software without restriction, including without limitation\n")
file(APPEND ${usf_release_folder}/usf.hpp "// the rights to use, copy, modify, merge, publish, distribute, sublicense,\n")
file(APPEND ${usf_release_folder}/usf.hpp "// and/or sell copies of the Software, and to permit persons to whom the\n")
file(APPEND ${usf_release_folder}/usf.hpp "// Software is furnished to do so, subject to the following conditions:\n")
file(APPEND ${usf_release_folder}/usf.hpp "//\n")
file(APPEND ${usf_release_folder}/usf.hpp "// The above copyright notice and this permission notice shall be included\n")
file(APPEND ${usf_release_folder}/usf.hpp "// in all copies or substantial portions of the Software.\n")
file(APPEND ${usf_release_folder}/usf.hpp "//\n")
file(APPEND ${usf_release_folder}/usf.hpp "// THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND, EXPRESS\n")
file(APPEND ${usf_release_folder}/usf.hpp "// OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,\n")
file(APPEND ${usf_release_folder}/usf.hpp "// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL\n")
file(APPEND ${usf_release_folder}/usf.hpp "// THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER\n")
file(APPEND ${usf_release_folder}/usf.hpp "// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING\n")
file(APPEND ${usf_release_folder}/usf.hpp "// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER\n")
file(APPEND ${usf_release_folder}/usf.hpp "// DEALINGS IN THE SOFTWARE.\n")
file(APPEND ${usf_release_folder}/usf.hpp "//\n")
file(APPEND ${usf_release_folder}/usf.hpp "// ----------------------------------------------------------------------------\n\n")
file(APPEND ${usf_release_folder}/usf.hpp "#ifndef USF_HPP\n")
file(APPEND ${usf_release_folder}/usf.hpp "#define USF_HPP\n\n")

file(APPEND ${usf_release_folder}/usf.hpp "${usf_config_hpp}\n\n")
file(APPEND ${usf_release_folder}/usf.hpp "${usf_traits_hpp}\n\n")
file(APPEND ${usf_release_folder}/usf.hpp "${usf_locale_hpp}\n\n")
file(APPEND ${usf_release_folder}/usf.hpp "${usf_integer_hpp}\n\n")
file(APPEND ${usf_release_folder}/usf.hpp "${usf_float_hpp}\n\n")
file(APPEND ${usf_release_folder}/usf.hpp "${usf_arg_format_hpp}\n\n")
file(APPEND ${usf_release_folder}/usf.hpp "${usf_arg_custom_type_hpp}\n\n")
file(APPEND ${usf_release_folder}/usf.hpp "${usf_argument_hpp}\n\n")
file(APPEND ${usf_release_folder}/usf.hpp "${usf_main_hpp}\n")

file(APPEND ${usf_release_folder}/usf.hpp "#endif // USF_HPP\n")
