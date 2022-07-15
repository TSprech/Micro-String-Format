// ----------------------------------------------------------------------------
// DO NOT MANUALLY MODIFY THIS FILE, IT IS AUTO GENERATED USING A CMAKE SCRIPT!
// ----------------------------------------------------------------------------
// @file    usf.hpp
// @brief   usflib single header auto generated file.
// @date    23 June 2022
// ----------------------------------------------------------------------------
//
// μSF - Micro String Format  - https://github.com/hparracho/usflib
// Copyright (c) 2022 Helder Parracho (hparracho@gmail.com)
//
// See README.md file for additional credits and acknowledgments.
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the "Software"),
// to deal in the Software without restriction, including without limitation
// the rights to use, copy, modify, merge, publish, distribute, sublicense,
// and/or sell copies of the Software, and to permit persons to whom the
// Software is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
// OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
// THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
// DEALINGS IN THE SOFTWARE.
//
// ----------------------------------------------------------------------------

#ifndef USF_HPP
#define USF_HPP

// ----------------------------------------------------------------------------
// @file    usf_config.hpp
// @brief   usflib configuration header file.
// @date    14 January 2019
// ----------------------------------------------------------------------------

#ifndef USF_CONFIG_HPP
#define USF_CONFIG_HPP

#include <algorithm>
#include <array>
#include <cassert>
#include <charconv>
#include <climits>
#include <cmath>
#include <limits>
#include <span>
#include <stdexcept>
#include <string>
#include <string_view>
#include <type_traits>
#include <memory_resource>

#include "eternalAdapted.hpp"

// ----------------------------------------------------------------------------
// usflib configuration options
// ----------------------------------------------------------------------------

// Configuration of floating point support.
// USF_DISABLE_FLOAT_SUPPORT           : disables the support of floating point types (it will save considerable code size)

// Configuration of format output string termination option.
// USF_DISABLE_STRING_TERMINATION      : disables the null termination of the format output string

// Configuration of possible behavior when a condition is violated.
// USF_TERMINATE_ON_CONTRACT_VIOLATION : std::terminate() will be called (default)
// USF_ABORT_ON_CONTRACT_VIOLATION     : std::abort() will be called (more suitable for embedded platforms, maybe?)
// USF_THROW_ON_CONTRACT_VIOLATION     : an exception will be thrown

// Configuration of locale support
// #define USF_DISABLE_LOCALE_SUPPORT

// Configures the translation function as the typical gettext _ representation
 #define _tr(key, arr) usf::Translate(arr[key])
 #define _tl(key, arr, loc) usf::Translate(arr[key], loc)
 #define _t(data, nothing) data;

// ----------------------------------------------------------------------------
// Compiler version detection
// ----------------------------------------------------------------------------

// MSVC++ 14.0 _MSC_VER == 1900 (Visual Studio 2015)
// MSVC++ 14.1 _MSC_VER >= 1910 (Visual Studio 2017)
#if defined(_MSC_VER) && !defined(__clang__)
#define USF_COMPILER_MSVC
#define USF_MSVC_VERSION (_MSC_VER / 10 - 10 * (5 + (_MSC_VER < 1900)))
#if (USF_MSVC_VERSION < 140)
#error usflib requires MSVC++ 14.0 or greater
#endif
// Note: VC14.0/1900 (VS2015) lacks too much from C++14.
#define USF_CPLUSPLUS (_MSC_VER <= 1900 ? 201103L : _MSVC_LANG)
#endif

#define USF_COMPILER_VERSION(major, minor, patch) (10 * (10 * (major) + (minor)) + (patch))

#if defined(__clang__)
#define USF_COMPILER_CLANG
#define USF_CLANG_VERSION USF_COMPILER_VERSION(__clang_major__, __clang_minor__, __clang_patchlevel__)
#if (USF_CLANG_VERSION < 340)
#error usflib requires Clang 3.4.0 or greater
#endif
#endif

#if defined(__GNUC__) && !defined(__clang__)
#define USF_COMPILER_GCC
#define USF_GCC_VERSION USF_COMPILER_VERSION(__GNUC__, __GNUC_MINOR__, __GNUC_PATCHLEVEL__)
#if (USF_GCC_VERSION < 481)
#error usflib requires GCC 4.8.1 or greater
#endif
#endif

// ----------------------------------------------------------------------------
// C++ language version detection
// ----------------------------------------------------------------------------
#ifndef USF_CPLUSPLUS
#define USF_CPLUSPLUS __cplusplus
#endif

#define USF_CPP11_OR_GREATER (USF_CPLUSPLUS >= 201103L)
#define USF_CPP14_OR_GREATER (USF_CPLUSPLUS >= 201402L)
#define USF_CPP17_OR_GREATER (USF_CPLUSPLUS >= 201703L)

#if !USF_CPP11_OR_GREATER
#error usflib requires compiler and library support \
for the ISO C++ 2011 standard. This support must be enabled \
with the -std=c++11 or -std=gnu++11 compiler options.
#endif

// C++17 features
#if USF_CPP17_OR_GREATER
#define USF_CPP17_CONSTEXPR constexpr
#else
#define USF_CPP17_CONSTEXPR
#endif

// Fall through attribute
#if USF_CPP17_OR_GREATER
#define USF_FALLTHROUGH [[fallthrough]]
#else
#if defined(USF_COMPILER_GCC) && (USF_GCC_VERSION >= 710)
#define USF_FALLTHROUGH [[gnu::fallthrough]]
#elif defined(USF_COMPILER_CLANG)
#define USF_FALLTHROUGH [[clang::fallthrough]]
#else
#define USF_FALLTHROUGH /*fall through*/
#endif
#endif

// Always inline attribute
#if defined(USF_COMPILER_GCC) || defined(USF_COMPILER_CLANG)
#define USF_ALWAYS_INLINE [[gnu::always_inline]]
#elif defined(USF_COMPILER_MSVC)
#define USF_ALWAYS_INLINE __forceinline
#else
#define USF_ALWAYS_INLINE inline
#endif

// __has_include() support
#if defined(__has_include) && !defined(__INTELLISENSE__)
#define USF_HAS_INCLUDE(x)  __has_include(x)
#else
#define USF_HAS_INCLUDE(x) 0
#endif

// std::string_view support
#if USF_CPP17_OR_GREATER && USF_HAS_INCLUDE(<string_view>)

#include <string_view>

#define USF_STD_BASIC_STRING_VIEW std::basic_string_view
#elif USF_CPP14_OR_GREATER && USF_HAS_INCLUDE(<experimental / string_view>)
#include <experimental/string_view>
#define USF_STD_BASIC_STRING_VIEW std::experimental::basic_string_view
#endif

// char8_t support (C++20 only)
#define USF_CPP20_CHAR8_T_SUPPORT

// ----------------------------------------------------------------------------
// Target detection (maybe not the best way of doing it...)
// ----------------------------------------------------------------------------
#if (UINTPTR_MAX == UINT32_MAX)
#define USF_TARGET_32_BITS 1
#elif (UINTPTR_MAX == UINT64_MAX)
#define USF_TARGET_64_BITS 1
#else
#error "usflib could not determine target architecture."
#endif

// ----------------------------------------------------------------------------
// Missing intrinsic functions definition for MSVC
// ----------------------------------------------------------------------------
#if defined(USF_COMPILER_MSVC)
#include <intrin.h>

#pragma intrinsic(_BitScanReverse, _BitScanReverse64)

int __builtin_clz(uint32_t value) {
  unsigned long leading_zero = 0;
  return _BitScanReverse(&leading_zero, value) ? static_cast<int>(31 - leading_zero) : 32;
}

int __builtin_clzll(uint64_t value) {
  unsigned long leading_zero = 0;
  return _BitScanReverse64(&leading_zero, value) ? static_cast<int>(63 - leading_zero) : 64;
}
#endif  // defined(USF_COMPILER_MSVC)

// ----------------------------------------------------------------------------
// Error handling
// ----------------------------------------------------------------------------
// Two macros ensures any macro passed will
// be expanded before being stringified.
#define USF_STRINGIFY_DETAIL(x) #x
#define USF_STRINGIFY(x) USF_STRINGIFY_DETAIL(x)

#if defined(USF_THROW_ON_CONTRACT_VIOLATION)

namespace usf {
namespace internal {
  template <typename Except>
  [[noreturn]] inline constexpr void throw_exception(const char* const msg) {
    static_assert(std::is_convertible<Except, std::exception>::value,
                  "usf::throw_exception(): exception type should inherit from std::exception.");

    throw Except(msg);
  }
}
}  // namespace usf::internal

#define USF_CONTRACT_VIOLATION(except) usf::internal::throw_exception<except>("Failure at " __FILE__ ", Line " USF_STRINGIFY(__LINE__))
#elif defined(USF_ABORT_ON_CONTRACT_VIOLATION)
#define USF_CONTRACT_VIOLATION(except) std::abort()
#else
#define USF_CONTRACT_VIOLATION(except) std::terminate()
#endif

#define USF_ENFORCE(cond, except) ((!!(cond)) ? static_cast<void>(0) : USF_CONTRACT_VIOLATION(except))

#endif  // USF_CONFIG_HPP


/**
 * @file eternalAdapted.hpp
 * @brief This is an abridged version of mapbox's great eternal library to handle constexpr map lookup.
 * @author eternal Library: Mapbox
 * @author Adapted Changes: TSprech
 * @date 2022/05/06
 * @copyright mapbox Library:
 * ISC License
 * Copyright (c) 2018, Mapbox
 * Permission to use, copy, modify, and/or distribute this software for any purpose with or without fee is hereby
 * granted, provided that the above copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL,
 * DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
 * WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE
 * USE OR PERFORMANCE OF THIS SOFTWARE.
 * @copyright Adapted Code: ©, 2022, TSprech - Apache License 2.0
 * @warning This software is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * @bug None (yet)
 */

#ifndef ETERNALADAPTED_HPP
#define ETERNALADAPTED_HPP

#include <cstdint>
#include <functional>
#include <utility>
#include <string_view>

namespace eternal {
  namespace impl {

    template <typename T>
    constexpr void swap(T& a, T& b) noexcept {
      T tmp{a};
      a = b;
      b = tmp;
    }

    template <typename Key>
    class compare_key {
     public:
      const Key key;

      constexpr compare_key(const Key& key_) noexcept
          : key(key_) {}

      template <typename Element>
      constexpr bool operator<(const Element& rhs) const noexcept {
        return key < rhs->first;
      }
    };

    template <typename Key, typename Value>
    class element {
     public:
      using key_type = Key;
      using mapped_type = Value;
      using value_type = std::pair<key_type, mapped_type>;
      using compare_key_type = compare_key<key_type>;

      constexpr element(const key_type& key, const mapped_type& value) noexcept
          : pair(key, value) {}

      constexpr bool operator<(const element& rhs) const noexcept {
        return pair.first < rhs.pair.first;
      }

      constexpr bool operator<(const compare_key_type& rhs) const noexcept {
        return pair.first < rhs.key;
      }

      constexpr const auto& operator*() const noexcept {
        return pair;
      }

      constexpr const auto* operator->() const noexcept {
        return &pair;
      }

      constexpr void swap(element& rhs) noexcept {
        impl::swap(pair.first, rhs.pair.first);
        impl::swap(pair.second, rhs.pair.second);
      }

     private:
      value_type pair;
    };

    template <typename Key, typename Hasher = std::hash<Key>>
    class compare_key_hash : public compare_key<Key> {
      using base_type = compare_key<Key>;

     public:
      const std::size_t hash;

      constexpr compare_key_hash(const Key& key_) noexcept
          : base_type(key_), hash(Hasher()(key_)) {
      }

      template <typename Element>
      constexpr bool operator<(const Element& rhs) const noexcept {
        return hash < rhs.hash || (!(rhs.hash < hash) && base_type::operator<(rhs));
      }
    };

    template <typename Key, typename Value, typename Hasher = std::hash<Key>>
    class element_hash : public element<Key, Value> {
      using base_type = element<Key, Value>;

     public:
      using key_type = Key;
      using mapped_type = Value;
      using compare_key_type = compare_key_hash<key_type, Hasher>;

      friend compare_key_type;

      constexpr element_hash(const key_type& key, const mapped_type& value) noexcept
          : base_type(key, value), hash(Hasher()(key)) {}

      template <typename T>
      constexpr bool operator<(const T& rhs) const noexcept {
        return hash < rhs.hash || (!(rhs.hash < hash) && base_type::operator<(rhs));
      }

      constexpr void swap(element_hash& rhs) noexcept {
        impl::swap(hash, rhs.hash);
        base_type::swap(rhs);
      }

     private:
      std::size_t hash;
    };
  }  // namespace impl

  template <typename Element>
  class iterator {
   public:
    constexpr iterator(const Element* pos_) noexcept
        : pos(pos_) {}

    constexpr bool operator==(const iterator& rhs) const noexcept {
      return pos == rhs.pos;
    }

    constexpr bool operator!=(const iterator& rhs) const noexcept {
      return pos != rhs.pos;
    }

    constexpr iterator& operator++() noexcept {
      ++pos;
      return *this;
    }

    constexpr iterator& operator+=(std::size_t i) noexcept {
      pos += i;
      return *this;
    }

    constexpr iterator operator+(std::size_t i) const noexcept {
      return pos + i;
    }

    constexpr iterator& operator--() noexcept {
      --pos;
      return *this;
    }

    constexpr iterator& operator-=(std::size_t i) noexcept {
      pos -= i;
      return *this;
    }

    constexpr std::size_t operator-(const iterator& rhs) const noexcept {
      return pos - rhs.pos;
    }

    constexpr const auto& operator*() const noexcept {
      return **pos;
    }

    constexpr const auto* operator->() const noexcept {
      return &**pos;
    }

   private:
    const Element* pos;
  };

  namespace impl {

    template <typename Compare, typename Iterator, typename Key>
    constexpr auto bound(Iterator left, Iterator right, const Key& key) noexcept {
      auto count = right - left;
      while (count > 0) {
        const auto step = count / 2;
        right = left + step;
        if (Compare()(*right, key)) {
          left = ++right;
          count -= step + 1;
        } else {
          count = step;
        }
      }
      return left;
    }

    struct less {
      template <typename A, typename B>
      constexpr bool operator()(const A& a, const B& b) const noexcept {
        return a < b;
      }
    };

    struct greater_equal {
      template <typename A, typename B>
      constexpr bool operator()(const A& a, const B& b) const noexcept {
        return !(b < a);
      }
    };

    template <typename Element, std::size_t N>
    class map {
     private:
      static_assert(N > 0, "map is empty");

      Element data_[N];

      template <typename T, std::size_t... I>
      constexpr map(const T (&data)[N], std::index_sequence<I...>) noexcept
          : data_{{data[I].first, data[I].second}...} {
        static_assert(sizeof...(I) == N, "index_sequence has identical length");
        // Yes, this is a bubblesort. It's usually evaluated at compile-time, it's fast for data
        // that is already sorted (like static maps), it has a small code size, and it's stable.
        for (auto left = data_, right = data_ + N - 1; data_ < right; right = left, left = data_) {
          for (auto it = data_; it < right; ++it) {
            if (it[1] < it[0]) {
              it[0].swap(it[1]);
              left = it;
            }
          }
        }
      }

      using compare_key_type = typename Element::compare_key_type;

     public:
      template <typename T>
      constexpr map(const T (&data)[N]) noexcept
          : map(data, std::make_index_sequence<N>()) {}

      using key_type = typename Element::key_type;
      using mapped_type = typename Element::mapped_type;
      using value_type = typename Element::value_type;
      using size_type = std::size_t;
      using difference_type = std::ptrdiff_t;
      using const_reference = const value_type&;
      using const_pointer = const value_type*;
      using const_iterator = iterator<Element>;

      constexpr bool unique() const noexcept {
        for (auto right = data_ + N - 1, it = data_; it < right; ++it) {
          if (!(it[0] < it[1])) {
            return false;
          }
        }
        return true;
      }

      constexpr const mapped_type& at(const key_type& key) const noexcept {
        return find(key)->second;
      }

      constexpr std::size_t size() const noexcept {
        return N;
      }

      constexpr const_iterator begin() const noexcept {
        return data_;
      }

      constexpr const_iterator cbegin() const noexcept {
        return begin();
      }

      constexpr const_iterator end() const noexcept {
        return data_ + N;
      }

      constexpr const_iterator cend() const noexcept {
        return end();
      }

      constexpr const_iterator lower_bound(const key_type& key) const noexcept {
        return bound<less>(data_, data_ + N, compare_key_type{key});
      }

      constexpr const_iterator upper_bound(const key_type& key) const noexcept {
        return bound<greater_equal>(data_, data_ + N, compare_key_type{key});
      }

      constexpr std::pair<const_iterator, const_iterator> equal_range(const key_type& key) const noexcept {
        const compare_key_type compare_key{key};
        auto first = bound<less>(data_, data_ + N, compare_key);
        return {first, bound<greater_equal>(first, data_ + N, compare_key)};
      }

      constexpr std::size_t count(const key_type& key) const noexcept {
        const auto range = equal_range(key);
        return range.second - range.first;
      }

      constexpr const_iterator find(const key_type& key) const noexcept {
        const compare_key_type compare_key{key};
        auto it = bound<less>(data_, data_ + N, compare_key);
        if (it != data_ + N && greater_equal()(*it, compare_key)) {
          return it;
        } else {
          return end();
        }
      }

      constexpr bool contains(const key_type& key) const noexcept {
        return find(key) != end();
      }
    };

    /// TODO: Combine the map and translation map to remove repeated code
    template <typename Element, std::size_t N>
    class translation_map {
     private:
      static_assert(N > 0, "map is empty");

      Element data_[N];

      template <typename T, std::size_t... I>
      constexpr translation_map(const T (&data)[N], std::index_sequence<I...>) noexcept
          : data_{{data[I].at(0), data[I]}...} {
        static_assert(sizeof...(I) == N, "index_sequence has identical length");
        // Yes, this is a bubblesort. It's usually evaluated at compile-time, it's fast for data
        // that is already sorted (like static maps), it has a small code size, and it's stable.
        for (auto left = data_, right = data_ + N - 1; data_ < right; right = left, left = data_) {
          for (auto it = data_; it < right; ++it) {
            if (it[1] < it[0]) {
              it[0].swap(it[1]);
              left = it;
            }
          }
        }
      }

      using compare_key_type = typename Element::compare_key_type;

     public:
      template <typename T>
      constexpr translation_map(const T (&data)[N]) noexcept
          : translation_map(data, std::make_index_sequence<N>()) {}

      using key_type = typename Element::key_type;
      using mapped_type = typename Element::mapped_type;
      using value_type = typename Element::value_type;
      using size_type = std::size_t;
      using difference_type = std::ptrdiff_t;
      using const_reference = const value_type&;
      using const_pointer = const value_type*;
      using const_iterator = iterator<Element>;

      constexpr bool unique() const noexcept {
        for (auto right = data_ + N - 1, it = data_; it < right; ++it) {
          if (!(it[0] < it[1])) {
            return false;
          }
        }
        return true;
      }

      constexpr const mapped_type& at(const key_type& key) const noexcept {
        return find(key)->second;
      }

      constexpr std::size_t size() const noexcept {
        return N;
      }

      constexpr const_iterator begin() const noexcept {
        return data_;
      }

      constexpr const_iterator cbegin() const noexcept {
        return begin();
      }

      constexpr const_iterator end() const noexcept {
        return data_ + N;
      }

      constexpr const_iterator cend() const noexcept {
        return end();
      }

      constexpr const_iterator lower_bound(const key_type& key) const noexcept {
        return bound<less>(data_, data_ + N, compare_key_type{key});
      }

      constexpr const_iterator upper_bound(const key_type& key) const noexcept {
        return bound<greater_equal>(data_, data_ + N, compare_key_type{key});
      }

      constexpr std::pair<const_iterator, const_iterator> equal_range(const key_type& key) const noexcept {
        const compare_key_type compare_key{key};
        auto first = bound<less>(data_, data_ + N, compare_key);
        return {first, bound<greater_equal>(first, data_ + N, compare_key)};
      }

      constexpr std::size_t count(const key_type& key) const noexcept {
        const auto range = equal_range(key);
        return range.second - range.first;
      }

      constexpr const_iterator find(const key_type& key) const noexcept {
        const compare_key_type compare_key{key};
        auto it = bound<less>(data_, data_ + N, compare_key);
        if (it != data_ + N && greater_equal()(*it, compare_key)) {
          return it;
        } else {
          return end();
        }
      }

      constexpr bool contains(const key_type& key) const noexcept {
        return find(key) != end();
      }

      /**
       * @brief A specialized operator overload for translation arrays, it resolves a map and key pair into a span for translation.
       * @param key The u8 literal or u8 sv literal that is the first entry in the translation array.
       * @returns A span on the translation array.
       */
      constexpr std::span<const std::u8string_view> operator[] (key_type key) const {
        return at(key);
      }
    };
  }  // namespace impl

  template <typename Key, typename Value, std::size_t N>
  static constexpr auto map(const std::pair<const Key, const Value> (&items)[N]) noexcept {
    return impl::map<impl::element<Key, Value>, N>(items);
  }

  template <typename Value, std::size_t ArrSize, std::size_t N>
  static constexpr auto translation_map(const std::array<Value, ArrSize> (&items)[N]) noexcept {
    return impl::translation_map<impl::element<Value, std::array<Value, ArrSize>>, N>(items);
  }

  template <typename Key, typename Value, std::size_t N>
  static constexpr auto hash_map(const std::pair<const Key, const Value> (&items)[N]) noexcept {
    return impl::map<impl::element_hash<Key, Value>, N>(items);
  }
}  // namespace eternal

// eternal::string

namespace eternal {
  namespace impl {

    // Use different constants for 32 bit vs. 64 bit size_t
    constexpr std::size_t hash_offset = std::conditional_t<sizeof(std::size_t) < 8,
                                                           std::integral_constant<uint32_t, 0x811C9DC5>,
                                                           std::integral_constant<uint64_t, 0xCBF29CE484222325>>::value;

    constexpr std::size_t hash_prime = std::conditional_t<sizeof(std::size_t) < 8,
                                                          std::integral_constant<uint32_t, 0x1000193>,
                                                          std::integral_constant<uint64_t, 0x100000001B3>>::value;

    // FNV-1a hash
    constexpr static std::size_t str_hash(const char* str,
                                          const std::size_t value = hash_offset) noexcept {
      return *str ? str_hash(str + 1, (value ^ static_cast<std::size_t>(*str)) * hash_prime) : value;
    }

    constexpr bool str_less(const char* lhs, const char* rhs) noexcept {
      return *lhs && *rhs && *lhs == *rhs ? str_less(lhs + 1, rhs + 1) : *lhs < *rhs;
    }

    constexpr bool str_equal(const char* lhs, const char* rhs) noexcept {
      return *lhs == *rhs && (*lhs == '\0' || str_equal(lhs + 1, rhs + 1));
    }

    constexpr bool u8str_less(const char8_t* lhs, const char8_t* rhs) noexcept {
      return *lhs && *rhs && *lhs == *rhs ? u8str_less(lhs + 1, rhs + 1) : *lhs < *rhs;
    }

    constexpr bool u8str_equal(const char8_t* lhs, const char8_t* rhs) noexcept {
      return *lhs == *rhs && (*lhs == '\0' || u8str_equal(lhs + 1, rhs + 1));
    }
  }  // namespace impl

  class string {
   private:
    const char* data_;

   public:
    constexpr string(char const* data) noexcept
        : data_(data) {}

    constexpr string(const string&) noexcept = default;
    constexpr string(string&&) noexcept = default;
    constexpr string& operator=(const string&) noexcept = default;
    constexpr string& operator=(string&&) noexcept = default;

    constexpr bool operator<(const string& rhs) const noexcept {
      return impl::str_less(data_, rhs.data_);
    }

    constexpr bool operator==(const string& rhs) const noexcept {
      return impl::str_equal(data_, rhs.data_);
    }

    constexpr const char* data() const noexcept {
      return data_;
    }

    constexpr const char* c_str() const noexcept {
      return data_;
    }
  };

  class u8string {
   private:
    const char8_t* data_;

   public:
    constexpr u8string(char8_t const* data) noexcept
        : data_(data) {}

    constexpr u8string(std::basic_string_view<char8_t> data) noexcept
        : data_(data.data()) {}

    constexpr u8string(const u8string&) noexcept = default;
    constexpr u8string(u8string&&) noexcept = default;
    constexpr u8string& operator=(const u8string&) noexcept = default;
    constexpr u8string& operator=(u8string&&) noexcept = default;

    constexpr bool operator<(const u8string& rhs) const noexcept {
      return impl::u8str_less(data_, rhs.data_);
    }

    constexpr bool operator==(const u8string& rhs) const noexcept {
      return impl::u8str_equal(data_, rhs.data_);
    }

    constexpr const char8_t* data() const noexcept {
      return data_;
    }

    constexpr const char8_t* c_str() const noexcept {
      return data_;
    }
  };
}  // namespace eternal

namespace std {
  template <>
  struct hash<::eternal::string> {
    constexpr std::size_t operator()(const ::eternal::string& str) const {
      return ::eternal::impl::str_hash(str.data());
    }
  };
}  // namespace std

#endif  //ETERNALADAPTED_HPP


// ----------------------------------------------------------------------------
// @file    usf_traits.hpp
// @brief   Traits classes.
//          NB: usf::CharTraits class has the same purpose of the
//          std::char_traits class but is not compatible and cannot be
//          interchanged. Different interface and different implementation.
//          Intended for internal use only!
// @date    14 January 2019
// ----------------------------------------------------------------------------

#ifndef USF_TRAITS_HPP
#define USF_TRAITS_HPP

namespace usf::internal {

  // ----------------------------------------------------------------------------
  // Custom char traits
  // ----------------------------------------------------------------------------

  class CharTraits {
   public:
    // --------------------------------------------------------------------
    // PUBLIC STATIC FUNCTIONS
    // --------------------------------------------------------------------

    /**
    * @brief Fills in a certain number of fill characters from a starting iterator.
    * @tparam CharDst The character type of the input string.
    * @tparam CharSrc The character type of the fill char.
    * @param dst The string iterator which is the start of where fill characters begin (it will replaced with a fill character).
    * @param ch The fill character.
    * @param count The number of sequential fill characters to write.
    */
    template <typename CharDst, typename CharSrc, typename std::enable_if<std::is_convertible<CharSrc, CharDst>::value, bool>::type = true>
    inline static constexpr void assign(CharDst *&dst, CharSrc ch, std::ptrdiff_t count) noexcept {
      while ((count--) > 0) { *dst++ = static_cast<CharDst>(ch); }
    }

    template <typename CharDst, typename CharSrc,
              typename std::enable_if<std::is_convertible<CharSrc, CharDst>::value, bool>::type = true>
    inline static constexpr void copy(CharDst *&dst, const CharSrc *src, std::ptrdiff_t count) noexcept {
      while ((count--) > 0) { *dst++ = static_cast<CharDst>(*src++); }
    }

    template <typename CharT>
    inline static constexpr std::ptrdiff_t length(const CharT *str) noexcept {
      const CharT *str_begin = str;

      while (*str != CharT{}) { ++str; }

      return str - str_begin;
    }
  };

  // ----------------------------------------------------------------------------
  // Custom type traits
  // ----------------------------------------------------------------------------

  template <typename T>
  struct always_false : std::false_type {
  };

}  // namespace usf::internal

#endif  // USF_TRAITS_HPP


//
// Created by treys on 6/6/2022.
//

#ifndef USF_LOCALES_TERRITORIES_HPP
#define USF_LOCALES_TERRITORIES_HPP

#include <cstdint>

namespace usf {
  enum class Languages : uint16_t {
#ifdef USF_ENABLE_LANG_EN
    en,
#endif
#ifdef USF_ENABLE_LANG_AF
    af,
#endif
#ifdef USF_ENABLE_LANG_AGQ
    agq,
#endif
#ifdef USF_ENABLE_LANG_AK
    ak,
#endif
#ifdef USF_ENABLE_LANG_AM
    am,
#endif
#ifdef USF_ENABLE_LANG_AR
    ar,
#endif
#ifdef USF_ENABLE_LANG_AS
    as,
#endif
#ifdef USF_ENABLE_LANG_ASA
    asa,
#endif
#ifdef USF_ENABLE_LANG_AST
    ast,
#endif
#ifdef USF_ENABLE_LANG_AZ
    az,
#endif
#ifdef USF_ENABLE_LANG_BAS
    bas,
#endif
#ifdef USF_ENABLE_LANG_BE
    be,
#endif
#ifdef USF_ENABLE_LANG_BEM
    bem,
#endif
#ifdef USF_ENABLE_LANG_BEZ
    bez,
#endif
#ifdef USF_ENABLE_LANG_BG
    bg,
#endif
#ifdef USF_ENABLE_LANG_BM
    bm,
#endif
#ifdef USF_ENABLE_LANG_BN
    bn,
#endif
#ifdef USF_ENABLE_LANG_BO
    bo,
#endif
#ifdef USF_ENABLE_LANG_BR
    br,
#endif
#ifdef USF_ENABLE_LANG_BRX
    brx,
#endif
#ifdef USF_ENABLE_LANG_BS
    bs,
#endif
#ifdef USF_ENABLE_LANG_CA
    ca,
#endif
#ifdef USF_ENABLE_LANG_CCP
    ccp,
#endif
#ifdef USF_ENABLE_LANG_CE
    ce,
#endif
#ifdef USF_ENABLE_LANG_CEB
    ceb,
#endif
#ifdef USF_ENABLE_LANG_CGG
    cgg,
#endif
#ifdef USF_ENABLE_LANG_CHR
    chr,
#endif
#ifdef USF_ENABLE_LANG_CKB
    ckb,
#endif
#ifdef USF_ENABLE_LANG_CS
    cs,
#endif
#ifdef USF_ENABLE_LANG_CY
    cy,
#endif
#ifdef USF_ENABLE_LANG_DA
    da,
#endif
#ifdef USF_ENABLE_LANG_DAV
    dav,
#endif
#ifdef USF_ENABLE_LANG_DE
    de,
#endif
#ifdef USF_ENABLE_LANG_DJE
    dje,
#endif
#ifdef USF_ENABLE_LANG_DOI
    doi,
#endif
#ifdef USF_ENABLE_LANG_DSB
    dsb,
#endif
#ifdef USF_ENABLE_LANG_DUA
    dua,
#endif
#ifdef USF_ENABLE_LANG_DYO
    dyo,
#endif
#ifdef USF_ENABLE_LANG_DZ
    dz,
#endif
#ifdef USF_ENABLE_LANG_EBU
    ebu,
#endif
#ifdef USF_ENABLE_LANG_EE
    ee,
#endif
#ifdef USF_ENABLE_LANG_EL
    el,
#endif
#ifdef USF_ENABLE_LANG_EO
    eo,
#endif
#ifdef USF_ENABLE_LANG_ES
    es,
#endif
#ifdef USF_ENABLE_LANG_ET
    et,
#endif
#ifdef USF_ENABLE_LANG_EU
    eu,
#endif
#ifdef USF_ENABLE_LANG_EWO
    ewo,
#endif
#ifdef USF_ENABLE_LANG_FA
    fa,
#endif
#ifdef USF_ENABLE_LANG_FF
    ff,
#endif
#ifdef USF_ENABLE_LANG_FI
    fi,
#endif
#ifdef USF_ENABLE_LANG_FIL
    fil,
#endif
#ifdef USF_ENABLE_LANG_FO
    fo,
#endif
#ifdef USF_ENABLE_LANG_FR
    fr,
#endif
#ifdef USF_ENABLE_LANG_FUR
    fur,
#endif
#ifdef USF_ENABLE_LANG_FY
    fy,
#endif
#ifdef USF_ENABLE_LANG_GA
    ga,
#endif
#ifdef USF_ENABLE_LANG_GD
    gd,
#endif
#ifdef USF_ENABLE_LANG_GL
    gl,
#endif
#ifdef USF_ENABLE_LANG_GSW
    gsw,
#endif
#ifdef USF_ENABLE_LANG_GU
    gu,
#endif
#ifdef USF_ENABLE_LANG_GUZ
    guz,
#endif
#ifdef USF_ENABLE_LANG_GV
    gv,
#endif
#ifdef USF_ENABLE_LANG_HA
    ha,
#endif
#ifdef USF_ENABLE_LANG_HAW
    haw,
#endif
#ifdef USF_ENABLE_LANG_HE
    he,
#endif
#ifdef USF_ENABLE_LANG_HI
    hi,
#endif
#ifdef USF_ENABLE_LANG_HR
    hr,
#endif
#ifdef USF_ENABLE_LANG_HSB
    hsb,
#endif
#ifdef USF_ENABLE_LANG_HU
    hu,
#endif
#ifdef USF_ENABLE_LANG_HY
    hy,
#endif
#ifdef USF_ENABLE_LANG_IA
    ia,
#endif
#ifdef USF_ENABLE_LANG_ID
    id,
#endif
#ifdef USF_ENABLE_LANG_IG
    ig,
#endif
#ifdef USF_ENABLE_LANG_II
    ii,
#endif
#ifdef USF_ENABLE_LANG_IS
    is,
#endif
#ifdef USF_ENABLE_LANG_IT
    it,
#endif
#ifdef USF_ENABLE_LANG_JA
    ja,
#endif
#ifdef USF_ENABLE_LANG_JGO
    jgo,
#endif
#ifdef USF_ENABLE_LANG_JMC
    jmc,
#endif
#ifdef USF_ENABLE_LANG_JV
    jv,
#endif
#ifdef USF_ENABLE_LANG_KA
    ka,
#endif
#ifdef USF_ENABLE_LANG_KAB
    kab,
#endif
#ifdef USF_ENABLE_LANG_KAM
    kam,
#endif
#ifdef USF_ENABLE_LANG_KDE
    kde,
#endif
#ifdef USF_ENABLE_LANG_KEA
    kea,
#endif
#ifdef USF_ENABLE_LANG_KGP
    kgp,
#endif
#ifdef USF_ENABLE_LANG_KHQ
    khq,
#endif
#ifdef USF_ENABLE_LANG_KI
    ki,
#endif
#ifdef USF_ENABLE_LANG_KK
    kk,
#endif
#ifdef USF_ENABLE_LANG_KKJ
    kkj,
#endif
#ifdef USF_ENABLE_LANG_KL
    kl,
#endif
#ifdef USF_ENABLE_LANG_KLN
    kln,
#endif
#ifdef USF_ENABLE_LANG_KM
    km,
#endif
#ifdef USF_ENABLE_LANG_KN
    kn,
#endif
#ifdef USF_ENABLE_LANG_KO
    ko,
#endif
#ifdef USF_ENABLE_LANG_KOK
    kok,
#endif
#ifdef USF_ENABLE_LANG_KS
    ks,
#endif
#ifdef USF_ENABLE_LANG_KSB
    ksb,
#endif
#ifdef USF_ENABLE_LANG_KSF
    ksf,
#endif
#ifdef USF_ENABLE_LANG_KSH
    ksh,
#endif
#ifdef USF_ENABLE_LANG_KU
    ku,
#endif
#ifdef USF_ENABLE_LANG_KW
    kw,
#endif
#ifdef USF_ENABLE_LANG_KY
    ky,
#endif
#ifdef USF_ENABLE_LANG_LAG
    lag,
#endif
#ifdef USF_ENABLE_LANG_LB
    lb,
#endif
#ifdef USF_ENABLE_LANG_LG
    lg,
#endif
#ifdef USF_ENABLE_LANG_LKT
    lkt,
#endif
#ifdef USF_ENABLE_LANG_LN
    ln,
#endif
#ifdef USF_ENABLE_LANG_LO
    lo,
#endif
#ifdef USF_ENABLE_LANG_LRC
    lrc,
#endif
#ifdef USF_ENABLE_LANG_LT
    lt,
#endif
#ifdef USF_ENABLE_LANG_LU
    lu,
#endif
#ifdef USF_ENABLE_LANG_LUO
    luo,
#endif
#ifdef USF_ENABLE_LANG_LUY
    luy,
#endif
#ifdef USF_ENABLE_LANG_LV
    lv,
#endif
#ifdef USF_ENABLE_LANG_MAI
    mai,
#endif
#ifdef USF_ENABLE_LANG_MAS
    mas,
#endif
#ifdef USF_ENABLE_LANG_MER
    mer,
#endif
#ifdef USF_ENABLE_LANG_MFE
    mfe,
#endif
#ifdef USF_ENABLE_LANG_MG
    mg,
#endif
#ifdef USF_ENABLE_LANG_MGH
    mgh,
#endif
#ifdef USF_ENABLE_LANG_MGO
    mgo,
#endif
#ifdef USF_ENABLE_LANG_MI
    mi,
#endif
#ifdef USF_ENABLE_LANG_MK
    mk,
#endif
#ifdef USF_ENABLE_LANG_ML
    ml,
#endif
#ifdef USF_ENABLE_LANG_MN
    mn,
#endif
#ifdef USF_ENABLE_LANG_MNI
    mni,
#endif
#ifdef USF_ENABLE_LANG_MR
    mr,
#endif
#ifdef USF_ENABLE_LANG_MS
    ms,
#endif
#ifdef USF_ENABLE_LANG_MT
    mt,
#endif
#ifdef USF_ENABLE_LANG_MUA
    mua,
#endif
#ifdef USF_ENABLE_LANG_MY
    my,
#endif
#ifdef USF_ENABLE_LANG_MZN
    mzn,
#endif
#ifdef USF_ENABLE_LANG_NAQ
    naq,
#endif
#ifdef USF_ENABLE_LANG_NB
    nb,
#endif
#ifdef USF_ENABLE_LANG_ND
    nd,
#endif
#ifdef USF_ENABLE_LANG_NDS
    nds,
#endif
#ifdef USF_ENABLE_LANG_NE
    ne,
#endif
#ifdef USF_ENABLE_LANG_NL
    nl,
#endif
#ifdef USF_ENABLE_LANG_NMG
    nmg,
#endif
#ifdef USF_ENABLE_LANG_NN
    nn,
#endif
#ifdef USF_ENABLE_LANG_NNH
    nnh,
#endif
#ifdef USF_ENABLE_LANG_NO
    no,
#endif
#ifdef USF_ENABLE_LANG_NUS
    nus,
#endif
#ifdef USF_ENABLE_LANG_NYN
    nyn,
#endif
#ifdef USF_ENABLE_LANG_OM
    om,
#endif
#ifdef USF_ENABLE_LANG_OR
    or,
#endif
#ifdef USF_ENABLE_LANG_OS
    os,
#endif
#ifdef USF_ENABLE_LANG_PA
    pa,
#endif
#ifdef USF_ENABLE_LANG_PCM
    pcm,
#endif
#ifdef USF_ENABLE_LANG_PL
    pl,
#endif
#ifdef USF_ENABLE_LANG_PS
    ps,
#endif
#ifdef USF_ENABLE_LANG_PT
    pt,
#endif
#ifdef USF_ENABLE_LANG_QU
    qu,
#endif
#ifdef USF_ENABLE_LANG_RM
    rm,
#endif
#ifdef USF_ENABLE_LANG_RN
    rn,
#endif
#ifdef USF_ENABLE_LANG_RO
    ro,
#endif
#ifdef USF_ENABLE_LANG_ROF
    rof,
#endif
#ifdef USF_ENABLE_LANG_ROOT
    root,
#endif
#ifdef USF_ENABLE_LANG_RU
    ru,
#endif
#ifdef USF_ENABLE_LANG_RW
    rw,
#endif
#ifdef USF_ENABLE_LANG_RWK
    rwk,
#endif
#ifdef USF_ENABLE_LANG_SA
    sa,
#endif
#ifdef USF_ENABLE_LANG_SAH
    sah,
#endif
#ifdef USF_ENABLE_LANG_SAQ
    saq,
#endif
#ifdef USF_ENABLE_LANG_SAT
    sat,
#endif
#ifdef USF_ENABLE_LANG_SBP
    sbp,
#endif
#ifdef USF_ENABLE_LANG_SC
    sc,
#endif
#ifdef USF_ENABLE_LANG_SD
    sd,
#endif
#ifdef USF_ENABLE_LANG_SE
    se,
#endif
#ifdef USF_ENABLE_LANG_SEH
    seh,
#endif
#ifdef USF_ENABLE_LANG_SES
    ses,
#endif
#ifdef USF_ENABLE_LANG_SG
    sg,
#endif
#ifdef USF_ENABLE_LANG_SHI
    shi,
#endif
#ifdef USF_ENABLE_LANG_SI
    si,
#endif
#ifdef USF_ENABLE_LANG_SK
    sk,
#endif
#ifdef USF_ENABLE_LANG_SL
    sl,
#endif
#ifdef USF_ENABLE_LANG_SMN
    smn,
#endif
#ifdef USF_ENABLE_LANG_SN
    sn,
#endif
#ifdef USF_ENABLE_LANG_SO
    so,
#endif
#ifdef USF_ENABLE_LANG_SQ
    sq,
#endif
#ifdef USF_ENABLE_LANG_SR
    sr,
#endif
#ifdef USF_ENABLE_LANG_SU
    su,
#endif
#ifdef USF_ENABLE_LANG_SV
    sv,
#endif
#ifdef USF_ENABLE_LANG_SW
    sw,
#endif
#ifdef USF_ENABLE_LANG_TA
    ta,
#endif
#ifdef USF_ENABLE_LANG_TE
    te,
#endif
#ifdef USF_ENABLE_LANG_TEO
    teo,
#endif
#ifdef USF_ENABLE_LANG_TG
    tg,
#endif
#ifdef USF_ENABLE_LANG_TH
    th,
#endif
#ifdef USF_ENABLE_LANG_TI
    ti,
#endif
#ifdef USF_ENABLE_LANG_TK
    tk,
#endif
#ifdef USF_ENABLE_LANG_TO
    to,
#endif
#ifdef USF_ENABLE_LANG_TR
    tr,
#endif
#ifdef USF_ENABLE_LANG_TT
    tt,
#endif
#ifdef USF_ENABLE_LANG_TWQ
    twq,
#endif
#ifdef USF_ENABLE_LANG_TZM
    tzm,
#endif
#ifdef USF_ENABLE_LANG_UG
    ug,
#endif
#ifdef USF_ENABLE_LANG_UK
    uk,
#endif
#ifdef USF_ENABLE_LANG_UR
    ur,
#endif
#ifdef USF_ENABLE_LANG_UZ
    uz,
#endif
#ifdef USF_ENABLE_LANG_VAI
    vai,
#endif
#ifdef USF_ENABLE_LANG_VI
    vi,
#endif
#ifdef USF_ENABLE_LANG_VUN
    vun,
#endif
#ifdef USF_ENABLE_LANG_WAE
    wae,
#endif
#ifdef USF_ENABLE_LANG_WO
    wo,
#endif
#ifdef USF_ENABLE_LANG_XH
    xh,
#endif
#ifdef USF_ENABLE_LANG_XOG
    xog,
#endif
#ifdef USF_ENABLE_LANG_YAV
    yav,
#endif
#ifdef USF_ENABLE_LANG_YI
    yi,
#endif
#ifdef USF_ENABLE_LANG_YO
    yo,
#endif
#ifdef USF_ENABLE_LANG_YRL
    yrl,
#endif
#ifdef USF_ENABLE_LANG_YUE
    yue,
#endif
#ifdef USF_ENABLE_LANG_ZGH
    zgh,
#endif
#ifdef USF_ENABLE_LANG_ZH
    zh,
#endif
#ifdef USF_ENABLE_LANG_ZU
    zu,
#endif
  };
  enum class Territories : uint16_t {
#ifdef USF_ENABLE_TERR_001
    001,
#endif
#ifdef USF_ENABLE_TERR_150
    150,
#endif
#ifdef USF_ENABLE_TERR_419
    419,
#endif
#ifdef USF_ENABLE_TERR_AD
    AD,
#endif
#ifdef USF_ENABLE_TERR_AE
    AE,
#endif
#ifdef USF_ENABLE_TERR_AF
    AF,
#endif
#ifdef USF_ENABLE_TERR_AG
    AG,
#endif
#ifdef USF_ENABLE_TERR_AI
    AI,
#endif
#ifdef USF_ENABLE_TERR_AL
    AL,
#endif
#ifdef USF_ENABLE_TERR_AM
    AM,
#endif
#ifdef USF_ENABLE_TERR_AO
    AO,
#endif
#ifdef USF_ENABLE_TERR_AR
    AR,
#endif
#ifdef USF_ENABLE_TERR_AS
    AS,
#endif
#ifdef USF_ENABLE_TERR_AT
    AT,
#endif
#ifdef USF_ENABLE_TERR_AU
    AU,
#endif
#ifdef USF_ENABLE_TERR_AW
    AW,
#endif
#ifdef USF_ENABLE_TERR_AX
    AX,
#endif
#ifdef USF_ENABLE_TERR_ADLM
    Adlm,
#endif
#ifdef USF_ENABLE_TERR_ARAB
    Arab,
#endif
#ifdef USF_ENABLE_TERR_BA
    BA,
#endif
#ifdef USF_ENABLE_TERR_BB
    BB,
#endif
#ifdef USF_ENABLE_TERR_BD
    BD,
#endif
#ifdef USF_ENABLE_TERR_BE
    BE,
#endif
#ifdef USF_ENABLE_TERR_BF
    BF,
#endif
#ifdef USF_ENABLE_TERR_BG
    BG,
#endif
#ifdef USF_ENABLE_TERR_BH
    BH,
#endif
#ifdef USF_ENABLE_TERR_BI
    BI,
#endif
#ifdef USF_ENABLE_TERR_BJ
    BJ,
#endif
#ifdef USF_ENABLE_TERR_BL
    BL,
#endif
#ifdef USF_ENABLE_TERR_BM
    BM,
#endif
#ifdef USF_ENABLE_TERR_BN
    BN,
#endif
#ifdef USF_ENABLE_TERR_BO
    BO,
#endif
#ifdef USF_ENABLE_TERR_BQ
    BQ,
#endif
#ifdef USF_ENABLE_TERR_BR
    BR,
#endif
#ifdef USF_ENABLE_TERR_BS
    BS,
#endif
#ifdef USF_ENABLE_TERR_BT
    BT,
#endif
#ifdef USF_ENABLE_TERR_BW
    BW,
#endif
#ifdef USF_ENABLE_TERR_BY
    BY,
#endif
#ifdef USF_ENABLE_TERR_BZ
    BZ,
#endif
#ifdef USF_ENABLE_TERR_BENG
    Beng,
#endif
#ifdef USF_ENABLE_TERR_CA
    CA,
#endif
#ifdef USF_ENABLE_TERR_CC
    CC,
#endif
#ifdef USF_ENABLE_TERR_CD
    CD,
#endif
#ifdef USF_ENABLE_TERR_CF
    CF,
#endif
#ifdef USF_ENABLE_TERR_CG
    CG,
#endif
#ifdef USF_ENABLE_TERR_CH
    CH,
#endif
#ifdef USF_ENABLE_TERR_CI
    CI,
#endif
#ifdef USF_ENABLE_TERR_CK
    CK,
#endif
#ifdef USF_ENABLE_TERR_CL
    CL,
#endif
#ifdef USF_ENABLE_TERR_CM
    CM,
#endif
#ifdef USF_ENABLE_TERR_CN
    CN,
#endif
#ifdef USF_ENABLE_TERR_CO
    CO,
#endif
#ifdef USF_ENABLE_TERR_CR
    CR,
#endif
#ifdef USF_ENABLE_TERR_CU
    CU,
#endif
#ifdef USF_ENABLE_TERR_CV
    CV,
#endif
#ifdef USF_ENABLE_TERR_CW
    CW,
#endif
#ifdef USF_ENABLE_TERR_CX
    CX,
#endif
#ifdef USF_ENABLE_TERR_CY
    CY,
#endif
#ifdef USF_ENABLE_TERR_CZ
    CZ,
#endif
#ifdef USF_ENABLE_TERR_CYRL
    Cyrl,
#endif
#ifdef USF_ENABLE_TERR_DE
    DE,
#endif
#ifdef USF_ENABLE_TERR_DG
    DG,
#endif
#ifdef USF_ENABLE_TERR_DJ
    DJ,
#endif
#ifdef USF_ENABLE_TERR_DK
    DK,
#endif
#ifdef USF_ENABLE_TERR_DM
    DM,
#endif
#ifdef USF_ENABLE_TERR_DO
    DO,
#endif
#ifdef USF_ENABLE_TERR_DZ
    DZ,
#endif
#ifdef USF_ENABLE_TERR_DEVA
    Deva,
#endif
#ifdef USF_ENABLE_TERR_EA
    EA,
#endif
#ifdef USF_ENABLE_TERR_EC
    EC,
#endif
#ifdef USF_ENABLE_TERR_EE
    EE,
#endif
#ifdef USF_ENABLE_TERR_EG
    EG,
#endif
#ifdef USF_ENABLE_TERR_EH
    EH,
#endif
#ifdef USF_ENABLE_TERR_ER
    ER,
#endif
#ifdef USF_ENABLE_TERR_ES
    ES,
#endif
#ifdef USF_ENABLE_TERR_ET
    ET,
#endif
#ifdef USF_ENABLE_TERR_FI
    FI,
#endif
#ifdef USF_ENABLE_TERR_FJ
    FJ,
#endif
#ifdef USF_ENABLE_TERR_FK
    FK,
#endif
#ifdef USF_ENABLE_TERR_FM
    FM,
#endif
#ifdef USF_ENABLE_TERR_FO
    FO,
#endif
#ifdef USF_ENABLE_TERR_FR
    FR,
#endif
#ifdef USF_ENABLE_TERR_GA
    GA,
#endif
#ifdef USF_ENABLE_TERR_GB
    GB,
#endif
#ifdef USF_ENABLE_TERR_GD
    GD,
#endif
#ifdef USF_ENABLE_TERR_GE
    GE,
#endif
#ifdef USF_ENABLE_TERR_GF
    GF,
#endif
#ifdef USF_ENABLE_TERR_GG
    GG,
#endif
#ifdef USF_ENABLE_TERR_GH
    GH,
#endif
#ifdef USF_ENABLE_TERR_GI
    GI,
#endif
#ifdef USF_ENABLE_TERR_GL
    GL,
#endif
#ifdef USF_ENABLE_TERR_GM
    GM,
#endif
#ifdef USF_ENABLE_TERR_GN
    GN,
#endif
#ifdef USF_ENABLE_TERR_GP
    GP,
#endif
#ifdef USF_ENABLE_TERR_GQ
    GQ,
#endif
#ifdef USF_ENABLE_TERR_GR
    GR,
#endif
#ifdef USF_ENABLE_TERR_GT
    GT,
#endif
#ifdef USF_ENABLE_TERR_GU
    GU,
#endif
#ifdef USF_ENABLE_TERR_GW
    GW,
#endif
#ifdef USF_ENABLE_TERR_GY
    GY,
#endif
#ifdef USF_ENABLE_TERR_GURU
    Guru,
#endif
#ifdef USF_ENABLE_TERR_HK
    HK,
#endif
#ifdef USF_ENABLE_TERR_HN
    HN,
#endif
#ifdef USF_ENABLE_TERR_HR
    HR,
#endif
#ifdef USF_ENABLE_TERR_HT
    HT,
#endif
#ifdef USF_ENABLE_TERR_HU
    HU,
#endif
#ifdef USF_ENABLE_TERR_HANS
    Hans,
#endif
#ifdef USF_ENABLE_TERR_HANT
    Hant,
#endif
#ifdef USF_ENABLE_TERR_IC
    IC,
#endif
#ifdef USF_ENABLE_TERR_ID
    ID,
#endif
#ifdef USF_ENABLE_TERR_IE
    IE,
#endif
#ifdef USF_ENABLE_TERR_IL
    IL,
#endif
#ifdef USF_ENABLE_TERR_IM
    IM,
#endif
#ifdef USF_ENABLE_TERR_IN
    IN,
#endif
#ifdef USF_ENABLE_TERR_IO
    IO,
#endif
#ifdef USF_ENABLE_TERR_IQ
    IQ,
#endif
#ifdef USF_ENABLE_TERR_IR
    IR,
#endif
#ifdef USF_ENABLE_TERR_IS
    IS,
#endif
#ifdef USF_ENABLE_TERR_IT
    IT,
#endif
#ifdef USF_ENABLE_TERR_JE
    JE,
#endif
#ifdef USF_ENABLE_TERR_JM
    JM,
#endif
#ifdef USF_ENABLE_TERR_JO
    JO,
#endif
#ifdef USF_ENABLE_TERR_JP
    JP,
#endif
#ifdef USF_ENABLE_TERR_KE
    KE,
#endif
#ifdef USF_ENABLE_TERR_KG
    KG,
#endif
#ifdef USF_ENABLE_TERR_KH
    KH,
#endif
#ifdef USF_ENABLE_TERR_KI
    KI,
#endif
#ifdef USF_ENABLE_TERR_KM
    KM,
#endif
#ifdef USF_ENABLE_TERR_KN
    KN,
#endif
#ifdef USF_ENABLE_TERR_KP
    KP,
#endif
#ifdef USF_ENABLE_TERR_KR
    KR,
#endif
#ifdef USF_ENABLE_TERR_KW
    KW,
#endif
#ifdef USF_ENABLE_TERR_KY
    KY,
#endif
#ifdef USF_ENABLE_TERR_KZ
    KZ,
#endif
#ifdef USF_ENABLE_TERR_LA
    LA,
#endif
#ifdef USF_ENABLE_TERR_LB
    LB,
#endif
#ifdef USF_ENABLE_TERR_LC
    LC,
#endif
#ifdef USF_ENABLE_TERR_LI
    LI,
#endif
#ifdef USF_ENABLE_TERR_LK
    LK,
#endif
#ifdef USF_ENABLE_TERR_LR
    LR,
#endif
#ifdef USF_ENABLE_TERR_LS
    LS,
#endif
#ifdef USF_ENABLE_TERR_LT
    LT,
#endif
#ifdef USF_ENABLE_TERR_LU
    LU,
#endif
#ifdef USF_ENABLE_TERR_LV
    LV,
#endif
#ifdef USF_ENABLE_TERR_LY
    LY,
#endif
#ifdef USF_ENABLE_TERR_LATN
    Latn,
#endif
#ifdef USF_ENABLE_TERR_MA
    MA,
#endif
#ifdef USF_ENABLE_TERR_MC
    MC,
#endif
#ifdef USF_ENABLE_TERR_MD
    MD,
#endif
#ifdef USF_ENABLE_TERR_MF
    MF,
#endif
#ifdef USF_ENABLE_TERR_MG
    MG,
#endif
#ifdef USF_ENABLE_TERR_MH
    MH,
#endif
#ifdef USF_ENABLE_TERR_MINE
    MINE,
#endif
#ifdef USF_ENABLE_TERR_MK
    MK,
#endif
#ifdef USF_ENABLE_TERR_ML
    ML,
#endif
#ifdef USF_ENABLE_TERR_MM
    MM,
#endif
#ifdef USF_ENABLE_TERR_MN
    MN,
#endif
#ifdef USF_ENABLE_TERR_MO
    MO,
#endif
#ifdef USF_ENABLE_TERR_MP
    MP,
#endif
#ifdef USF_ENABLE_TERR_MQ
    MQ,
#endif
#ifdef USF_ENABLE_TERR_MR
    MR,
#endif
#ifdef USF_ENABLE_TERR_MS
    MS,
#endif
#ifdef USF_ENABLE_TERR_MT
    MT,
#endif
#ifdef USF_ENABLE_TERR_MU
    MU,
#endif
#ifdef USF_ENABLE_TERR_MV
    MV,
#endif
#ifdef USF_ENABLE_TERR_MW
    MW,
#endif
#ifdef USF_ENABLE_TERR_MX
    MX,
#endif
#ifdef USF_ENABLE_TERR_MY
    MY,
#endif
#ifdef USF_ENABLE_TERR_MZ
    MZ,
#endif
#ifdef USF_ENABLE_TERR_NA
    NA,
#endif
#ifdef USF_ENABLE_TERR_NC
    NC,
#endif
#ifdef USF_ENABLE_TERR_NE
    NE,
#endif
#ifdef USF_ENABLE_TERR_NF
    NF,
#endif
#ifdef USF_ENABLE_TERR_NG
    NG,
#endif
#ifdef USF_ENABLE_TERR_NI
    NI,
#endif
#ifdef USF_ENABLE_TERR_NL
    NL,
#endif
#ifdef USF_ENABLE_TERR_NO
    NO,
#endif
#ifdef USF_ENABLE_TERR_NP
    NP,
#endif
#ifdef USF_ENABLE_TERR_NR
    NR,
#endif
#ifdef USF_ENABLE_TERR_NU
    NU,
#endif
#ifdef USF_ENABLE_TERR_NZ
    NZ,
#endif
#ifdef USF_ENABLE_TERR_OM
    OM,
#endif
#ifdef USF_ENABLE_TERR_OLCK
    Olck,
#endif
#ifdef USF_ENABLE_TERR_PA
    PA,
#endif
#ifdef USF_ENABLE_TERR_PE
    PE,
#endif
#ifdef USF_ENABLE_TERR_PF
    PF,
#endif
#ifdef USF_ENABLE_TERR_PG
    PG,
#endif
#ifdef USF_ENABLE_TERR_PH
    PH,
#endif
#ifdef USF_ENABLE_TERR_PK
    PK,
#endif
#ifdef USF_ENABLE_TERR_PL
    PL,
#endif
#ifdef USF_ENABLE_TERR_PM
    PM,
#endif
#ifdef USF_ENABLE_TERR_PN
    PN,
#endif
#ifdef USF_ENABLE_TERR_PR
    PR,
#endif
#ifdef USF_ENABLE_TERR_PS
    PS,
#endif
#ifdef USF_ENABLE_TERR_PT
    PT,
#endif
#ifdef USF_ENABLE_TERR_PW
    PW,
#endif
#ifdef USF_ENABLE_TERR_PY
    PY,
#endif
#ifdef USF_ENABLE_TERR_QA
    QA,
#endif
#ifdef USF_ENABLE_TERR_RE
    RE,
#endif
#ifdef USF_ENABLE_TERR_RO
    RO,
#endif
#ifdef USF_ENABLE_TERR_RU
    RU,
#endif
#ifdef USF_ENABLE_TERR_RW
    RW,
#endif
#ifdef USF_ENABLE_TERR_SA
    SA,
#endif
#ifdef USF_ENABLE_TERR_SB
    SB,
#endif
#ifdef USF_ENABLE_TERR_SC
    SC,
#endif
#ifdef USF_ENABLE_TERR_SD
    SD,
#endif
#ifdef USF_ENABLE_TERR_SE
    SE,
#endif
#ifdef USF_ENABLE_TERR_SG
    SG,
#endif
#ifdef USF_ENABLE_TERR_SH
    SH,
#endif
#ifdef USF_ENABLE_TERR_SI
    SI,
#endif
#ifdef USF_ENABLE_TERR_SJ
    SJ,
#endif
#ifdef USF_ENABLE_TERR_SK
    SK,
#endif
#ifdef USF_ENABLE_TERR_SL
    SL,
#endif
#ifdef USF_ENABLE_TERR_SM
    SM,
#endif
#ifdef USF_ENABLE_TERR_SN
    SN,
#endif
#ifdef USF_ENABLE_TERR_SO
    SO,
#endif
#ifdef USF_ENABLE_TERR_SR
    SR,
#endif
#ifdef USF_ENABLE_TERR_SS
    SS,
#endif
#ifdef USF_ENABLE_TERR_ST
    ST,
#endif
#ifdef USF_ENABLE_TERR_SV
    SV,
#endif
#ifdef USF_ENABLE_TERR_SX
    SX,
#endif
#ifdef USF_ENABLE_TERR_SY
    SY,
#endif
#ifdef USF_ENABLE_TERR_SZ
    SZ,
#endif
#ifdef USF_ENABLE_TERR_TARASK
    TARASK,
#endif
#ifdef USF_ENABLE_TERR_TC
    TC,
#endif
#ifdef USF_ENABLE_TERR_TD
    TD,
#endif
#ifdef USF_ENABLE_TERR_TG
    TG,
#endif
#ifdef USF_ENABLE_TERR_TH
    TH,
#endif
#ifdef USF_ENABLE_TERR_TJ
    TJ,
#endif
#ifdef USF_ENABLE_TERR_TK
    TK,
#endif
#ifdef USF_ENABLE_TERR_TL
    TL,
#endif
#ifdef USF_ENABLE_TERR_TM
    TM,
#endif
#ifdef USF_ENABLE_TERR_TN
    TN,
#endif
#ifdef USF_ENABLE_TERR_TO
    TO,
#endif
#ifdef USF_ENABLE_TERR_TR
    TR,
#endif
#ifdef USF_ENABLE_TERR_TT
    TT,
#endif
#ifdef USF_ENABLE_TERR_TV
    TV,
#endif
#ifdef USF_ENABLE_TERR_TZ
    TZ,
#endif
#ifdef USF_ENABLE_TERR_TFNG
    Tfng,
#endif
#ifdef USF_ENABLE_TERR_UA
    UA,
#endif
#ifdef USF_ENABLE_TERR_UG
    UG,
#endif
#ifdef USF_ENABLE_TERR_UM
    UM,
#endif
#ifdef USF_ENABLE_TERR_US
    US,
#endif
#ifdef USF_ENABLE_TERR_UY
    UY,
#endif
#ifdef USF_ENABLE_TERR_VA
    VA,
#endif
#ifdef USF_ENABLE_TERR_VC
    VC,
#endif
#ifdef USF_ENABLE_TERR_VE
    VE,
#endif
#ifdef USF_ENABLE_TERR_VG
    VG,
#endif
#ifdef USF_ENABLE_TERR_VI
    VI,
#endif
#ifdef USF_ENABLE_TERR_VN
    VN,
#endif
#ifdef USF_ENABLE_TERR_VU
    VU,
#endif
#ifdef USF_ENABLE_TERR_VAII
    Vaii,
#endif
#ifdef USF_ENABLE_TERR_WF
    WF,
#endif
#ifdef USF_ENABLE_TERR_WS
    WS,
#endif
#ifdef USF_ENABLE_TERR_XK
    XK,
#endif
#ifdef USF_ENABLE_TERR_YE
    YE,
#endif
#ifdef USF_ENABLE_TERR_YT
    YT,
#endif
#ifdef USF_ENABLE_TERR_ZA
    ZA,
#endif
#ifdef USF_ENABLE_TERR_ZM
    ZM,
#endif
#ifdef USF_ENABLE_TERR_ZW
    ZW,
#endif
  };
}  // namespace usf

#endif  // USF_LOCALES_TERRITORIES_HPP


//
// Created by treys on 5/25/2022.
//

#ifndef USF_LOCALE_HPP
#define USF_LOCALE_HPP
#ifndef USF_DISABLE_LOCALE_SUPPORT

#include <array>
#include <span>
#include <string_view>
#include <tuple>

using namespace std::string_view_literals;

namespace usf {
  using cldr_t = std::u8string_view;

  // REQUIRED
  struct Symbols {
    cldr_t decimal;
    cldr_t group;
    cldr_t list;
    cldr_t percent_sign;
    cldr_t plus_sign;
    cldr_t minus_sign;
    cldr_t exponential;
    cldr_t superscripting_exponent;
    cldr_t per_mille;
    cldr_t infinity;
    cldr_t nan;
    cldr_t time_separator;
  };

  struct Numbers {
    Symbols symbols;
  };

  struct Identity {
    uint8_t revision;
    Languages language;
    Territories territory;
  };

  struct Locale {
    Identity identity;
    Numbers numbers;
  };

  constexpr inline Locale c_locale{
      .identity = {
          .revision = 41,
          .language = Languages::en,
          .territory = Territories::US},
      .numbers = {.symbols = {.decimal = u8"."sv, .group = u8","sv, .list = u8";"sv, .percent_sign = u8"%"sv, .plus_sign = u8"+"sv, .minus_sign = u8"-"sv, .exponential = u8"E"sv, .superscripting_exponent = u8"×"sv, .per_mille = u8"‰"sv, .infinity = u8"∞"sv, .nan = u8"NaN"sv, .time_separator = u8":"sv}}};

  using locale_t = Locale;

  constexpr inline auto Translate(std::span<const std::u8string_view> translations) {
    return translations;
  }

  constexpr inline auto Translate(std::span<const std::u8string_view> translations, locale_t loc) -> std::u8string_view {
    return translations[static_cast<uint16_t>(loc.identity.language)];
  }
}  // namespace usf

#endif
#endif  // USF_LOCALE_HPP


// ----------------------------------------------------------------------------
// @file    usf_integer.hpp
// @brief   Integer conversion and helper functions.
// @date    14 January 2019
// ----------------------------------------------------------------------------

#ifndef USF_INTEGER_HPP
#define USF_INTEGER_HPP

namespace usf::internal {

  constexpr uint32_t pow10_uint32_lut[]{
      1,
      10,
      100,
      1000,
      10000,
      100000,
      1000000,
      10000000,
      100000000,
      1000000000};

  constexpr uint64_t pow10_uint64_lut[]{
      1,
      10,
      100,
      1000,
      10000,
      100000,
      1000000,
      10000000,
      100000000,
      1000000000,
      10000000000,
      100000000000,
      1000000000000,
      10000000000000,
      100000000000000,
      1000000000000000,
      10000000000000000,
      100000000000000000,
      1000000000000000000,
      10000000000000000000U};

  constexpr char digits_hex_uppercase[]{"0123456789ABCDEF"};
  constexpr char digits_hex_lowercase[]{"0123456789abcdef"};

  class Integer {
   public:
    // --------------------------------------------------------------------
    // PUBLIC STATIC FUNCTIONS
    // --------------------------------------------------------------------

    // -------- POWERS OF 10 ----------------------------------------------
    static constexpr uint32_t pow10_uint32(const int index) noexcept {
      assert(index >= 0 && index < 10);

      return pow10_uint32_lut[index];
    }

    static constexpr uint64_t pow10_uint64(const int index) noexcept {
      assert(index >= 0 && index < 20);

      return pow10_uint64_lut[index];
    }

    // -------- COUNT DIGITS ----------------------------------------------
    // Based on the code from:
    // http://graphics.stanford.edu/~seander/bithacks.html#IntegerLog10
    // --------------------- ----------------------------------------------

    /**
     * @brief Counts how many digits are in an integer number.
     * @param n The number whose number of digits are being counted.
     * @returns The number of digits in n.
     */
    static constexpr int count_digits_dec(const uint32_t n) noexcept {
      if (n < 10) return 1;

      // The algorithm below doesn't work when `n` is 0 because:
      // 1. the result of __builtin_clz() is undefined if `n` is 0.
      // 2. the `pow10_uint32_lut` lookup table has the value 1 in
      //    the first element and not a 0 as the algorithm expects.
      // (both cases are covered by the previous if statement or
      //  by the slower commented OR operation below).

      // n = n | 1;

      const int t = (32 - __builtin_clz(n)) * 1233 >> 12;
      return t - (n < pow10_uint32_lut[t]) + 1;
    }

    static constexpr int count_digits_dec(const uint64_t n) noexcept {
      if (n <= std::numeric_limits<uint32_t>::max()) {
        return count_digits_dec(static_cast<uint32_t>(n));
      }

      // The algorithm below doesn't work when `n` is 0 because:
      // 1. the result of __builtin_clzll() is undefined if `n` is 0.
      // 2. the `pow10_uint64_lut` lookup table has the value 1 in
      //    the first element and not a 0 as the algorithm expects.
      // (both cases are covered by the previous if statement or
      //  by the slower commented OR operation below).

      // n = n | 1;

      const int t = (64 - __builtin_clzll(n)) * 1233 >> 12;
      return t - (n < pow10_uint64_lut[t]) + 1;
    }

    static constexpr int count_digits_bin(const uint32_t n) noexcept {
      // The result of __builtin_clz() is undefined if `n` is 0.
      return (n < 2) ? 1 : (32 - __builtin_clz(n));
    }

    static constexpr int count_digits_bin(const uint64_t n) noexcept {
      // The result of __builtin_clzll() is undefined if `n` is 0.
      return (n < 2) ? 1 : (64 - __builtin_clzll(n));
    }

    template <typename T,
              typename std::enable_if<
                  std::numeric_limits<T>::is_integer && std::is_unsigned<T>::value, bool>::type
              = true>
    static constexpr int count_digits_oct(T n) noexcept {
      int digits = 1;
      while ((n >>= 3U) != 0) { ++digits; }
      return digits;
    }

    template <typename T,
              typename std::enable_if<
                  std::numeric_limits<T>::is_integer && std::is_unsigned<T>::value, bool>::type
              = true>
    static constexpr int count_digits_hex(T n) noexcept {
      int digits = 1;
      while ((n >>= 4U) != 0) { ++digits; }
      return digits;
    }

    // -------- FAST DIVIDE BY 10 -----------------------------------------
    // Based on the code from Hacker's Delight:
    // http://www.hackersdelight.org/divcMore.pdf
    // --------------------- ----------------------------------------------
    static constexpr uint32_t div10(const uint32_t n) noexcept {
#if defined(__arm__)
      uint32_t q = (n >> 1) + (n >> 2);
      q += (q >> 4);
      q += (q >> 8);
      q += (q >> 16);
      q >>= 3;

      const uint32_t r = n - (q << 3) - (q << 1);

      return q + ((r + 6) >> 4);
      // return q + (r > 9);
#else
      return n / 10;
#endif
    }

    static constexpr uint64_t div10(const uint64_t n) noexcept {
#if defined(__arm__)
      uint64_t q = (n >> 1) + (n >> 2);
      q += (q >> 4);
      q += (q >> 8);
      q += (q >> 16);
      q += (q >> 32);
      q >>= 3;

      const uint64_t r = n - (q << 3) - (q << 1);

      return q + ((r + 6) >> 4);
      // return q + (r > 9);
#else
      return n / 10;
#endif
    }

    // -------- CONVERTERS ------------------------------------------------
    // The following converters write the value from back to front.
    // It is assumed that the pointer `dst` is already placed at the
    // position after the last character. The pointer position is
    // calculated using the corresponding count_digits_xxx() functions.

    // Example:
    // value ->  1234
    // array -> [........]
    // dst   ->      ^

    // -------- DECIMAL CONVERSION ----------------------------------------
    template <typename CharT>
    static constexpr void convert_dec(CharT *dst, uint32_t value) noexcept {
      do {
        const uint32_t v = value;
        value = div10(value);
        *(--dst) = static_cast<CharT>('0' + (v - (value * 10)));
      } while (value);
    }

    template <typename CharT>
    static constexpr void convert_dec(CharT *dst, uint64_t value) noexcept {
      while (value > std::numeric_limits<uint32_t>::max()) {
        const uint64_t v = value;
        value = div10(value);
        *(--dst) = static_cast<CharT>('0' + (v - (value * 10)));
      }

      convert_dec(dst, static_cast<uint32_t>(value));
    }

    // -------- BINARY CONVERSION -----------------------------------------
    template <typename CharT>
    static constexpr void convert_bin(CharT *dst, uint32_t value) noexcept {
      do {
        const uint32_t v = value;
        value >>= 1U;
        *(--dst) = static_cast<CharT>('0' + (v - (value << 1U)));
      } while (value);
    }

    template <typename CharT>
    static constexpr void convert_bin(CharT *dst, uint64_t value) noexcept {
      while (value > std::numeric_limits<uint32_t>::max()) {
        const uint64_t v = value;
        value >>= 1U;
        *(--dst) = static_cast<CharT>('0' + (v - (value << 1U)));
      }

      convert_bin(dst, static_cast<uint32_t>(value));
    }

    // -------- OCTAL CONVERSION ------------------------------------------
    template <typename CharT>
    static constexpr void convert_oct(CharT *dst, uint32_t value) noexcept {
      do {
        const uint32_t v = value;
        value >>= 3U;
        *(--dst) = static_cast<CharT>('0' + (v - (value << 3U)));
      } while (value);
    }

    template <typename CharT>
    static constexpr void convert_oct(CharT *dst, uint64_t value) noexcept {
      while (value > std::numeric_limits<uint32_t>::max()) {
        const uint64_t v = value;
        value >>= 3U;
        *(--dst) = static_cast<CharT>('0' + (v - (value << 3U)));
      }

      convert_oct(dst, static_cast<uint32_t>(value));
    }

    // -------- HEXADECIMAL CONVERSION ------------------------------------
    template <typename CharT>
    static constexpr void convert_hex(CharT *dst, uint32_t value, const bool uppercase) noexcept {
      const char *digits = uppercase ? digits_hex_uppercase : digits_hex_lowercase;

      do {
        const uint32_t v = value;
        value >>= 4U;
        *(--dst) = static_cast<CharT>(digits[v - (value << 4U)]);
      } while (value);
    }

    template <typename CharT>
    static constexpr void convert_hex(CharT *dst, uint64_t value, const bool uppercase) noexcept {
      const char *digits = uppercase ? digits_hex_uppercase : digits_hex_lowercase;

      while (value > std::numeric_limits<uint32_t>::max()) {
        const uint64_t v = value;
        value >>= 4U;
        *(--dst) = static_cast<CharT>(digits[v - (value << 4U)]);
      }

      convert_hex(dst, static_cast<uint32_t>(value), uppercase);
    }
  };

}  // namespace usf::internal

#endif  // USF_INTEGER_HPP


// ----------------------------------------------------------------------------
// @file    usf_float.hpp
// @brief   Floating point conversion and helper functions. Naive and limited
//          implementation with the usual precision/rounding errors (good for now).
// @date    07 January 2019
// ----------------------------------------------------------------------------

#if !defined(USF_DISABLE_FLOAT_SUPPORT)

#ifndef USF_FLOAT_HPP
#define USF_FLOAT_HPP

namespace usf::internal {

  class Float {
   public:
    // --------------------------------------------------------------------
    // PUBLIC STATIC FUNCTIONS
    // --------------------------------------------------------------------

    template <typename CharT>
    static constexpr int convert(CharT *const significand, int &exponent, double value, const bool format_fixed, const int precision) noexcept {
      uint64_t ipart = 0;  // i is integer part, eg for 3.1415 the i part is 3
      uint64_t fpart = 0;  // f is the float part, eg for 3.1415 the f part is 1415

      int ipart_digits = 0;  // Number of i digits
      int fpart_digits = 0;  // Number of f digits

      int fpart_padding = 0;

      if (value < 1) {
        // Negative exponent

        value *= 1e19;

        fpart = static_cast<uint64_t>(value);
        fpart_digits = Integer::count_digits_dec(fpart);

        exponent = fpart_digits - 20;

        fpart_padding = -exponent - 1;

        //if(!format_fixed && precision > 19 - fpart_padding)
        if (fpart_padding > 14 - precision) {
          fpart = static_cast<uint64_t>(value * static_cast<double>(Integer::pow10_uint64(fpart_padding)));
          fpart_digits = Integer::count_digits_dec(fpart);
        }
      } else {
        // Positive exponent

        ipart = static_cast<uint64_t>(value);
        ipart_digits = Integer::count_digits_dec(ipart);

        fpart = static_cast<uint64_t>((value - static_cast<double>(ipart)) * 1e14);
        fpart_digits = Integer::count_digits_dec(fpart);

        exponent = ipart_digits - 1;

        fpart_padding = 14 - fpart_digits;
      }

      const auto round_index = 1 + precision + (format_fixed ? exponent : 0);

      if (round_index < 0) {
        // Specified precision higher than converted value.
        // Should print all zeros. Bail!
        significand[0] = '0';
        exponent = 0;
        return 1;
      }

      CharT *it = significand;

      if (ipart != 0) {
        it += ipart_digits;
        Integer::convert_dec(it, ipart);
      }

      if (fpart != 0) {
        if (ipart != 0) {
          CharTraits::assign(it, '0', fpart_padding);
        }

        it += fpart_digits;
        Integer::convert_dec(it, fpart);
      }

      const auto significand_size = remove_trailing_zeros(significand, it);

      if (significand_size <= round_index) {
        // Rounding not needed. Bail!
        return significand_size;
      }

      //Round to the specified precision.
      return round(significand, significand_size, exponent, format_fixed, round_index);
    }

   private:
    // --------------------------------------------------------------------
    // PRIVATE STATIC FUNCTIONS
    // --------------------------------------------------------------------

    template <typename CharT>
    static constexpr int round(CharT *const significand, const int significand_size, int &exponent,
                               const bool format_fixed, const int round_index) noexcept {
      CharT *it = significand + round_index;

      bool round_up = false;

      if (round_index == significand_size - 1) {
        // Round the last digit of the significand buffer.
        // It can simultaneously be the first one if the
        // significant buffer has only one digit.

        const bool prev_digit_odd = (round_index > 0) ? (('0' - *(it - 1)) & 1) != 0 : false;

        if (*it > '5' || (*it == '5' && prev_digit_odd)) {
          // Round up if digit is:
          // 1) greater than 5
          //    e.g. 2.6 -> 3
          // 2) exactly 5 and previous digit is odd
          //    e.g. 2.5 -> 2
          //    e.g. 3.5 -> 3
          round_up = true;
        }
      } else if (*it >= '5') {
        // Round any digit except the last one. Since the trailing zeros were
        // removed, we only need to test if the digit is at least '5' because it
        // is granted that other non-zero digits are present after this position.
        round_up = true;
      }

      if (round_up) {
        bool carry = false;

        if (round_index > 0) {
          --it;

          do {
            if (*it < '9') {
              carry = false;
              ++(*it);
            } else {
              carry = true;
              *it = '0';
            }
          } while (--it >= significand && carry);
        } else {
          carry = true;
        }

        // Buffer termination is not necessary since the caller functions
        // rely on the returned size and not on null terminator.

        if (carry) {
          significand[0] = '1';
          ++exponent;
          return 1;
        }
      } else if (round_index == 0) {
        significand[0] = '0';
        exponent = 0;
        return 1;
      }

      // Do not remove the trailing zeros if format is fixed.
      if (format_fixed) { return round_index; }

      return remove_trailing_zeros(significand, significand + round_index);
    }

    // Evaluates the range [first, last), truncates all the trailing zeros and return the
    // new range size. Keeps always at least 1 element of the range (even if it is zero).
    template <typename CharT>
    static constexpr int remove_trailing_zeros(const CharT *const first, CharT *last) noexcept {
      while ((last - 1) > first && *(last - 1) == '0') { --last; }

      // Buffer termination is not really necessary since the caller
      // functions rely on the returned size and not on null terminator.

      return static_cast<int>(last - first);
    }
  };

}  // namespace usf::internal

#endif  // USF_FLOAT_HPP
#endif  // !defined(USF_DISABLE_FLOAT_SUPPORT)


// ----------------------------------------------------------------------------
// @file    usf_arg_format.hpp
// @brief   Argument format parser class.
// @date    07 January 2019
// ----------------------------------------------------------------------------

#ifndef USF_ARG_FORMAT_HPP
#define USF_ARG_FORMAT_HPP

namespace usf::internal {

  template <typename CharT>
  class ArgFormat {
   public:
    // --------------------------------------------------------------------
    // PUBLIC TYPE ALIASES
    // --------------------------------------------------------------------

    using iterator = CharT *;
    using const_iterator = const CharT *;

    // --------------------------------------------------------------------
    // PUBLIC DEFINITIONS
    // --------------------------------------------------------------------

    enum class Align : uint8_t {
      kNone = (0U << 1U),
      kLeft = (1U << 1U),
      kRight = (2U << 1U),
      kCenter = (3U << 1U),
      kNumeric = (4U << 1U)
    };

    enum class Sign : uint8_t {
      kNone = (0U << 4U),
      kMinus = (1U << 4U),
      kPlus = (2U << 4U),
      kSpace = (3U << 4U)
    };

    enum class Type : uint8_t {
      kNone,
      kChar,
      kIntegerDec,
      kIntegerHex,
      kIntegerOct,
      kIntegerBin,
      kPointer,
      kFloatFixed,
      kFloatScientific,
      kFloatGeneral,
      kString,
      kTranslatableString,
      kInvalid
    };

    // --------------------------------------------------------------------
    // PUBLIC MEMBER FUNCTIONS
    // --------------------------------------------------------------------

    /**
     * @brief Parses format specifier strings into its respective variables.
     * @param fmt The string_view  to the format string.
     * @param arg_count The number of format specifiers in the entire string, used for checking positional argument index validity.
     */
    constexpr ArgFormat(std::basic_string_view<CharT> &fmt, const int arg_count) {  // TODO: Divide more
      // An fmt string of {:d} is used as an example, but the same principle applies to formats using other characters (eg {:f}, {:b})
      // At this point, the fmt string_view contains format instructions in the form of {:d}
      const_iterator it = fmt.cbegin();  // This iterator will iterate through the fmt string to figure out what the format is

      USF_ENFORCE(*it == '{', std::runtime_error);  // Make sure that the format starts with a { otherwise something has gone wrong in parsing the args

      ++it;  // Move the iterator past the '{' character

      if (*it >= '0' && *it <= '9') {  // This is entered when the format is a positional argument (eg {0}, {1})
        // Index limited to `arg_count` value.
        m_index = static_cast<int8_t>(parse_positive_small_int(it, arg_count));  // TODO: What this does
      }

      // At this point the  iterator string is ":d}"
      if (*it == ':' && *(it + 1) != '}') {  // This is entered when there is a character between the ':' and ending '}'
        // A format spec is expected next...
        m_flags = Flags::kNone;  // TODO: What do flags do

        ++it;  // Remove the empty format flag (which is ':')

        // Try to parse alignment flag at second character of format spec as a fill character is first
        m_flags = parse_align_flag(*(it + 1));  // This would, for example, apply for a {:_=14d}, where the '=' is numeric align and '_' is the fill char

        if (m_flags != Flags::kNone) {                                // If an alignment character was found at the second character, extract the fill character
          USF_ENFORCE(*it != '{' && *it != '}', std::runtime_error);  // The fill character can be any character except '{' or '}'

          m_fill_char = *it;                // The fill character should be at the current iterator since the alignment is in the next position
          it += 2;                          // Increment past both the fill and alignment characters
        } else {                            // Alignment flag not present at the second character of format spec, so try to parse the alignment flag at the first character instead
          m_flags = parse_align_flag(*it);  // Attempt a parse at single character, eg for {:=14d}

          if (m_flags != Flags::kNone) {  // If an alignment character was found, increment the iterator past it
            ++it;
          }
        }

        // Parse sign flag
        switch (*it) {
          case '-':
            m_flags |= Flags::kSignMinus;
            ++it;
            break;
          case '+':
            m_flags |= Flags::kSignPlus;
            ++it;
            break;
          case ' ':
            m_flags |= Flags::kSignSpace;
            ++it;
            break;
          default:
            break;
        }

        // Parse hash flag
        if (*it == '#') {
          m_flags |= Flags::kHash;
          ++it;
        }

        bool fill_zero = false;

        // Parse fill zero flag
        if (*it == '0') {    // Zero pading looks like "{:014d}" where 14 is the total width
          fill_zero = true;  // If the iterator is 0, then there is 0 fill
          ++it;              // Proceed past the '0'
        }

        // Parse width
        if (*it >= '0' && *it <= '9') {                 // Parse how many characters wide the format should end up as
          m_width = parse_positive_small_int(it, 255);  // Limit width to 255 characters
        }

        // Parse precision
        if (*it == '.') {  // Check for the decimal point which signifies a float precision
          ++it;
          USF_ENFORCE(*it >= '0' && *it <= '9', std::runtime_error);             // Check for a missing/invalid precision specifier
          m_precision = static_cast<int8_t>(parse_positive_small_int(it, 127));  // Extract the precision length from the string
        }

        // Parse type
        if (*it != '}') {  // Check that the format isn't something like {}
          switch (*it++) {
            // Check each of the possible types
            case 'c':
              m_type = Type::kChar;
              break;

            case 'd':
              m_type = Type::kIntegerDec;
              break;

            case 'X':
              m_flags |= Flags::kUppercase;
              USF_FALLTHROUGH;
            case 'x':
              m_type = Type::kIntegerHex;
              break;

            case 'o':
              m_type = Type::kIntegerOct;
              break;

            case 'B':
              m_flags |= Flags::kUppercase;
              USF_FALLTHROUGH;
            case 'b':
              m_type = Type::kIntegerBin;
              break;

            case 'P':
              m_flags |= Flags::kUppercase;
              USF_FALLTHROUGH;
            case 'p':
              m_type = Type::kPointer;
              break;

            case 'F':
              m_flags |= Flags::kUppercase;
              USF_FALLTHROUGH;
            case 'f':
              m_type = Type::kFloatFixed;
              break;

            case 'E':
              m_flags |= Flags::kUppercase;
              USF_FALLTHROUGH;
            case 'e':
              m_type = Type::kFloatScientific;
              break;

            case 'G':
              m_flags |= Flags::kUppercase;
              USF_FALLTHROUGH;
            case 'g':
              m_type = Type::kFloatGeneral;
              break;

            case 's':
              m_type = Type::kString;
              break;

            case 't':
              m_type = Type::kTranslatableString;
              break;

            default:  // A character specifier must be found otherwise there is an error
              m_type = Type::kInvalid;
              break;
          }

          USF_ENFORCE(m_type != Type::kInvalid, std::runtime_error);
        }

        // Validate the read format spec!

        if (fill_zero) {
          // Fill zero flag has precedence over any other alignment and fill character.
          m_flags = static_cast<uint8_t>((m_flags & (~Flags::kAlignBitmask)) | Flags::kAlignNumeric);
          m_fill_char = '0';
        }

        if (align() == Align::kNumeric) {
          // Numeric alignment are only valid for numeric and pointer types.
          USF_ENFORCE(type_is_numeric() || type_is_pointer(), std::runtime_error);
        }

        if (sign() != Sign::kNone) {
          // Sign is only valid for numeric types.
          USF_ENFORCE(type_is_numeric(), std::runtime_error);
        }

        if (hash()) {
          // Alternative format is valid for hexadecimal (including
          // pointers), octal, binary and all floating point types.
          USF_ENFORCE(type_allow_hash(), std::runtime_error);
        }

        if (m_precision != -1) {
          // Precision is only valid for floating point and string types.
          USF_ENFORCE(type_is_float() || type_is_string(), std::runtime_error);
        }
      }

      // Test for unterminated argument format spec.
      USF_ENFORCE(it < fmt.cend() && *it++ == '}', std::runtime_error);

      fmt.remove_prefix(static_cast<uint32_t>(it - fmt.cbegin()));  // TODO: Resolve sign error better
    }

    // Writes the alignment (sign, prefix and fill before) for any
    // argument type. Returns the fill counter to write after argument.
    constexpr int write_alignment(iterator &it, const_iterator end, int digits, const bool negative) const {  // TODO: Figure out what this does
      digits += sign_width(negative) + prefix_width();

      int fill_after = 0;

      if (width() <= digits) {                               // If the width of the numbers is <= the calculated full length, it means that there is a negative sign / prefix (eg hex, bin, oct) or it is a decimal number
        USF_ENFORCE(it + digits < end, std::runtime_error);  // Check that the total width of the characters to be written does not exceed the size of the buffer
        write_sign(it, negative);                            // Write a negative sign if applicable
        write_prefix(it);                                    // Write the prefix if applicable
      } else {
        USF_ENFORCE(it + width() < end, std::runtime_error);  // Check that the total width of the characters to be written by appending the number will not exceed the buffer size

        int fill_count = width() - digits;  //

        const Align al = align();

        if (al == Align::kLeft) {
          fill_after = fill_count;
        } else if (al == Align::kCenter) {
          fill_after = fill_count - (fill_count / 2);
          fill_count /= 2;
        }

        if (al != Align::kLeft && al != Align::kNumeric) {  // If it is right align
          // None (default right), Right or Center alignment
          CharTraits::assign(it, fill_char(), fill_count);
        }

        write_sign(it, negative);
        write_prefix(it);

        if (al == Align::kNumeric) {
          CharTraits::assign(it, fill_char(), fill_count);
        }
      }

      return fill_after;
    }

    inline constexpr CharT fill_char() const noexcept { return m_fill_char; }

    inline constexpr Type type() const noexcept { return m_type; }

    inline constexpr int width() const noexcept { return static_cast<int>(m_width); }

    inline constexpr int precision() const noexcept { return static_cast<int>(m_precision); }

    inline constexpr int index() const noexcept { return static_cast<int>(m_index); }

    inline constexpr Align align() const noexcept { return Align(m_flags & Flags::kAlignBitmask); }

    inline constexpr Sign sign() const noexcept { return Sign(m_flags & Flags::kSignBitmask); }

    inline constexpr bool is_empty() const noexcept { return (m_flags & Flags::kEmpty) != 0; }

    inline constexpr bool hash() const noexcept { return (m_flags & Flags::kHash) != 0; }

    inline constexpr bool uppercase() const noexcept { return (m_flags & Flags::kUppercase) != 0; }

    inline constexpr bool type_is_none() const noexcept { return m_type == Type::kNone; }

    inline constexpr bool type_is_char() const noexcept { return m_type == Type::kChar; }

    inline constexpr bool type_is_string() const noexcept { return m_type == Type::kString; }

    inline constexpr bool type_is_translatable_string() const noexcept { return m_type == Type::kTranslatableString; }

    inline constexpr bool type_is_pointer() const noexcept { return m_type == Type::kPointer; }

    inline constexpr bool type_is_integer() const noexcept {
      return m_type >= Type::kIntegerDec && m_type <= Type::kIntegerBin;
    }

    inline constexpr bool type_is_float() const noexcept {
      return m_type >= Type::kFloatFixed && m_type <= Type::kFloatGeneral;
    }

    inline constexpr bool type_is_numeric() const noexcept {
      return m_type >= Type::kIntegerDec && m_type <= Type::kFloatGeneral;
    }

    inline constexpr bool type_is_integer_dec() const noexcept { return m_type == Type::kIntegerDec; }

    inline constexpr bool type_is_integer_hex() const noexcept { return m_type == Type::kIntegerHex; }

    inline constexpr bool type_is_integer_oct() const noexcept { return m_type == Type::kIntegerOct; }

    inline constexpr bool type_is_integer_bin() const noexcept { return m_type == Type::kIntegerBin; }

    inline constexpr bool type_is_float_fixed() const noexcept { return m_type == Type::kFloatFixed; }

    inline constexpr bool type_is_float_scientific() const noexcept { return m_type == Type::kFloatScientific; }

    inline constexpr bool type_is_float_general() const noexcept { return m_type == Type::kFloatGeneral; }

    inline constexpr bool type_allow_hash() const noexcept {
      // Alternative format is valid for hexadecimal (including
      // pointers), octal, binary and all floating point types.
      return m_type >= Type::kIntegerHex && m_type <= Type::kFloatGeneral;
    }

    inline constexpr void default_align_left() noexcept {
      if ((m_flags & Flags::kAlignBitmask) == Flags::kAlignNone) {
        m_flags |= Flags::kAlignLeft;
      }
    }

   private:
    // --------------------------------------------------------------------
    // PRIVATE DEFINITIONS
    // --------------------------------------------------------------------

    enum Flags : uint8_t {
      kNone = (0U << 0U),

      kEmpty = (1U << 0U),

      kAlignNone = (0U << 1U),
      kAlignLeft = (1U << 1U),
      kAlignRight = (2U << 1U),
      kAlignCenter = (3U << 1U),
      kAlignNumeric = (4U << 1U),
      kAlignBitmask = (7U << 1U),

      kSignNone = (0U << 4U),
      kSignMinus = (1U << 4U),
      kSignPlus = (2U << 4U),
      kSignSpace = (3U << 4U),
      kSignBitmask = (3U << 4U),

      kHash = (1U << 6U),

      kUppercase = (1U << 7U)
    };

    // --------------------------------------------------------------------
    // PRIVATE MEMBER FUNCTIONS
    // --------------------------------------------------------------------

    /**
     * @brief Returns whether or not there is a negative sign for the number.
     * @param negative
     * @returns 1 or 0 for if there is or is not a negative sign.
     */
    inline constexpr int sign_width(const bool negative) const noexcept {
      return (!negative && sign() <= Sign::kMinus) ? 0 : 1;
    }

    /**
     * @returns The width in characters of the prefix.
     */
    inline constexpr int prefix_width() const noexcept {
      // Alternative format is valid for hexadecimal (including
      // pointers), octal, binary and all floating point types.
      return (!hash() || type_is_float()) ? 0 : type_is_integer_oct() ? 1
                                                                      : 2;
    }

    constexpr void write_sign(iterator &it, const bool negative) const noexcept {
      if (negative) {
        *it++ = '-';
      } else {
        const Sign s = sign();

        if (s != Sign::kNone) {
          if (s == Sign::kPlus) {
            *it++ = '+';
          } else if (s == Sign::kSpace) {
            *it++ = ' ';
          }
        }
      }
    }

    constexpr void write_prefix(iterator &it) const noexcept {
      // Alternative format is valid for hexadecimal (including
      // pointers), octal, binary and all floating point types.
      if (hash() && !type_is_float()) {
        *it++ = '0';

        if (type_is_integer_bin()) {
          *it++ = uppercase() ? 'B' : 'b';
        } else if (type_is_integer_hex() || type_is_pointer()) {
          *it++ = uppercase() ? 'X' : 'x';
        }
      }
    }

    // --------------------------------------------------------------------
    // PRIVATE STATIC FUNCTIONS
    // --------------------------------------------------------------------

    /**
     * @brief Parses the input as a positive integer that fits into a 'uint8_t' type.
     * @note This function assumes that the first character is a digit and terminates parsing at the presence of the first non-digit character or when value overflows.
     * @param it The iterator that is the start of the number string | eg. it = 1 when number string = 142.
     * @param max_value The max value the inputted string can convert to before it is an error.
     * @returns The string's value as an 8 bit number.
     */
    static constexpr uint8_t parse_positive_small_int(const_iterator &it, const int max_value) {
      assert(max_value < 256);

      int value = 0;  // The running converted number
      do {            // TODO: Stop after 3 parsed characters as extra measure
        value = (value * 10) + static_cast<int>(*it++ - '0');
        USF_ENFORCE(value <= max_value, std::runtime_error);  // Check for overflow
      } while (*it >= '0' && *it <= '9');                     // Keep parsing the string until a non-numeric value is reached

      return static_cast<uint8_t>(value);
    }

    /**
     * @brief Maps a character to an alignment type
     * @param ch The character that could be an alignment type
     * @returns The flag which represents the alignment of the argument, or a none flag if the character is not an alignment signifier
     */
    static constexpr uint8_t parse_align_flag(const CharT ch) noexcept {
      switch (ch) {
        case '<':
          return Flags::kAlignLeft;
          break;
        case '>':
          return Flags::kAlignRight;
          break;
        case '^':
          return Flags::kAlignCenter;
          break;
        case '=':
          return Flags::kAlignNumeric;
          break;
        default:
          return Flags::kNone;
          break;
      }
    }

    // --------------------------------------------------------------------
    // PRIVATE MEMBER VARIABLES
    // --------------------------------------------------------------------

    CharT m_fill_char = ' ';
    Type m_type = Type::kNone;
    uint8_t m_flags = Flags::kEmpty;
    uint8_t m_width = 0;
    int8_t m_precision = -1;
    int8_t m_index = -1;
  };

}  // namespace usf::internal

#endif  // USF_ARG_FORMAT_HPP


// ----------------------------------------------------------------------------
// @file    usf_arg_custom_type.hpp
// @brief   User-defined custom type class (using the delegate idiom).
// @date    07 January 2019
// ----------------------------------------------------------------------------

#ifndef USF_ARG_CUSTOM_TYPE_HPP
#define USF_ARG_CUSTOM_TYPE_HPP

namespace usf {
namespace internal {

  template <typename CharT>
  class ArgCustomType {
   public:
    // --------------------------------------------------------------------
    // PUBLIC MEMBER FUNCTIONS
    // --------------------------------------------------------------------

    constexpr ArgCustomType() = delete;

    template <typename T, std::span<CharT> (*func)(std::span<CharT>, const T&)>
    static constexpr ArgCustomType create(const T* obj) {
      return ArgCustomType(invoke_func<T, func>, obj);
    }

    constexpr std::span<CharT> operator()(std::span<CharT> dst) const {
      return m_function(dst, m_obj);
    }

   private:
    // --------------------------------------------------------------------
    // PRIVATE TYPE ALIASES
    // --------------------------------------------------------------------

    using FunctionType = std::span<CharT> (*)(std::span<CharT>, const void*);

    // --------------------------------------------------------------------
    // PRIVATE MEMBER FUNCTIONS
    // --------------------------------------------------------------------

    constexpr ArgCustomType(const FunctionType func, const void* obj)
        : m_function{func}, m_obj{obj} {}

    template <typename T, std::span<CharT> (*func)(std::span<CharT>, const T&)>
    static constexpr std::span<CharT> invoke_func(std::span<CharT> dst, const void* obj) {
      return func(dst, *static_cast<const T*>(obj));
    }

    // --------------------------------------------------------------------
    // PRIVATE VARIABLES
    // --------------------------------------------------------------------

    const FunctionType m_function{nullptr};
    const void* m_obj{nullptr};
  };

}
}  // namespace usf::internal

#endif  // USF_ARG_CUSTOM_TYPE_HPP


// ----------------------------------------------------------------------------
// @file    usf_argument.hpp
// @brief   Argument format processor class.
// @date    14 January 2019
// ----------------------------------------------------------------------------

#ifndef USF_ARGUMENT_HPP
#define USF_ARGUMENT_HPP

namespace usf {
  namespace internal {

    template <typename CharT>
    class Argument {
     public:
      // --------------------------------------------------------------------
      // PUBLIC TYPE ALIASES
      // --------------------------------------------------------------------

      using iterator = CharT *;
      using const_iterator = const CharT *;

      using Format = ArgFormat<CharT>;

      // --------------------------------------------------------------------
      // PUBLIC MEMBER FUNCTIONS
      // --------------------------------------------------------------------

      constexpr Argument() = delete;

      constexpr Argument(const bool value) noexcept
          : m_bool(value), m_type_id(TypeId::kBool) {}

      constexpr Argument(const CharT value) noexcept
          : m_char(value), m_type_id(TypeId::kChar) {}

      constexpr Argument(const int32_t value) noexcept
          : m_int32(value), m_type_id(TypeId::kInt32) {}

      constexpr Argument(const uint32_t value) noexcept
          : m_uint32(value), m_type_id(TypeId::kUint32) {}

      constexpr Argument(const int64_t value) noexcept
          : m_int64(value), m_type_id(TypeId::kInt64) {}

      constexpr Argument(const uint64_t value) noexcept
          : m_uint64(value), m_type_id(TypeId::kUint64) {}

      constexpr Argument(const void *value) noexcept
          : m_pointer(reinterpret_cast<std::uintptr_t>(value)), m_type_id(TypeId::kPointer) {}

#if !defined(USF_DISABLE_FLOAT_SUPPORT)

      constexpr Argument(const double value) noexcept
          : m_float(value), m_type_id(TypeId::kFloat) {}

#endif

      constexpr Argument(const std::basic_string_view<CharT> value) noexcept
          : m_string(value), m_type_id(TypeId::kString) {}

      constexpr Argument(const std::span<const std::basic_string_view<CharT>> value) noexcept
          : m_translatable_string(value), m_type_id(TypeId::kTranslatableString) {}

      constexpr Argument(const ArgCustomType<CharT> value) noexcept
          : m_custom(value), m_type_id(TypeId::kCustom) {}

      /**
       * @brief Formats the string according the the format specifier variables.
       * @param dst The string where the formatted data will be written.
       * @param format The object which contains all the format data.
       */
      constexpr void format(std::span<CharT> &dst, Format &format, locale_t locale = c_locale) const { // std_locale is a locale which defaults to then en_US locale style, this can be customized in the usf_locale file
        iterator it = dst.begin().base();

        switch (m_type_id) {  // Format it according to its type
          case TypeId::kBool:
            format_bool(it, dst.end().base(), format, m_bool);
            break;
          case TypeId::kChar:
            format_char(it, dst.end().base(), format, m_char);
            break;
          case TypeId::kInt32:
            format_integer(it, dst.end().base(), format, m_int32);
            break;
          case TypeId::kUint32:
            format_integer(it, dst.end().base(), format, m_uint32);
            break;
          case TypeId::kInt64:
            format_integer(it, dst.end().base(), format, m_int64);
            break;
          case TypeId::kUint64:
            format_integer(it, dst.end().base(), format, m_uint64);
            break;
          case TypeId::kPointer:
            format_pointer(it, dst.end().base(), format, m_pointer);
            break;
#if !defined(USF_DISABLE_FLOAT_SUPPORT)
          case TypeId::kFloat:
            format_float(it, dst.end().base(), format, m_float, locale);
            break;
#endif
          case TypeId::kString:
            format_string(it, dst.end().base(), format, m_string);
            break;
          case TypeId::kTranslatableString:
            format_string(it, dst.end().base(), format, *(m_translatable_string.begin() + static_cast<uint16_t>(locale.identity.language)));
            break;
          case TypeId::kCustom:
            USF_ENFORCE(format.is_empty(), std::runtime_error);
            it = m_custom(dst).end().base();
            break;
        }

        dst = dst.subspan(static_cast<uint32_t>(it - dst.begin().base()));  // TODO: Sign conversion (WHY)
      }

     private:
      // --------------------------------------------------------------------
      // PRIVATE STATIC FUNCTIONS
      // --------------------------------------------------------------------

      static constexpr void format_bool(iterator &it, iterator end,
                                        const Format &format, const bool value) {
        if (format.type_is_none()) {
          format_string(it, end, format, value ? "true" : "false", value ? 4 : 5);
        } else if (format.type_is_integer()) {
          format_integer(it, end, format, static_cast<uint32_t>(value));
        } else {
          // Argument type / format mismatch
          USF_CONTRACT_VIOLATION(std::runtime_error);
        }
      }

      static constexpr void format_char(iterator &it, iterator end,
                                        Format &format, const CharT value) {
        if (format.type_is_none() || format.type_is_char()) {
          // Characters and strings align to left by default.
          format.default_align_left();

          const int fill_after = format.write_alignment(it, end, 1, false);
          *it++ = value;
          CharTraits::assign(it, format.fill_char(), fill_after);
        } else if (format.type_is_integer()) {
          format_integer(it, end, format, static_cast<int32_t>(value));
        } else {
          // Argument type / format mismatch
          USF_CONTRACT_VIOLATION(std::runtime_error);
        }
      }

      template <typename T, typename std::enable_if<std::is_signed<T>::value, bool>::type = true>
      static constexpr void format_integer(iterator &it, iterator end,
                                           const Format &format, const T value) {
        using unsigned_type = typename std::make_unsigned<T>::type;

        const bool negative = (value < 0);                                          // TODO: This shouldn't be needed with to_chars
        const auto uvalue = static_cast<unsigned_type>(negative ? -value : value);  // Get the absolute value

        format_integer(it, end, format, uvalue, negative);
      }

      template <typename T, typename std::enable_if<std::is_unsigned<T>::value, bool>::type = true>
      static constexpr void format_integer(iterator &it, iterator end, const Format &format,
                                           const T value, const bool negative = false) {
        int fill_after = 0;  //

        if (format.type_is_none() || format.type_is_integer_dec()) {  // If there is no specified format or base 10
          const auto digits = Integer::count_digits_dec(value);       // Count how many digits value has
          fill_after = format.write_alignment(it, end, digits, negative);
          it += digits;                             // Offset the iterator by the numbe of digits
                                                    //          auto [ptr, err] = std::to_chars(it, it + digits, value);  // TODO: Is it + digits always correct?
                                                    //          it = ptr;
          Integer::convert_dec(it, value);          // This function writes the number from right to left, which is why the iterator was advanced by the number of digits
        } else if (format.type_is_integer_hex()) {  // If it is hex format
          const auto digits = Integer::count_digits_hex(value);
          fill_after = format.write_alignment(it, end, digits, negative);
          it += digits;
          Integer::convert_hex(it, value, format.uppercase());
        } else if (format.type_is_integer_oct()) {  // If it is octal
          const auto digits = Integer::count_digits_oct(value);
          fill_after = format.write_alignment(it, end, digits, negative);
          it += digits;
          Integer::convert_oct(it, value);
        } else if (format.type_is_integer_bin()) {
          const auto digits = Integer::count_digits_bin(value);
          fill_after = format.write_alignment(it, end, digits, negative);
          it += digits;
          Integer::convert_bin(it, value);
        } else {
          // Argument type / format mismatch
          USF_CONTRACT_VIOLATION(std::runtime_error);
        }

        CharTraits::assign(it, format.fill_char(), fill_after);
      }

      static constexpr void format_pointer(iterator &it, const_iterator end,
                                           const Format &format, const std::uintptr_t value) {
        if (format.type_is_none() || format.type_is_pointer()) {
#if defined(USF_TARGET_64_BITS)
          const auto ivalue = static_cast<uint64_t>(value);
#else
          const auto ivalue = static_cast<uint32_t>(value);
#endif
          const auto digits = Integer::count_digits_hex(ivalue);
          const auto fill_after = format.write_alignment(it, end, digits, false);
          it += digits;
          Integer::convert_hex(it, ivalue, format.uppercase());
          CharTraits::assign(it, format.fill_char(), fill_after);
        } else {
          // Argument type / format mismatch
          USF_CONTRACT_VIOLATION(std::runtime_error);
        }
      }

#if !defined(USF_DISABLE_FLOAT_SUPPORT)

      static constexpr void format_float(iterator &it, iterator end, const Format &format, double value, locale_t locale) {
        // Test for argument type / format match
        USF_ENFORCE(format.type_is_none() || format.type_is_float(), std::runtime_error);

        if (std::isnan(value)) {
//          format_string(it, end, format, format.uppercase() ? "NAN" : "nan", 3);
          format_string(it, end, format, locale.numbers.symbols.nan.data(), static_cast<int>(locale.numbers.symbols.nan.length()));
        } else {
          const bool negative = std::signbit(value);

          if (std::isinf(value)) {
//            format_string(it, end, format, format.uppercase() ? "INF" : "inf", 3, negative);
            format_string(it, end, format, locale.numbers.symbols.infinity.data(), static_cast<int>(locale.numbers.symbols.infinity.length()), negative);
          } else {
            if (negative) { value = -value; }

            struct fp_t {
              union {
                double d;
                uint64_t i;
              };
            };

            const fp_t fp_value{{value}};

            if (fp_value.i == 0) {
              format_float_zero(it, end, format, negative);
            } else if (value >= 1E-19 && value <= 1.8446744E19) {
              int precision = format.precision();

              if (precision < 0) { precision = 6; }

              bool format_fixed = format.type_is_float_fixed();
              bool significant_figures = false;

              if (format.type_is_none() || format.type_is_float_general()) {
                // General format
                significant_figures = true;

                if (precision > 0) { --precision; }
              }

              CharT significand[36]{};  // 34 characters should be the maximum size needed
              int exponent = 0;

              const auto significand_size = Float::convert(significand, exponent, value, format_fixed, precision);

              if (significant_figures) {
                if (exponent >= -4 && exponent <= precision) {
                  format_fixed = true;
                }

                if (!format.hash()) { precision = significand_size - 1; }

                if (format_fixed) {
                  precision -= exponent;
                }
              }

              int fill_after = 0;

              if (format_fixed) {
                // Fixed point format
                if (exponent < 0) {
                  // 0.<0>SIGNIFICAND[0:N]<0>

                  const int full_digits = precision + 2;
                  fill_after = format.write_alignment(it, end, full_digits, negative);

                  *it++ = '0';
                  format_string(it, end, locale.numbers.symbols.decimal.data(), static_cast<int>(locale.numbers.symbols.decimal.size()));

                  int zero_digits = -exponent - 1;
                  CharTraits::assign(it, '0', zero_digits);
                  CharTraits::copy(it, significand, significand_size);

                  // Padding is needed if conversion function removes trailing zeros.
                  zero_digits = precision - zero_digits - significand_size;
                  CharTraits::assign(it, '0', zero_digits);
                } else {
                  const int full_digits = exponent + 1 + precision + static_cast<int>(precision > 0 || format.hash());
                  fill_after = format.write_alignment(it, end, full_digits, negative);

                  const int ipart_digits = exponent + 1;

                  if (ipart_digits >= significand_size) {
                    // [SIGNIFICAND]<0><.><0>

                    CharTraits::copy(it, significand, significand_size);
                    CharTraits::assign(it, '0', ipart_digits - significand_size);

                    if (precision > 0 || format.hash()) {
                      format_string(it, end, locale.numbers.symbols.decimal.data(), static_cast<int>(locale.numbers.symbols.decimal.size()));
                    }

                    if (precision > 0) {
                      CharTraits::assign(it, '0', precision);
                    }
                  } else {
                    // SIGNIFICAND[0:x].SIGNIFICAND[x:N]<0>

                    CharTraits::copy(it, significand, ipart_digits);
                    format_string(it, end, locale.numbers.symbols.decimal.data(), static_cast<int>(locale.numbers.symbols.decimal.size()));

                    const int copy_size = significand_size - ipart_digits;
                    CharTraits::copy(it, significand + ipart_digits, copy_size);

                    // Padding is needed if conversion function removes trailing zeros.
                    CharTraits::assign(it, '0', precision - copy_size);
                  }
                }
              } else {
                // Exponent format
                // SIGNIFICAND[0:N]<.>eEXP
                // OR
                // SIGNIFICAND[0].SIGNIFICAND[1:N]<0>eEXP

                const int full_digits = 5 + precision + static_cast<int>(precision > 0 || format.hash());
                fill_after = format.write_alignment(it, end, full_digits, negative);

                *it++ = *significand;

                if (precision > 0 || format.hash()) {
                  format_string(it, end, locale.numbers.symbols.decimal.data(), static_cast<int>(locale.numbers.symbols.decimal.size()));

                  const int copy_size = significand_size - 1;
                  CharTraits::copy(it, significand + 1, copy_size);
                  CharTraits::assign(it, '0', precision - copy_size);
                }

                write_float_exponent(it, exponent, format.uppercase());
              }

              CharTraits::assign(it, format.fill_char(), fill_after);

              //it += sprintf(it, "[%s] Size:%d Exponent:%d Precision:%d Fixed:%d->", significand, significand_size, exponent, precision, int(format_fixed));
            } else {
              format_string(it, end, format, format.uppercase() ? "OVF" : "ovf", 3, negative);
            }
          }
        }
      }

      static constexpr void
      write_float_exponent(iterator &it, int exponent, const bool uppercase) noexcept {
        *it++ = uppercase ? 'E' : 'e';

        if (exponent < 0) {
          exponent = -exponent;
          *it++ = '-';
        } else {
          *it++ = '+';
        }

        // No point in making a proper integer to string
        // conversion for exponent since we only support [e-19; e19].
        assert(exponent <= 19);

        if (exponent < 10) {
          *it++ = '0';
          *it++ = static_cast<CharT>('0' + exponent);
        } else {
          *it++ = '1';
          *it++ = static_cast<CharT>('0' + (exponent - 10));
        }
      }

      static constexpr void format_float_zero(iterator &it, const_iterator end, const Format &format, const bool negative) {
        int precision = 0;

        if (format.type_is_float_fixed() || format.type_is_float_scientific()) {
          precision = format.precision();
        }

        int digits = 1;

        if (precision > 0) { digits += precision + 1; }

        if (format.type_is_float_scientific()) { digits += 4; }

        const int fill_after = format.write_alignment(it, end, digits, negative);

        *it++ = '0';

        if (precision > 0) {
          *it++ = '.';
          CharTraits::assign(it, '0', precision);
        }

        if (format.type_is_float_scientific()) {
          *it++ = format.uppercase() ? 'E' : 'e';
          *it++ = '+';
          *it++ = '0';
          *it++ = '0';
        }

        CharTraits::assign(it, format.fill_char(), fill_after);
      }

#endif  // !defined(USF_DISABLE_FLOAT_SUPPORT)

      static constexpr void format_string(iterator &it, const_iterator end,
                                          Format &format, std::basic_string_view<CharT> str) {
        // Test for argument type / format match
        USF_ENFORCE(format.type_is_none() || format.type_is_string() || format.type_is_translatable_string(), std::runtime_error);

        // Characters and strings align to left by default.
        format.default_align_left();

        // If precision is specified use it up to string size.
        const int str_length = (format.precision() == -1)
                                   ? static_cast<int>(str.size())
                                   : std::min(static_cast<int>(format.precision()), static_cast<int>(str.size()));

        format_string(it, end, format, str.data(), str_length);
      }

//      static constexpr void format_translatable_string(iterator &it, const_iterator end, Format &format, const std::basic_string_view<CharT> &str) {
//        // Test for argument type / format match
//        USF_ENFORCE(format.type_is_none() || format.type_is_translatable_string(), std::runtime_error);
//
//        // Characters and strings align to left by default.
//        format.default_align_left();
//
//        // If precision is specified use it up to string size.
//        const int str_length = (format.precision() == -1) ? static_cast<int>(str.size()) : std::min(static_cast<int>(format.precision()), static_cast<int>(str.size()));
//
//        format_string(it, end, format, str.data(), str_length);
//      }


      template <typename CharSrc,
                typename std::enable_if<std::is_convertible<CharSrc, CharT>::value, bool>::type = true>
      static constexpr void format_string(iterator &it, const_iterator end,
                                          const Format &format, const CharSrc *str,
                                          const int str_length, const bool negative = false) {
        const int fill_after = format.write_alignment(it, end, str_length, negative);

        CharTraits::copy(it, str, str_length);
        CharTraits::assign(it, format.fill_char(), fill_after);
      }

      template <typename CharSrc,
                typename std::enable_if<std::is_convertible<CharSrc, CharT>::value, bool>::type = true>
      static constexpr void format_string(iterator &it, const_iterator end, const CharSrc *str, const int str_length) {
        assert(it + str_length < end); // TODO: This entire function
        CharTraits::copy(it, str, str_length);
      }

      // --------------------------------------------------------------------
      // PRIVATE MEMBER VARIABLES
      // --------------------------------------------------------------------

      enum class TypeId {
        kBool = 0,
        kChar,
        kInt32,
        kUint32,
        kInt64,
        kUint64,
        kPointer,
#if !defined(USF_DISABLE_FLOAT_SUPPORT)
        kFloat,
#endif
        kString,
        kTranslatableString,
        kCustom
      };

      union {
        bool m_bool;
        CharT m_char;
        int32_t m_int32;
        uint32_t m_uint32;
        int64_t m_int64;
        uint64_t m_uint64;
        std::uintptr_t m_pointer;
#if !defined(USF_DISABLE_FLOAT_SUPPORT)
        double m_float;
#endif
        std::basic_string_view<CharT> m_string;
        std::span<const std::basic_string_view<CharT>> m_translatable_string;
        ArgCustomType<CharT> m_custom;
      };

      TypeId m_type_id;
    };

    // Boolean
    template <typename CharT>
    inline constexpr Argument<CharT>
    make_argument(const bool arg) {
      return arg;
    }

    // Character (char)
    template <typename CharT>
    inline constexpr Argument<CharT>
    make_argument(const char arg) {
      return static_cast<CharT>(arg);
    }

    // Character (CharT != char)
    template <typename CharT, typename std::enable_if<!std::is_same<CharT, char>::value, bool>::type = true>
    inline constexpr Argument<CharT>
    make_argument(const CharT arg) {
      return arg;
    }

    // 8 bit signed integer
    template <typename CharT>
    inline constexpr Argument<CharT>
    make_argument(const int8_t arg) {
      return static_cast<int32_t>(arg);
    }

    // 8 bit unsigned integer
    template <typename CharT>
    inline constexpr Argument<CharT>
    make_argument(const uint8_t arg) {
      return static_cast<uint32_t>(arg);
    }

    // 16 bit signed integer
    template <typename CharT>
    inline constexpr Argument<CharT>
    make_argument(const int16_t arg) {
      return static_cast<int32_t>(arg);
    }

    // 16 bit unsigned integer
    template <typename CharT>
    inline constexpr Argument<CharT>
    make_argument(const uint16_t arg) {
      return static_cast<uint32_t>(arg);
    }

    // 32 bit signed integer
    template <typename CharT>
    inline constexpr Argument<CharT>
    make_argument(const int arg) {
      return static_cast<int32_t>(arg);
    }

    // 32 bit unsigned integer
    template <typename CharT>
    inline constexpr Argument<CharT>
    make_argument(const unsigned int arg) {
      return static_cast<uint32_t>(arg);
    }

#if (__LONG_MAX__ != __LONG_LONG_MAX__)

    // 32 bit signed integer
    template <typename CharT>
    inline constexpr Argument<CharT>
    make_argument(const long int arg) {
      return static_cast<int32_t>(arg);
    }

    // 32 bit unsigned integer
    template <typename CharT>
    inline constexpr Argument<CharT>
    make_argument(const unsigned long int arg) {
      return static_cast<uint32_t>(arg);
    }

#endif  // (__LONG_MAX__ != __LONG_LONG_MAX__)

    // 64 bit signed integer
    template <typename CharT>
    inline constexpr Argument<CharT>
    make_argument(const int64_t arg) {
      if (arg >= std::numeric_limits<int32_t>::min()
          && arg <= std::numeric_limits<int32_t>::max()) {
        return static_cast<int32_t>(arg);
      }

      return arg;
    }

    // 64 bit unsigned integer
    template <typename CharT>
    inline constexpr Argument<CharT>
    make_argument(const uint64_t arg) {
      if (arg <= std::numeric_limits<uint32_t>::max()) {
        return static_cast<uint32_t>(arg);
      }

      return arg;
    }

    // Pointer (void*)
    template <typename CharT>
    inline constexpr Argument<CharT>
    make_argument(void *arg) {
      return arg;
    }

    // Pointer (const void*)
    template <typename CharT>
    inline constexpr Argument<CharT>
    make_argument(const void *arg) {
      return arg;
    }

#if !defined(USF_DISABLE_FLOAT_SUPPORT)

    // Floating point (float)
    template <typename CharT>
    inline constexpr Argument<CharT>
    make_argument(float arg) {
      return static_cast<double>(arg);
    }

    // Floating point (double)
    template <typename CharT>
    inline constexpr Argument<CharT>
    make_argument(double arg) {
      return arg;
    }

#endif  // !defined(USF_DISABLE_FLOAT_SUPPORT)

    // String (convertible to string view)
    template <typename CharT, typename T,
              typename std::enable_if<std::is_convertible<T, std::basic_string_view<CharT>>::value, bool>::type = true>
    inline constexpr Argument<CharT> make_argument(const T &arg) {
      return std::basic_string_view<CharT>(arg);
    }

    // Translation key
    template <typename CharT>
    inline constexpr Argument<CharT> make_argument(const std::span<const std::basic_string_view<CharT>>& arg) {
      return arg;
    }

  }  // namespace internal

  // User-defined custom type formatter forward declaration
  template <typename CharT, typename T>
  struct Formatter {
    static std::span<CharT> format_to(std::span<CharT>, const T &);
  };

  namespace internal {

    // User-defined custom type
    template <typename CharT, typename T, typename std::enable_if<!std::is_convertible<T, std::basic_string_view<CharT>>::value, bool>::type = true>
    inline constexpr Argument<CharT> make_argument(const T &arg) {
      using decay_T = typename std::decay<decltype(arg)>::type;

      return ArgCustomType<CharT>::template create<decay_T, &usf::Formatter<CharT, decay_T>::format_to>(&arg);
    }

  }  // namespace internal
}  // namespace usf

#endif  // USF_ARGUMENT_HPP


// ----------------------------------------------------------------------------
// @file    usf_main.hpp
// @brief   Main process functions and public interface.
// @date    14 January 2019
// ----------------------------------------------------------------------------

#ifndef USF_MAIN_HPP
#define USF_MAIN_HPP

namespace usf {
  namespace internal {
    inline locale_t global_locale;

    template <typename CharT>
    constexpr inline void parse_format_string(std::span<CharT> &str, std::basic_string_view<CharT> &fmt) {
      CharT *str_it = str.begin().base();
      const CharT *fmt_it = fmt.cbegin();
      while (fmt_it < fmt.cend() && str_it < str.end().base()) {
        if (*fmt_it == '{') {
          if (*(fmt_it + 1) == '{') {
            // Found '{{' escape character, skip the first and copy the second '{'.
            ++fmt_it;
            *str_it++ = *fmt_it++;
          } else {
            // A type format should follow...
            break;
          }
        } else if (*fmt_it == '}') {
          USF_ENFORCE(*(fmt_it + 1) == '}', std::runtime_error);

          // Found '}}' escape character, skip the first and copy the second '}'.
          ++fmt_it;
          *str_it++ = *fmt_it++;
        } else {
          // Copy literal text
          *str_it++ = *fmt_it++;
        }
      }

      //USF_ENFORCE(str_it < str.end(), std::runtime_error);

      str = str.subspan(static_cast<uint32_t>(str_it - str.data()));    // TODO: Sign conversion error
                                                                        //    str.remove_prefix();
      fmt.remove_prefix(static_cast<uint32_t>(fmt_it - fmt.cbegin()));  // TODO: Sign conversion error
    }

    template <typename CharT>
    constexpr inline void process(std::span<CharT> &str, std::basic_string_view<CharT> &fmt,
                           const Argument<CharT> *const args, const int arg_count, locale_t locale = c_locale) {
      // Argument's sequential index
      int arg_seq_index = 0;

      parse_format_string(str, fmt);

      while (!fmt.empty()) {
        ArgFormat<CharT> format(fmt, arg_count);  // Parse the format specifier and generate a format object

        // Determine which argument index to use, sequential or positional.
        int arg_index = format.index();

        if (arg_index < 0) {  // If it is sequential (arg_index == -1)
          USF_ENFORCE(arg_seq_index < arg_count, std::runtime_error);
          arg_index = arg_seq_index++;  // Assign it the next index
        }

        args[arg_index].format(str, format, locale);

        parse_format_string(str, fmt);
      }
    }

  }  // namespace internal

  template <typename CharT, typename... Args>
  constexpr inline std::span<CharT> basic_format_to(std::span<CharT> str, std::basic_string_view<CharT> fmt) {
    auto str_begin = str.begin();

    internal::parse_format_string(str, fmt);

    USF_ENFORCE(fmt.empty(), std::runtime_error);

#if !defined(USF_DISABLE_STRING_TERMINATION)
    // If not disabled in configuration, null terminate the resulting string.
    str[0] = CharT{};
#endif

    // Return a string span to the resulting string
    return std::span<CharT>(str_begin, str.begin());
  }

  template <typename CharT, typename... Args>
  constexpr inline std::span<CharT> basic_format_to(std::span<CharT> str, std::basic_string_view<CharT> fmt, Args &&...args) {
    // Nobody should be that crazy, still... it costs nothing to be sure!
    static_assert(sizeof...(Args) < 128, "usf::basic_format_to(): crazy number of arguments supplied!");

    auto str_begin = str.begin();  // This keeps the start of the string since the str pointer will be incremented throughout the following methods

    const internal::Argument<CharT> arguments[sizeof...(Args)]{internal::make_argument<CharT>(args)...};

    internal::process(str, fmt, arguments, static_cast<int>(sizeof...(Args)));

#if !defined(USF_DISABLE_STRING_TERMINATION)
    // If not disabled in configuration, null terminate the resulting string.
    str[0] = CharT{};  // Since str has been incremented through the above methods, it now resides at the end of the formatted string so the termination can be written directly at it
#endif

    // Return a string span to the resulting string
    return std::span<CharT>(str_begin, str.begin());  // The complete string is now residing between str_begin and str, so return that
  }

#ifndef USF_DISABLE_LOCALE_SUPPORT
  template <typename CharT, typename... Args>
  constexpr inline std::span<CharT> basic_format_to(std::span<CharT> str, locale_t locale, std::basic_string_view<CharT> fmt, Args &&...args) {
    // Nobody should be that crazy, still... it costs nothing to be sure!
    static_assert(sizeof...(Args) < 128, "usf::basic_format_to(): crazy number of arguments supplied!");

    auto str_begin = str.begin();  // This keeps the start of the string since the str pointer will be incremented throughout the following methods

    const internal::Argument<CharT> arguments[sizeof...(Args)]{internal::make_argument<CharT>(args)...};

    internal::process(str, fmt, arguments, static_cast<int>(sizeof...(Args)), locale);

#if !defined(USF_DISABLE_STRING_TERMINATION)
    // If not disabled in configuration, null terminate the resulting string.
    str[0] = CharT{};  // Since str has been incremented through the above methods, it now resides at the end of the formatted string so the termination can be written directly at it
#endif

    // Return a string span to the resulting string
    return std::span<CharT>(str_begin, str.begin());  // The complete string is now residing between str_begin and str, so return that
  }
#endif

  template <typename CharT, typename... Args>
  constexpr inline CharT *
  basic_format_to(CharT *str, const std::ptrdiff_t str_count, std::basic_string_view<CharT> fmt, Args &&...args) {
    return basic_format_to(std::span<CharT>(str, str_count), fmt, args...).end().base();
  }

  // ----------------------------------------------------------------------------
  // Formats a char string
  // ---------------------------------------------------------------------------
  template <typename... Args>
  constexpr inline std::span<char> format_to(std::span<char> str, std::string_view fmt, Args &&...args) {
    return basic_format_to(str, fmt, args...);
  }

  template <typename... Args>
  constexpr inline char *format_to(char *str, const std::ptrdiff_t str_count, std::string_view fmt, Args &&...args) {
    return basic_format_to(str, str_count, fmt, args...);
  }

  // ----------------------------------------------------------------------------
  // Formats a wchar_t string
  // ---------------------------------------------------------------------------
  template <typename... Args>
  constexpr inline std::span<wchar_t> format_to(std::span<wchar_t> str, std::wstring_view fmt, Args &&...args) {
    return basic_format_to(str, fmt, args...);
  }

  template <typename... Args>
  constexpr inline wchar_t *format_to(wchar_t *str, const std::ptrdiff_t str_count, std::wstring_view fmt, Args &&...args) {
    return basic_format_to(str, str_count, fmt, args...);
  }

// ----------------------------------------------------------------------------
// Formats a char8_t string
// ---------------------------------------------------------------------------
#if defined(USF_CPP20_CHAR8_T_SUPPORT)
//  template <typename... Args>
//  constexpr inline std::span<char8_t> format_to(std::span<char8_t> str, std::u8string_view fmt, Args &&...args) {
//    return basic_format_to(str, fmt, args...);
//  }

  template <typename... Args>
  constexpr inline std::span<char8_t> format_to(std::span<char8_t> str, locale_t locale, std::u8string_view fmt, Args &&...args) {
    return basic_format_to(str, locale, fmt, args...);
  }

  template <typename... Args>
  constexpr inline std::span<char8_t> format_to(std::span<char8_t> str, std::u8string_view fmt, Args &&...args) {
    return basic_format_to(str, internal::global_locale, fmt, args...);
  }

//  template <typename... Args>
//  std::span<char8_t> format_to(std::pmr::u8string&& str, std::u8string_view fmt, Args &&...args) {
//    str.reserve(str.max_size());
//    return basic_format_to(str, internal::global_locale, fmt, args...);
//  }

  template <typename... Args>
  constexpr inline char8_t *format_to(char8_t *str, const std::ptrdiff_t str_count, char8_t fmt, Args &&...args) {
    return basic_format_to(str, str_count, fmt, args...);
  }
#endif  // defined(USF_CPP20_CHAR8_T_SUPPORT)

  // ----------------------------------------------------------------------------
  // Formats a char16_t string
  // ---------------------------------------------------------------------------
  template <typename... Args>
  constexpr inline std::span<char16_t> format_to(std::span<char16_t> str, std::u16string_view fmt, Args &&...args) {
    return basic_format_to(str, fmt, args...);
  }

  template <typename... Args>
  constexpr inline char16_t *format_to(char16_t *str, const std::ptrdiff_t str_count, std::u16string_view fmt, Args &&...args) {
    return basic_format_to(str, str_count, fmt, args...);
  }

  // ----------------------------------------------------------------------------
  // Formats a char32_t string
  // ---------------------------------------------------------------------------
  template <typename... Args>
  constexpr inline std::span<char32_t> format_to(std::span<char32_t> str, std::u32string_view fmt, Args &&...args) {
    return basic_format_to(str, fmt, args...);
  }

  template <typename... Args>
  constexpr inline char32_t *format_to(char32_t *str, const std::ptrdiff_t str_count, std::u32string_view fmt, Args &&...args) {
    return basic_format_to(str, str_count, fmt, args...);
  }

  // ----------------------------------------------------------------------------
  // Formats a byte string as char string
  // ----------------------------------------------------------------------------
  //  template <typename... Args> constexpr
  //  ByteStringSpan format_to(ByteStringSpan str, StringView fmt, Args&&... args)
  //  {
  //      static_assert(CHAR_BIT == 8, "usf::format_to(): invalid char size.");
  //      char *end = basic_format_to(reinterpret_cast<char*>(str.data()), str.size(), fmt, args...);
  //
  //      return ByteStringSpan(str.begin(), reinterpret_cast<uint8_t*>(end));
  //  }

  template <typename... Args>
  constexpr inline uint8_t *format_to(uint8_t *str, const std::ptrdiff_t str_count, std::string_view fmt, Args &&...args) {
    static_assert(CHAR_BIT == 8, "usf::format_to(): invalid char size.");
    return reinterpret_cast<uint8_t *>(basic_format_to(reinterpret_cast<char *>(str), str_count, fmt, args...));
  }

  inline auto GlobalLocale(locale_t new_locale) -> void {
    internal::global_locale = new_locale;
  }

}  // namespace usf

#endif  // USF_MAIN_HPP

#endif // USF_HPP
