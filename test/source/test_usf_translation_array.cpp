//
// Created by treys on 6/7/2022.
//

#include "gtest/gtest.h"
#include "sample_locales.hpp"
#include "unit_tests_config.hpp"

using namespace std::literals::string_view_literals;

// Translation arrays must be in alphabetical order by their 2 letter language identifier with en being first
static constexpr std::array<std::u8string_view, 4> hello_translated = {
    u8"Hello"sv,      // en
    u8"Halløj"sv,     // da
    u8"¿Qué tal?"sv,  // es
    u8"こんにちは"sv  // jp
};

constexpr std::array goodbye_translated = {
    u8"Goodbye"sv,    // en
    u8"Farvel"sv,     // da
    u8"Adiós"sv,      // es
    u8"さようなら"sv  // jp
};

constexpr auto ExampleData_map = eternal::translation_map({hello_translated, goodbye_translated});

static_assert(ExampleData_map[u8"Hello"][0] == u8"Hello"); // Make sure the array resolution happens at compile time

TEST(USF, TranslationArray) {
  std::array<char8_t, 256> str{};
  usf::format_to(str, ja_JP, u8"{}", ExampleData_map[u8"Hello"]);
  EXPECT_STREQ(reinterpret_cast<const char*>(str.data()), reinterpret_cast<const char*>(u8"こんにちは"));
  usf::format_to(str, da_DK, u8"{:t}", _tr(u8"Hello", ExampleData_map));
  EXPECT_STREQ(reinterpret_cast<const char*>(str.data()), reinterpret_cast<const char*>(u8"Halløj"));
  usf::format_to(str, en_US, u8"{}", _tr(u8"Hello", ExampleData_map));
  EXPECT_STREQ(reinterpret_cast<const char*>(str.data()), reinterpret_cast<const char*>(u8"Hello"));
  usf::format_to(str, es_ES, u8"{:t}", ExampleData_map[u8"Hello"]);
  EXPECT_STREQ(reinterpret_cast<const char*>(str.data()), reinterpret_cast<const char*>(u8"¿Qué tal?"));

  usf::format_to(str, es_ES, u8"{}", _tr(u8"Goodbye", ExampleData_map));
  EXPECT_STREQ(reinterpret_cast<const char*>(str.data()), reinterpret_cast<const char*>(u8"Adiós"));
  usf::format_to(str, en_US, u8"{:t}", ExampleData_map[u8"Goodbye"]);
  EXPECT_STREQ(reinterpret_cast<const char*>(str.data()), reinterpret_cast<const char*>(u8"Goodbye"));
  usf::format_to(str, da_DK, u8"{}", _tr(u8"Goodbye", ExampleData_map));
  EXPECT_STREQ(reinterpret_cast<const char*>(str.data()), reinterpret_cast<const char*>(u8"Farvel"));
  usf::format_to(str, ja_JP, u8"{:t}", ExampleData_map[u8"Goodbye"]);
  EXPECT_STREQ(reinterpret_cast<const char*>(str.data()), reinterpret_cast<const char*>(u8"さようなら"));

  EXPECT_STREQ(reinterpret_cast<const char*>(_tl(u8"Goodbye", ExampleData_map, ja_JP).data()), reinterpret_cast<const char*>(u8"さようなら"));
}

TEST(USF, TranslationArrayGlobal) {
  std::array<char8_t, 256> str{};
  usf::GlobalLocale(ja_JP);
  usf::format_to(str, u8"{}", ExampleData_map[u8"Hello"]);
  EXPECT_STREQ(reinterpret_cast<const char*>(str.data()), reinterpret_cast<const char*>(u8"こんにちは"));
  usf::GlobalLocale(da_DK);
  usf::format_to(str, u8"{:t}", _tr(u8"Hello", ExampleData_map));
  EXPECT_STREQ(reinterpret_cast<const char*>(str.data()), reinterpret_cast<const char*>(u8"Halløj"));
  usf::GlobalLocale(en_US);
  usf::format_to(str, u8"{}", _tr(u8"Hello", ExampleData_map));
  EXPECT_STREQ(reinterpret_cast<const char*>(str.data()), reinterpret_cast<const char*>(u8"Hello"));
  usf::GlobalLocale(es_ES);
  usf::format_to(str, u8"{:t}", ExampleData_map[u8"Hello"]);
  EXPECT_STREQ(reinterpret_cast<const char*>(str.data()), reinterpret_cast<const char*>(u8"¿Qué tal?"));

  usf::GlobalLocale(es_ES);
  usf::format_to(str, u8"{}", _tr(u8"Goodbye", ExampleData_map));
  EXPECT_STREQ(reinterpret_cast<const char*>(str.data()), reinterpret_cast<const char*>(u8"Adiós"));
  usf::GlobalLocale(en_US);
  usf::format_to(str, u8"{:t}", ExampleData_map[u8"Goodbye"]);
  EXPECT_STREQ(reinterpret_cast<const char*>(str.data()), reinterpret_cast<const char*>(u8"Goodbye"));
  usf::GlobalLocale(da_DK);
  usf::format_to(str, u8"{}", _tr(u8"Goodbye", ExampleData_map));
  EXPECT_STREQ(reinterpret_cast<const char*>(str.data()), reinterpret_cast<const char*>(u8"Farvel"));
  usf::GlobalLocale(ja_JP);
  usf::format_to(str, u8"{:t}", ExampleData_map[u8"Goodbye"]);
  EXPECT_STREQ(reinterpret_cast<const char*>(str.data()), reinterpret_cast<const char*>(u8"さようなら"));
}