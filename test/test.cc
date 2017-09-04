#define CATCH_CONFIG_MAIN
#include "catch.hpp"

#include <fstlib.h>

using namespace std;

inline std::vector<std::string> match(const std::vector<char>& byte_code, const char* str)
{
  std::vector<std::string> outputs;
  fst::exact_match_search(byte_code.data(), byte_code.size(), str, [&](const char* s, size_t l) {
    outputs.emplace_back(s, l);
  });
  return outputs;
}

inline std::vector<fst::CommonPrefixSearchResult> prefix_match(
  const std::vector<char>& byte_code, const char* str)
{
  std::vector<fst::CommonPrefixSearchResult> ret;
  fst::common_prefix_search(byte_code.data(), byte_code.size(), str, [&](const auto& result) {
    ret.emplace_back(result);
  });
  return ret;
}

TEST_CASE("Simple virtual machine test", "[general]")
{
  vector<pair<string, string>> input = {
    { "apr", "30" },
    { "aug", "31" },
    { "dec", "31" },
    { "feb", "28" },
    { "feb", "29" },
    { "jan", "31" },
    { "jul", "31" },
    { "jun", "30" },
  };

  auto sm = fst::make_state_machine(input);
  REQUIRE(sm->count == 13);

  auto byte_code = fst::compile(*sm);

  REQUIRE(match(byte_code, "apr")[0] == "30");
  REQUIRE(match(byte_code, "aug")[0] == "31");
  REQUIRE(match(byte_code, "dec")[0] == "31");
  REQUIRE(match(byte_code, "feb")[0] == "28");
  REQUIRE(match(byte_code, "feb")[1] == "29");
  REQUIRE(match(byte_code, "jul")[0] == "31");
  REQUIRE(match(byte_code, "jun")[0] == "30");

  REQUIRE(match(byte_code, "").empty());
  REQUIRE(match(byte_code, "_").empty());
  REQUIRE(match(byte_code, "a").empty());
  REQUIRE(match(byte_code, "ap").empty());
  REQUIRE(match(byte_code, "ap_").empty());
  REQUIRE(match(byte_code, "apr_").empty());
}

TEST_CASE("Edge case test1", "[general]")
{
  vector<pair<string, string>> input = {
    { "a",  "0" },
    { "ab", "1" },
  };

  auto sm = fst::make_state_machine(input);

  REQUIRE(sm->count == 3);

  auto byte_code = fst::compile(*sm);

  REQUIRE(match(byte_code, "a")[0] == "0");
  REQUIRE(match(byte_code, "ab")[0] == "1");
}

TEST_CASE("Edge case test2", "[general]")
{
  vector<pair<string, string>> input = {
    { "aa",  "0" },
    { "abb", "1" },
  };

  auto sm = fst::make_state_machine(input);

  REQUIRE(sm->count == 4);

  auto byte_code = fst::compile(*sm);

  REQUIRE(match(byte_code, "aa")[0] == "0");
  REQUIRE(match(byte_code, "abb")[0] == "1");
}

TEST_CASE("Edge case test3", "[general]")
{
  vector<pair<string, string>> input = {
    { "abc", "0" },
    { "bc",  "1" },
  };

  auto sm = fst::make_state_machine(input);

  REQUIRE(sm->count == 4);

  auto byte_code = fst::compile(*sm);

  REQUIRE(match(byte_code, "abc")[0] == "0");
  REQUIRE(match(byte_code, "bc")[0] == "1");
}

TEST_CASE("Edge case test4", "[general]")
{
  vector<pair<string, string>> input = {
    { "z",   "0"  },
    { "zc",  "10" },
    { "zcd", "11" },
    { "zd",  "1"  },
  };

  auto sm = fst::make_state_machine(input);

  REQUIRE(sm->count == 4);

  auto byte_code = fst::compile(*sm);

  REQUIRE(match(byte_code, "z")[0] == "0");
  REQUIRE(match(byte_code, "zc")[0] == "10");
  REQUIRE(match(byte_code, "zcd")[0] == "11");
  REQUIRE(match(byte_code, "zd")[0] == "1");
}

TEST_CASE("Edge case test5", "[general]")
{
  vector<pair<string, string>> input = {
    { "aba", "1" },
    { "abz", "2" },
    { "baz", "31" },
    { "bz",  "32" },
  };

  auto sm = fst::make_state_machine(input);

  REQUIRE(sm->count == 6);

  auto byte_code = fst::compile(*sm);

  REQUIRE(match(byte_code, "aba")[0] == "1");
  REQUIRE(match(byte_code, "abz")[0] == "2");
  REQUIRE(match(byte_code, "baz")[0] == "31");
  REQUIRE(match(byte_code, "bz")[0] == "32");
}

TEST_CASE("Duplicate final states test", "[general]")
{
  vector<pair<string, string>> input = {
    { "az", "0" },
    { "bz", "1" },
    { "cy", "2" },
    { "dz", "3" },
  };

  auto byte_code = fst::build(input);

  REQUIRE(match(byte_code, "az")[0] == "0");
  REQUIRE(match(byte_code, "bz")[0] == "1");
  REQUIRE(match(byte_code, "cy")[0] == "2");
  REQUIRE(match(byte_code, "dz")[0] == "3");
}

TEST_CASE("Duplicate final states test2", "[general]")
{
  vector<pair<string, string>> input = {
    { "a_a", "0" },
    { "ab", "1" },
    { "ab_a", "2" },
    { "b_a", "3" },
  };

  auto byte_code = fst::build(input);

  REQUIRE(match(byte_code, "a_a")[0] == "0");
  REQUIRE(match(byte_code, "ab")[0] == "1");
  REQUIRE(match(byte_code, "ab_a")[0] == "2");
  REQUIRE(match(byte_code, "b_a")[0] == "3");
}

TEST_CASE("UTF-8 test", "[general]")
{
  vector<pair<string, string>> input = {
    { u8"あ",   "0" },
    { u8"あい", "1" },
  };

  auto sm = fst::make_state_machine(input);

  REQUIRE(sm->count == 7);

  auto byte_code = fst::compile(*sm);

  REQUIRE(match(byte_code, u8"あ")[0] == "0");
  REQUIRE(match(byte_code, u8"あい")[0] == "1");
}

TEST_CASE("Common prefix search test", "[general]")
{
  vector<pair<string, string>> input = {
    { "a", "0" },
    { "and", "1" },
    { "android", "2" },
  };

  auto byte_code = fst::build(input);
  auto ret = prefix_match(byte_code, "android phone");

  REQUIRE(ret.size() == 3);

  REQUIRE(ret[0].length == 1);
  REQUIRE(ret[0].outputs.size() == 1);
  REQUIRE(ret[0].outputs[0] == "0");

  REQUIRE(ret[1].length == 3);
  REQUIRE(ret[1].outputs.size() == 1);
  REQUIRE(ret[1].outputs[0] == "1");

  REQUIRE(ret[2].length == 7);
  REQUIRE(ret[2].outputs.size() == 1);
  REQUIRE(ret[2].outputs[0] == "2");
}

TEST_CASE("Common prefix search test2", "[general]")
{
  vector<pair<string, string>> input = {
    { "a", "0" },
    { "and", "1" },
    { "android", "2" },
  };

  auto byte_code = fst::build(input);
  auto ret = prefix_match(byte_code, "android phone");

  REQUIRE(ret.size() == 3);

  REQUIRE(ret[0].length == 1);
  REQUIRE(ret[0].outputs.size() == 1);
  REQUIRE(ret[0].outputs[0] == "0");

  REQUIRE(ret[1].length == 3);
  REQUIRE(ret[1].outputs.size() == 1);
  REQUIRE(ret[1].outputs[0] == "1");

  REQUIRE(ret[2].length == 7);
  REQUIRE(ret[2].outputs.size() == 1);
  REQUIRE(ret[2].outputs[0] == "2");
}

TEST_CASE("Invalid arc jump test", "[general]")
{
  vector<pair<string, string>> input = {
    { "aazasl;kfjasfl;",  "0" },
    { "acza;slkdfjas;",   "1" },
    { "adzs;ldfkjas;",  "2" },
    { "aezs;lkdfjals;f",  "3" },
    { "afzasf;laksjf;l",  "4" },
    { "agzaslfkjsa;ldfk", "5" },
    { "ahzaslkdfjas;df",  "6" },
    { "aizs;ldkfjas;dlf", "7" },
    { "akzs;ldkfjas;",  "8" },
    { "alzs;lfkjasdf;l",  "9" },
  };

  auto byte_code = fst::build(input);

  REQUIRE(match(byte_code, "az").empty());
}
