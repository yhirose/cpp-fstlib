#define CATCH_CONFIG_MAIN
#include "catch.hpp"

#include <fstlib.h>

using namespace std;

inline vector<fst::CommonPrefixSearchResult> prefix_match(
  const vector<char>& byte_code, const char* str)
{
  vector<fst::CommonPrefixSearchResult> ret;
  fst::common_prefix_search(byte_code.data(), byte_code.size(), str, [&](const auto& result) {
    ret.emplace_back(result);
  });
  return ret;
}

inline vector<fst::output_t> exact_match(const vector<char>& byte_code, const char* str)
{
  vector<fst::output_t> outputs;
#ifdef USE_UINT32_OUTPUT_T
  fst::exact_match_search(byte_code.data(), byte_code.size(), str, [&](fst::output_t val) {
    outputs.emplace_back(val);
  });
#else
  fst::exact_match_search(byte_code.data(), byte_code.size(), str, [&](const char* s, size_t l) {
    outputs.emplace_back(s, l);
  });
#endif
  return outputs;
}

#ifdef USE_UINT32_OUTPUT_T
#define V(x) (x)
#else
#define V(x) (#x)
#endif

TEST_CASE("Simple virtual machine test", "[general]")
{
  vector<pair<string, fst::output_t>> input = {
    { "apr", V(30) },
    { "aug", V(31) },
    { "dec", V(31) },
    { "feb", V(28) },
    { "feb", V(29) },
    { "jan", V(31) },
    { "jul", V(31) },
    { "jun", V(30) },
  };

  auto sm = fst::make_state_machine(input);
  REQUIRE(sm->count == 13);

  auto ret = exact_match_search(*sm, "feb");
  REQUIRE(ret.size() == 2);
  REQUIRE(ret[0] == V(28));
  REQUIRE(ret[1] == V(29));

  ret = exact_match_search(*sm, "jul");
  REQUIRE(ret.size() == 1);
  REQUIRE(ret[0] == V(31));

  auto byte_code = fst::compile(*sm);

  REQUIRE(exact_match(byte_code, "apr")[0] == V(30));
  REQUIRE(exact_match(byte_code, "aug")[0] == V(31));
  REQUIRE(exact_match(byte_code, "dec")[0] == V(31));
  REQUIRE(exact_match(byte_code, "feb")[0] == V(28));
  REQUIRE(exact_match(byte_code, "feb")[1] == V(29));
  REQUIRE(exact_match(byte_code, "jul")[0] == V(31));
  REQUIRE(exact_match(byte_code, "jun")[0] == V(30));

  REQUIRE(exact_match(byte_code, "").empty());
  REQUIRE(exact_match(byte_code, "_").empty());
  REQUIRE(exact_match(byte_code, "a").empty());
  REQUIRE(exact_match(byte_code, "ap").empty());
  REQUIRE(exact_match(byte_code, "ap_").empty());
  REQUIRE(exact_match(byte_code, "apr_").empty());
}

TEST_CASE("Edge case test1", "[general]")
{
  vector<pair<string, fst::output_t>> input = {
    { "a",  V(0) },
    { "ab", V(1) },
  };

  auto sm = fst::make_state_machine(input);

  REQUIRE(sm->count == 3);

  auto byte_code = fst::compile(*sm);

  REQUIRE(exact_match(byte_code, "a")[0] == V(0));
  REQUIRE(exact_match(byte_code, "ab")[0] == V(1));
}

TEST_CASE("Edge case test2", "[general]")
{
  vector<pair<string, fst::output_t>> input = {
    { "aa",  V(0) },
    { "abb", V(1) },
  };

  auto sm = fst::make_state_machine(input);

  REQUIRE(sm->count == 4);

  auto byte_code = fst::compile(*sm);

  REQUIRE(exact_match(byte_code, "aa")[0] == V(0));
  REQUIRE(exact_match(byte_code, "abb")[0] == V(1));
}

TEST_CASE("Edge case test3", "[general]")
{
  vector<pair<string, fst::output_t>> input = {
    { "abc", V(0) },
    { "bc",  V(1) },
  };

  auto sm = fst::make_state_machine(input);

  REQUIRE(sm->count == 4);

  auto byte_code = fst::compile(*sm);

  REQUIRE(exact_match(byte_code, "abc")[0] == V(0));
  REQUIRE(exact_match(byte_code, "bc")[0] == V(1));
}

TEST_CASE("Edge case test4", "[general]")
{
  vector<pair<string, fst::output_t>> input = {
    { "z",   V(0)  },
    { "zc",  V(10) },
    { "zcd", V(11) },
    { "zd",  V(1)  },
  };

  auto sm = fst::make_state_machine(input);

  REQUIRE(sm->count == 4);

  auto byte_code = fst::compile(*sm);

  REQUIRE(exact_match(byte_code, "z")[0] == V(0));
  REQUIRE(exact_match(byte_code, "zc")[0] == V(10));
  REQUIRE(exact_match(byte_code, "zcd")[0] == V(11));
  REQUIRE(exact_match(byte_code, "zd")[0] == V(1));
}

TEST_CASE("Edge case test5", "[general]")
{
  vector<pair<string, fst::output_t>> input = {
    { "aba", V(1) },
    { "abz", V(2) },
    { "baz", V(31) },
    { "bz",  V(32) },
  };

  auto sm = fst::make_state_machine(input);

  REQUIRE(sm->count == 6);

  auto byte_code = fst::compile(*sm);

  REQUIRE(exact_match(byte_code, "aba")[0] == V(1));
  REQUIRE(exact_match(byte_code, "abz")[0] == V(2));
  REQUIRE(exact_match(byte_code, "baz")[0] == V(31));
  REQUIRE(exact_match(byte_code, "bz")[0] == V(32));
}

TEST_CASE("Duplicate final states test", "[general]")
{
  vector<pair<string, fst::output_t>> input = {
    { "az", V(0) },
    { "bz", V(1) },
    { "cy", V(2) },
    { "dz", V(3) },
  };

  auto byte_code = fst::build(input);

  REQUIRE(exact_match(byte_code, "az")[0] == V(0));
  REQUIRE(exact_match(byte_code, "bz")[0] == V(1));
  REQUIRE(exact_match(byte_code, "cy")[0] == V(2));
  REQUIRE(exact_match(byte_code, "dz")[0] == V(3));
}

TEST_CASE("Duplicate final states test2", "[general]")
{
  vector<pair<string, fst::output_t>> input = {
    { "a_a",  V(0) },
    { "ab",   V(1) },
    { "ab_a", V(2) },
    { "b_a",  V(3) },
  };

  auto byte_code = fst::build(input);

  REQUIRE(exact_match(byte_code, "a_a")[0] == V(0));
  REQUIRE(exact_match(byte_code, "ab")[0] == V(1));
  REQUIRE(exact_match(byte_code, "ab_a")[0] == V(2));
  REQUIRE(exact_match(byte_code, "b_a")[0] == V(3));
}

TEST_CASE("UTF-8 test", "[general]")
{
  vector<pair<string, fst::output_t>> input = {
    { u8"あ",   V(0) },
    { u8"あい", V(1) },
  };

  auto sm = fst::make_state_machine(input);

  REQUIRE(sm->count == 7);

  auto byte_code = fst::compile(*sm);

  REQUIRE(exact_match(byte_code, u8"あ")[0] == V(0));
  REQUIRE(exact_match(byte_code, u8"あい")[0] == V(1));
}

TEST_CASE("Common prefix search test", "[general]")
{
  vector<pair<string, fst::output_t>> input = {
    { "a",       V(0) },
    { "and",     V(1) },
    { "android", V(2) },
  };

  auto byte_code = fst::build(input);
  auto ret = prefix_match(byte_code, "android phone");

  REQUIRE(ret.size() == 3);

  REQUIRE(ret[0].length == 1);
  REQUIRE(ret[0].outputs.size() == 1);
  REQUIRE(ret[0].outputs[0] == V(0));

  REQUIRE(ret[1].length == 3);
  REQUIRE(ret[1].outputs.size() == 1);
  REQUIRE(ret[1].outputs[0] == V(1));

  REQUIRE(ret[2].length == 7);
  REQUIRE(ret[2].outputs.size() == 1);
  REQUIRE(ret[2].outputs[0] == V(2));
}

TEST_CASE("Common prefix search test2", "[general]")
{
  vector<pair<string, fst::output_t>> input = {
    { "a", V(0) },
    { "and", V(1) },
    { "android", V(2) },
  };

  auto byte_code = fst::build(input);
  auto ret = prefix_match(byte_code, "android phone");

  REQUIRE(ret.size() == 3);

  REQUIRE(ret[0].length == 1);
  REQUIRE(ret[0].outputs.size() == 1);
  REQUIRE(ret[0].outputs[0] == V(0));

  REQUIRE(ret[1].length == 3);
  REQUIRE(ret[1].outputs.size() == 1);
  REQUIRE(ret[1].outputs[0] == V(1));

  REQUIRE(ret[2].length == 7);
  REQUIRE(ret[2].outputs.size() == 1);
  REQUIRE(ret[2].outputs[0] == V(2));
}

TEST_CASE("Invalid arc jump test", "[string]")
{
  vector<pair<string, fst::output_t>> input = {
    { "aazasl;kfjasfl;",  V(0) },
    { "acza;slkdfjas;",   V(1) },
    { "adzs;ldfkjas;",    V(2) },
    { "aezs;lkdfjals;f",  V(3) },
    { "afzasf;laksjf;l",  V(4) },
    { "agzaslfkjsa;ldfk", V(5) },
    { "ahzaslkdfjas;df",  V(6) },
    { "aizs;ldkfjas;dlf", V(7) },
    { "akzs;ldkfjas;",    V(8) },
    { "alzs;lfkjasdf;l",  V(9) },
  };

  auto byte_code = fst::build(input);

  REQUIRE(exact_match(byte_code, "az").empty());
}
