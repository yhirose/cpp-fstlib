#define CATCH_CONFIG_MAIN
#include "catch.hpp"

#include <fstlib.h>

using namespace std;

#define USE_UINT32_OUTPUT_T

#ifdef USE_UINT32_OUTPUT_T
typedef uint32_t output_t;
#define V(x) (x)
#else
typedef std::string output_t;
#define V(x) (#x)
#endif

inline vector<fst::CommonPrefixSearchResult<output_t>>
prefix_match(const vector<char> &byte_code, const char *str) {
  vector<fst::CommonPrefixSearchResult<output_t>> ret;
  fst::common_prefix_search<output_t>(
      byte_code.data(), byte_code.size(), str,
      [&](const auto &result) { ret.emplace_back(result); });
  return ret;
}

template <typename output_t>
inline vector<output_t> exact_match_t(const vector<char> &byte_code,
                                      const char *str) {
  vector<output_t> outputs;
  fst::exact_match_search<output_t>(
      byte_code.data(), byte_code.size(), str,
      [&](const output_t &val) { outputs.emplace_back(val); });
  return outputs;
}

inline vector<output_t> exact_match(const vector<char> &byte_code,
                                    const char *str) {
  return exact_match_t<output_t>(byte_code, str);
}

TEST_CASE("Simple virtual machine test", "[general]") {
  vector<pair<string, output_t>> input = {
      {"apr", V(30)}, {"aug", V(31)}, {"dec", V(31)}, {"feb", V(28)},
      {"feb", V(29)}, {"jan", V(31)}, {"jul", V(31)}, {"jun", V(30)},
  };

  auto sm = fst::make_state_machine(input);
  REQUIRE(sm->count == 13);

  auto ret = fst::exact_match_search<output_t>(*sm, "feb");
  REQUIRE(ret.size() == 2);
  REQUIRE(ret[0] == V(28));
  REQUIRE(ret[1] == V(29));

  ret = fst::exact_match_search<output_t>(*sm, "jul");
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

TEST_CASE("Edge case test1", "[general]") {
  vector<pair<string, output_t>> input = {
      {"a", V(0)},
      {"ab", V(1)},
  };

  auto sm = fst::make_state_machine(input);

  REQUIRE(sm->count == 3);

  auto byte_code = fst::compile(*sm);

  REQUIRE(exact_match(byte_code, "a")[0] == V(0));
  REQUIRE(exact_match(byte_code, "ab")[0] == V(1));
}

TEST_CASE("Edge case test2", "[general]") {
  vector<pair<string, output_t>> input = {
      {"aa", V(0)},
      {"abb", V(1)},
  };

  auto sm = fst::make_state_machine(input);

  REQUIRE(sm->count == 4);

  auto byte_code = fst::compile(*sm);

  REQUIRE(exact_match(byte_code, "aa")[0] == V(0));
  REQUIRE(exact_match(byte_code, "abb")[0] == V(1));
}

TEST_CASE("Edge case test3", "[general]") {
  vector<pair<string, output_t>> input = {
      {"abc", V(0)},
      {"bc", V(1)},
  };

  auto sm = fst::make_state_machine(input);

  REQUIRE(sm->count == 4);

  auto byte_code = fst::compile(*sm);

  REQUIRE(exact_match(byte_code, "abc")[0] == V(0));
  REQUIRE(exact_match(byte_code, "bc")[0] == V(1));
}

TEST_CASE("Edge case test4", "[general]") {
  vector<pair<string, output_t>> input = {
      {"z", V(0)},
      {"zc", V(10)},
      {"zcd", V(11)},
      {"zd", V(1)},
  };

  auto sm = fst::make_state_machine(input);

  REQUIRE(sm->count == 4);

  auto byte_code = fst::compile(*sm);

  REQUIRE(exact_match(byte_code, "z")[0] == V(0));
  REQUIRE(exact_match(byte_code, "zc")[0] == V(10));
  REQUIRE(exact_match(byte_code, "zcd")[0] == V(11));
  REQUIRE(exact_match(byte_code, "zd")[0] == V(1));
}

TEST_CASE("Edge case test5", "[general]") {
  vector<pair<string, output_t>> input = {
      {"aba", V(1)},
      {"abz", V(2)},
      {"baz", V(31)},
      {"bz", V(32)},
  };

  auto sm = fst::make_state_machine(input);

  REQUIRE(sm->count == 6);

  auto byte_code = fst::compile(*sm);

  REQUIRE(exact_match(byte_code, "aba")[0] == V(1));
  REQUIRE(exact_match(byte_code, "abz")[0] == V(2));
  REQUIRE(exact_match(byte_code, "baz")[0] == V(31));
  REQUIRE(exact_match(byte_code, "bz")[0] == V(32));
}

TEST_CASE("Duplicate final states test", "[general]") {
  vector<pair<string, output_t>> input = {
      {"az", V(0)},
      {"bz", V(1)},
      {"cy", V(2)},
      {"dz", V(3)},
  };

  auto byte_code = fst::build(input);

  REQUIRE(exact_match(byte_code, "az")[0] == V(0));
  REQUIRE(exact_match(byte_code, "bz")[0] == V(1));
  REQUIRE(exact_match(byte_code, "cy")[0] == V(2));
  REQUIRE(exact_match(byte_code, "dz")[0] == V(3));
}

TEST_CASE("Duplicate final states test2", "[general]") {
  vector<pair<string, output_t>> input = {
      {"a_a", V(0)},
      {"ab", V(1)},
      {"ab_a", V(2)},
      {"b_a", V(3)},
  };

  auto byte_code = fst::build(input);

  REQUIRE(exact_match(byte_code, "a_a")[0] == V(0));
  REQUIRE(exact_match(byte_code, "ab")[0] == V(1));
  REQUIRE(exact_match(byte_code, "ab_a")[0] == V(2));
  REQUIRE(exact_match(byte_code, "b_a")[0] == V(3));
}

TEST_CASE("UTF-8 test", "[general]") {
  vector<pair<string, output_t>> input = {
      {u8"あ", V(0)},
      {u8"あい", V(1)},
  };

  auto sm = fst::make_state_machine(input);

  REQUIRE(sm->count == 7);

  auto byte_code = fst::compile(*sm);

  REQUIRE(exact_match(byte_code, u8"あ")[0] == V(0));
  REQUIRE(exact_match(byte_code, u8"あい")[0] == V(1));
}

TEST_CASE("Common prefix search test", "[general]") {
  vector<pair<string, output_t>> input = {
      {"a", V(0)},
      {"and", V(1)},
      {"android", V(2)},
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

TEST_CASE("Common prefix search test2", "[general]") {
  vector<pair<string, output_t>> input = {
      {"a", V(0)},
      {"and", V(1)},
      {"android", V(2)},
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

TEST_CASE("Invalid arc jump test", "[string]") {
  vector<pair<string, output_t>> input = {
      {"aazasl;kfjasfl;", V(0)}, {"acza;slkdfjas;", V(1)},
      {"adzs;ldfkjas;", V(2)},   {"aezs;lkdfjals;f", V(3)},
      {"afzasf;laksjf;l", V(4)}, {"agzaslfkjsa;ldfk", V(5)},
      {"ahzaslkdfjas;df", V(6)}, {"aizs;ldkfjas;dlf", V(7)},
      {"akzs;ldkfjas;", V(8)},   {"alzs;lfkjasdf;l", V(9)},
  };

  auto byte_code = fst::build(input);

  REQUIRE(exact_match(byte_code, "az").empty());
}

TEST_CASE("Single output value test", "[general]") {
  vector<pair<string, output_t>> input = {
      {"a", V(0)},
      {"ab", V(1)},
  };

  auto sm = fst::make_state_machine(input);

  REQUIRE(sm->count == 3);

  auto byte_code = fst::compile(*sm);

  output_t output;
  auto ret =
      fst::exact_match_search(byte_code.data(), byte_code.size(), "a", output);
  REQUIRE(ret);
  REQUIRE(output == V(0));

  ret =
      fst::exact_match_search(byte_code.data(), byte_code.size(), "ab", output);
  REQUIRE(ret);
  REQUIRE(output == V(1));
}

TEST_CASE("Build interface test", "[general]") {
  auto byte_code = fst::build<output_t>([](auto add_entry) {
    add_entry("a", V(0));
    add_entry("b", V(1));
  });

  REQUIRE(exact_match(byte_code, "a")[0] == V(0));
  REQUIRE(exact_match(byte_code, "b")[0] == V(1));
}

TEST_CASE("Auto Index Build interface test", "[general]") {
  auto byte_code = fst::build([](auto add_entry) {
    add_entry("a");
    add_entry("b");
  });

  REQUIRE(exact_match_t<uint32_t>(byte_code, "a")[0] == 0);
  REQUIRE(exact_match_t<uint32_t>(byte_code, "b")[0] == 1);
}

TEST_CASE("Nested Build interface test", "[general]") {
  auto byte_code = fst::build<output_t>([](auto add_entry) {
    add_entry("a", V(0));
    add_entry("b", V(1));

    auto byte_code = fst::build<output_t>([](auto add_entry) {
      add_entry("a1", V(0));
      add_entry("b1", V(1));
    });

    REQUIRE(exact_match(byte_code, "a1")[0] == V(0));
    REQUIRE(exact_match(byte_code, "b1")[0] == V(1));
  });

  REQUIRE(exact_match(byte_code, "a")[0] == V(0));
  REQUIRE(exact_match(byte_code, "b")[0] == V(1));
}

TEST_CASE("Exact match search", "[readme]") {
  std::vector<std::pair<std::string, uint32_t>> input = {
      {"apr", 30}, {"aug", 31}, {"dec", 31}, {"feb", 28},
      {"feb", 29}, {"jan", 31}, {"jul", 31}, {"jun", 30},
  };

  std::vector<char> t = fst::build(input);

  REQUIRE(fst::exact_match_search<uint32_t>(t.data(), t.size(), "apr")[0] ==
          30);
  REQUIRE(fst::exact_match_search<uint32_t>(t.data(), t.size(), "ap").empty());
  REQUIRE(
      fst::exact_match_search<uint32_t>(t.data(), t.size(), "apr_").empty());
  REQUIRE(fst::exact_match_search<uint32_t>(t.data(), t.size(), "feb")[0] ==
          28);
  REQUIRE(fst::exact_match_search<uint32_t>(t.data(), t.size(), "feb")[1] ==
          29);
}

TEST_CASE("Common prefix search", "[readme]") {
  auto t = fst::build<std::string>([](auto add_entry) {
    add_entry("a", "one");
    add_entry("and", "two");
    add_entry("android", "three");
  });

  auto ret = fst::common_prefix_search<std::string>(t.data(), t.size(),
                                                    "android phone");

  REQUIRE(ret[0].length == 1);
  REQUIRE(ret[0].outputs[0] == "one");

  REQUIRE(ret[1].length == 3);
  REQUIRE(ret[1].outputs[0] == "two");

  REQUIRE(ret[2].length == 7);
  REQUIRE(ret[2].outputs[0] == "three");
}

TEST_CASE("Auto Index Dictionary", "[readme]") {
  auto t = fst::build({
      "a",
      "and",
      "android",
  });

  auto ret =
      fst::common_prefix_search<uint32_t>(t.data(), t.size(), "android phone");

  REQUIRE(ret[0].length == 1);
  REQUIRE(ret[0].outputs[0] == 0);

  REQUIRE(ret[1].length == 3);
  REQUIRE(ret[1].outputs[0] == 1);

  REQUIRE(ret[2].length == 7);
  REQUIRE(ret[2].outputs[0] == 2);
}
