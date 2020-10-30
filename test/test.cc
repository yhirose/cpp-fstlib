#define CATCH_CONFIG_MAIN
#include "catch.hpp"

#include <fstlib.h>
#include <spellcheck.h>

using namespace std;

#define USE_UINT64_OUTPUT_T

#ifdef USE_UINT64_OUTPUT_T
typedef uint64_t output_t;
#define V(x) (x)
#else
typedef string output_t;
#define V(x) (#x)
#endif

template <typename Input, typename Callback>
void make_map(const Input &input, bool sorted, Callback callback) {
  stringstream out;
  auto [result, _] = fst::compile<output_t>(input, out, sorted);
  REQUIRE(result == fst::Result::Success);

  const auto &byte_code = out.str();
  fst::Map<output_t> matcher(byte_code);
  callback(matcher);
}

template <typename Input, typename Callback>
void make_map_with_auto_index(const Input &input, bool sorted,
                              Callback callback) {
  stringstream out;
  auto [result, _] = fst::compile(input, out, true, sorted);
  REQUIRE(result == fst::Result::Success);

  const auto &byte_code = out.str();
  fst::Map<uint32_t> matcher(byte_code);
  callback(matcher);
}

template <typename Input, typename Callback>
void make_set(const Input &input, bool sorted, Callback callback) {
  stringstream out;
  auto [result, _] = fst::compile(input, out, false, sorted);
  REQUIRE(result == fst::Result::Success);

  const auto &byte_code = out.str();
  fst::Set matcher(byte_code);
  callback(matcher);
}

TEST_CASE("Success", "[compile]") {
  vector<pair<string, output_t>> input = {
      {"jan", V(31)}, {"feb", V(28)}, {"mar", V(31)}, {"apr", V(30)},
      {"may", V(31)}, {"jun", V(30)}, {"jul", V(31)}, {"aug", V(31)},
      {"sep", V(30)}, {"oct", V(31)}, {"nov", V(30)}, {"dec", V(31)},
  };

  stringstream out;
  auto [result, _] = fst::compile<output_t>(input, out, false);
  REQUIRE(result == fst::Result::Success);
}

TEST_CASE("Success with no output", "[compile]") {
  vector<string> input = {
      "jan", "feb", "mar", "apr", "may", "jun",
      "jul", "aug", "sep", "oct", "nov", "dec",
  };

  stringstream out;
  {
    auto [result, _] = fst::compile(input, out, true, false);
    REQUIRE(result == fst::Result::Success);
  }
  {
    auto [result, _] = fst::compile(input, out, false, false);
    REQUIRE(result == fst::Result::Success);
  }
}

TEST_CASE("Empty key", "[compile]") {
  vector<pair<string, output_t>> input = {
      {"jan", V(31)}, {"feb", V(28)}, {"", V(31)},    {"apr", V(30)},
      {"may", V(31)}, {"jun", V(30)}, {"jul", V(31)}, {"aug", V(31)},
      {"sep", V(30)}, {"oct", V(31)}, {"nov", V(30)}, {"dec", V(31)},
  };

  stringstream out;
  auto [result, index] = fst::compile<output_t>(input, out, false);
  REQUIRE(result == fst::Result::EmptyKey);
  REQUIRE(index == 2);
}

TEST_CASE("Empty key with no output", "[compile]") {
  vector<string> input = {
      "jan", "feb", "",    "apr", "may", "jun",
      "jul", "aug", "sep", "oct", "nov", "dec",
  };

  stringstream out;
  {
    auto [result, index] = fst::compile(input, out, true, false);
    REQUIRE(result == fst::Result::EmptyKey);
    REQUIRE(index == 2);
  }
  {
    auto [result, index] = fst::compile(input, out, false, false);
    REQUIRE(result == fst::Result::EmptyKey);
    REQUIRE(index == 2);
  }
}

TEST_CASE("Unsorted key", "[compile]") {
  vector<pair<string, output_t>> input = {
      {"jan", V(31)}, {"feb", V(28)}, {"mar", V(31)}, {"apr", V(30)},
      {"may", V(31)}, {"jun", V(30)}, {"jul", V(31)}, {"aug", V(31)},
      {"sep", V(30)}, {"oct", V(31)}, {"nov", V(30)}, {"dec", V(31)},
  };

  stringstream out;
  auto [result, index] = fst::compile<output_t>(input, out, true);
  REQUIRE(result == fst::Result::UnsortedKey);
  REQUIRE(index == 1);
}

TEST_CASE("Unsorted key with no output", "[compile]") {
  vector<string> input = {
      "jan", "feb", "mar", "apr", "may", "jun",
      "jul", "aug", "sep", "oct", "nov", "dec",
  };

  stringstream out;
  {
    auto [result, index] = fst::compile(input, out, true, true);
    REQUIRE(result == fst::Result::UnsortedKey);
    REQUIRE(index == 1);
  }
  {
    auto [result, index] = fst::compile(input, out, false, true);
    REQUIRE(result == fst::Result::UnsortedKey);
    REQUIRE(index == 1);
  }
}

TEST_CASE("Duplicate key", "[compile]") {
  vector<pair<string, output_t>> input = {
      {"jan", V(31)}, {"feb", V(28)}, {"feb", V(29)}, {"mar", V(31)},
      {"apr", V(30)}, {"may", V(31)}, {"jun", V(30)}, {"jul", V(31)},
      {"aug", V(31)}, {"sep", V(30)}, {"oct", V(31)}, {"nov", V(30)},
      {"dec", V(31)},
  };

  stringstream out;
  auto [result, index] = fst::compile<output_t>(input, out, false);
  REQUIRE(result == fst::Result::DuplicateKey);
  REQUIRE(index == 2);
}

TEST_CASE("Duplicate key with no value", "[compile]") {
  vector<string> input = {
      "jan", "feb", "feb", "mar", "apr", "may", "jun",
      "jul", "aug", "sep", "oct", "nov", "dec",
  };

  stringstream out;
  {
    auto [result, index] = fst::compile(input, out, true, false);
    REQUIRE(result == fst::Result::DuplicateKey);
    REQUIRE(index == 2);
  }
  {
    auto [result, index] = fst::compile(input, out, false, false);
    REQUIRE(result == fst::Result::DuplicateKey);
    REQUIRE(index == 2);
  }
}

TEST_CASE("Normal Map test", "[map]") {
  vector<pair<string, output_t>> input = {
      {"jan", V(31)}, {"feb", V(28)}, {"mar", V(31)}, {"apr", V(30)},
      {"may", V(31)}, {"jun", V(30)}, {"jul", V(31)}, {"aug", V(31)},
      {"sep", V(30)}, {"oct", V(31)}, {"nov", V(30)}, {"dec", V(31)},
  };

  make_map(input, false, [](auto &map) {
    {
      output_t actual;
      REQUIRE(map.exact_match_search("apr", actual));
      REQUIRE(actual == V(30));
    }

    REQUIRE(map.contains("jan"));
    REQUIRE(map.at("jan") == V(31));
    REQUIRE(map["apr"] == V(30));
    REQUIRE(map[string("apr")] == V(30));
    REQUIRE(map[string_view("apr")] == V(30));

    REQUIRE(map["feb"] == V(28));
    REQUIRE(map["mar"] == V(31));
    REQUIRE(map["apr"] == V(30));
    REQUIRE(map["may"] == V(31));
    REQUIRE(map["jun"] == V(30));
    REQUIRE(map["jul"] == V(31));
    REQUIRE(map["aug"] == V(31));
    REQUIRE(map["sep"] == V(30));
    REQUIRE(map["oct"] == V(31));
    REQUIRE(map["nov"] == V(30));
    REQUIRE(map["dec"] == V(31));

    REQUIRE(map.contains("") == false);
    REQUIRE(map.contains("_") == false);
    REQUIRE(map.contains("a") == false);
    REQUIRE(map.contains("ap") == false);
    REQUIRE(map.contains("ap_") == false);
    REQUIRE(map.contains("apr_") == false);
  });
}

TEST_CASE("Edge case test1", "[map]") {
  vector<pair<string, output_t>> input = {
      {"a", V(0)},
      {"ab", V(1)},
  };

  make_map(input, true, [](auto &map) {
    REQUIRE(map["a"] == V(0));
    REQUIRE(map["ab"] == V(1));
  });
}

TEST_CASE("Edge case test2", "[map]") {
  vector<pair<string, output_t>> input = {
      {"aa", V(0)},
      {"abb", V(1)},
  };

  make_map(input, true, [](auto &map) {
    REQUIRE(map["aa"] == V(0));
    REQUIRE(map["abb"] == V(1));
  });
}

TEST_CASE("Edge case test3", "[map]") {
  vector<pair<string, output_t>> input = {
      {"abc", V(0)},
      {"bc", V(1)},
  };

  make_map(input, true, [](auto &map) {
    REQUIRE(map["abc"] == V(0));
    REQUIRE(map["bc"] == V(1));
  });
}

TEST_CASE("Edge case test4", "[map]") {
  vector<pair<string, output_t>> input = {
      {"z", V(0)},
      {"zc", V(10)},
      {"zcd", V(11)},
      {"zd", V(1)},
  };

  make_map(input, true, [](auto &map) {
    REQUIRE(map["z"] == V(0));
    REQUIRE(map["zc"] == V(10));
    REQUIRE(map["zcd"] == V(11));
    REQUIRE(map["zd"] == V(1));
  });
}

TEST_CASE("Edge case test5", "[map]") {
  vector<pair<string, output_t>> input = {
      {"aba", V(1)},
      {"abz", V(2)},
      {"baz", V(31)},
      {"bz", V(32)},
  };

  make_map(input, true, [](auto &map) {
    REQUIRE(map["aba"] == V(1));
    REQUIRE(map["abz"] == V(2));
    REQUIRE(map["baz"] == V(31));
    REQUIRE(map["bz"] == V(32));
  });
}

TEST_CASE("Duplicate final states test", "[map]") {
  vector<pair<string, output_t>> input = {
      {"az", V(0)},
      {"bz", V(1)},
      {"cy", V(2)},
      {"dz", V(3)},
  };

  make_map(input, true, [](auto &map) {
    REQUIRE(map["az"] == V(0));
    REQUIRE(map["bz"] == V(1));
    REQUIRE(map["cy"] == V(2));
    REQUIRE(map["dz"] == V(3));
  });
}

TEST_CASE("Duplicate final states test2", "[map]") {
  vector<pair<string, output_t>> input = {
      {"a_a", V(0)},
      {"ab", V(1)},
      {"ab_a", V(2)},
      {"b_a", V(3)},
  };

  make_map(input, true, [](auto &map) {
    REQUIRE(map["a_a"] == V(0));
    REQUIRE(map["ab"] == V(1));
    REQUIRE(map["ab_a"] == V(2));
    REQUIRE(map["b_a"] == V(3));
  });
}

TEST_CASE("UTF-8 test", "[map]") {
  vector<pair<string, output_t>> input = {
      {u8"あ", V(0)},
      {u8"あい", V(1)},
  };

  make_map(input, true, [](auto &map) {
    REQUIRE(map[u8"あ"] == V(0));
    REQUIRE(map[u8"あい"] == V(1));
  });
}

TEST_CASE("Common prefix search test", "[map]") {
  vector<pair<string, output_t>> input = {
      {"a", V(0)},
      {"and", V(1)},
      {"android", V(2)},
  };

  make_map(input, true, [](auto &map) {
    auto ret = map.common_prefix_search("android phone");

    REQUIRE(ret.size() == 3);

    REQUIRE(ret[0].first == 1);
    REQUIRE(ret[0].second == V(0));

    REQUIRE(ret[1].first == 3);
    REQUIRE(ret[1].second == V(1));

    REQUIRE(ret[2].first == 7);
    REQUIRE(ret[2].second == V(2));
  });
}

TEST_CASE("Invalid arc jump test", "[string]") {
  vector<pair<string, output_t>> input = {
      {"aazasl;kfjasfl;", V(0)}, {"acza;slkdfjas;", V(1)},
      {"adzs;ldfkjas;", V(2)},   {"aezs;lkdfjals;f", V(3)},
      {"afzasf;laksjf;l", V(4)}, {"agzaslfkjsa;ldfk", V(5)},
      {"ahzaslkdfjas;df", V(6)}, {"aizs;ldkfjas;dlf", V(7)},
      {"akzs;ldkfjas;", V(8)},   {"alzs;lfkjasdf;l", V(9)},
  };

  make_map(input, true,
           [](auto &map) { REQUIRE(map.contains("az") == false); });
}

TEST_CASE("Single output value test", "[map]") {
  vector<pair<string, output_t>> input = {
      {"a", V(0)},
      {"ab", V(1)},
  };

  make_map(input, true, [](auto &map) {
    REQUIRE(map["a"] == V(0));
    REQUIRE(map["ab"] == V(1));
  });
}

TEST_CASE("Auto Index Dictionary", "[map]") {
  vector<string> input = {"a", "and", "android"};

  make_map_with_auto_index(input, true, [](auto &map) {
    auto ret = map.common_prefix_search("android phone");

    REQUIRE(ret.size() == 3);

    REQUIRE(ret[0].first == 1);
    REQUIRE(ret[0].second == 0);

    REQUIRE(ret[1].first == 3);
    REQUIRE(ret[1].second == 1);

    REQUIRE(ret[2].first == 7);
    REQUIRE(ret[2].second == 2);
  });
}

TEST_CASE("Normal Set test", "[set]") {
  vector<string> input = {
      "jan", "feb", "mar", "apr", "may", "jun",
      "jul", "aug", "sep", "oct", "nov", "dec",
  };

  make_set(input, false, [](auto &set) {
    REQUIRE(set.contains("jan"));
    REQUIRE(set.contains("feb"));
    REQUIRE(set.contains("mar"));
    REQUIRE(set.contains("apr"));
    REQUIRE(set.contains("may"));
    REQUIRE(set.contains("jun"));
    REQUIRE(set.contains("jul"));
    REQUIRE(set.contains("aug"));
    REQUIRE(set.contains("sep"));
    REQUIRE(set.contains("oct"));
    REQUIRE(set.contains("nov"));
    REQUIRE(set.contains("dec"));

    REQUIRE(set.contains("") == false);
    REQUIRE(set.contains("_") == false);
    REQUIRE(set.contains("a") == false);
    REQUIRE(set.contains("ap") == false);
    REQUIRE(set.contains("ap_") == false);
    REQUIRE(set.contains("apr_") == false);
  });
}

TEST_CASE("Japanese Set test", "[set]") {
  vector<string> input = {
      u8"一", u8"一二", u8"一二三", u8"二", u8"二三",
  };

  stringstream ss;
  {
    auto [result, _] = fst::compile(input, ss, false, false);
    REQUIRE(result == fst::Result::Success);
  }

  const auto &byte_code = ss.str();
  fst::Set set(byte_code);

  {
    REQUIRE(set.contains(u8"一"));
    REQUIRE(set.contains(u8"一二"));
    REQUIRE(set.contains(u8"一二三"));
    REQUIRE(set.contains(u8"二"));
    REQUIRE(set.contains(u8"二三"));
    REQUIRE(!set.contains(u8"一二三四"));
    REQUIRE(!set.contains(""));
  }
}

TEST_CASE("Decompile map", "[decompile]") {
  vector<pair<string, output_t>> input = {
      {"jan", V(31)}, {"feb", V(28)}, {"mar", V(31)}, {"apr", V(30)},
      {"may", V(31)}, {"jun", V(30)}, {"jul", V(31)}, {"aug", V(31)},
      {"sep", V(30)}, {"oct", V(31)}, {"nov", V(30)}, {"dec", V(31)},
  };

  stringstream ss;
  {
    auto [result, _] = fst::compile<output_t>(input, ss, false);
    REQUIRE(result == fst::Result::Success);
  }

  const auto &byte_code = ss.str();

  stringstream out;
  fst::decompile(byte_code, out);

  auto expected = R"(apr	30
aug	31
dec	31
feb	28
jul	31
jun	30
jan	31
mar	31
may	31
nov	30
oct	31
sep	30
)";

  REQUIRE(out.str() == expected);
}

TEST_CASE("Decompile map, no need output", "[decompile]") {
  vector<string> input = {
      "jan", "feb", "mar", "apr", "may", "jun",
      "jul", "aug", "sep", "oct", "nov", "dec",
  };

  stringstream ss;
  {
    auto [result, _] = fst::compile(input, ss, true, false);
    REQUIRE(result == fst::Result::Success);
  }

  const auto &byte_code = ss.str();

  stringstream out;
  fst::decompile(byte_code, out, false);

  auto expected = R"(apr
aug
dec
feb
jul
jun
jan
mar
may
nov
oct
sep
)";

  REQUIRE(out.str() == expected);
}

TEST_CASE("Decompile map, need output", "[decompile]") {
  vector<string> input = {
      "jan", "feb", "mar", "apr", "may", "jun",
      "jul", "aug", "sep", "oct", "nov", "dec",
  };

  stringstream ss;
  {
    auto [result, _] = fst::compile(input, ss, true, false);
    REQUIRE(result == fst::Result::Success);
  }

  const auto &byte_code = ss.str();

  stringstream out;
  fst::decompile(byte_code, out);

  auto expected = R"(apr	3
aug	7
dec	11
feb	1
jul	6
jun	5
jan	0
mar	2
may	4
nov	10
oct	9
sep	8
)";

  REQUIRE(out.str() == expected);
}

TEST_CASE("Decompile set", "[decompile]") {
  vector<string> input = {
      "jan", "feb", "mar", "apr", "may", "jun",
      "jul", "aug", "sep", "oct", "nov", "dec",
  };

  stringstream ss;
  {
    auto [result, _] = fst::compile(input, ss, false, false);
    REQUIRE(result == fst::Result::Success);
  }

  const auto &byte_code = ss.str();

  stringstream out;
  fst::decompile(byte_code, out);

  auto expected = R"(apr
aug
dec
feb
jul
jun
jan
mar
may
nov
oct
sep
)";

  REQUIRE(out.str().size() == strlen(expected));
  REQUIRE(out.str() == expected);
}

TEST_CASE("Edit distance search map", "[edit distance]") {
  vector<string> input = {
      "jan", "feb", "mar", "apr", "may", "jun",
      "jul", "aug", "sep", "oct", "nov", "dec",
  };

  stringstream ss;
  {
    auto [result, _] = fst::compile(input, ss, true, false);
    REQUIRE(result == fst::Result::Success);
  }

  const auto &byte_code = ss.str();
  fst::Map<uint32_t> matcher(byte_code);

  auto ret = matcher.edit_distance_search("joe", 2);
  REQUIRE(ret.size() == 4);
}

TEST_CASE("Edit distance search set", "[edit distance]") {
  vector<string> input = {
      "jan", "feb", "mar", "apr", "may", "jun",
      "jul", "aug", "sep", "oct", "nov", "dec",
  };

  stringstream ss;
  {
    auto [result, _] = fst::compile(input, ss, false, false);
    REQUIRE(result == fst::Result::Success);
  }

  const auto &byte_code = ss.str();
  fst::Set matcher(byte_code);

  auto ret = matcher.edit_distance_search("joe", 2);
  REQUIRE(ret.size() == 4);
}

TEST_CASE("Japanese edit distance search", "[edit distance]") {
  vector<string> input = {
      u8"一", u8"一二", u8"一二三", u8"一二三四",
  };

  stringstream ss;
  {
    auto [result, _] = fst::compile(input, ss, false, false);
    REQUIRE(result == fst::Result::Success);
  }

  const auto &byte_code = ss.str();
  fst::Set matcher(byte_code);

  {
    auto ret = matcher.edit_distance_search(u8"二", 1);
    REQUIRE(ret.size() == 2);
    REQUIRE(ret[0] == u8"一");
    REQUIRE(ret[1] == u8"一二");
  }

  {
    auto ret = matcher.edit_distance_search(u8"二", 2);
    REQUIRE(ret.size() == 3);
    REQUIRE(ret[0] == u8"一");
    REQUIRE(ret[1] == u8"一二");
    REQUIRE(ret[2] == u8"一二三");
  }
}

TEST_CASE("Spellcheck", "[spellcheck]") {
  vector<string> input = {
      "jan", "feb", "mar", "apr", "may", "jun",
      "jul", "aug", "sep", "oct", "nov", "dec",
  };

  stringstream ss;
  {
    auto [result, _] = fst::compile(input, ss, false, false);
    REQUIRE(result == fst::Result::Success);
  }

  const auto &byte_code = ss.str();

  fst::Set matcher(byte_code);
  REQUIRE(matcher == true);

  {
    auto &&[ret, candidates] = fst::spellcheck(matcher, "jun");
    REQUIRE(ret == true);
  }

  {
    auto &&[ret, candidates] = fst::spellcheck(matcher, "joe");
    REQUIRE(ret == false);

    auto &&[candidate, similarity] = candidates.front();
    REQUIRE(candidate == "jan");
    REQUIRE(std::floor(similarity * 1000) == 349);
  }
}
