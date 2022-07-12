#include <gtest/gtest.h>

#include <cmath>
#include <fstream>
#include <fstlib.h>

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
  EXPECT_EQ(fst::Result::Success, result);

  const auto &byte_code = out.str();
  fst::map<output_t> matcher(byte_code);
  callback(matcher);
}

template <typename Input, typename Callback>
void make_map_with_auto_index(const Input &input, bool sorted,
                              Callback callback) {
  stringstream out;
  auto [result, _] = fst::compile(input, out, true, sorted);
  EXPECT_EQ(fst::Result::Success, result);

  const auto &byte_code = out.str();
  fst::map<uint32_t> matcher(byte_code);
  callback(matcher);
}

template <typename Input, typename Callback>
void make_set(const Input &input, bool sorted, Callback callback) {
  stringstream out;
  auto [result, _] = fst::compile(input, out, false, sorted);
  EXPECT_EQ(fst::Result::Success, result);

  const auto &byte_code = out.str();
  fst::set matcher(byte_code);
  callback(matcher);
}

TEST(CompileTest, Success) {
  vector<pair<string, output_t>> input = {
      {"jan", V(31)}, {"feb", V(28)}, {"mar", V(31)}, {"apr", V(30)},
      {"may", V(31)}, {"jun", V(30)}, {"jul", V(31)}, {"aug", V(31)},
      {"sep", V(30)}, {"oct", V(31)}, {"nov", V(30)}, {"dec", V(31)},
  };

  stringstream out;
  auto [result, _] = fst::compile<output_t>(input, out, false);
  EXPECT_EQ(fst::Result::Success, result);
}

TEST(CompileTest, Success_with_no_output) {
  vector<string> input = {
      "jan", "feb", "mar", "apr", "may", "jun",
      "jul", "aug", "sep", "oct", "nov", "dec",
  };

  stringstream out;
  {
    auto [result, _] = fst::compile(input, out, true, false);
    EXPECT_EQ(fst::Result::Success, result);
  }
  {
    auto [result, _] = fst::compile(input, out, false, false);
    EXPECT_EQ(fst::Result::Success, result);
  }
}

TEST(CompileTest, Empty_key) {
  vector<pair<string, output_t>> input = {
      {"jan", V(31)}, {"feb", V(28)}, {"", V(31)},    {"apr", V(30)},
      {"may", V(31)}, {"jun", V(30)}, {"jul", V(31)}, {"aug", V(31)},
      {"sep", V(30)}, {"oct", V(31)}, {"nov", V(30)}, {"dec", V(31)},
  };

  stringstream out;
  auto [result, index] = fst::compile<output_t>(input, out, false);
  EXPECT_EQ(fst::Result::EmptyKey, result);
  EXPECT_EQ(2, index);
}

TEST(CompileTest, Empty_key_with_no_output) {
  vector<string> input = {
      "jan", "feb", "",    "apr", "may", "jun",
      "jul", "aug", "sep", "oct", "nov", "dec",
  };

  stringstream out;
  {
    auto [result, index] = fst::compile(input, out, true, false);
    EXPECT_EQ(fst::Result::EmptyKey, result);
    EXPECT_EQ(2, index);
  }
  {
    auto [result, index] = fst::compile(input, out, false, false);
    EXPECT_EQ(fst::Result::EmptyKey, result);
    EXPECT_EQ(2, index);
  }
}

TEST(CompileTest, Unsorted_key) {
  vector<pair<string, output_t>> input = {
      {"jan", V(31)}, {"feb", V(28)}, {"mar", V(31)}, {"apr", V(30)},
      {"may", V(31)}, {"jun", V(30)}, {"jul", V(31)}, {"aug", V(31)},
      {"sep", V(30)}, {"oct", V(31)}, {"nov", V(30)}, {"dec", V(31)},
  };

  stringstream out;
  auto [result, index] = fst::compile<output_t>(input, out, true);
  EXPECT_EQ(fst::Result::UnsortedKey, result);
  EXPECT_EQ(1, index);
}


TEST(CompileTest, Try_to_fix_calculate_output_error) {
    string keyFilePath = "keywords.txt";
    ifstream fin(keyFilePath);
    string word;
    std::map<string, uint64_t> input;

    while (getline(fin, word)) {
        input[word] = input.size();
    }

    stringstream out;
    {
        FstWriter<uint64_t, true> writer(out, true, false, false, [&](const auto& feeder) {
                                                                      for (const auto& item : input) {
                                                                          feeder(item.first);
                                                                      }
                                                                  });
        auto ret = build_fst_core<uint64_t>(
                [&](const auto& feeder) {
                    size_t input_index = 0;
                    for (const auto& item : input) {
                        const auto& word = item.first;
                        const auto& output = item.second;
                        if (!feeder(word, output, input_index)) {
                            break;
                        }
                        input_index++;
                    }
                },
                writer, true);
        ASSERT_TRUE(ret.first == fst::Result::Success);
    }

    string data = out.str();
    fst::map<uint64_t> fm(data.c_str(), data.size());
    for (auto& item : input) {
        ASSERT_EQ(item.second, fm[item.first]);
    }
    fm.enumerate([&](const auto& word, auto output) {
                     ASSERT_EQ(output, input[word]) << word;
                 });
}


TEST(CompileTest, Unsorted_key_with_no_output) {
  vector<string> input = {
      "jan", "feb", "mar", "apr", "may", "jun",
      "jul", "aug", "sep", "oct", "nov", "dec",
  };

  stringstream out;
  {
    auto [result, index] = fst::compile(input, out, true, true);
    EXPECT_EQ(fst::Result::UnsortedKey, result);
    EXPECT_EQ(1, index);
  }
  {
    auto [result, index] = fst::compile(input, out, false, true);
    EXPECT_EQ(fst::Result::UnsortedKey, result);
    EXPECT_EQ(1, index);
  }
}

TEST(CompileTest, Duplicate_key) {
  vector<pair<string, output_t>> input = {
      {"jan", V(31)}, {"feb", V(28)}, {"feb", V(29)}, {"mar", V(31)},
      {"apr", V(30)}, {"may", V(31)}, {"jun", V(30)}, {"jul", V(31)},
      {"aug", V(31)}, {"sep", V(30)}, {"oct", V(31)}, {"nov", V(30)},
      {"dec", V(31)},
  };

  stringstream out;
  auto [result, index] = fst::compile<output_t>(input, out, false);
  EXPECT_EQ(fst::Result::DuplicateKey, result);
  EXPECT_EQ(2, index);
}

TEST(CompileTest, Duplicate_key_with_no_value) {
  vector<string> input = {
      "jan", "feb", "feb", "mar", "apr", "may", "jun",
      "jul", "aug", "sep", "oct", "nov", "dec",
  };

  stringstream out;
  {
    auto [result, index] = fst::compile(input, out, true, false);
    EXPECT_EQ(fst::Result::DuplicateKey, result);
    EXPECT_EQ(2, index);
  }
  {
    auto [result, index] = fst::compile(input, out, false, false);
    EXPECT_EQ(fst::Result::DuplicateKey, result);
    EXPECT_EQ(2, index);
  }
}

TEST(MapTest, Normal_Map_test) {
  vector<pair<string, output_t>> input = {
      {"jan", V(31)}, {"feb", V(28)}, {"mar", V(31)}, {"apr", V(30)},
      {"may", V(31)}, {"jun", V(30)}, {"jul", V(31)}, {"aug", V(31)},
      {"sep", V(30)}, {"oct", V(31)}, {"nov", V(30)}, {"dec", V(31)},
  };

  make_map(input, false, [](auto &map) {
    {
      output_t actual;
      EXPECT_TRUE(map.exact_match_search("apr", actual));
      EXPECT_EQ(V(30), actual);
    }

    EXPECT_TRUE(map.contains("jan"));
    EXPECT_EQ(V(31), map.at("jan"));
    EXPECT_EQ(V(30), map["apr"]);
    EXPECT_EQ(V(30), map[string("apr")]);
    EXPECT_EQ(V(30), map[string_view("apr")]);

    EXPECT_EQ(V(28), map["feb"]);
    EXPECT_EQ(V(31), map["mar"]);
    EXPECT_EQ(V(30), map["apr"]);
    EXPECT_EQ(V(31), map["may"]);
    EXPECT_EQ(V(30), map["jun"]);
    EXPECT_EQ(V(31), map["jul"]);
    EXPECT_EQ(V(31), map["aug"]);
    EXPECT_EQ(V(30), map["sep"]);
    EXPECT_EQ(V(31), map["oct"]);
    EXPECT_EQ(V(30), map["nov"]);
    EXPECT_EQ(V(31), map["dec"]);

    EXPECT_FALSE(map.contains(""));
    EXPECT_FALSE(map.contains("_"));
    EXPECT_FALSE(map.contains("a"));
    EXPECT_FALSE(map.contains("ap"));
    EXPECT_FALSE(map.contains("ap_"));
    EXPECT_FALSE(map.contains("apr_"));

    {
      auto ret = map.predictive_search("ju");
      std::sort(ret.begin(), ret.end(),
                [](auto a, auto b) { return a.first < b.first; });

      EXPECT_EQ(2, ret.size());
      EXPECT_EQ("jul", ret[0].first);
      EXPECT_EQ(31, ret[0].second);
      EXPECT_EQ("jun", ret[1].first);
      EXPECT_EQ(30, ret[1].second);
    }
  });
}

TEST(MapTest, Edge_case_test1) {
  vector<pair<string, output_t>> input = {
      {"a", V(0)},
      {"ab", V(1)},
  };

  make_map(input, true, [](auto &map) {
    EXPECT_EQ(V(0), map["a"]);
    EXPECT_EQ(V(1), map["ab"]);
  });
}

TEST(MapTest, Edge_case_test2) {
  vector<pair<string, output_t>> input = {
      {"aa", V(0)},
      {"abb", V(1)},
  };

  make_map(input, true, [](auto &map) {
    EXPECT_EQ(V(0), map["aa"]);
    EXPECT_EQ(V(1), map["abb"]);
  });
}

TEST(MapTest, Edge_case_test3) {
  vector<pair<string, output_t>> input = {
      {"abc", V(0)},
      {"bc", V(1)},
  };

  make_map(input, true, [](auto &map) {
    EXPECT_EQ(V(0), map["abc"]);
    EXPECT_EQ(V(1), map["bc"]);
  });
}

TEST(MapTest, Edge_case_test4) {
  vector<pair<string, output_t>> input = {
      {"z", V(0)},
      {"zc", V(10)},
      {"zcd", V(11)},
      {"zd", V(1)},
  };

  make_map(input, true, [](auto &map) {
    EXPECT_EQ(V(0), map["z"]);
    EXPECT_EQ(V(10), map["zc"]);
    EXPECT_EQ(V(11), map["zcd"]);
    EXPECT_EQ(V(1), map["zd"]);
  });
}

TEST(MapTest, Edge_case_test5) {
  vector<pair<string, output_t>> input = {
      {"aba", V(1)},
      {"abz", V(2)},
      {"baz", V(31)},
      {"bz", V(32)},
  };

  make_map(input, true, [](auto &map) {
    EXPECT_EQ(V(1), map["aba"]);
    EXPECT_EQ(V(2), map["abz"]);
    EXPECT_EQ(V(31), map["baz"]);
    EXPECT_EQ(V(32), map["bz"]);
  });
}

TEST(MapTest, Duplicate_final_states_test) {
  vector<pair<string, output_t>> input = {
      {"az", V(0)},
      {"bz", V(1)},
      {"cy", V(2)},
      {"dz", V(3)},
  };

  make_map(input, true, [](auto &map) {
    EXPECT_EQ(V(0), map["az"]);
    EXPECT_EQ(V(1), map["bz"]);
    EXPECT_EQ(V(2), map["cy"]);
    EXPECT_EQ(V(3), map["dz"]);
  });
}

TEST(MapTest, Duplicate_final_states_test2) {
  vector<pair<string, output_t>> input = {
      {"a_a", V(0)},
      {"ab", V(1)},
      {"ab_a", V(2)},
      {"b_a", V(3)},
  };

  make_map(input, true, [](auto &map) {
    EXPECT_EQ(V(0), map["a_a"]);
    EXPECT_EQ(V(1), map["ab"]);
    EXPECT_EQ(V(2), map["ab_a"]);
    EXPECT_EQ(V(3), map["b_a"]);
  });
}

TEST(MapTest, UTF8_test) {
  vector<pair<string, output_t>> input = {
      {u8"あ", V(0)},
      {u8"あい", V(1)},
  };

  make_map(input, true, [](auto &map) {
    EXPECT_EQ(V(0), map[u8"あ"]);
    EXPECT_EQ(V(1), map[u8"あい"]);
  });
}

TEST(MapTest, Common_prefix_search_test) {
  vector<pair<string, output_t>> input = {
      {"a", V(0)},
      {"and", V(1)},
      {"android", V(2)},
  };

  make_map(input, true, [](auto &map) {
    auto ret = map.common_prefix_search("android phone");

    EXPECT_EQ(3, ret.size());

    EXPECT_EQ(1, ret[0].first);
    EXPECT_EQ(V(0), ret[0].second);

    EXPECT_EQ(3, ret[1].first);
    EXPECT_EQ(V(1), ret[1].second);

    EXPECT_EQ(7, ret[2].first);
    EXPECT_EQ(V(2), ret[2].second);
  });
}

TEST(StringTest, Invalid_arc_jump_test) {
  vector<pair<string, output_t>> input = {
      {"aazasl;kfjasfl;", V(0)}, {"acza;slkdfjas;", V(1)},
      {"adzs;ldfkjas;", V(2)},   {"aezs;lkdfjals;f", V(3)},
      {"afzasf;laksjf;l", V(4)}, {"agzaslfkjsa;ldfk", V(5)},
      {"ahzaslkdfjas;df", V(6)}, {"aizs;ldkfjas;dlf", V(7)},
      {"akzs;ldkfjas;", V(8)},   {"alzs;lfkjasdf;l", V(9)},
  };

  make_map(input, true, [](auto &map) { EXPECT_FALSE(map.contains("az")); });
}

TEST(MapTest, Single_output_value_test) {
  vector<pair<string, output_t>> input = {
      {"a", V(0)},
      {"ab", V(1)},
  };

  make_map(input, true, [](auto &map) {
    EXPECT_EQ(V(0), map["a"]);
    EXPECT_EQ(V(1), map["ab"]);
  });
}

TEST(MapTest, Auto_Index_Dictionary) {
  vector<string> input = {"a", "and", "android"};

  make_map_with_auto_index(input, true, [](auto &map) {
    auto ret = map.common_prefix_search("android phone");

    EXPECT_EQ(3, ret.size());

    EXPECT_EQ(1, ret[0].first);
    EXPECT_EQ(0, ret[0].second);

    EXPECT_EQ(3, ret[1].first);
    EXPECT_EQ(1, ret[1].second);

    EXPECT_EQ(7, ret[2].first);
    EXPECT_EQ(2, ret[2].second);
  });
}

TEST(SetTest, Normal_Set_test) {
  vector<string> input = {
      "jan", "feb", "mar", "apr", "may", "jun",
      "jul", "aug", "sep", "oct", "nov", "dec",
  };

  make_set(input, false, [](auto &set) {
    EXPECT_TRUE(set.contains("jan"));
    EXPECT_TRUE(set.contains("feb"));
    EXPECT_TRUE(set.contains("mar"));
    EXPECT_TRUE(set.contains("apr"));
    EXPECT_TRUE(set.contains("may"));
    EXPECT_TRUE(set.contains("jun"));
    EXPECT_TRUE(set.contains("jul"));
    EXPECT_TRUE(set.contains("aug"));
    EXPECT_TRUE(set.contains("sep"));
    EXPECT_TRUE(set.contains("oct"));
    EXPECT_TRUE(set.contains("nov"));
    EXPECT_TRUE(set.contains("dec"));

    EXPECT_FALSE(set.contains(""));
    EXPECT_FALSE(set.contains("_"));
    EXPECT_FALSE(set.contains("a"));
    EXPECT_FALSE(set.contains("ap"));
    EXPECT_FALSE(set.contains("ap_"));
    EXPECT_FALSE(set.contains("apr_"));

    {
      auto ret = set.predictive_search("ju");
      std::sort(ret.begin(), ret.end());

      EXPECT_EQ(2, ret.size());
      EXPECT_EQ("jul", ret[0]);
      EXPECT_EQ("jun", ret[1]);
    }
  });
}

TEST(SetTest, Japanese_Set_test) {
  vector<string> input = {
      u8"一", u8"一二", u8"一二三", u8"二", u8"二三",
  };

  stringstream ss;
  {
    auto [result, _] = fst::compile(input, ss, false, false);
    EXPECT_EQ(fst::Result::Success, result);
  }

  const auto &byte_code = ss.str();
  fst::set set(byte_code);

  {
    EXPECT_TRUE(set.contains(u8"一"));
    EXPECT_TRUE(set.contains(u8"一二"));
    EXPECT_TRUE(set.contains(u8"一二三"));
    EXPECT_TRUE(set.contains(u8"二"));
    EXPECT_TRUE(set.contains(u8"二三"));
    EXPECT_FALSE(set.contains(u8"一二三四"));
    EXPECT_FALSE(set.contains(""));
  }
}

TEST(DecompileTest, Decompile_map) {
  vector<pair<string, output_t>> input = {
      {"jan", V(31)}, {"feb", V(28)}, {"mar", V(31)}, {"apr", V(30)},
      {"may", V(31)}, {"jun", V(30)}, {"jul", V(31)}, {"aug", V(31)},
      {"sep", V(30)}, {"oct", V(31)}, {"nov", V(30)}, {"dec", V(31)},
  };

  stringstream ss;
  {
    auto [result, _] = fst::compile<output_t>(input, ss, false);
    EXPECT_EQ(fst::Result::Success, result);
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

  EXPECT_EQ(expected, out.str());
}

TEST(DecompileTest, Decompile_map_no_need_output) {
  vector<string> input = {
      "jan", "feb", "mar", "apr", "may", "jun",
      "jul", "aug", "sep", "oct", "nov", "dec",
  };

  stringstream ss;
  {
    auto [result, _] = fst::compile(input, ss, true, false);
    EXPECT_EQ(fst::Result::Success, result);
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

  EXPECT_EQ(expected, out.str());
}

TEST(DecompileTest, Decompile_map_need_output) {
  vector<string> input = {
      "jan", "feb", "mar", "apr", "may", "jun",
      "jul", "aug", "sep", "oct", "nov", "dec",
  };

  stringstream ss;
  {
    auto [result, _] = fst::compile(input, ss, true, false);
    EXPECT_EQ(fst::Result::Success, result);
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

  EXPECT_EQ(expected, out.str());
}

TEST(DecompileTest, Decompile_set) {
  vector<string> input = {
      "jan", "feb", "mar", "apr", "may", "jun",
      "jul", "aug", "sep", "oct", "nov", "dec",
  };

  stringstream ss;
  {
    auto [result, _] = fst::compile(input, ss, false, false);
    EXPECT_EQ(fst::Result::Success, result);
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

  EXPECT_EQ(strlen(expected), out.str().size());
  EXPECT_EQ(expected, out.str());
}

TEST(EditDistanceTest, Edit_distance_search_map) {
  vector<string> input = {
      "jan", "feb", "mar", "apr", "may", "jun",
      "jul", "aug", "sep", "oct", "nov", "dec",
  };

  stringstream ss;
  {
    auto [result, _] = fst::compile(input, ss, true, false);
    EXPECT_EQ(fst::Result::Success, result);
  }

  const auto &byte_code = ss.str();
  fst::map<uint32_t> matcher(byte_code);

  auto ret = matcher.edit_distance_search("joe", 2);
  EXPECT_EQ(4, ret.size());
}

TEST(EditDistanceTest, Edit_distance_search_set) {
  vector<string> input = {
      "jan", "feb", "mar", "apr", "may", "jun",
      "jul", "aug", "sep", "oct", "nov", "dec",
  };

  stringstream ss;
  {
    auto [result, _] = fst::compile(input, ss, false, false);
    EXPECT_EQ(fst::Result::Success, result);
  }

  const auto &byte_code = ss.str();
  fst::set matcher(byte_code);

  auto ret = matcher.edit_distance_search("joe", 2);
  EXPECT_EQ(4, ret.size());
}

TEST(EditDistanceTest, Japanese_edit_distance_search) {
  vector<string> input = {
      u8"一",
      u8"一二",
      u8"一二三",
      u8"一二三四",
  };

  stringstream ss;
  {
    auto [result, _] = fst::compile(input, ss, false, false);
    EXPECT_EQ(fst::Result::Success, result);
  }

  const auto &byte_code = ss.str();
  fst::set matcher(byte_code);

  {
    auto ret = matcher.edit_distance_search(u8"二", 1);
    EXPECT_EQ(2, ret.size());
    EXPECT_EQ(u8"一", ret[0]);
    EXPECT_EQ(u8"一二", ret[1]);
  }

  {
    auto ret = matcher.edit_distance_search(u8"二", 2);
    EXPECT_EQ(3, ret.size());
    EXPECT_EQ(u8"一", ret[0]);
    EXPECT_EQ(u8"一二", ret[1]);
    EXPECT_EQ(u8"一二三", ret[2]);
  }
}

TEST(SpellcheckTest, Spellcheck_set) {
  vector<string> input = {
      "jan", "feb", "mar", "apr", "may", "jun",
      "jul", "aug", "sep", "oct", "nov", "dec",
  };

  stringstream ss;
  {
    auto [result, _] = fst::compile(input, ss, false, false);
    EXPECT_EQ(fst::Result::Success, result);
  }

  const auto &byte_code = ss.str();

  fst::set matcher(byte_code);
  EXPECT_TRUE(matcher);

  {
    const auto &items = matcher.suggest("joe");

    auto &[similarity, word] = items.front();
    EXPECT_EQ("jan", word);
    EXPECT_EQ(349, std::floor(similarity * 1000));
  }
}

TEST(SpellcheckTest, Spellcheck_map) {
  vector<pair<string, output_t>> input = {
      {"jan", V(31)}, {"feb", V(28)}, {"mar", V(31)}, {"apr", V(30)},
      {"may", V(31)}, {"jun", V(30)}, {"jul", V(31)}, {"aug", V(31)},
      {"sep", V(30)}, {"oct", V(31)}, {"nov", V(30)}, {"dec", V(31)},
  };

  stringstream ss;
  {
    auto [result, _] = fst::compile<output_t>(input, ss, false);
    EXPECT_EQ(fst::Result::Success, result);
  }

  const auto &byte_code = ss.str();

  fst::map<output_t> matcher(byte_code);
  EXPECT_TRUE(matcher);

  {
    const auto &items = matcher.suggest("joe");

    auto &[similarity, word, output] = items.front();
    EXPECT_EQ("jan", word);
    EXPECT_EQ(31, output);
    EXPECT_EQ(349, std::floor(similarity * 1000));
  }
}

TEST(ReadmeTest, General) {
  const std::vector<std::pair<std::string, std::string>> items = {
      {"hello", u8"こんにちは!"},
      {"world", u8"世界!"},
      {"hello world", u8"こんにちは世界!"}, // incorrect sort order entry...
  };

  std::stringstream out;
  auto sorted = false; // ask fst::compile to sort entries
  auto [result, error_line] = fst::compile<std::string>(items, out, sorted);

  if (result == fst::Result::Success) {
    const auto &byte_code = out.str();
    fst::map<std::string> matcher(byte_code.data(), byte_code.size());

    if (matcher) {
      EXPECT_TRUE(matcher.contains("hello world"));
      EXPECT_FALSE(matcher.contains("Hello World"));
      EXPECT_EQ(u8"こんにちは!", matcher["hello"]);

      {
        auto prefixes = matcher.common_prefix_search("hello world!");
        EXPECT_EQ(2, prefixes.size());
        {
          auto [l, o] = prefixes[0];
          EXPECT_EQ(5, l);
          EXPECT_EQ(u8"こんにちは!", o);
        }
        {
          auto [l, o] = prefixes[1];
          EXPECT_EQ(11, l);
          EXPECT_EQ(u8"こんにちは世界!", o);
        }
      }

      {
        std::string output;
        auto length =
            matcher.longest_common_prefix_search("hello world!", output);
        EXPECT_EQ(11, length);
        EXPECT_EQ(u8"こんにちは世界!", output);
      }

      {
        auto predictives = matcher.predictive_search("he");
        EXPECT_EQ(2, predictives.size());
        {
          auto [k, o] = predictives[0];
          EXPECT_EQ("hello", k);
          EXPECT_EQ(u8"こんにちは", o);
        }
        {
          auto [k, o] = predictives[1];
          EXPECT_EQ("hello world", k);
          EXPECT_EQ(u8"こんにちは世界!", o);
        }
      }

      {
        auto predictives = matcher.predictive_search("hello w");
        EXPECT_EQ(1, predictives.size());
        {
          auto [k, o] = predictives[0];
          EXPECT_EQ("hello world", k);
          EXPECT_EQ(u8"こんにちは世界!", o);
        }
      }

      {
        auto edit_distances = matcher.edit_distance_search("hellow", 1);
        EXPECT_EQ(1, edit_distances.size());
        {
          auto [k, o] = edit_distances[0];
          EXPECT_EQ("hello", k);
          EXPECT_EQ(u8"こんにちは", o);
        }
      }

      {
        auto suggestions = matcher.suggest("hellow");
        EXPECT_EQ(3, suggestions.size());
        {
          auto [r, k, o] = suggestions[0];
          EXPECT_TRUE(0.810184 < r && r < 0.810186);
          EXPECT_EQ("hello", k);
          EXPECT_EQ(u8"こんにちは", o);
        }
        {
          auto [r, k, o] = suggestions[1];
          EXPECT_TRUE(0.504131 < r && r < 0.504133);
          EXPECT_EQ("hello world", k);
          EXPECT_EQ(u8"こんにちは世界!", o);
        }
        {
          auto [r, k, o] = suggestions[2];
          EXPECT_TRUE(0.0962962 < r && r < 0.0962964);
          EXPECT_EQ("world", k);
          EXPECT_EQ(u8"世界!", o);
        }
      }
    }
  }
}
