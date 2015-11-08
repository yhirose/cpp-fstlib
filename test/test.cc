#define CATCH_CONFIG_MAIN
#include "catch.hpp"

#include <fstlib.h>

using namespace std;

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

    auto initial_state = fst::make_state_machine(input);

    REQUIRE(initial_state->id == 12);

    auto byte_code = fst::compile(initial_state);

    REQUIRE(fst::exact_match_search(byte_code, "apr")[0] == "30");
    REQUIRE(fst::exact_match_search(byte_code, "aug")[0] == "31");
    REQUIRE(fst::exact_match_search(byte_code, "dec")[0] == "31");
    REQUIRE(fst::exact_match_search(byte_code, "feb")[0] == "28");
    REQUIRE(fst::exact_match_search(byte_code, "feb")[1] == "29");
    REQUIRE(fst::exact_match_search(byte_code, "jul")[0] == "31");
    REQUIRE(fst::exact_match_search(byte_code, "jun")[0] == "30");

    REQUIRE(fst::exact_match_search(byte_code, "").empty());
    REQUIRE(fst::exact_match_search(byte_code, "_").empty());
    REQUIRE(fst::exact_match_search(byte_code, "a").empty());
    REQUIRE(fst::exact_match_search(byte_code, "ap").empty());
    REQUIRE(fst::exact_match_search(byte_code, "ap_").empty());
    REQUIRE(fst::exact_match_search(byte_code, "apr_").empty());
}

TEST_CASE("Edge case test1", "[general]")
{
    vector<pair<string, string>> input = {
        { "a",  "0" },
        { "ab", "1" },
    };

    auto initial_state = fst::make_state_machine(input);

    REQUIRE(initial_state->id == 2);

    auto byte_code = fst::compile(initial_state);

    REQUIRE(fst::exact_match_search(byte_code, "a")[0] == "0");
    REQUIRE(fst::exact_match_search(byte_code, "ab")[0] == "1");
}

TEST_CASE("Edge case test2", "[general]")
{
    vector<pair<string, string>> input = {
        { "aa",  "0" },
        { "abb", "1" },
    };

    auto initial_state = fst::make_state_machine(input);

    REQUIRE(initial_state->id == 3);

    auto byte_code = fst::compile(initial_state);

    REQUIRE(fst::exact_match_search(byte_code, "aa")[0] == "0");
    REQUIRE(fst::exact_match_search(byte_code, "abb")[0] == "1");
}

TEST_CASE("Edge case test3", "[general]")
{
    vector<pair<string, string>> input = {
        { "abc", "0" },
        { "bc",  "1" },
    };

    auto initial_state = fst::make_state_machine(input);

    REQUIRE(initial_state->id == 3);

    auto byte_code = fst::compile(initial_state);

    REQUIRE(fst::exact_match_search(byte_code, "abc")[0] == "0");
    REQUIRE(fst::exact_match_search(byte_code, "bc")[0] == "1");
}

TEST_CASE("Edge case test4", "[general]")
{
    vector<pair<string, string>> input = {
        { "z",   "0"  },
        { "zc",  "10" },
        { "zcd", "11" },
        { "zd",  "1"  },
    };

    auto initial_state = fst::make_state_machine(input);

    REQUIRE(initial_state->id == 3);

    auto byte_code = fst::compile(initial_state);

    REQUIRE(fst::exact_match_search(byte_code, "z")[0] == "0");
    REQUIRE(fst::exact_match_search(byte_code, "zc")[0] == "10");
    REQUIRE(fst::exact_match_search(byte_code, "zcd")[0] == "11");
    REQUIRE(fst::exact_match_search(byte_code, "zd")[0] == "1");
}

TEST_CASE("Edge case test5", "[general]")
{
    vector<pair<string, string>> input = {
        { "aba", "1" },
        { "abz", "2" },
        { "baz", "31" },
        { "bz",  "32" },
    };

    auto initial_state = fst::make_state_machine(input);

    REQUIRE(initial_state->id == 5);

    auto byte_code = fst::compile(initial_state);

    REQUIRE(fst::exact_match_search(byte_code, "aba")[0] == "1");
    REQUIRE(fst::exact_match_search(byte_code, "abz")[0] == "2");
    REQUIRE(fst::exact_match_search(byte_code, "baz")[0] == "31");
    REQUIRE(fst::exact_match_search(byte_code, "bz")[0] == "32");
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

    REQUIRE(fst::exact_match_search(byte_code, "az")[0] == "0");
    REQUIRE(fst::exact_match_search(byte_code, "bz")[0] == "1");
    REQUIRE(fst::exact_match_search(byte_code, "cy")[0] == "2");
    REQUIRE(fst::exact_match_search(byte_code, "dz")[0] == "3");
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

    REQUIRE(fst::exact_match_search(byte_code, "a_a")[0] == "0");
    REQUIRE(fst::exact_match_search(byte_code, "ab")[0] == "1");
    REQUIRE(fst::exact_match_search(byte_code, "ab_a")[0] == "2");
    REQUIRE(fst::exact_match_search(byte_code, "b_a")[0] == "3");
}

TEST_CASE("UTF-8 test", "[general]")
{
    vector<pair<string, string>> input = {
        { u8"あ",   "0" },
        { u8"あい", "1" },
    };

    auto initial_state = fst::make_state_machine(input);

    REQUIRE(initial_state->id == 6);

    auto byte_code = fst::compile(initial_state);

    REQUIRE(fst::exact_match_search(byte_code, u8"あ")[0] == "0");
    REQUIRE(fst::exact_match_search(byte_code, u8"あい")[0] == "1");
}

TEST_CASE("Common prefix search test", "[general]")
{
    vector<pair<string, string>> input = {
        { "a", "0" },
        { "and", "1" },
        { "android", "2" },
    };

    auto byte_code = fst::build(input);
    auto ret = fst::common_prefix_search(byte_code, "android phone");

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
    auto ret = fst::common_prefix_search(byte_code, "android phone");

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

