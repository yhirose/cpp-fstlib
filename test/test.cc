#define CATCH_CONFIG_MAIN
#include "catch.hpp"

#include <fstlib.h>

using namespace std;

TEST_CASE("Simple state machine test", "[general]")
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

    REQUIRE(fst::search(initial_state, "apr")[0] == "30");
    REQUIRE(fst::search(initial_state, "aug")[0] == "31");
    REQUIRE(fst::search(initial_state, "dec")[0] == "31");
    REQUIRE(fst::search(initial_state, "feb")[0] == "28");
    REQUIRE(fst::search(initial_state, "feb")[1] == "29");
    REQUIRE(fst::search(initial_state, "jul")[0] == "31");
    REQUIRE(fst::search(initial_state, "jun")[0] == "30");

    REQUIRE(fst::search(initial_state, "").empty());
    REQUIRE(fst::search(initial_state, "_").empty());
    REQUIRE(fst::search(initial_state, "a").empty());
    REQUIRE(fst::search(initial_state, "ap").empty());
    REQUIRE(fst::search(initial_state, "ap_").empty());
    REQUIRE(fst::search(initial_state, "apr_").empty());
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

    auto initial_state = fst::make_state_machine(input);

    REQUIRE(initial_state->id == 12);

    auto byte_code = fst::compile(initial_state);

    REQUIRE(fst::search(byte_code, "apr")[0] == "30");
    REQUIRE(fst::search(byte_code, "aug")[0] == "31");
    REQUIRE(fst::search(byte_code, "dec")[0] == "31");
    REQUIRE(fst::search(byte_code, "feb")[0] == "28");
    REQUIRE(fst::search(byte_code, "feb")[1] == "29");
    REQUIRE(fst::search(byte_code, "jul")[0] == "31");
    REQUIRE(fst::search(byte_code, "jun")[0] == "30");

    REQUIRE(fst::search(byte_code, "").empty());
    REQUIRE(fst::search(byte_code, "_").empty());
    REQUIRE(fst::search(byte_code, "a").empty());
    REQUIRE(fst::search(byte_code, "ap").empty());
    REQUIRE(fst::search(byte_code, "ap_").empty());
    REQUIRE(fst::search(byte_code, "apr_").empty());
}

TEST_CASE("Common prefix test", "[general]")
{
    vector<pair<string, string>> input = {
        { "a",  "0" },
        { "ab", "1" },
    };

    auto initial_state = fst::make_state_machine(input);
    REQUIRE(initial_state->id == 2);
    REQUIRE(fst::search(initial_state, "a")[0] == "0");
    REQUIRE(fst::search(initial_state, "ab")[0] == "1");

    auto byte_code = fst::compile(initial_state);
    REQUIRE(fst::search(byte_code, "a")[0] == "0");
    REQUIRE(fst::search(byte_code, "ab")[0] == "1");
}

TEST_CASE("Common prefix test2", "[general]")
{
    vector<pair<string, string>> input = {
        { "aa",  "0" },
        { "abb", "1" },
    };

    auto initial_state = fst::make_state_machine(input);
    REQUIRE(initial_state->id == 3);
    REQUIRE(fst::search(initial_state, "aa")[0] == "0");
    REQUIRE(fst::search(initial_state, "abb")[0] == "1");

    auto byte_code = fst::compile(initial_state);
    REQUIRE(fst::search(byte_code, "aa")[0] == "0");
    REQUIRE(fst::search(byte_code, "abb")[0] == "1");
}

TEST_CASE("Common prefix test3", "[general]")
{
    vector<pair<string, string>> input = {
        { "abc", "0" },
        { "bc",  "1" },
    };

    auto initial_state = fst::make_state_machine(input);
    REQUIRE(initial_state->id == 3);
    REQUIRE(fst::search(initial_state, "abc")[0] == "0");
    REQUIRE(fst::search(initial_state, "bc")[0] == "1");

    auto byte_code = fst::compile(initial_state);
    REQUIRE(fst::search(byte_code, "abc")[0] == "0");
    REQUIRE(fst::search(byte_code, "bc")[0] == "1");
}

TEST_CASE("Common prefix test4", "[general]")
{
    vector<pair<string, string>> input = {
        { "z",   "0"  },
        { "zc",  "10" },
        { "zcd", "11" },
        { "zd",  "1"  },
    };

    auto initial_state = fst::make_state_machine(input);
    REQUIRE(initial_state->id == 3);
    REQUIRE(fst::search(initial_state, "z")[0] == "0");
    REQUIRE(fst::search(initial_state, "zc")[0] == "10");
    REQUIRE(fst::search(initial_state, "zcd")[0] == "11");
    REQUIRE(fst::search(initial_state, "zd")[0] == "1");

    auto byte_code = fst::compile(initial_state);
    REQUIRE(fst::search(byte_code, "z")[0] == "0");
    REQUIRE(fst::search(byte_code, "zc")[0] == "10");
    REQUIRE(fst::search(byte_code, "zcd")[0] == "11");
    REQUIRE(fst::search(byte_code, "zd")[0] == "1");
}

TEST_CASE("Common prefix test5", "[general]")
{
    vector<pair<string, string>> input = {
        { "aba", "1" },
        { "abz", "2" },
        { "baz", "31" },
        { "bz",  "32" },
    };

    auto initial_state = fst::make_state_machine(input);
    REQUIRE(initial_state->id == 5);
    REQUIRE(fst::search(initial_state, "aba")[0] == "1");
    REQUIRE(fst::search(initial_state, "abz")[0] == "2");
    REQUIRE(fst::search(initial_state, "baz")[0] == "31");
    REQUIRE(fst::search(initial_state, "bz")[0] == "32");

    auto byte_code = fst::compile(initial_state);
    REQUIRE(fst::search(byte_code, "aba")[0] == "1");
    REQUIRE(fst::search(byte_code, "abz")[0] == "2");
    REQUIRE(fst::search(byte_code, "baz")[0] == "31");
    REQUIRE(fst::search(byte_code, "bz")[0] == "32");
}

TEST_CASE("Duplicate final states test", "[general]")
{
    vector<pair<string, string>> input = {
        { "az", "0" },
        { "bz", "1" },
        { "cy", "2" },
        { "dz", "3" },
    };

    auto initial_state = fst::make_state_machine(input);
    auto byte_code = fst::compile(initial_state);

    REQUIRE(fst::command_count(byte_code) == 6);

    REQUIRE(fst::search(initial_state, "az")[0] == "0");
    REQUIRE(fst::search(initial_state, "bz")[0] == "1");
    REQUIRE(fst::search(initial_state, "cy")[0] == "2");
    REQUIRE(fst::search(initial_state, "dz")[0] == "3");

    REQUIRE(fst::search(byte_code, "az")[0] == "0");
    REQUIRE(fst::search(byte_code, "bz")[0] == "1");
    REQUIRE(fst::search(byte_code, "cy")[0] == "2");
    REQUIRE(fst::search(byte_code, "dz")[0] == "3");
}

TEST_CASE("UTF-8 test", "[general]")
{
    vector<pair<string, string>> input = {
        { u8"あ",   "0" },
        { u8"あい", "1" },
    };

    auto initial_state = fst::make_state_machine(input);

    REQUIRE(initial_state->id == 6);
    REQUIRE(fst::search(initial_state, u8"あ")[0] == "0");
    REQUIRE(fst::search(initial_state, u8"あい")[0] == "1");

    auto byte_code = fst::compile(initial_state);
    REQUIRE(fst::search(byte_code, u8"あ")[0] == "0");
    REQUIRE(fst::search(byte_code, u8"あい")[0] == "1");
}
