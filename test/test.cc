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
    auto ret = fst::search(byte_code, "ab");
    REQUIRE(fst::search(byte_code, "a")[0] == "0");
    REQUIRE(fst::search(byte_code, "ab")[0] == "1");
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
