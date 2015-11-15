# cpp-fstlib
C++11 header-only finite state transducer.
The algorithm used in this library is based on "[Minimal Acyclic Subsequential Transducers](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.24.3698&rep=rep1&type=pdf)".

### Exact match search

```c++
std::vector<std::pair<std::string, std::string>> input = {
    { "apr", "30" },
    { "aug", "31" },
    { "dec", "31" },
    { "feb", "28" },
    { "feb", "29" },
    { "jan", "31" },
    { "jul", "31" },
    { "jun", "30" },
};

const auto& byte_code = fst::build(input);
auto d = byte_code.data();
auto s = byte_code.size();

assert(fst::exact_match_search(d, s, "apr")[0] == "30");
assert(fst::exact_match_search(d, s, "aug")[0] == "31");
assert(fst::exact_match_search(d, s, "dec")[0] == "31");
assert(fst::exact_match_search(d, s, "feb")[0] == "28");
assert(fst::exact_match_search(d, s, "feb")[1] == "29");
assert(fst::exact_match_search(d, s, "jul")[0] == "31");
assert(fst::exact_match_search(d, s, "jun")[0] == "30");

assert(fst::exact_match_search(d, s, "").empty());
assert(fst::exact_match_search(d, s, "_").empty());
assert(fst::exact_match_search(d, s, "a").empty());
assert(fst::exact_match_search(d, s, "ap").empty());
assert(fst::exact_match_search(d, s, "ap_").empty());
assert(fst::exact_match_search(d, s, "apr_").empty());
```

### Common prefix search

```c++
std::vector<std::pair<std::string, std::string>> input = {
    { "a",       "0" },
    { "and",     "1" },
    { "android", "2" },
};

const auto& byte_code = fst::build(input);

const auto& ret = fst::common_prefix_search(
	byte_code.data(), byte_code.size(), "android phone");

assert(ret.size() == 3);

assert(ret[0].length == 1);
assert(ret[0].outputs.size() == 1);
assert(ret[0].outputs[0] == "0");

assert(ret[1].length == 3);
assert(ret[1].outputs.size() == 1);
assert(ret[1].outputs[0] == "1");

assert(ret[2].length == 7);
assert(ret[2].outputs.size() == 1);
assert(ret[2].outputs[0] == "2");
```
