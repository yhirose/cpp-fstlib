# cpp-fstlib
C++11 header-only finite state transducer.
It can be used as [Trie data structure](https://en.wikipedia.org/wiki/Trie).
The algorithm used in this library is based on "[Minimal Acyclic Subsequential Transducers](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.24.3698&rep=rep1&type=pdf)".

Usage
-----

### Exact match search

```cpp
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

auto t = fst::build(input);

assert(fst::exact_match_search(t.data(), t.size(), "apr")[0] == "30");
assert(fst::exact_match_search(t.data(), t.size(), "ap").empty());
assert(fst::exact_match_search(t.data(), t.size(), "apr_").empty());
assert(fst::exact_match_search(t.data(), t.size(), "feb")[1] == "29");
```

### Common prefix search

```cpp
auto t = fst::build([](auto feed) {
    feed("a",       "0");
    feed("and",     "1");
    feed("android", "2");
});

auto ret = fst::common_prefix_search(t.data(), t.size(), "android phone");

assert(ret[0].length == 1);
assert(ret[0].outputs[0] == "0");
assert(ret[1].length == 3);
assert(ret[1].outputs[0] == "1");
assert(ret[2].length == 7);
assert(ret[2].outputs[0] == "2");
```

API
---

### build

```cpp
std::vector<char>
build(const std::vector<std::pair<std::string, std::string>>& input)

std::vector<char>
build(std::function<void (std::function<void (const std::string& str, std::string value)> feed)> input);
```

### exact_match_search

```cpp
std::vector<std::string>
exact_match_search(const char* byte_code, size_t byte_code_size, const char* str)
```

### common_prefix_search

```cpp
struct CommonPrefixSearchResult {
    size_t                   length;
    std::vector<std::string> outputs;
};

std::vector<CommonPrefixSearchResult>
common_prefix_search(const char* byte_code, size_t byte_code_size, const char* str)
```

Tested compilers
----------------

  * Visual Studio 2015
  * Clang 3.5

License
-------

MIT license (© 2015 Yuji Hirose)
