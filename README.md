# cpp-fstlib
C++11 header-only FST (finite state transducer) library.
We can use it as [Trie data structure](https://en.wikipedia.org/wiki/Trie).
This library uses the algorithm "[Minimal Acyclic Subsequential Transducers](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.24.3698&rep=rep1&type=pdf)".

Usage
-----

  1. Prepare entries sorted by key string
  2. Build a FST database
  3. Try 'exact match search' or 'common prefix search'

  NOTE: Key type should be `std:stirng`. Value type can be either `uint32_t` or `std::string`. If value is not provided, value type will be `uint32_t` and the value starts with 0.

### Exact match search

```cpp
std::vector<std::pair<std::string, uint32_t>> input = {
  { "apr", 30 },
  { "aug", 31 },
  { "dec", 31 },
  { "feb", 28 },
  { "feb", 29 },
  { "jan", 31 },
  { "jul", 31 },
  { "jun", 30 },
};

std::vector<char> t = fst::build(input);

assert(fst::exact_match_search<uint32_t>(t.data(), t.size(), "apr")[0] == 30);
assert(fst::exact_match_search<uint32_t>(t.data(), t.size(), "ap").empty());
assert(fst::exact_match_search<uint32_t>(t.data(), t.size(), "apr_").empty());
assert(fst::exact_match_search<uint32_t>(t.data(), t.size(), "feb")[0] == 28);
assert(fst::exact_match_search<uint32_t>(t.data(), t.size(), "feb")[1] == 29);
```

### Common prefix search

```cpp
auto t = fst::build<std::string>([](auto add_entry) {
  add_entry("a",       "one");
  add_entry("and",     "two");
  add_entry("android", "three");
});

auto ret = fst::common_prefix_search<std::string>(t.data(), t.size(), "android phone");

assert(ret[0].length == 1);
assert(ret[0].outputs[0] == "one");

assert(ret[1].length == 3);
assert(ret[1].outputs[0] == "two");

assert(ret[2].length == 7);
assert(ret[2].outputs[0] == "three");
```

### Auto Index Dictionary

```cpp
auto t = fst::build({
  "a",
  "and",
  "android",
});

auto ret = fst::common_prefix_search<uint32_t>(t.data(), t.size(), "android phone");

assert(ret[0].length == 1);
assert(ret[0].outputs[0] == 0);

assert(ret[1].length == 3);
assert(ret[1].outputs[0] == 1);

assert(ret[2].length == 7);
assert(ret[2].outputs[0] == 2);
```

API
---

### build

```cpp
std::vector<char> build(
  const std::vector<std::string>& input)

std::vector<char> build(
  const std::vector<std::pair<std::string, std::string>>& input)

std::vector<char> build(
  std::function<void (
    std::function<void (const std::string& str)> add_entry)> input);

std::vector<char> build(
  std::function<void (
    std::function<void (const std::string& str, const std::string& value)> add_entry)> input);
```

### exact_match_search

```cpp
std::vector<std::string> exact_match_search(
  const char* byte_code,
  size_t      byte_code_size,
  const char* str)

bool exact_match_search(
  const char*  byte_code,
  size_t       byte_code_size,
  const char*  str,
  std::string& output)
```

### common_prefix_search

```cpp
struct CommonPrefixSearchResult {
  size_t                   length;
  std::vector<std::string> outputs;
};

std::vector<CommonPrefixSearchResult> common_prefix_search(
  const char* byte_code,
  size_t      byte_code_size,
  const char* str)
```

Tested compilers
----------------

  * Visual Studio 2017
  * Visual Studio 2015
  * Clang 4
  * Clang 3.8
  * Clang 3.5
  * g++ 5.4

License
-------

MIT license (© 2015 Yuji Hirose)
