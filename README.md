# cpp-fstlib

[![](https://github.com/yhirose/cpp-fstlib/workflows/CMake/badge.svg)](https://github.com/yhirose/cpp-fstlib/actions)

C++17 header-only FST (finite state transducer) library.
We can use it as [Trie data structure](https://en.wikipedia.org/wiki/Trie).
This library uses the algorithm "[Minimal Acyclic Subsequential Transducers](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.24.3698&rep=rep1&type=pdf)".

## Play cpp-fstlib with cli

```bash
> git clone http://github/yhirose/cpp-fstlib
> cd cpp-fstlib
> make build && cd build
> cmake .. && make
> ./cmd/fst compile /usr/share/dict/words words.fst

> ./cmd/fst search words.fst hello
83713

> ./cmd/fst prefix words.fst helloworld
h: 81421
he: 82951
hell: 83657
hello: 83713

> ./cmd/fst longest words.fst helloworld
hello: 83713

> ./cmd/fst predictive words.fst predictiv
predictive: 153474
predictively: 153475
predictiveness: 153476

> ./cmd/fst fuzzy words.fst fuzzy -ed 2 // Edit distance 2
Suzy: 195759
buzz: 28064
buzzy: 28076
...

> ./cmd/fst spellcheck words.fst thier
their: 0.946667
thir: 0.762667
tier: 0.752
thief: 0.736
trier: 0.704
```

## API reference

```cpp
namespace fst {

enum class Result { Success, EmptyKey, UnsortedKey, DuplicateKey };

std::pair<Result, size_t /* error input index */> compile<uint32_t>(
  const std::vector<std::pair<std::string, uint32_t>> &input,
  std::ostream &os,
  bool sorted
);

std::pair<Result, size_t /* error input index */> compile<std::string>(
  const std::vector<std::pair<std::string, std::string>> &input,
  std::ostream &os
);

std::pair<Result, size_t /* error input index */> compile(
  const std::vector<std::string> &key_only_input,
  std::ostream &os,
  bool need_output, // true: map, false: set
  bool sorted
);

template <typename output_t> class map {
public:
  map(const char *byte_code, size_t byte_code_size);

  operator bool() const;

  bool contains(std::string_view sv) const;

  output_t operator[](std::string_view sv) const;

  output_t at(std::string_view sv) const;

  bool exact_match_search(std::string_view sv, output_t &output) const;

  std::vector<std::pair<size_t length, output_t output>>
  common_prefix_search(std::string_view sv) const;

  size_t longest_common_prefix_search(std::string_view sv, output_t &output) const;

  std::vector<std::pair<std::string, output_t>>
  predictive_search(std::string_view sv) const;

  std::vector<std::pair<std::string, output_t>>
  edit_distance_search(std::string_view sv, size_t max_edits) const;

  std::vector<std::tuple<double, std::string, output_t>>
  suggest(std::string_view word) const;
}

class set {
public:
  set(const char *byte_code, size_t byte_code_size);

  operator bool() const;

  bool contains(std::string_view sv) const;

  std::vector<size_t> common_prefix_search(std::string_view sv) const;

  size_t longest_common_prefix_search(std::string_view sv) const;

  std::vector<std::string> predictive_search(std::string_view sv) const;

  std::vector<std::string>
  edit_distance_search(std::string_view sv, size_t max_edits) const;

  std::vector<std::pair<double, std::string>>
  suggest(std::string_view word) const;
}

} // namespace fst
```

## API usage

```cpp
const std::vector<std::pair<std::string, std::string>> items = {
  {"hello", "こんにちは!"},
  {"world", "世界!"},
  {"hello world", "こんにちは世界!"}, // incorrect sort order entry...
};

std::stringstream out;
auto sorted = false; // ask fst::compile to sort entries
auto [result, error_line] = fst::compile<std::string>(items, out, sorted);

if (result == fst::Result::Success) {
  const auto& byte_code = out.str();
  fst::map<std::string> matcher(byte_code.data(), byte_code.size());

  if (matcher) {
    assert(matcher.contains("hello world"));
    assert(!matcher.contains("Hello World"));
    assert(matcher["hello"] == "こんにちは!");

    auto prefixes = matcher.common_prefix_search("hello world!");
    assert(prefixes.size() == 2);
    assert(prefixes[0].first == 5);
    assert(prefixes[0].second == "こんにちは!");
    assert(prefixes[1].first == 11);
    assert(prefixes[1].second == "こんにちは世界!");

    std::string output;
    auto length = matcher.longest_common_prefix_search("hello world!", output);
    assert(length == 11);
    assert(output == "こんにちは世界!");

    auto predictives = matcher.predictive_search("he");
    assert(predictives.size() == 2);
    assert(predictives[0].first == "hello");
    assert(predictives[0].second == "こんにちは!");
    assert(predictives[1].first == "hello world");
    assert(predictives[1].second == "こんにちは世界!");

    std::cout << "[Edit distance 1]" << std::endl;
    for (auto [k, o]: matcher.edit_distance_search("hellow", 1)) {
      std::cout << "key: " << k << " output: " << o << std::endl;
    }

    std::cout << "[Suggestions]" << std::endl;
    for (auto [r, k, o]: matcher.suggest("hellow")) {
      std::cout << "ratio: " << r << " key: " << k << " output: " << o << std::endl;
    }
  }
}
```

```
[Edit distance 1]
key: hello output: こんにちは
[Suggestions]
ratio: 0.810185 key: hello output: こんにちは
ratio: 0.504132 key: hello world output: こんにちは世界!
ratio: 0.0962963 key: world output: 世界!
```

License
-------

MIT license (© 2022 Yuji Hirose)
