# cpp-fstlib

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
98742

> ./cmd/fst prefix words.fst helloworld
h: 96806
he: 98099
hell: 98711
hello: 98742

> ./cmd/fst longest words.fst helloworld
hello: 98742
```

## API reference

```cpp
namespace fst {

std::pair<Result, size_t> compile<uint32_t>(
  const std::vector<std::string, uint32_t> &input,
  std::ostream &os,
  bool sorted
 );

std::pair<Result, size_t> compile<std::string>(
  const std::vector<std::string, std::string> &input,
  std::ostream &os
);

std::pair<Result, size_t> compile(
  const std::vector<std::string> &input,
  std::ostream &os,
  bool sorted
);

template <typename output_t = uint32_t> class Matcher {
public:
  Matcher(const char *byte_code, size_t byte_code_size);

  bool exact_match_search(
    const char *str, size_t len,
    bool sorted
  ) const;

  bool common_prefix_search(
    const char *str, size_t len,
    std::function<void(size_t, const output_t &)> prefixes
  ) const;

  size_t longest_common_prefix_search(
    const char *str, size_t len,
    output_t &output
  ) const;
}

} // namespace fst
```

## API usage

```cpp
const std::vector<std::pair<std::string, std::string>> items = {
  {"hello!", "こんにちは!"},
  {"hello world!", "こんにちは世界!"}, // incorrect sort order entry...
  {"world!", "世界!"},
};

std::stringstream out;
auto sorted = false; // ask fst::compile to sort entries
fst::compile<std::string>(items, out, sorted);

const auto& byte_code = out.str();
fst::Matcher<std::string> matcher(byte_code.data(), byte_code.size());

if (matcher) {
  const std::string s = "hello world! example.";
  std::string output;
  auto prefix_len = matcher.longest_common_prefix_search(s.data(), s.length(), output);

  assert(prefix_len == 12);
  assert(output == "こんにちは世界!");
}
```

### Try out a demo on Repl.it!

https://repl.it/@yhirose/FST

License
-------

MIT license (© 2020 Yuji Hirose)
