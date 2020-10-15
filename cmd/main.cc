#include "flags.h"
#include <fstlib.h>
#include <fstream>
#include <spellcheck.h>
#include <sstream>

using namespace std;

template <typename output_t> struct traints {
  static output_t convert(uint32_t n) {}
  static output_t convert(const string &s) {}
};

template <> struct traints<uint32_t> {
  static uint32_t convert(uint32_t n) { return n; }
  static uint32_t convert(const string &s) { return stoi(s); }
};

template <> struct traints<string> {
  static string convert(uint32_t n) { return to_string(n); }
  static string convert(const string &s) { return s; }
};

vector<string> split(const string &input, char delimiter) {
  istringstream stream(input);
  string field;
  vector<string> result;
  while (getline(stream, field, delimiter)) {
    result.push_back(field);
  }
  return result;
}

bool ends_with(std::string const &value, std::string const &ending) {
  if (ending.size() > value.size()) return false;
  return std::equal(ending.rbegin(), ending.rend(), value.rbegin());
}

template <typename output_t>
vector<pair<string, output_t>> load_input(istream &fin, char delimiter) {
  vector<pair<string, output_t>> input;
  string line;
  while (getline(fin, line)) {
    auto fields = split(line, delimiter);
    if (fields.size() > 1) {
      input.emplace_back(fields[0], traints<output_t>::convert(fields[1]));
    } else {
      input.emplace_back(line, traints<output_t>::convert(
                                   static_cast<uint32_t>(input.size())));
    }
  }
  return input;
}

template <typename output_t>
vector<pair<string, output_t>> load_input(istream &fin, string_view format) {
  vector<pair<string, output_t>> input;
  if (format == "csv") {
    input = load_input<output_t>(fin, ',');
  } else if (format == "tsv") {
    input = load_input<output_t>(fin, '\t');
  } else {
    throw runtime_error("invalid format...");
  }
  return input;
}

vector<string> load_input(istream &fin) {
  vector<string> input;
  string line;
  while (getline(fin, line)) {
    input.emplace_back(line);
  }
  return input;
}

vector<char> load_byte_code(istream &is) {
  is.seekg(0, ios_base::end);
  auto size = (unsigned int)is.tellg();
  is.seekg(0, ios_base::beg);
  vector<char> byte_code(size);
  is.read(byte_code.data(), size);
  return byte_code;
}

void show_error_message(fst::Result result, size_t line) {
  string error_message;

  switch (result) {
  case fst::Result::EmptyKey: error_message = "empty key"; break;
  case fst::Result::UnsortedKey: error_message = "unsorted key"; break;
  case fst::Result::DuplicateKey: error_message = "duplicate key"; break;
  default: error_message = "Unknown"; break;
  }

  cerr << "line " << line << ": " << error_message << endl;
}

template <typename output_t>
void map_regression_test(const vector<pair<string, output_t>> &input,
                         const string &byte_code) {
  fst::Map<output_t> matcher(byte_code.data(), byte_code.size());

  if (matcher) {
    for (const auto &[word, expected] : input) {
      auto actual = fst::OutputTraits<output_t>::initial_value();
      if (!matcher.exact_match_search(word, actual)) {
        if (expected != actual) {
          cerr << "word: " << word << endl;
          cerr << "expected value: " << expected << endl;
          cerr << "actual value: " << actual << endl;
        }
      } else {
        cerr << "couldn't find '" << word << "'" << endl;
      }
    }
  } else {
    cerr << "someting is wrong in byte_code..." << endl;
  }
}

void map_regression_test(const vector<string> &input, const string &byte_code) {
  fst::Map<uint32_t> matcher(byte_code.data(), byte_code.size());

  if (matcher) {
    uint32_t expected = 0;
    for (const auto &word : input) {
      uint32_t actual = 0;
      if (matcher.exact_match_search(word, actual)) {
        if (expected != actual) {
          cerr << "word: " << word << endl;
          cerr << "expected value: " << expected << endl;
          cerr << "actual value: " << actual << endl;
        }
      } else {
        cerr << "couldn't find '" << word << "'" << endl;
      }
      expected++;
    }
  } else {
    cerr << "someting is wrong in byte_code..." << endl;
  }
}

template <typename output_t>
void set_regression_test(const vector<pair<string, output_t>> &input,
                         const string &byte_code) {
  fst::Set matcher(byte_code.data(), byte_code.size());

  if (matcher) {
    for (const auto &[word, _] : input) {
      if (!matcher.contains(word)) {
        cerr << "couldn't find '" << word << "'" << endl;
      }
    }
  } else {
    cerr << "someting is wrong in byte_code..." << endl;
  }
}

void set_regression_test(const vector<string> &input, const string &byte_code) {
  fst::Set matcher(byte_code.data(), byte_code.size());

  if (matcher) {
    for (const auto &word : input) {
      if (!matcher.contains(word)) {
        cerr << "couldn't find '" << word << "'" << endl;
      }
    }
  } else {
    cerr << "someting is wrong in byte_code..." << endl;
  }
}

template <typename output_t>
void regression_test(const vector<pair<string, output_t>> &input,
                     const string &byte_code, bool need_output) {
  if (need_output) {
    map_regression_test(input, byte_code);
  } else {
    set_regression_test(input, byte_code);
  }
}

void regression_test(const vector<string> &input, const string &byte_code,
                     bool need_output) {
  if (need_output) {
    map_regression_test(input, byte_code);
  } else {
    set_regression_test(input, byte_code);
  }
}

template <typename T>
bool spellcheck_word(const T &matcher, const std::string word) {
  auto &&[ret, candidates] = fst::spellcheck(matcher, word);

  if (ret) {
    cout << "correct word!" << std::endl;
    return true;
  }

  size_t count = 10;
  for (auto [candidate, similarity] : candidates) {
    if (count == 0) { break; }
    cout << candidate << ": " << similarity << std::endl;
    count--;
  }
  return false;
}

template <typename output_t, typename T, typename U>
void map_search_word(const T &byte_code, string_view cmd, bool verbose,
                     const U &matcher, string_view word, size_t edit_distance) {
  if (cmd == "spellcheck") {
    spellcheck_word(matcher, string(word));
    return;
  }

  auto ret = false;
  if (cmd == "search") {
    output_t output;
    ret = matcher.exact_match_search(word, output);
    if (ret) { cout << output << endl; }
  } else if (cmd == "prefix") {
    ret =
        matcher.common_prefix_search(word, [&](size_t len, const auto &output) {
          cout << word.substr(0, len) << ": " << output << endl;
        });
  } else if (cmd == "longest") {
    output_t output;
    auto len = matcher.longest_common_prefix_search(word, output);
    if (len > 0) {
      ret = true;
      cout << word.substr(0, len) << ": " << output << endl;
    }
  } else if (cmd == "fuzzy") {
    auto results = matcher.edit_distance_search(word, edit_distance);
    ret = !results.empty();
    for (const auto &[word, output] : results) {
      cout << word << ": " << output << endl;
    }
  }
  if (!ret) { cout << "not found..." << endl; }
}

template <typename output_t, typename T>
void map_search(const T &byte_code, string_view cmd, bool verbose,
                string_view word, size_t edit_distance) {
  fst::Map<output_t> matcher(byte_code.data(), byte_code.size());
  matcher.set_trace(verbose);

  if (matcher) {
    if (word.empty()) {
      string word;
      while (getline(cin, word)) {
        map_search_word<output_t>(byte_code, cmd, verbose, matcher, word,
                                  edit_distance);
      }
    } else {
      map_search_word<output_t>(byte_code, cmd, verbose, matcher, word,
                                edit_distance);
    }
  } else {
    cerr << "invalid file..." << endl;
  }
}

template <typename T, typename U>
void set_search_word(const T &byte_code, string_view cmd, bool verbose,
                     const U &matcher, string_view word, size_t edit_distance) {
  bool ret = false;
  if (cmd == "search") {
    ret = matcher.contains(word);
    if (ret) { cout << "exist!" << endl; }
  } else if (cmd == "prefix") {
    ret = matcher.common_prefix_search(
        word, [&](size_t len) { cout << word.substr(0, len) << endl; });
  } else if (cmd == "longest") {
    auto len = matcher.longest_common_prefix_search(word);
    if (len > 0) {
      ret = true;
      cout << word.substr(0, len) << endl;
    }
  } else if (cmd == "fuzzy") {
    auto results = matcher.edit_distance_search(word, edit_distance);
    ret = !results.empty();
    for (const auto &word : results) {
      cout << word << endl;
    }
  } else { // "spellcheck"
    ret = spellcheck_word(matcher, string(word));
  }
  if (!ret) { cerr << "not found..." << endl; }
}

template <typename T>
void set_search(const T &byte_code, string_view cmd, bool verbose,
                string_view word, size_t edit_distance) {
  fst::Set matcher(byte_code.data(), byte_code.size());
  matcher.set_trace(verbose);

  if (matcher) {
    if (word.empty()) {
      string word;
      while (getline(cin, word)) {
        set_search_word(byte_code, cmd, verbose, matcher, word, edit_distance);
      }
    } else {
      set_search_word(byte_code, cmd, verbose, matcher, word, edit_distance);
    }
  } else {
    cerr << "invalid file..." << endl;
  }
}

template <typename T>
void search(const T &byte_code, string_view cmd, bool verbose, string_view word,
            size_t edit_distance) {
  auto type = fst::get_output_type(byte_code.data(), byte_code.size());

  if (type == fst::OutputType::uint32_t) {
    map_search<uint32_t>(byte_code, cmd, verbose, word, edit_distance);
  } else if (type == fst::OutputType::string) {
    map_search<string>(byte_code, cmd, verbose, word, edit_distance);
  } else if (type == fst::OutputType::none_t) {
    set_search(byte_code, cmd, verbose, word, edit_distance);
  }
}

template <typename output_t, typename T, typename U>
int build(istream &is, const string &format, T fn1, U fn2) {
  fst::Result result;
  size_t line;

  if (format.empty()) {
    if (std::is_same<output_t, std::string>::value) { return 1; }
    auto input = load_input(is);
    tie(result, line) = fn2(input);
  } else {
    auto input = load_input<output_t>(is, format);
    tie(result, line) = fn1(input);
  }

  if (result == fst::Result::Success) { return 0; }
  show_error_message(result, line);
  return 1;
}

void usage() {
  cout << R"(usage: fst [options] <command> [<args>]

  commends:
    compile      source FST  - make fst byte code
    decompile    FST         - decompile fst byte code

    search       FST [word]  - exact match search
    prefix       FST [word]  - common prefix search
    longest      FST [word]  - longest common prefix search
    fuzzy        FST [word]  - edit distance search
    spellcheck   FST [word]  - check spelling

    dot          source      - convert to dot format
    dump         source      - convert to byte code dump

  options:
    -f           source file format ('csv' or 'tsv')
    -t           output type ('uint32_t' or 'string')
    -v           verbose output
    -set         compile without output
    -no          decompile map without output
    -sorted      skip sorting input
    -ed number   edit distance (deault is 1)

  note:
    On macOS, you can show FST graph with `./fst dot source | dot -T png | open -a Preview.app -f`.
)";
}

int error(int code) {
  usage();
  return code;
}

int main(int argc, char **argv) {
  const flags::args args(argc, argv);

  if (args.positional().size() < 2) { return error(1); }

  auto opt_format = args.get<string>("f");
  auto opt_output_type = args.get<string>("t");
  auto opt_verbose = args.get<bool>("v", false);
  auto opt_set = args.get<bool>("set", false);
  auto opt_no = args.get<bool>("no", false);
  auto opt_sorted = args.get<bool>("sorted", false);
  auto opt_ed = args.get<size_t>("ed", 1);

  auto cmd = args.positional().at(0);
  const string in_path{args.positional().at(1)};

  string format;
  if (opt_format) {
    format = *opt_format;
  } else {
    if (ends_with(in_path, ".csv")) {
      format = "csv";
    } else if (ends_with(in_path, ".csv")) {
      format = "csv";
    }
  }

  try {
    if (cmd == "compile") {
      if (args.positional().size() < 3) { return error(1); }
      const string out_path{args.positional().at(2)};

      ifstream fin(in_path);
      if (!fin) { return error(1); }

      ofstream fout(out_path, ios_base::binary);
      if (!fout) { return error(1); }

      if (*opt_output_type == "string") {
        return build<string>(
            fin, format,
            [&](const auto &input) {
              return fst::compile<string>(input, fout, opt_sorted, opt_verbose);
            },
            [&](const auto &input) { return pair(fst::Result::Success, 0); });
      } else {
        return build<uint32_t>(
            fin, format,
            [&](const auto &input) {
              return fst::compile<uint32_t>(input, fout, opt_sorted,
                                            opt_verbose);
            },
            [&](const auto &input) {
              return fst::compile(input, fout, !opt_set, opt_sorted,
                                  opt_verbose);
            });
      }

    } else if (cmd == "dump") {
      ifstream fin(in_path);
      if (!fin) { return error(1); }

      if (*opt_output_type == "string") {
        return build<string>(
            fin, format,
            [&](const auto &input) {
              return fst::dump<string>(input, cout, opt_verbose);
            },
            [&](const auto &input) { return pair(fst::Result::Success, 0); });
      } else {
        return build<uint32_t>(
            fin, format,
            [&](const auto &input) {
              return fst::dump<uint32_t>(input, cout, opt_sorted, opt_verbose);
            },
            [&](const auto &input) {
              return fst::dump(input, cout, !opt_set, opt_sorted, opt_verbose);
            });
      }

    } else if (cmd == "dot") {
      ifstream fin(in_path);
      if (!fin) { return error(1); }

      if (*opt_output_type == "string") {
        return build<string>(
            fin, format,
            [&](const auto &input) {
              return fst::dot<string>(input, cout, opt_sorted);
            },
            [&](const auto &input) { return pair(fst::Result::Success, 0); });
      } else {
        return build<uint32_t>(
            fin, format,
            [&](const auto &input) {
              return fst::dot<uint32_t>(input, cout, opt_sorted);
            },
            [&](const auto &input) {
              return fst::dot(input, cout, !opt_set, opt_sorted);
            });
      }

    } else if (cmd == "search" || cmd == "prefix" || cmd == "longest" ||
               cmd == "fuzzy" || cmd == "spellcheck") {
      ifstream fin(in_path, ios_base::binary);
      if (!fin) { return error(1); }

      auto byte_code = load_byte_code(fin);

      std::string word;
      if (args.positional().size() > 2) { word = args.positional().at(2); }

      search(byte_code, cmd, opt_verbose, word, opt_ed);

    } else if (cmd == "decompile") {
      ifstream fin(in_path, ios_base::binary);
      if (!fin) { return error(1); }

      auto byte_code = load_byte_code(fin);

      fst::decompile(byte_code.data(), byte_code.size(), std::cout, !opt_no);

    } else if (cmd == "test") {
      ifstream fin(in_path);
      if (!fin) { return error(1); }

      stringstream ss;

      if (*opt_output_type == "string") {
        return build<string>(
            fin, format,
            [&](const auto &input) {
              auto ret =
                  fst::compile<string>(input, ss, opt_sorted, opt_verbose);
              if (ret.first == fst::Result::Success) {
                regression_test(input, ss.str(), true);
              }
              return ret;
            },
            [&](const auto &input) { return pair(fst::Result::Success, 0); });
      } else {
        return build<uint32_t>(
            fin, format,
            [&](const auto &input) {
              auto ret =
                  fst::compile<uint32_t>(input, ss, opt_sorted, opt_verbose);
              if (ret.first == fst::Result::Success) {
                regression_test(input, ss.str(), true);
              }
              return ret;
            },
            [&](const auto &input) {
              auto ret =
                  fst::compile(input, ss, !opt_set, opt_sorted, opt_verbose);
              if (ret.first == fst::Result::Success) {
                regression_test(input, ss.str(), !opt_set);
              }
              return ret;
            });
      }

    } else {
      return error(1);
    }
  } catch (const invalid_argument &) {
    cerr << "invalid format..." << endl;
    return 1;
  } catch (const runtime_error &err) { cerr << err.what() << endl; }

  return 0;
}
