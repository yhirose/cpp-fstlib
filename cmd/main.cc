
#include <fstlib.h>
#include <fstream>
#include <sstream>

using namespace std;

typedef uint32_t output_t;
// typedef string output_t;

template <typename T> struct traints {
  static T convert(uint32_t n) {}
  static T convert(const std::string &s) {}
};

template <> struct traints<uint32_t> {
  static uint32_t convert(uint32_t n) { return n; }
  static uint32_t convert(const std::string &s) { return stoi(s); }
};

template <> struct traints<string> {
  static string convert(uint32_t n) { return std::to_string(n); }
  static string convert(const std::string &s) { return s; }
};

void usage() {
  cout << R"(usage: fst <command> [<args>]

    compile     DictionaryFile FstFile  - make fst byte code
    decompile   FstFile                 - decompile fst byte code

    search      FstFile                 - exact match search
    prefix      FstFile                 - common prefix search

    dot         DictionaryFile          - convert to dot format
                On macOS: `./fst dot DictionaryFile | dot -T png | open -a Preview.app -f`
)";
}

vector<string> split(const string &input, char delimiter) {
  istringstream stream(input);
  string field;
  vector<string> result;
  while (getline(stream, field, delimiter)) {
    result.push_back(field);
  }
  return result;
}

vector<pair<string, output_t>> load_input(istream &fin, char delimiter) {
  vector<pair<string, output_t>> input;

  string line;
  while (getline(fin, line)) {
    auto fields = split(line, delimiter);
    if (fields.size() > 1) {
      input.emplace_back(fields[0], traints<output_t>::convert(fields[1]));
    } else {
      input.emplace_back(line, traints<output_t>::convert(input.size()));
    }
  }

  sort(input.begin(), input.end(),
       [](const auto &a, const auto &b) { return a.first < b.first; });

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
  std::string error_message;

  switch (result) {
  case fst::Result::EmptyKey: error_message = "empty key"; break;
  case fst::Result::UnsortedKey: error_message = "unsorted key"; break;
  case fst::Result::DuplicateKey: error_message = "duplicate key"; break;
  default: error_message = "Unknown"; break;
  }

  std::cerr << "line " << line << ": " << error_message << std::endl;
}

void regression_test(const vector<pair<string, output_t>> &input,
                     const char *out_path, bool need_output, bool allow_multi) {
  ifstream f(out_path, ios_base::binary);
  auto byte_code = load_byte_code(f);

  fst::container<output_t> cont(byte_code.data(), byte_code.size(), need_output,
                                allow_multi);

  if (cont.is_valid()) {
    for (const auto &[word, output] : input) {
      std::vector<output_t> outputs;
      auto ret = cont.query(word.data(), word.size(), [&](const auto &output) {
        outputs.push_back(output);
      });
      if (!ret) {
        std::cerr << "couldn't find '" << word << "'" << std::endl;
      } else if (need_output) {
        if (std::find(outputs.begin(), outputs.end(), output) ==
            outputs.end()) {
          for (auto &x : outputs) {
            std::cerr << x << std::endl;
          }
          std::cerr << "word: " << word << std::endl;
          std::cerr << "expected value: " << output << std::endl;
          std::cerr << "actual value: " << outputs[0] << std::endl;
        }
      }
    }
  } else {
    std::cerr << "someting is wrong in byte_code..." << std::endl;
  }
}

int main(int argc, const char **argv) {
  int argi = 1;

  if (argi >= argc) {
    usage();
    return 1;
  }

  try {
    // TODO: Support full CSV and TSV format
    char delimiter = '\t';

    string cmd = argv[argi++];

    if (cmd == "compile") {
      if (argi >= argc) {
        usage();
        return 1;
      }

      ifstream fin(argv[argi++]);
      if (!fin) {
        usage();
        return 1;
      }

      if (argi >= argc) {
        usage();
        return 1;
      }

      ofstream fout(argv[argi++], ios_base::binary);
      if (!fout) {
        usage();
        return 1;
      }

      auto byte_code = fst::build(load_input(fin, delimiter));
      fout.write(byte_code.data(), byte_code.size());

    } else if (cmd == "search" || cmd == "prefix") {
      if (argi >= argc) {
        usage();
        return 1;
      }

      ifstream fin(argv[argi++], ios_base::binary);
      if (!fin) {
        usage();
        return 1;
      }

      auto byte_code = load_byte_code(fin);

      string line;
      while (getline(cin, line)) {
        if (cmd == "search") {
          auto outputs = fst::exact_match_search<output_t>(
              byte_code.data(), byte_code.size(), line.c_str());
          for (const auto &item : outputs) {
            cout << item << endl;
          }
          if (outputs.empty()) { cout << "not found..." << endl; }
        } else { // "prefix"
          auto results = fst::common_prefix_search<output_t>(
              byte_code.data(), byte_code.size(), line.c_str());
          for (const auto &result : results) {
            cout << "length: " << result.length << endl;
            for (const auto &item : result.outputs) {
              cout << item << endl;
            }
          }
          if (results.empty()) { cout << "not found..." << endl; }
        }
      }

    } else if (cmd == "dot") {
      if (argi >= argc) {
        usage();
        return 1;
      }

      ifstream fin(argv[argi++]);
      if (!fin) {
        usage();
        return 1;
      }

      auto sm = fst::make_state_machine(load_input(fin, delimiter));
      optimize(*sm);
      fst::dot(*sm, cout);

    } else if (cmd == "dump") {
      if (argi >= argc) {
        usage();
        usage();
        return 1;
      }

      ifstream fin(argv[argi++]);
      if (!fin) {
        usage();
        return 1;
      }

      auto sm = fst::make_state_machine(load_input(fin, delimiter));
      optimize(*sm);
      fst::dump(*sm, cout);

    } else if (cmd == "decompile") {
      if (argi >= argc) {
        usage();
        return 1;
      }

      ifstream fin(argv[argi++], ios_base::binary);
      if (!fin) {
        usage();
        return 1;
      }

      auto byte_code = load_byte_code(fin);

      fst::decompile<output_t>(byte_code.data(), byte_code.size(),
                               [&](const std::string &key, output_t &output) {
                                 std::cout << key << delimiter << output
                                           << std::endl;
                               });

    } else if (cmd == "test") {
      if (argi >= argc) {
        usage();
        return 1;
      }

      ifstream fin(argv[argi++]);
      if (!fin) {
        usage();
        return 1;
      }

      cerr << "# loading dictionary..." << endl;
      auto input = load_input(fin, delimiter);

      cerr << "# making fst..." << endl;
      auto sm = fst::make_state_machine(input);
      cerr << "# state count: " << sm->count << endl;
      optimize(*sm);
      cerr << "# state count: " << sm->count << endl;

      cerr << "# compile fst..." << endl;
      auto byte_code = fst::compile(*sm);
      cerr << "# byte code size: " << byte_code.size() << endl;

      cerr << "# test all words..." << endl;
      for (const auto &[word, output] : input) {
        auto results = fst::exact_match_search<output_t>(
            byte_code.data(), byte_code.size(), word.c_str());
        if (results.empty()) { cout << word << ": NG" << endl; }
      }

    } else if (cmd == "compile2") {
      if (argi >= argc) {
        usage();
        return 1;
      }

      ifstream fin(argv[argi++]);
      if (!fin) {
        usage();
        return 1;
      }

      auto out_path = argv[argi++];
      ofstream fout(out_path, ios_base::binary);
      if (!fout) {
        usage();
        return 1;
      }

      auto need_output = true;
      if (argi < argc) { need_output = std::string(argv[argi++]) == "true"; }

      auto allow_multi = true;
      if (argi < argc) { allow_multi = std::string(argv[argi++]) == "true"; }

      auto input = load_input(fin, delimiter);

      auto [result, line] =
          fst::make_fst<output_t>(input, fout, need_output, allow_multi, true);

      fout.close();

      if (result != fst::Result::Success) {
        show_error_message(result, line);
      } else {
        regression_test(input, out_path, need_output, allow_multi);
      }

    } else if (cmd == "dump2") {
      if (argi >= argc) {
        usage();
        return 1;
      }

      ifstream fin(argv[argi++]);
      if (!fin) {
        usage();
        return 1;
      }

      auto input = load_input(fin, delimiter);

      auto [result, line] = fst::dump_fst<output_t>(input, std::cout, true);

      if (result != fst::Result::Success) { show_error_message(result, line); }

    } else if (cmd == "dot2") {
      if (argi >= argc) {
        usage();
        return 1;
      }

      ifstream fin(argv[argi++]);
      if (!fin) {
        usage();
        return 1;
      }

      auto input = load_input(fin, delimiter);

      auto [result, line] = fst::make_dot<output_t>(input, std::cout);

      if (result != fst::Result::Success) { show_error_message(result, line); }

    } else if (cmd == "search2") {
      if (argi >= argc) {
        usage();
        return 1;
      }

      ifstream fin(argv[argi++], ios_base::binary);
      if (!fin) {
        usage();
        return 1;
      }

      auto need_output = true;
      if (argi < argc) { need_output = std::string(argv[argi++]) == "true"; }

      auto allow_multi = true;
      if (argi < argc) { allow_multi = std::string(argv[argi++]) == "true"; }

      auto trace = false;
      if (argi < argc) { trace = std::string(argv[argi++]) == "true"; }

      auto byte_code = load_byte_code(fin);

      fst::container<output_t> cont(byte_code.data(), byte_code.size(),
                                    need_output, allow_multi);
      cont.set_trace(trace);

      if (cont.is_valid()) {
        string line;
        while (getline(cin, line)) {
          std::vector<output_t> outputs;
          std::vector<output_t> prefixes;
          auto ret = cont.query(
              line.data(), line.size(),
              [&](const auto &output) {
                std::cout << "output: " << output << endl;
              },
              [&](size_t len, const auto &output) {
                std::cout << "prefix: " << output << " (" << line.substr(0, len)
                          << ")" << endl;
              });
          if (!ret) { std::cout << "not found..." << endl; }
        }
      } else {
        std::cout << "invalid file..." << std::endl;
      }

    } else {
      usage();
      return 1;
    }
  } catch (const runtime_error &err) { cerr << err.what() << endl; }

  return 0;
}
