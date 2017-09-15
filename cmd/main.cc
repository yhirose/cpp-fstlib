
#include <fstlib.h>
#include <fstream>
#include <sstream>

using namespace std;

typedef uint32_t output_t;
//typedef string output_t;

template <typename T>
struct traints {
  static T convert(uint32_t n) {}
  static T convert(const std::string& s) {}
};

template <>
struct traints<uint32_t> {
  static uint32_t convert(uint32_t n) {
    return n;
  }
  static uint32_t convert(const std::string& s) {
    return stoi(s);
  }
};

template <>
struct traints<string> {
  static string convert(uint32_t n) {
    return std::to_string(n);
  }
  static string convert(const std::string& s) {
    return s;
  }
};

void usage() {
  cout << R"(usage: fst <command> [<args>]

    compile     DictionaryFile FstFile     - make fst byte code

    search      FstFile                    - exact match search
    prefix      FstFile                    - common prefix search

    dot         DictionaryFile             - convert to dot format
)";
}

// TODO: Support full CSV and TSV format
vector<string> split(const string& input, char delimiter) {
  istringstream stream(input);
  string field;
  vector<string> result;
  while (getline(stream, field, delimiter)) {
    result.push_back(field);
  }
  return result;
}

vector<pair<string, output_t>> load_input(istream& fin) {
  vector<pair<string, output_t>> input;

  string line;
  while (getline(fin, line)) {
    auto fields = split(line, ',');
    if (fields.size() > 1) {
      input.emplace_back(fields[0], traints<output_t>::convert(fields[1]));
    } else {
      input.emplace_back(line, traints<output_t>::convert(input.size()));
    }
  }

  sort(input.begin(), input.end(),
       [](const auto& a, const auto& b) { return a.first < b.first; });

  return input;
}

vector<char> load_byte_code(istream& is) {
  is.seekg(0, ios_base::end);
  auto size = (unsigned int)is.tellg();
  is.seekg(0, ios_base::beg);
  vector<char> byte_code(size);
  is.read(byte_code.data(), size);
  return byte_code;
}

int main(int argc, const char** argv) {
  int argi = 1;

  if (argi >= argc) {
    usage();
    return 1;
  }

  try {
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

      auto byte_code = fst::build(load_input(fin));
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

      fin.seekg(0, ios_base::end);
      auto size = (unsigned int)fin.tellg();
      fin.seekg(0, ios_base::beg);
      vector<char> byte_code(size);
      fin.read(byte_code.data(), size);

      string line;
      while (getline(cin, line)) {
        if (cmd == "search") {
          auto outputs = fst::exact_match_search<output_t>(
              byte_code.data(), byte_code.size(), line.c_str());
          for (const auto& item : outputs) {
            cout << item << endl;
          }
          if (outputs.empty()) {
            cout << "not found..." << endl;
          }
        } else {  // "prefix"
          auto results = fst::common_prefix_search<output_t>(
              byte_code.data(), byte_code.size(), line.c_str());
          for (const auto& result : results) {
            cout << "length: " << result.length << endl;
            for (const auto& item : result.outputs) {
              cout << item << endl;
            }
          }
          if (results.empty()) {
            cout << "not found..." << endl;
          }
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

      auto sm = fst::make_state_machine(load_input(fin));
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

      auto sm = fst::make_state_machine(load_input(fin));
      fst::print(*sm, cout);

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
      auto input = load_input(fin);

      cerr << "# making fst..." << endl;
      auto sm = fst::make_state_machine(input);
      cerr << "# state count: " << sm->count << endl;

      cerr << "# compile fst..." << endl;
      auto byte_code = fst::compile(*sm);
      cerr << "# byte code size: " << byte_code.size() << endl;

      cerr << "# test all words..." << endl;
      for (const auto& item : input) {
        auto results = fst::exact_match_search<output_t>(
            byte_code.data(), byte_code.size(), item.first.c_str());
        if (results.empty()) {
          cout << item.first << ": NG" << endl;
        }
      }
    } else {
      usage();
      return 1;
    }
  } catch (const runtime_error& err) {
    cerr << err.what() << endl;
  }

  return 0;
}
