
#include "darts/darts.h"
#include "fstlib.h"
#include "marisa.h"
#include "ux-trie/ux.hpp"
#include <chrono>
#include <fstream>

using namespace std;

bool read_file(const char *path, vector<char> &buff) {
  ifstream ifs(path, ios::in | ios::binary);
  if (ifs.fail()) { return false; }

  buff.resize(static_cast<unsigned int>(ifs.seekg(0, ios::end).tellg()));
  if (!buff.empty()) {
    ifs.seekg(0, ios::beg).read(&buff[0], static_cast<streamsize>(buff.size()));
  }
  return true;
}

vector<pair<string, uint32_t>> load_input(istream &in) {
  vector<pair<string, uint32_t>> input;
  string word;

  while (getline(in, word)) {
    input.emplace_back(word, static_cast<uint32_t>(input.size()));
  }

  sort(input.begin(), input.end(),
       [](const auto &a, const auto &b) { return a.first < b.first; });

  return input;
}

struct StopWatch {
  StopWatch(const char *label) : label_(label) {
    start_ = chrono::system_clock::now();
  }
  ~StopWatch() {
    auto end = chrono::system_clock::now();
    auto diff = end - start_;
    auto count = chrono::duration_cast<chrono::milliseconds>(diff).count();
    cerr << label_ << "\t" << count << " millisec." << endl;
  }
  const char *label_;
  chrono::system_clock::time_point start_;
};

size_t file_size(const char *path) {
  ifstream fin(path, ios_base::binary);
  fin.seekg(0, ios_base::end);
  return fin.tellg();
}

int main(int argc, const char **argv) {
  if (argc < 2) {
    cerr << "usage: benchmark <dictionary file>" << endl;
    return 1;
  }

  ifstream fin(argv[1]);
  if (!fin) {
    cerr << "file is not found." << endl;
    return 1;
  }

  auto input = load_input(fin);

  vector<const char *> keys;
  vector<size_t> lengths;
  for (const auto &item : input) {
    keys.push_back(item.first.c_str());
    lengths.push_back(item.first.length());
  }

  cerr << keys.size() << " keys" << endl;
  cerr << endl;

  bool darts = false;
  bool ux = false;
  bool marisa = false;
  bool fstlib = true;
  bool fstlib_only_keys = true;

  if (argc > 2 && string(argv[2]) == "-a") {
    darts = true;
    ux = true;
    marisa = true;
  }

  int count = 5;

  bool build = true;
  bool exact = true;
  bool common_prefix = true;

  size_t dummy = 0;

  // Darts
  if (darts) {
    cerr << "#### darts (double array) ####" << endl;
    const char *PATH = "darts.bin";

    if (build) {
      Darts::DoubleArray da;

      {
        StopWatch sw("build");
        da.build(keys.size(), &keys[0], &lengths[0], 0, nullptr);
        da.save(PATH);
        fprintf(stdout, "size\t%0.1f mega bytes (%d bytes)\n",
                (double)(file_size(PATH) * 100 / 1024 / 1024) / 100.0,
                (int)file_size(PATH));
      }
    }

    {
      Darts::DoubleArray da;
      da.open(PATH);

      if (exact) {
        StopWatch sw("exact");
        for (int i = 0; i < count; i++) {
          for (auto key : keys) {
            dummy += da.exactMatchSearch<int>(key);
          }
        }
      }

      if (common_prefix) {
        StopWatch sw("prefix");
        int result[BUFSIZ];
        for (int i = 0; i < count; i++) {
          for (auto key : keys) {
            dummy += da.commonPrefixSearch<int>(key, result, BUFSIZ);
          }
        }
      }
    }

    cerr << endl;
  }

  // Ux-trie
  if (ux) {
    cerr << "#### ux (louds) ####" << endl;
    const char *PATH = "ux.bin";

    vector<string> keys;
    for (const auto &item : input) {
      keys.push_back(item.first);
    }

    if (build) {
      ux::Trie ux;

      {
        StopWatch sw("build");
        ux.build(keys);
        ux.save(PATH);
        fprintf(stdout, "size\t%0.1f mega bytes (%d bytes)\n",
                (double)(file_size(PATH) * 100 / 1024 / 1024) / 100.0,
                (int)file_size(PATH));
      }
    }

    {
      ux::Trie ux;
      ux.load(PATH);

      if (exact) {
        StopWatch sw("exact");
        size_t retLen;
        for (int i = 0; i < count; i++) {
          for (auto key : keys) {
            dummy += ux.prefixSearch(key.c_str(), key.length(), retLen);
          }
        }
      }

      if (common_prefix) {
        StopWatch sw("prefix");
        vector<ux::id_t> retIDs;
        for (int i = 0; i < count; i++) {
          for (auto key : keys) {
            retIDs.clear();
            dummy += ux.commonPrefixSearch(key.c_str(), key.length(), retIDs,
                                           (size_t)-1);
          }
        }
      }
    }

    cerr << endl;
  }

  // Marisa-trie
  if (marisa) {
    marisa::Keyset keyset;
    for (const auto &item : input) {
      keyset.push_back(item.first.c_str());
    }

    cerr << "#### marisa (louds) ####" << endl;
    const char *PATH = "marisa.bin";

    if (build) {
      marisa::Trie ma;

      {
        StopWatch sw("build");
        ma.build(keyset);
        ma.save(PATH);
        fprintf(stdout, "size\t%0.1f mega bytes (%d bytes)\n",
                (double)(file_size(PATH) * 100 / 1024 / 1024) / 100.0,
                (int)file_size(PATH));
      }
    }

    {
      marisa::Trie ma;
      ma.load(PATH);

      if (exact) {
        StopWatch sw("exact");
        marisa::Agent agent;
        for (int i = 0; i < count; i++) {
          for (auto key : keys) {
            agent.set_query(key);
            dummy += ma.lookup(agent);
          }
        }
      }

      if (common_prefix) {
        StopWatch sw("prefix");
        marisa::Agent agent;
        for (int i = 0; i < count; i++) {
          for (auto key : keys) {
            agent.set_query(key);
            while (ma.common_prefix_search(agent)) {
              ;
            }
          }
        }
      }
    }

    cerr << endl;
  }

  // fstlib
  if (fstlib) {
    cerr << "#### fstlib ####" << endl;
    const char *PATH = "fstlib.bin";

    if (build) {
      StopWatch sw("build");
      ofstream fout(PATH, ios_base::binary);
      auto [result, line] = fst::compile<uint32_t>(input, fout, true, true);
      fout.close();

      fprintf(stdout, "size\t%0.1f mega bytes (%d bytes)\n",
              (double)(file_size(PATH) * 100 / 1024 / 1024) / 100.0,
              (int)file_size(PATH));
    }

    {
      vector<char> byte_code;
      read_file(PATH, byte_code);

      if (exact) {
        StopWatch sw("exact");

        fst::Map<uint32_t> matcher(byte_code.data(), byte_code.size());

        if (matcher) {
          for (int i = 0; i < count; i++) {
            for (int j = 0; j < keys.size(); j++) {
              uint32_t output = 0;
              auto ret =
                  matcher.exact_match_search(keys[j], lengths[j], output);
              if (!ret) { cerr << "error: (" << keys[j] << ")" << endl; }
            }
          }
        }
      }

      if (common_prefix) {
        StopWatch sw("prefix");

        fst::Map<uint32_t> matcher(byte_code.data(), byte_code.size());

        if (matcher) {
          for (int i = 0; i < count; i++) {
            for (int j = 0; j < keys.size(); j++) {
              auto ret = matcher.common_prefix_search(
                  keys[j], lengths[j], [&](size_t, const auto &) {});
              if (!ret) { cerr << "error: (" << keys[j] << ")" << endl; }
            }
          }
        }
      }
    }

    cerr << endl;
  }

  // fstlib with only keys
  if (fstlib_only_keys) {
    cerr << "#### fstlib (only keys) ####" << endl;
    const char *PATH = "fstlib_only_keys.bin";

    if (build) {
      vector<string> keys2;
      for (const auto &item : input) {
        keys2.push_back(item.first);
      }

      StopWatch sw("build");
      ofstream fout(PATH, ios_base::binary);
      auto [result, line] = fst::compile(keys2, fout, true, true);
      fout.close();

      fprintf(stdout, "size\t%0.1f mega bytes (%d bytes)\n",
              (double)(file_size(PATH) * 100 / 1024 / 1024) / 100.0,
              (int)file_size(PATH));
    }

    {
      vector<char> byte_code;
      read_file(PATH, byte_code);

      if (exact) {
        StopWatch sw("exact");

        fst::Map<uint32_t> matcher(byte_code.data(), byte_code.size());

        if (matcher) {
          for (int i = 0; i < count; i++) {
            for (int j = 0; j < keys.size(); j++) {
              uint32_t output = 0;
              auto ret =
                  matcher.exact_match_search(keys[j], lengths[j], output);
              if (!ret) { cerr << "error: (" << keys[j] << ")" << endl; }
            }
          }
        }
      }

      if (common_prefix) {
        StopWatch sw("prefix");

        fst::Map<uint32_t> matcher(byte_code.data(), byte_code.size());

        if (matcher) {
          for (int i = 0; i < count; i++) {
            for (int j = 0; j < keys.size(); j++) {
              auto ret = matcher.common_prefix_search(
                  keys[j], lengths[j], [&](size_t, const auto &) {});
              if (!ret) { cerr << "error: (" << keys[j] << ")" << endl; }
            }
          }
        }
      }
    }

    cerr << endl;
  }

  cerr << (dummy ? " \n" : "  \n") << endl;

  return 0;
}
