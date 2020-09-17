//
//  fstlib.h
//
//  Copyright (c) 2020 Yuji Hirose. All rights reserved.
//  MIT License
//

#ifndef CPPFSTLIB_FSTLIB_H_
#define CPPFSTLIB_FSTLIB_H_

#include <algorithm>
#include <any>
#include <cassert>
#include <cstring>
#include <functional>
#include <iostream>
#include <list>
#include <memory>
#include <numeric>
#include <queue>
#include <set>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

namespace fst {

//-----------------------------------------------------------------------------
// variable byte encoding
//-----------------------------------------------------------------------------

template <typename Val> inline size_t vb_encode_value_length(Val n) {
  size_t len = 0;
  while (n >= 128) {
    len++;
    n >>= 7;
  }
  len++;
  return len;
}

template <typename Val> inline size_t vb_encode_value(Val n, char *out) {
  size_t len = 0;
  while (n >= 128) {
    out[len] = (char)(n & 0x7f);
    len++;
    n >>= 7;
  }
  out[len] = (char)(n + 128);
  len++;
  return len;
}

template <typename Val, typename Cont> void vb_encode_value(Val n, Cont &out) {
  while (n >= 128) {
    out.push_back((typename Cont::value_type)(n & 0x7f));
    n >>= 7;
  }
  out.push_back((typename Cont::value_type)(n + 128));
}

template <typename Val>
inline size_t vb_encode_value_reverse(Val n, char *out) {
  auto len = vb_encode_value(n, out);
  for (size_t i = 0; i < len / 2; i++) {
    std::swap(out[i], out[len - i - 1]);
  }
  return len;
}

template <typename Val>
inline size_t vb_encode_value_reverse(Val n, std::ostream &os) {
  char buf[16];
  auto len = vb_encode_value_reverse(n, buf);
  os.write(buf, len);
  return len;
}

template <typename Val>
inline size_t vb_decode_value_reverse(const char *data, Val &n) {
  auto p = (const uint8_t *)data;
  int i = 0;
  n = 0;
  size_t cnt = 0;
  while (p[i] < 128) {
    n += (p[i--] << (7 * cnt++));
  }
  n += (p[i--] - 128) << (7 * cnt);
  return i * -1;
}

//-----------------------------------------------------------------------------
// lower_bound_index
//-----------------------------------------------------------------------------

template <class T>
inline size_t lower_bound_index(size_t first, size_t last, T less) {
  size_t len = last - first;
  size_t half;
  size_t middle;

  while (len > 0) {
    half = len >> 1;
    middle = first + half;

    if (less(middle)) {
      first = middle;
      first++;
      len = len - half - 1;
    } else {
      len = half;
    }
  }

  return first;
}

//-----------------------------------------------------------------------------
// MurmurHash64B - 64-bit MurmurHash2 for 32-bit platforms
//
// URL:: https://github.com/aappleby/smhasher/blob/master/src/MurmurHash2.cpp
// License: Public Domain
//-----------------------------------------------------------------------------

inline uint64_t MurmurHash64B(const void *key, size_t len, uint64_t seed) {
  const uint32_t m = 0x5bd1e995;
  const size_t r = 24;

  uint32_t h1 = uint32_t(seed) ^ uint32_t(len);
  uint32_t h2 = uint32_t(seed >> 32);

  const uint32_t *data = (const uint32_t *)key;

  while (len >= 8) {
    uint32_t k1 = *data++;
    k1 *= m;
    k1 ^= k1 >> r;
    k1 *= m;
    h1 *= m;
    h1 ^= k1;
    len -= 4;

    uint32_t k2 = *data++;
    k2 *= m;
    k2 ^= k2 >> r;
    k2 *= m;
    h2 *= m;
    h2 ^= k2;
    len -= 4;
  }

  if (len >= 4) {
    uint32_t k1 = *data++;
    k1 *= m;
    k1 ^= k1 >> r;
    k1 *= m;
    h1 *= m;
    h1 ^= k1;
    len -= 4;
  }

  switch (len) {
  case 3: h2 ^= ((unsigned char *)data)[2] << 16;
  case 2: h2 ^= ((unsigned char *)data)[1] << 8;
  case 1: h2 ^= ((unsigned char *)data)[0]; h2 *= m;
  };

  h1 ^= h2 >> 18;
  h1 *= m;
  h2 ^= h1 >> 22;
  h2 *= m;
  h1 ^= h2 >> 17;
  h1 *= m;
  h2 ^= h1 >> 19;
  h2 *= m;

  uint64_t h = h1;

  h = (h << 32) | h2;

  return h;
}

//-----------------------------------------------------------------------------
// get_prefix_length
//-----------------------------------------------------------------------------

inline size_t get_prefix_length(const std::string &s1, const std::string &s2) {
  size_t i = 0;
  while (i < s1.size() && i < s2.size() && s1[i] == s2[i]) {
    i++;
  }
  return i;
}

inline bool get_prefix_length(const std::string &s1, const std::string &s2,
                              size_t &l) {
  l = 0;
  while (l < s1.size() && l < s2.size()) {
    auto ch1 = static_cast<uint8_t>(s1[l]);
    auto ch2 = static_cast<uint8_t>(s2[l]);
    if (ch1 < ch2) { break; }
    if (ch1 > ch2) { return false; }
    l++;
  }
  return true;
}

//-----------------------------------------------------------------------------
// OutputTraits
//-----------------------------------------------------------------------------

enum class OutputType { invalid = -1, uint32_t, string };

template <typename output_t> struct OutputTraits {};

template <> struct OutputTraits<uint32_t> {
  using value_type = uint32_t;

  static OutputType type() { return OutputType::uint32_t; }

  static value_type initial_value() { return 0; }

  static bool empty(value_type val) { return val == 0; }

  static void clear(value_type &val) { val = 0; }

  static std::string to_string(value_type val) { return std::to_string(val); }

  static void prepend_value(value_type &base, value_type val) { base += val; }

  static value_type get_suffix(value_type a, value_type b) { return a - b; }

  static value_type get_common_prefix(value_type a, value_type b) {
    return std::min(a, b);
  }

  static size_t write_value(char *buff, size_t buff_len, value_type val) {
    memcpy(&buff[buff_len], &val, sizeof(val));
    return sizeof(val);
  }

  static size_t get_byte_value_size(value_type val) {
    return vb_encode_value_length(val);
  }

  static void write_byte_value(std::ostream &os, value_type val) {
    vb_encode_value_reverse(val, os);
  }

  static size_t read_byte_value(const char *p, value_type &val) {
    return vb_decode_value_reverse(p, val);
  }
};

template <> struct OutputTraits<std::string> {
  using value_type = std::string;

  static OutputType type() { return OutputType::string; }

  static value_type initial_value() { return value_type(); }

  static bool empty(const value_type &val) { return val.empty(); }

  static void clear(value_type &val) { val.clear(); }

  static value_type to_string(const value_type &val) { return val; }

  static void prepend_value(value_type &base, const value_type &val) {
    base.insert(0, val);
  }

  static value_type get_suffix(const value_type &a, const value_type &b) {
    return a.substr(b.size());
  }

  static value_type get_common_prefix(const value_type &a,
                                      const value_type &b) {
    return a.substr(0, get_prefix_length(a, b));
  }

  static size_t write_value(char *buff, size_t buff_len,
                            const value_type &val) {
    memcpy(&buff[buff_len], val.data(), val.size());
    return val.size();
  }

  static size_t get_byte_value_size(const value_type &val) {
    return vb_encode_value_length(val.size()) + val.size();
  }

  static void write_byte_value(std::ostream &os, const value_type &val) {
    os.write(val.data(), val.size());
    OutputTraits<uint32_t>::write_byte_value(os,
                                             static_cast<uint32_t>(val.size()));
  }

  static size_t read_byte_value(const char *p, value_type &val) {
    uint32_t str_len = 0;
    auto vb_len = OutputTraits<uint32_t>::read_byte_value(p, str_len);

    val.resize(str_len);
    memcpy(val.data(), p - vb_len - str_len + 1, str_len);

    return vb_len + str_len;
  }
};

//-----------------------------------------------------------------------------
// State
//-----------------------------------------------------------------------------

template <typename output_t> class State {
public:
  struct Transition {
    size_t id;
    bool final;
    output_t state_output;
    output_t output;

    bool operator==(const Transition &rhs) const {
      if (this != &rhs) {
        return id == rhs.id && final == rhs.final &&
               state_output == rhs.state_output && output == rhs.output;
      }
      return true;
    }
  };

  class Transitions {
  public:
    std::vector<char> arcs;
    std::vector<Transition> states_and_outputs;

    bool operator==(const Transitions &rhs) const {
      if (this != &rhs) {
        return arcs == rhs.arcs && states_and_outputs == rhs.states_and_outputs;
      }
      return true;
    }

    size_t size() const { return arcs.size(); }

    bool empty() const { return !size(); }

    const output_t &output(char arc) const {
      auto idx = get_index(arc);
      assert(idx != -1);
      return states_and_outputs[idx].output;
    }

    template <typename Functor> void for_each(Functor fn) const {
      for (auto i = 0u; i < arcs.size(); i++) {
        fn(arcs[i], states_and_outputs[i], i);
      }
    }

    template <typename Functor> void for_each_reverse(Functor fn) const {
      for (auto i = arcs.size(); i > 0; i--) {
        auto idx = i - 1;
        fn(arcs[idx], states_and_outputs[idx], idx);
      }
    }

    template <typename Functor> void for_each_arc(Functor fn) const {
      for (auto arc : arcs) {
        fn(arc);
      }
    }

  private:
    void clear() {
      arcs.clear();
      states_and_outputs.clear();
    }

    void set_transition(char arc, State<output_t> *state) {
      auto idx = get_index(arc);
      if (idx == -1) {
        idx = static_cast<int>(arcs.size());
        arcs.push_back(arc);
        states_and_outputs.emplace_back(Transition());
      }
      states_and_outputs[idx].id = state->id;
      states_and_outputs[idx].final = state->final;
      states_and_outputs[idx].state_output = state->state_output;
    }

    void set_output(char arc, const output_t &val) {
      auto idx = get_index(arc);
      states_and_outputs[idx].output = val;
    }

    void insert_output(char arc, const output_t &val) {
      auto idx = get_index(arc);
      auto &output = states_and_outputs[idx].output;
      OutputTraits<output_t>::prepend_value(output, val);
    }

    int get_index(char arc) const {
      for (size_t i = 0; i < arcs.size(); i++) {
        if (arcs[i] == arc) { return static_cast<int>(i); }
      }
      return -1;
    }

    friend class State;
  };

  State(size_t id) : id(id) {}

  const output_t &output(char arc) const { return transitions.output(arc); }

  bool operator==(const State &rhs) const {
    if (this != &rhs) {
      return final == rhs.final && transitions == rhs.transitions &&
             state_output == rhs.state_output;
    }
    return true;
  }

  uint64_t hash() const;

  void set_final(bool final) { this->final = final; }

  void set_transition(char arc, State<output_t> *state) {
    transitions.set_transition(arc, state);
  }

  void set_output(char arc, const output_t &output) {
    transitions.set_output(arc, output);
  }

  void prepend_suffix_to_output(char arc, const output_t &suffix) {
    transitions.insert_output(arc, suffix);
  }

  void push_to_state_outputs(const output_t &output) { state_output = output; }

  void prepend_suffix_to_state_outputs(const output_t &suffix) {
    OutputTraits<output_t>::prepend_value(state_output, suffix);
  }

  void reuse(size_t state_id) {
    id = state_id;
    set_final(false);
    transitions.clear();
    OutputTraits<output_t>::clear(state_output);
  }

  size_t id = -1;
  bool final = false;
  Transitions transitions;
  output_t state_output = OutputTraits<output_t>::initial_value();

private:
  State(const State &) = delete;
  State(State &&) = delete;
};

template <typename output_t> inline uint64_t State<output_t>::hash() const {
  char buff[1024];
  size_t buff_len = 0;

  transitions.for_each([&](char arc, const State::Transition &t, size_t i) {
    buff[buff_len++] = arc;

    auto val = static_cast<uint32_t>(t.id);
    memcpy(&buff[buff_len], &val, sizeof(val));
    buff_len += sizeof(val);

    if (!OutputTraits<output_t>::empty(t.output)) {
      buff_len += OutputTraits<output_t>::write_value(buff, buff_len, t.output);
    }
  });

  if (final && !OutputTraits<output_t>::empty(state_output)) {
    buff_len +=
        OutputTraits<output_t>::write_value(buff, buff_len, state_output);
  }

  return MurmurHash64B(buff, buff_len, 0);
}

//-----------------------------------------------------------------------------
// StatePool
//-----------------------------------------------------------------------------

template <typename output_t> class StatePool {
public:
  ~StatePool() {
    for (auto p : object_pool_) {
      delete p;
    }
  }

  State<output_t> *New(size_t state_id = -1) {
    auto p = new State<output_t>(state_id);
    object_pool_.insert(p);
    return p;
  }

  void Delete(State<output_t> *p) {
    object_pool_.erase(p);
    delete p;
  }

private:
  std::unordered_set<State<output_t> *> object_pool_;
};

//-----------------------------------------------------------------------------
// Dictionary
//-----------------------------------------------------------------------------

template <typename output_t> class Dictionary {
public:
  Dictionary(StatePool<output_t> &state_pool, bool trie)
      : state_pool_(state_pool), trie_(trie) {}

  State<output_t> *get(uint64_t key, State<output_t> *state) {
    if (trie_) { return nullptr; }

    auto id = bucket_id(key);
    auto [first, second, third] = buckets_[id];
    if (first && *first == *state) { return first; }
    if (second && *second == *state) {
      buckets_[id] = std::make_tuple(second, first, third);
      return second;
    }
    if (third && *third == *state) {
      buckets_[id] = std::make_tuple(third, first, second);
      return third;
    }
    return nullptr;
  }

  void put(uint64_t key, State<output_t> *state) {
    auto id = bucket_id(key);
    auto [first, second, third] = buckets_[id];
    if (third) { state_pool_.Delete(third); }
    buckets_[id] = std::make_tuple(state, first, second);
  }

private:
  StatePool<output_t> &state_pool_;
  bool trie_;

  static const size_t kBucketCount = 10000;
  size_t bucket_id(uint64_t key) const { return key % kBucketCount; }
  std::tuple<State<output_t> *, State<output_t> *, State<output_t> *>
      buckets_[kBucketCount] = {{nullptr, nullptr, nullptr}};
};

//-----------------------------------------------------------------------------
// find_minimized
//-----------------------------------------------------------------------------

template <typename output_t>
inline std::pair<bool, State<output_t> *>
find_minimized(State<output_t> *state, Dictionary<output_t> &dictionary) {
  auto h = state->hash();

  auto st = dictionary.get(h, state);
  if (st) { return std::make_pair(true, st); }

  dictionary.put(h, state);
  return std::make_pair(false, state);
};

//-----------------------------------------------------------------------------
// get_common_prefix_and_word_suffix
//-----------------------------------------------------------------------------

template <typename output_t>
inline void get_common_prefix_and_word_suffix(const output_t &current_output,
                                              const output_t &output,
                                              output_t &common_prefix,
                                              output_t &word_suffix) {
  common_prefix =
      OutputTraits<output_t>::get_common_prefix(output, current_output);
  word_suffix = OutputTraits<output_t>::get_suffix(output, common_prefix);
}

//-----------------------------------------------------------------------------
// build_fst_core
//-----------------------------------------------------------------------------

enum class Result { Success, EmptyKey, UnsortedKey, DuplicateKey };

template <typename output_t, typename Input, typename Writer>
inline std::pair<Result, size_t>
build_fst_core(const Input &input, Writer &writer, bool trie) {

  StatePool<output_t> state_pool;

  Dictionary<output_t> dictionary(state_pool, trie);
  size_t next_state_id = 0;
  size_t line = 1;
  Result result = Result::Success;

  // Main algorithm ported from the technical paper
  std::vector<State<output_t> *> temp_states;
  std::string previous_word;
  temp_states.push_back(state_pool.New(next_state_id++));

  input([&](const auto &current_word, const auto &_current_output) {
    auto current_output = _current_output;

    if (current_word.empty()) {
      result = Result::EmptyKey;
      return false;
    }

    // The following loop caluculates the length of the longest common
    // prefix of 'current_word' and 'previous_word'
    // auto prefix_length = get_prefix_length(previous_word, current_word);
    size_t prefix_length;
    if (!get_prefix_length(previous_word, current_word, prefix_length)) {
      result = Result::UnsortedKey;
      return false;
    }

    if (previous_word.size() == current_word.size() &&
        previous_word == current_word) {
      result = Result::DuplicateKey;
      return false;
    }

    // We minimize the states from the suffix of the previous word
    for (auto i = previous_word.size(); i > prefix_length; i--) {
      auto [found, state] =
          find_minimized<output_t>(temp_states[i], dictionary);

      if (found) {
        next_state_id--;
      } else {
        writer.write(*state);

        // Ownership of the object in temp_states[i] has been moved to the
        // dictionary...
        temp_states[i] = state_pool.New();
      }

      auto arc = previous_word[i - 1];
      temp_states[i - 1]->set_transition(arc, state);
    }

    // This loop initializes the tail states for the current word
    for (auto i = prefix_length + 1; i <= current_word.size(); i++) {
      assert(i <= temp_states.size());
      if (i == temp_states.size()) {
        temp_states.push_back(state_pool.New(next_state_id++));
      } else {
        temp_states[i]->reuse(next_state_id++);
      }
      auto arc = current_word[i - 1];
      temp_states[i - 1]->set_transition(arc, temp_states[i]);
    }

    if (current_word != previous_word) {
      auto state = temp_states[current_word.size()];
      state->set_final(true);
    }

    for (auto j = 1u; j <= prefix_length; j++) {
      auto prev_state = temp_states[j - 1];
      auto arc = current_word[j - 1];

      const auto &output = prev_state->output(arc);

      auto common_prefix = OutputTraits<output_t>::initial_value();
      auto word_suffix = OutputTraits<output_t>::initial_value();
      get_common_prefix_and_word_suffix(current_output, output, common_prefix,
                                        word_suffix);

      prev_state->set_output(arc, common_prefix);

      if (!OutputTraits<output_t>::empty(word_suffix)) {
        auto state = temp_states[j];
        state->transitions.for_each_arc([&](char arc) {
          state->prepend_suffix_to_output(arc, word_suffix);
        });
        if (state->final) {
          state->prepend_suffix_to_state_outputs(word_suffix);
        }
      }

      current_output =
          OutputTraits<output_t>::get_suffix(current_output, common_prefix);
    }

    if (current_word == previous_word) {
      auto state = temp_states[current_word.size()];
      state->push_to_state_outputs(current_output);
    } else {
      auto state = temp_states[prefix_length];
      auto arc = current_word[prefix_length];
      state->set_output(arc, current_output);
    }

    previous_word = current_word;
    line++;

    return true;
  });

  if (result != Result::Success) { return std::make_pair(result, line); }

  // Here we are minimizing the states of the last word
  for (auto i = static_cast<int>(previous_word.size()); i >= 0; i--) {
    auto [found, state] = find_minimized<output_t>(temp_states[i], dictionary);

    if (found) {
      next_state_id--;
    } else {
      writer.write(*state);
    }

    if (i > 0) {
      auto arc = previous_word[i - 1];
      temp_states[i - 1]->set_transition(arc, state);
    }
  }

  return std::make_pair(Result::Success, line);
}

//-----------------------------------------------------------------------------
// build_fst
//-----------------------------------------------------------------------------

template <typename output_t, typename Input, typename Writer>
inline std::pair<Result, size_t> build_fst(const Input &input, Writer &writer,
                                           bool trie = false) {
  return build_fst_core<output_t>(
      [&](const auto &feeder) {
        for (const auto &item : input) {
          const auto &word = item.first;
          const auto &output = item.second;
          if (!feeder(word, output)) { break; }
        }
      },
      writer, trie);
}

template <typename Input, typename Writer>
inline std::pair<Result, size_t> build_fst(const Input &input, Writer &writer,
                                           bool trie = false) {
  uint32_t id = 0;
  return build_fst_core<uint32_t>(
      [&](const auto &feeder) {
        for (const auto &word : input) {
          if (!feeder(word, id++)) { break; }
        }
      },
      writer, trie);
}

//-----------------------------------------------------------------------------
// compile
//-----------------------------------------------------------------------------

union FstFlags {
  struct {
    unsigned no_address : 1;
    unsigned last_transition : 1;
    unsigned final : 1;
    unsigned has_output : 1;
    unsigned has_state_output : 1;
    unsigned label_index : 3;
  } data;

  struct {
    unsigned no_address : 1;
    unsigned last_transition : 1;
    unsigned final : 1;
    unsigned has_output : 1;
    unsigned label_index : 4;
  } data_witout_state_output;

  uint8_t byte;

  // For char index
  static size_t char_index_size(bool need_state_output) {
    return need_state_output ? 8 : 16;
  }

  // For jump table
  bool is_jump_tag_byte() const { return byte == 0xff || byte == 0xfe; }

  size_t jump_table_element_size() const {
    return byte == 0xff ? 2 : (byte == 0xfe ? 1 : 0);
  }

  static uint8_t jump_tag_byte(bool need_two_bytes) {
    return need_two_bytes ? 0xff : 0xfe;
  }
};

struct FstHeader {
  uint8_t version = 0;
  uint8_t value_type = 0;
  uint8_t need_state_output = 0;
  uint8_t reserved = 0;
  uint32_t start_address = 0;
  char char_index[16] = {0};

  FstHeader() {}

  FstHeader(OutputType value_type, bool need_state_output, size_t start_address,
            const std::vector<size_t> &char_index_table)
      : value_type(static_cast<uint8_t>(value_type)),
        need_state_output(need_state_output),
        start_address(static_cast<uint32_t>(start_address)) {

    size_t char_index_size = FstFlags::char_index_size(need_state_output);

    for (size_t ch = 0; ch < 256; ch++) {
      auto index = char_index_table[ch];
      if (0 < index && index < char_index_size) {
        char_index[index] = static_cast<char>(ch);
      }
    }
  }

  void write(std::ostream &os) {
    os.write(reinterpret_cast<const char *>(this), sizeof(*this));
  }
};

template <typename output_t, bool need_state_output> struct FstRecord {
  FstFlags flags;

  char label = 0;
  size_t delta = 0;
  const output_t *output = nullptr;
  const output_t *state_output = nullptr;

  size_t byte_size() const {
    size_t sz = 1;
    if (need_state_output) {
      if (flags.data.label_index == 0) { sz += 1; }
    } else {
      if (flags.data_witout_state_output.label_index == 0) { sz += 1; }
    }
    if (!flags.data.no_address) { sz += vb_encode_value_length(delta); }
    if (flags.data.has_output) {
      sz += OutputTraits<output_t>::get_byte_value_size(*output);
    }
    if (need_state_output) {
      if (flags.data.has_state_output) {
        sz += OutputTraits<output_t>::get_byte_value_size(*state_output);
      }
    }
    return sz;
  }

  void write(std::ostream &os) {
    if (need_state_output) {
      if (flags.data.has_state_output) {
        OutputTraits<output_t>::write_byte_value(os, *state_output);
      }
    }
    if (flags.data.has_output) {
      OutputTraits<output_t>::write_byte_value(os, *output);
    }
    if (!flags.data.no_address) {
      OutputTraits<uint32_t>::write_byte_value(os,
                                               static_cast<uint32_t>(delta));
    }
    if (need_state_output) {
      if (flags.data.label_index == 0) { os << label; }
    } else {
      if (flags.data_witout_state_output.label_index == 0) { os << label; }
    }
    os.write(reinterpret_cast<const char *>(&flags.byte), sizeof(flags.byte));
  }
};

template <typename output_t, bool need_state_output = true>
class ByteCodeWriter {
public:
  template <typename Input>
  ByteCodeWriter(std::ostream &os, bool dump, bool verbose, const Input &input)
      : os_(os), dump_(dump), verbose_(verbose) {

    intialize_char_index_table(input);

    if (dump_) {
      std::cout << "Address\tArc\tN F L\tNxtAddr\tOutput\tStOuts\tSize"
                << std::endl;
      std::cout << "-------\t---\t-----\t-------\t------\t------\t----"
                << std::endl;
    }
  }

  ~ByteCodeWriter() {
    if (address_table_.empty()) { return; }

    auto start_byte_adress = address_table_.back();

    FstHeader header(OutputTraits<output_t>::type(), need_state_output,
                     start_byte_adress, char_index_table_);

    if (!dump_) { header.write(os_); }

    if (verbose_) {
      std::cerr << "# unique char count: " << char_count_.size() << std::endl;
      std::cerr << "# state count: " << record_index_map_.size() << std::endl;
      std::cerr << "# record count: " << address_table_.size() << std::endl;
      std::cerr << "# total size: " << address_ + sizeof(header) << std::endl;
    }
  }

  void write(const State<output_t> &state) {
    size_t char_index_size = need_state_output ? 8 : 16;
    auto transition_count = state.transitions.size();

    std::vector<size_t> jump_table;

    state.transitions.for_each_reverse([&](char arc, const auto &t, size_t i) {
      auto recored_index_iter = record_index_map_.find(t.id);
      auto has_address = recored_index_iter != record_index_map_.end();
      auto last_transition = transition_count - 1 == i;
      auto no_address = last_transition && has_address &&
                        record_index_map_[t.id] == address_table_.size() - 1;

      // If the state has 6 or more transitions, then generate jump table.
      auto need_jump_table = (i == 0) && (transition_count >= 8);

      FstRecord<output_t, need_state_output> rec;
      rec.flags.data.no_address = no_address;
      rec.flags.data.last_transition = last_transition;
      rec.flags.data.final = t.final;

      rec.delta = 0;
      size_t next_address = 0;
      if (!no_address) {
        if (has_address) {
          rec.delta = address_ - address_table_[recored_index_iter->second];
          next_address = address_ - rec.delta;
        }
      }

      rec.flags.data.has_output = false;
      if (!OutputTraits<output_t>::empty(t.output)) {
        rec.flags.data.has_output = true;
        rec.output = &t.output;
      }

      if (need_state_output) {
        rec.flags.data.has_state_output = false;
        if (!OutputTraits<output_t>::empty(t.state_output)) {
          rec.flags.data.has_state_output = true;
          rec.state_output = &t.state_output;
        }
      }

      size_t label_index = 0;
      auto index = char_index_table_[static_cast<uint8_t>(arc)];
      if (index < char_index_size) {
        label_index = index;
      } else {
        rec.label = arc;
      }
      if (need_state_output) {
        rec.flags.data.label_index = label_index;
      } else {
        rec.flags.data_witout_state_output.label_index = label_index;
      }

      // When the flags byte happens to be the same as jump tag byte, change to
      // use '.label' field instead. if (rec.flags.byte == 0xff ||
      if (rec.flags.is_jump_tag_byte()) {
        rec.label = arc;
        if (need_state_output) {
          rec.flags.data.label_index = 0;
        } else {
          rec.flags.data_witout_state_output.label_index = 0;
        }
      }

      auto byte_size = rec.byte_size();
      auto accessible_address = address_ + byte_size - 1;
      address_table_.push_back(accessible_address);
      address_ += byte_size;

      auto jump_table_element_size = 1;
      jump_table.push_back(accessible_address);

      if (need_jump_table) {
        for (auto &val : jump_table) {
          val = accessible_address - val;
          if (val > 0xff) { jump_table_element_size = 2; }
        }
        std::reverse(jump_table.begin(), jump_table.end());

        auto jump_table_size = 1 + vb_encode_value_length(transition_count) +
                               transition_count * jump_table_element_size;

        byte_size += jump_table_size;
        address_table_[address_table_.size() - 1] += jump_table_size;
        address_ += jump_table_size;
      }

      if (!dump_) {
        rec.write(os_);

        if (need_jump_table) {
          auto need_two_bytes = jump_table_element_size == 2;

          if (need_two_bytes) {
            write_jump_table<uint16_t>(os_, jump_table);
          } else {
            write_jump_table<uint8_t>(os_, jump_table);
          }

          vb_encode_value_reverse(transition_count, os_);

          auto jump_tag_byte = FstFlags::jump_tag_byte(need_two_bytes);
          os_.write((char *)&jump_tag_byte, 1);
        }
      } else {
        // Byte address
        std::cout << address_table_.back() << "\t";

        // Arc
        if (arc < 0x20) {
          std::cout << std::hex << (int)(uint8_t)arc << std::dec;
        } else {
          std::cout << arc;
        }
        std::cout << "\t";

        // Flags
        std::cout << (no_address ? "↑" : " ") << ' ' << (t.final ? '*' : ' ')
                  << ' ' << (last_transition ? "‾" : " ") << "\t";

        // Next Address
        if (!no_address) {
          if (next_address > 0) {
            std::cout << next_address;
          } else {
            std::cout << "x";
          }
        }
        std::cout << "\t";

        // Output
        if (!OutputTraits<output_t>::empty(t.output)) { std::cout << t.output; }
        std::cout << "\t";

        // State Output
        if (!OutputTraits<output_t>::empty(t.state_output)) {
          std::cout << t.state_output;
        }

        std::cout << "\t" << byte_size;
        std::cout << std::endl;
      }
    });

    if (!state.transitions.empty()) {
      record_index_map_[state.id] = address_table_.size() - 1;
    }
  }

private:
  template <typename Input>
  void intialize_char_index_table(const Input &input) {
    char_index_table_.assign(256, 0);

    input([&](const auto &word) {
      for (auto ch : word) {
        char_count_[ch]++;
      }
    });

    struct second_order {
      bool operator()(const std::pair<char, size_t> &x,
                      const std::pair<char, size_t> &y) const {
        return x.second < y.second;
      }
    };

    std::priority_queue<std::pair<char, size_t>,
                        std::vector<std::pair<char, size_t>>, second_order>
        que;

    for (auto x : char_count_) {
      que.push(x);
    }

    size_t index = 1;
    while (!que.empty()) {
      auto [ch, count] = que.top();
      char_index_table_[static_cast<uint8_t>(ch)] = index++;
      que.pop();
    }
  }

  template <typename T>
  void write_jump_table(std::ostream &os,
                        const std::vector<size_t> &jump_table) {
    std::vector<T> table(jump_table.size());
    for (size_t i = 0; i < jump_table.size(); i++) {
      table[i] = static_cast<T>(jump_table[i]);
    }
    os_.write((char *)table.data(), table.size() * sizeof(T));
  }

  std::ostream &os_;
  size_t dump_ = true;
  size_t verbose_ = true;

  std::unordered_map<char, size_t> char_count_;
  std::vector<size_t> char_index_table_;

  std::unordered_map<size_t, size_t> record_index_map_;

  size_t address_ = 0;
  std::vector<size_t> address_table_;
};

template <typename output_t, typename Input>
inline std::pair<Result, size_t> compile(const Input &input, std::ostream &os,
                                         bool verbose) {
  ByteCodeWriter<output_t> writer(os, false, verbose, [&](const auto &feeder) {
    for (const auto &[word, _] : input) {
      feeder(word);
    }
  });
  return build_fst<output_t>(input, writer);
}

template <typename Input>
inline std::pair<Result, size_t> compile(const Input &input, std::ostream &os,
                                         bool verbose) {
  ByteCodeWriter<uint32_t, false> writer(os, false, verbose,
                                         [&](const auto &feeder) {
                                           for (const auto &word : input) {
                                             feeder(word);
                                           }
                                         });
  return build_fst(input, writer);
}

template <typename output_t, typename Input>
inline std::pair<Result, size_t> dump(const Input &input, std::ostream &os,
                                      bool verbose) {
  ByteCodeWriter<output_t> writer(os, true, verbose, [&](const auto &feeder) {
    for (const auto &[word, _] : input) {
      feeder(word);
    }
  });
  return build_fst<output_t>(input, writer);
}

template <typename Input>
inline std::pair<Result, size_t> dump(const Input &input, std::ostream &os,
                                      bool verbose) {
  ByteCodeWriter<uint32_t> writer(os, true, verbose, [&](const auto &feeder) {
    for (const auto &word : input) {
      feeder(word);
    }
  });
  return build_fst(input, writer);
}

//-----------------------------------------------------------------------------
// dot
//-----------------------------------------------------------------------------

template <typename output_t> class DotWriter {
public:
  DotWriter(std::ostream &os) : os_(os) {
    os_ << "digraph{" << std::endl;
    os_ << "  rankdir = LR;" << std::endl;
  }

  ~DotWriter() { os_ << "}" << std::endl; }

  void write(const State<output_t> &state) {
    if (state.final) {
      output_t state_output;
      if (!OutputTraits<output_t>::empty(state.state_output)) {
        state_output = state.state_output;
      }
      os_ << "  s" << state.id << " [ shape = doublecircle, xlabel = \""
          << state_output << "\" ];" << std::endl;
    } else {
      os_ << "  s" << state.id << " [ shape = circle ];" << std::endl;
    }

    state.transitions.for_each(
        [&](char arc, const typename State<output_t>::Transition &t, size_t i) {
          std::string label;
          label += arc;
          os_ << "  s" << state.id << "->s" << t.id << " [ label = \"" << label;
          if (!OutputTraits<output_t>::empty(t.output)) {
            os_ << " (" << t.output << ")";
          }
          os_ << "\" fontcolor = red ];" << std::endl;
        });
  }

private:
  std::ostream &os_;
};

template <typename output_t, typename Input>
inline std::pair<Result, size_t> dot(const Input &input, std::ostream &os, bool trie = false) {

  DotWriter<output_t> writer(os);
  return build_fst<output_t>(input, writer, trie);
}

template <typename Input>
inline std::pair<Result, size_t> dot(const Input &input, std::ostream &os, bool trie = false) {
  DotWriter<uint32_t> writer(os);
  return build_fst(input, writer, trie);
}

//-----------------------------------------------------------------------------
// get_output_type
//-----------------------------------------------------------------------------

inline OutputType get_output_type(const char *byte_code,
                                  size_t byte_code_size) {
  FstHeader header;

  if (byte_code_size < sizeof(FstHeader)) { return OutputType::invalid; }

  auto p = byte_code + (byte_code_size - sizeof(FstHeader));
  memcpy(reinterpret_cast<char *>(&header), p, sizeof(FstHeader));

  if (header.version != 0) { return OutputType::invalid; }

  return static_cast<OutputType>(header.value_type);
}

//-----------------------------------------------------------------------------
// Matcher
//-----------------------------------------------------------------------------

template <typename output_t> class Matcher {
public:
  Matcher(const char *byte_code, size_t byte_code_size)
      : byte_code_(byte_code), byte_code_size_(byte_code_size) {

    if (byte_code_size < sizeof(FstHeader)) { return; }

    auto p = byte_code + (byte_code_size - sizeof(FstHeader));
    memcpy(reinterpret_cast<char *>(&header_), p, sizeof(FstHeader));

    if (header_.version != 0) { return; }

    if (static_cast<OutputType>(header_.value_type) !=
        OutputTraits<output_t>::type()) {
      return;
    }

    is_valid_ = true;
  }

  operator bool() { return is_valid_; }

  void set_trace(bool on) { trace_ = on; }

  bool exact_match_search(const char *str, size_t len, output_t &output) const {
    return match(str, len, [&](const auto &_) { output = _; });
  }

  bool common_prefix_search(
      const char *str, size_t len,
      std::function<void(size_t, const output_t &)> prefixes) const {
    return match(str, len, nullptr, prefixes);
  }

  bool longest_common_prefix_search(const char *str, size_t len,
                                    size_t &prefix_len,
                                    output_t &output) const {
    return common_prefix_search(str, len, [&](size_t len, const auto &_output) {
      prefix_len = len;
      output = _output;
    });
  }

private:
  bool match(
      const char *str, size_t len,
      std::function<void(const output_t &)> outputs = nullptr,
      std::function<void(size_t, const output_t &)> prefixes = nullptr) const {

    if (trace_) {
      std::cout << "Char\tAddress\tArc\tN F L\tNxtAddr\tOutput\tStOuts\tSize"
                << std::endl;
      std::cout << "----\t-------\t---\t-----\t-------\t------\t------\t----"
                << std::endl;
    }

    auto ret = false;
    auto output = OutputTraits<output_t>::initial_value();
    auto state_output = OutputTraits<output_t>::initial_value();

    size_t address = header_.start_address;
    size_t i = 0;
    while (i < len) {
      uint8_t ch = str[i];
      OutputTraits<output_t>::clear(state_output);

      auto end = byte_code_ + address;
      auto p = end;

      FstFlags flags;
      flags.byte = *p--;

      if (flags.is_jump_tag_byte()) {
        auto jump_table_element_size = flags.jump_table_element_size();

        size_t transition_count = 0;
        auto vb_len = vb_decode_value_reverse(p, transition_count);
        p -= vb_len;

        auto jump_table_size =
            1 + vb_len + transition_count * jump_table_element_size;

        p -= transition_count * jump_table_element_size;
        auto jump_table = p;

        auto base_address = byte_code_ + address - jump_table_size;

        auto found = lower_bound_index(0, transition_count, [&](size_t i) {
          auto p = base_address -
                   lookup_jump_table(jump_table, i, jump_table_element_size);
          FstFlags flags;
          flags.byte = *p--;
          return get_arc(flags, p) < ch;
        });

        if (found < transition_count) {
          address -=
              lookup_jump_table(jump_table, found, jump_table_element_size) +
              jump_table_size;
        } else {
          address -= std::distance(p, end);
        }
        continue;
      }

      auto arc = get_arc(flags, p);

      size_t delta = 0;
      if (!flags.data.no_address) { p -= vb_decode_value_reverse(p, delta); }

      auto output_suffix = OutputTraits<output_t>::initial_value();
      if (flags.data.has_output) {
        p -= OutputTraits<output_t>::read_byte_value(p, output_suffix);
      }

      if (header_.need_state_output) {
        if (flags.data.has_state_output) {
          p -= OutputTraits<output_t>::read_byte_value(p, state_output);
        }
      }

      auto byte_size = std::distance(p, end);

      size_t next_address = 0;
      if (!flags.data.no_address) {
        if (delta) { next_address = address - byte_size - delta + 1; }
      } else {
        next_address = address - byte_size;
      }

      if (trace_) {
        std::cout << ch << "\t";
        std::cout << address << "\t";
        std::cout << arc << "\t";
        std::cout << (flags.data.no_address ? "↑" : " ") << ' '
                  << (flags.data.final ? '*' : ' ') << ' '
                  << (flags.data.last_transition ? "‾" : " ") << "\t";

        // Next Address
        if (next_address) {
          std::cout << next_address;
        } else {
          std::cout << "x";
        }
        std::cout << "\t";

        if (flags.data.has_output) { std::cout << output_suffix; }
        std::cout << "\t";

        if (header_.need_state_output) {
          if (flags.data.has_state_output) { std::cout << state_output; }
        }
        std::cout << "\t";

        std::cout << byte_size;
        std::cout << std::endl;
      }

      if (ch == arc) {
        output += output_suffix;
        i++;
        if (flags.data.final) {
          if (prefixes) {
            if (OutputTraits<output_t>::empty(state_output)) {
              prefixes(i, output);
            } else {
              prefixes(i, output + state_output);
            }
            ret = true;
          }
          if (i == len) {
            if (outputs) {
              if (OutputTraits<output_t>::empty(state_output)) {
                outputs(output);
              } else {
                outputs(output + state_output);
              }
            }
            ret = true;
            break;
          }
        }
        if (!next_address) { break; }
        address = next_address;
      } else {
        if (flags.data.last_transition) { break; }
        address -= byte_size;
      }
    }

    return ret;
  }

  size_t lookup_jump_table(const char *p, size_t index,
                           size_t element_size) const {
    if (element_size == 2) {
      return reinterpret_cast<const uint16_t *>(p + 1)[index];
    } else {
      return reinterpret_cast<const uint8_t *>(p + 1)[index];
    }
  }

  uint8_t get_arc(FstFlags flags, const char *&p) const {
    auto index = header_.need_state_output
                     ? flags.data.label_index
                     : flags.data_witout_state_output.label_index;
    return index == 0 ? *p-- : header_.char_index[index];
  }

  const char *byte_code_;
  size_t byte_code_size_;

  FstHeader header_;
  bool is_valid_ = false;
  bool trace_ = false;
};

} // namespace fst

#endif // CPPFSTLIB_FSTLIB_H_
