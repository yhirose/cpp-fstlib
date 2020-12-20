//
//  fstlib.h
//
//  Copyright (c) 2020 Yuji Hirose. All rights reserved.
//  MIT License
//

#pragma once

#include <algorithm>
#include <any>
#include <cassert>
#include <cstring>
#include <functional>
#include <iomanip>
#include <iostream>
#include <list>
#include <memory>
#include <numeric>
#include <queue>
#include <set>
#include <sstream>
#include <stdexcept>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#if !defined(__cplusplus) || __cplusplus < 201703L
#error "Requires complete C++17 support"
#endif

namespace fst {

//-----------------------------------------------------------------------------
// variable byte encoding
//-----------------------------------------------------------------------------

template <typename Val> inline size_t vb_encode_value_length(Val n) {
  auto len = 0u;
  while (n >= 128) {
    len++;
    n >>= 7;
  }
  len++;
  return len;
}

template <typename Val> inline size_t vb_encode_value(Val n, char *out) {
  auto len = 0u;
  while (n >= 128) {
    out[len] = static_cast<char>(n & 0x7f);
    len++;
    n >>= 7;
  }
  out[len] = static_cast<char>(n + 128);
  len++;
  return len;
}

template <typename Val, typename Cont> void vb_encode_value(Val n, Cont &out) {
  while (n >= 128) {
    out.push_back(static_cast<typename Cont::value_type>(n & 0x7f));
    n >>= 7;
  }
  out.push_back(static_cast<typename Cont::value_type>(n + 128));
}

template <typename Val>
inline size_t vb_encode_value_reverse(Val n, char *out) {
  auto len = vb_encode_value(n, out);
  for (auto i = 0u; i < len / 2; i++) {
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
  auto p = reinterpret_cast<const uint8_t *>(data);
  auto i = 0;
  n = 0;
  auto cnt = 0u;
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
  auto len = last - first;

  while (len > 0) {
    auto half = len >> 1;
    auto middle = first + half;

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
  const auto m = uint32_t(0x5bd1e995);
  const auto r = 24u;

  auto h1 = static_cast<uint32_t>(seed) ^ static_cast<uint32_t>(len);
  auto h2 = static_cast<uint32_t>(seed >> 32);

  auto data = reinterpret_cast<const uint32_t *>(key);

  while (len >= 8) {
    auto k1 = *data++;
    k1 *= m;
    k1 ^= k1 >> r;
    k1 *= m;
    h1 *= m;
    h1 ^= k1;
    len -= 4;

    auto k2 = *data++;
    k2 *= m;
    k2 ^= k2 >> r;
    k2 *= m;
    h2 *= m;
    h2 ^= k2;
    len -= 4;
  }

  if (len >= 4) {
    auto k1 = *data++;
    k1 *= m;
    k1 ^= k1 >> r;
    k1 *= m;
    h1 *= m;
    h1 ^= k1;
    len -= 4;
  }

  switch (len) {
  case 3: h2 ^= reinterpret_cast<const unsigned char *>(data)[2] << 16;
  case 2: h2 ^= reinterpret_cast<const unsigned char *>(data)[1] << 8;
  case 1: h2 ^= reinterpret_cast<const unsigned char *>(data)[0]; h2 *= m;
  };

  h1 ^= h2 >> 18;
  h1 *= m;
  h2 ^= h1 >> 22;
  h2 *= m;
  h1 ^= h2 >> 17;
  h1 *= m;
  h2 ^= h1 >> 19;
  h2 *= m;

  auto h = static_cast<uint64_t>(h1);
  h = (h << 32) | h2;
  return h;
}

//-----------------------------------------------------------------------------
// char_to_string
//-----------------------------------------------------------------------------

inline std::string char_to_string(char arc) {
  std::stringstream ss;
  if (arc < 0x20) {
    ss << std::hex << std::setfill('0') << std::setw(2) << (int)(uint8_t)arc
       << std::dec;
  } else {
    ss << arc;
  }
  return ss.str();
}

//-----------------------------------------------------------------------------
// get_common_prefix_length
//-----------------------------------------------------------------------------

inline size_t get_common_prefix_length(const std::string &s1,
                                       const std::string &s2) {
  auto i = 0u;
  while (i < s1.size() && i < s2.size() && s1[i] == s2[i]) {
    i++;
  }
  return i;
}

//-----------------------------------------------------------------------------
// get_prefix_length
//-----------------------------------------------------------------------------

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

using none_t = int;

enum class OutputType { invalid = -1, none_t, uint32_t, uint64_t, string };

template <typename output_t> struct OutputTraits {};

template <> struct OutputTraits<none_t> {
  using value_type = none_t;

  static OutputType type() { return OutputType::none_t; }

  static bool empty(value_type val) { return val == 0; }

  static size_t read_byte_value(const char *p, value_type &val) { return 0; }
};

template <> struct OutputTraits<uint32_t> {
  using value_type = uint32_t;

  static OutputType type() { return OutputType::uint32_t; }

  static bool empty(value_type val) { return val == 0; }

  static std::string to_string(value_type val) { return std::to_string(val); }

  static void prepend_value(value_type &base, value_type val) { base += val; }

  static value_type get_suffix(value_type a, value_type b) { return a - b; }

  static value_type get_common_prefix(value_type a, value_type b) {
    return std::min(a, b);
  }

  template <typename T> static size_t write_value(T &buff, value_type val) {
    auto p = reinterpret_cast<const char *>(&val);
    buff.insert(buff.begin(), p, p + sizeof(val));
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

template <> struct OutputTraits<uint64_t> {
  using value_type = uint64_t;

  static OutputType type() { return OutputType::uint64_t; }

  static bool empty(value_type val) { return val == 0; }

  static std::string to_string(value_type val) { return std::to_string(val); }

  static void prepend_value(value_type &base, value_type val) { base += val; }

  static value_type get_suffix(value_type a, value_type b) { return a - b; }

  static value_type get_common_prefix(value_type a, value_type b) {
    return std::min(a, b);
  }

  template <typename T> static size_t write_value(T &buff, value_type val) {
    auto p = reinterpret_cast<const char *>(&val);
    buff.insert(buff.begin(), p, p + sizeof(val));
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

  static bool empty(const value_type &val) { return val.empty(); }

  static value_type to_string(const value_type &val) { return val; }

  static void prepend_value(value_type &base, const value_type &val) {
    base.insert(0, val);
  }

  static value_type get_suffix(const value_type &a, const value_type &b) {
    return a.substr(b.size());
  }

  static value_type get_common_prefix(const value_type &a,
                                      const value_type &b) {
    return a.substr(0, get_common_prefix_length(a, b));
  }

  template <typename T> static size_t write_value(T &buff, value_type val) {
    buff.insert(buff.begin(), val.data(), val.data() + val.size());
    return val.size();
  }

  static size_t get_byte_value_size(const value_type &val) {
    return vb_encode_value_length(val.size()) + val.size();
  }

  static void write_byte_value(std::ostream &os, const value_type &val) {
    os.write(val.data(), val.size());
    vb_encode_value_reverse(static_cast<uint32_t>(val.size()), os);
  }

  static size_t read_byte_value(const char *p, value_type &val) {
    uint32_t str_len = 0;
    auto vb_len = vb_decode_value_reverse(p, str_len);

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
        fn(arcs[i], states_and_outputs[i]);
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
      for (auto i = 0u; i < arcs.size(); i++) {
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
    state_output = output_t{};
  }

  size_t id = -1;
  bool final = false;
  Transitions transitions;
  output_t state_output = output_t{};

private:
  State(const State &) = delete;
  State(State &&) = delete;
};

template <typename output_t> inline uint64_t State<output_t>::hash() const {
  std::vector<char> buff;

  transitions.for_each([&](char arc, const State::Transition &t) {
    buff.push_back(arc);

    auto val = static_cast<uint32_t>(t.id);
    auto p = reinterpret_cast<const char *>(&val);
    buff.insert(buff.begin(), p, p + sizeof(val));

    if (!OutputTraits<output_t>::empty(t.output)) {
      OutputTraits<output_t>::write_value(buff, t.output);
    }
  });

  if (final && !OutputTraits<output_t>::empty(state_output)) {
    OutputTraits<output_t>::write_value(buff, state_output);
  }

  return MurmurHash64B(buff.data(), buff.size(), 0);
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
  Dictionary(StatePool<output_t> &state_pool) : state_pool_(state_pool) {}

  State<output_t> *get(uint64_t key, State<output_t> *state) {
    auto id = bucket_id(key);
    auto [first, second, third] = buckets_[id];
    if (first && *first == *state) { return first; }
    if (second && *second == *state) {
      buckets_[id] = std::tuple(second, first, third);
      return second;
    }
    if (third && *third == *state) {
      buckets_[id] = std::tuple(third, first, second);
      return third;
    }
    return nullptr;
  }

  void put(uint64_t key, State<output_t> *state) {
    auto id = bucket_id(key);
    auto [first, second, third] = buckets_[id];
    if (third) { state_pool_.Delete(third); }
    buckets_[id] = std::tuple(state, first, second);
  }

private:
  StatePool<output_t> &state_pool_;

  static const auto kBucketCount = 10000u;

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
  if (st) { return std::pair(true, st); }

  dictionary.put(h, state);
  return std::pair(false, state);
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
build_fst_core(const Input &input, Writer &writer, bool need_output) {
  StatePool<output_t> state_pool;

  Dictionary<output_t> dictionary(state_pool);
  auto next_state_id = 0u;
  auto error_input_index = 0u;
  auto result = Result::Success;

  // Main algorithm ported from the technical paper
  std::vector<State<output_t> *> temp_states;
  std::string previous_word;
  temp_states.push_back(state_pool.New(next_state_id++));

  input([&](const auto &current_word, const auto &_current_output,
            size_t input_index) {
    auto current_output = _current_output;

    if (current_word.empty()) {
      result = Result::EmptyKey;
      error_input_index = input_index;
      return false;
    }

    // The following loop caluculates the length of the longest common
    // prefix of 'current_word' and 'previous_word'
    size_t prefix_length;
    if (!get_prefix_length(previous_word, current_word, prefix_length)) {
      result = Result::UnsortedKey;
      error_input_index = input_index;
      return false;
    }

    if (previous_word.size() == current_word.size() &&
        previous_word == current_word) {
      result = Result::DuplicateKey;
      error_input_index = input_index;
      return false;
    }

    // We minimize the states from the suffix of the previous word
    for (auto i = previous_word.size(); i > prefix_length; i--) {
      auto [found, state] =
          find_minimized<output_t>(temp_states[i], dictionary);

      auto arc = previous_word[i - 1];

      if (found) {
        next_state_id--;
      } else {
        writer.write(*state, arc);

        // Ownership of the object in temp_states[i] has been moved to the
        // dictionary...
        temp_states[i] = state_pool.New();
      }

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

    if (need_output) {
      for (auto j = 1u; j <= prefix_length; j++) {
        auto prev_state = temp_states[j - 1];
        auto arc = current_word[j - 1];

        const auto &output = prev_state->output(arc);

        auto common_prefix = output_t{};
        auto word_suffix = output_t{};
        get_common_prefix_and_word_suffix(current_output, output, common_prefix,
                                          word_suffix);

        prev_state->set_output(arc, common_prefix);

        if (!OutputTraits<output_t>::empty(word_suffix)) {
          auto state = temp_states[j];

          for (auto arc : state->transitions.arcs) {
            state->prepend_suffix_to_output(arc, word_suffix);
          }

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
    }

    previous_word = current_word;
    return true;
  });

  if (result != Result::Success) {
    return std::pair(result, error_input_index);
  }

  // Here we are minimizing the states of the last word
  for (auto i = static_cast<int>(previous_word.size()); i >= 0; i--) {
    auto [found, state] = find_minimized<output_t>(temp_states[i], dictionary);

    auto arc = (i > 0) ? previous_word[i - 1] : 0;

    if (found) {
      next_state_id--;
    } else {
      writer.write(*state, arc);
    }

    if (i > 0) { temp_states[i - 1]->set_transition(arc, state); }
  }

  return std::pair(Result::Success, error_input_index);
}

//-----------------------------------------------------------------------------
// build_fst
//-----------------------------------------------------------------------------

template <typename output_t, typename Input, typename Writer>
inline std::pair<Result, size_t> build_fst(const Input &input, Writer &writer,
                                           bool need_output, bool sorted) {
  return build_fst_core<output_t>(
      [&](const auto &feeder) {
        if (sorted) {
          size_t input_index = 0;
          for (const auto &item : input) {
            const auto &word = item.first;
            const auto &output = item.second;
            if (!feeder(word, output, input_index)) { break; }
            input_index++;
          }
        } else {
          std::vector<size_t> sorted_indexes(input.size());
          {
            std::iota(sorted_indexes.begin(), sorted_indexes.end(), 0);
            std::sort(sorted_indexes.begin(), sorted_indexes.end(),
                      [&](const auto &a, const auto &b) {
                        return input[a].first < input[b].first;
                      });
          }

          for (auto input_index : sorted_indexes) {
            const auto &[word, output] = input[input_index];
            if (!feeder(word, output, input_index)) { break; }
          }
        }
      },
      writer, need_output);
}

template <typename Input, typename Writer>
inline std::pair<Result, size_t> build_fst(const Input &input, Writer &writer,
                                           bool need_output, bool sorted) {
  return build_fst_core<uint32_t>(
      [&](const auto &feeder) {
        if (sorted) {
          size_t input_index = 0;
          for (const auto &word : input) {
            if (!feeder(word, static_cast<uint32_t>(input_index),
                        input_index)) {
              break;
            }
            input_index++;
          }
        } else {
          std::vector<size_t> sorted_indexes(input.size());
          {
            std::iota(sorted_indexes.begin(), sorted_indexes.end(), 0);
            std::sort(sorted_indexes.begin(), sorted_indexes.end(),
                      [&](const auto &a, const auto &b) {
                        return input[a] < input[b];
                      });
          }

          for (auto input_index : sorted_indexes) {
            const auto word = input[input_index];
            if (!feeder(word, static_cast<uint32_t>(input_index),
                        input_index)) {
              break;
            }
          }
        }
      },
      writer, need_output);
}

//-----------------------------------------------------------------------------
// compile
//-----------------------------------------------------------------------------

union FstOpe {
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
  } data_no_state_output;

  struct {
    unsigned no_address : 1;
    unsigned last_transition : 1;
    unsigned final : 1;
    unsigned label_index : 5;
  } data_no_output;

  uint8_t byte = 0;

  FstOpe() = default;
  explicit FstOpe(uint8_t byte) : byte(byte) {}

  size_t label_index(bool need_output, bool need_state_output) const {
    if (!need_output) {
      return data_no_output.label_index;
    } else if (need_state_output) {
      return data.label_index;
    } else {
      return data_no_state_output.label_index;
    }
  }

  void set_label_index(bool need_output, bool need_state_output, size_t index) {
    if (!need_output) {
      data_no_output.label_index = index;
    } else if (need_state_output) {
      data.label_index = index;
    } else {
      data_no_state_output.label_index = index;
    }
  }

  // For char index
  static constexpr size_t char_index_size(bool need_output,
                                          bool need_state_output) {
    return !need_output ? 32 : (need_state_output ? 8 : 16);
  }

  // For jump table
  bool has_jump_table() const { return byte == 0xff || byte == 0xfe; }

  size_t jump_table_element_size() const {
    return (byte == 0xff || byte == 0xfd) ? 2 : 1;
  }

  static uint8_t jump_table_tag(bool need_two_bytes) {
    return need_two_bytes ? 0xff : 0xfe;
  }
};

template <typename output_t, bool need_state_output> struct FstRecord {
  FstOpe ope;

  char label = 0;
  size_t delta = 0;
  bool need_output = false;
  const output_t *output = nullptr;
  const output_t *state_output = nullptr;

  size_t byte_size() const {
    auto sz = 1u;
    if (ope.label_index(need_output, need_state_output) == 0) { sz += 1; }
    if (!ope.data.no_address) { sz += vb_encode_value_length(delta); }
    if (need_output) {
      if (ope.data.has_output) {
        sz += OutputTraits<output_t>::get_byte_value_size(*output);
      }
      if (need_state_output) {
        if (ope.data.has_state_output) {
          sz += OutputTraits<output_t>::get_byte_value_size(*state_output);
        }
      }
    }
    return sz;
  }

  void write(std::ostream &os) {
    if (need_output) {
      if (need_state_output) {
        if (ope.data.has_state_output) {
          OutputTraits<output_t>::write_byte_value(os, *state_output);
        }
      }
      if (ope.data.has_output) {
        OutputTraits<output_t>::write_byte_value(os, *output);
      }
    }
    if (!ope.data.no_address) {
      OutputTraits<uint32_t>::write_byte_value(os,
                                               static_cast<uint32_t>(delta));
    }
    if (ope.label_index(need_output, need_state_output) == 0) {
      os.write(&label, 1);
    }
    os.write(reinterpret_cast<const char *>(&ope.byte), sizeof(ope.byte));
  }
};

struct FstHeader {
  union {
    struct {
      unsigned output_type : 3;
      unsigned need_state_output : 1;
      unsigned reserved : 4;
    } data;

    uint8_t byte;
  } flags;

  uint32_t start_address = 0;
  char char_index[32] = {0};

  bool need_output = false;
  bool need_state_output = false;

  FstHeader() = default;

  FstHeader(OutputType output_type, bool need_state_output,
            size_t start_address, const std::vector<size_t> &char_index_table)
      : start_address(static_cast<uint32_t>(start_address)) {

    flags.data.output_type = static_cast<uint8_t>(output_type);
    flags.data.need_state_output = need_state_output;

    auto size = char_index_size();
    for (auto ch = 0u; ch < 256; ch++) {
      auto index = char_index_table[ch];
      if (0 < index && index < size) {
        char_index[index] = static_cast<char>(ch);
      }
    }
  }

  bool read(const char *byte_code, size_t byte_code_size) {
    auto remaining = byte_code_size;
    if (remaining < sizeof(uint8_t)) { return false; }

    auto p = byte_code + (byte_code_size - sizeof(uint8_t));
    flags.byte = *p--;

    remaining -= sizeof(uint8_t);
    if (remaining < sizeof(uint32_t)) { return false; }

    memcpy(&start_address, p - (sizeof(uint32_t) - 1), sizeof(start_address));
    p -= sizeof(uint32_t);

    remaining -= sizeof(uint32_t);
    auto size = char_index_size();
    if (remaining < size) { return false; }

    memcpy(char_index, p - (size - 1), size);

    // For performance
    need_output =
        static_cast<OutputType>(flags.data.output_type) != OutputType::none_t;
    need_state_output = flags.data.need_state_output;
    return true;
  }

  void write(std::ostream &os) {
    os.write(char_index, char_index_size());
    os.write(reinterpret_cast<char *>(&start_address), sizeof(start_address));
    os.write(reinterpret_cast<char *>(&flags.byte), sizeof(flags.byte));
  }

  size_t char_index_size() const {
    return FstOpe::char_index_size(need_output, need_state_output);
  }
};

template <typename output_t, bool need_state_output> class FstWriter {
public:
  template <typename Input>
  FstWriter(std::ostream &os, bool need_output, bool dump, bool verbose,
            const Input &input)
      : os_(os), need_output_(need_output), dump_(dump), verbose_(verbose) {

    initialize_char_index_table(input);

    if (dump_) {
      os << "Address\tArc\tN F L\tNxtAddr";
      if (need_output_) { os << "\tOutput\tStOuts"; }
      os << "\tSize" << std::endl;

      os << "-------\t---\t-----\t-------";
      if (need_output_) { os << "\t------\t------"; }
      os << "\t----" << std::endl;
    }
  }

  ~FstWriter() {
    if (address_table_.empty()) { return; }

    auto start_byte_adress = address_table_.back();

    auto output_type =
        need_output_ ? OutputTraits<output_t>::type() : OutputType::none_t;

    FstHeader header(output_type, need_state_output, start_byte_adress,
                     char_index_table_);

    if (!dump_) { header.write(os_); }

    if (verbose_) {
      std::cerr << "# unique char count: " << char_count_.size() << std::endl;
      std::cerr << "# state count: " << record_index_map_.size() << std::endl;
      std::cerr << "# record count: " << address_table_.size() << std::endl;
      std::cerr << "# total size: " << address_ + sizeof(header) << std::endl;
    }
  }

  void write(const State<output_t> &state, char prev_arc) {
    auto transition_count = state.transitions.size();
    const auto &[arcs, states_and_outputs] = state.transitions;

    auto char_index_size =
        FstOpe::char_index_size(need_output_, need_state_output);

    std::vector<size_t> jump_table(transition_count);
    auto need_jump_table = transition_count >= 8;

    size_t indexes_sorted_by_bigram_count[256];

    if (!need_jump_table) {
      uint16_t keys[256];
      for (auto i = 0u; i < arcs.size(); i++) {
        indexes_sorted_by_bigram_count[i] = i;
        keys[i] = bigram_key(prev_arc, arcs[i]);
      }

      std::sort(&indexes_sorted_by_bigram_count[0],
                &indexes_sorted_by_bigram_count[arcs.size()],
                [&](auto i1, auto i2) {
                  return bigram_count_[keys[i1]] > bigram_count_[keys[i2]];
                });
    }

    for (auto ri = arcs.size(); ri > 0; ri--) {
      auto i = ri - 1;

      auto arc_i = !need_jump_table ? indexes_sorted_by_bigram_count[i] : i;
      auto arc = arcs[arc_i];
      const auto &t = states_and_outputs[arc_i];

      auto recored_index_iter = record_index_map_.find(t.id);
      auto has_address = recored_index_iter != record_index_map_.end();
      auto last_transition = transition_count - 1 == i;
      auto no_address = last_transition && has_address &&
                        record_index_map_[t.id] == address_table_.size() - 1;

      // If the state has 6 or more transitions, then generate jump table.
      auto generate_jump_table = (i == 0) && need_jump_table;

      FstRecord<output_t, need_state_output> rec;
      rec.need_output = need_output_;
      rec.ope.data.no_address = no_address;
      rec.ope.data.last_transition = last_transition;
      rec.ope.data.final = t.final;

      rec.delta = 0;
      auto next_address = 0u;
      if (!no_address) {
        if (has_address) {
          rec.delta = address_ - address_table_[recored_index_iter->second];
          next_address = address_ - rec.delta;
        }
      }

      if (need_output_) {
        rec.ope.data.has_output = false;
        if (!OutputTraits<output_t>::empty(t.output)) {
          rec.ope.data.has_output = true;
          rec.output = &t.output;
        }

        if (need_state_output) {
          rec.ope.data.has_state_output = false;
          if (!OutputTraits<output_t>::empty(t.state_output)) {
            rec.ope.data.has_state_output = true;
            rec.state_output = &t.state_output;
          }
        }
      }

      auto label_index = 0u;
      auto index = char_index_table_[static_cast<uint8_t>(arc)];
      if (index < char_index_size) {
        label_index = index;
      } else {
        rec.label = arc;
      }
      rec.ope.set_label_index(need_output_, need_state_output, label_index);

      // When the ope byte happens to be the same as jump tag byte, change to
      // use '.label' field instead.
      if (rec.ope.has_jump_table()) {
        rec.label = arc;
        rec.ope.set_label_index(need_output_, need_state_output, 0);
      }

      auto byte_size = rec.byte_size();
      auto accessible_address = address_ + byte_size - 1;
      address_table_.push_back(accessible_address);
      address_ += byte_size;

      if (!dump_) {
        rec.write(os_);

        if (need_jump_table) { jump_table[i] = accessible_address; }

        if (generate_jump_table) {
          auto jump_table_element_size = 1;

          for (auto &val : jump_table) {
            val = accessible_address - val;
            if (val > 0xff) { jump_table_element_size = 2; }
          }

          auto jump_table_byte_size =
              1 + vb_encode_value_length(jump_table.size()) +
              jump_table.size() * jump_table_element_size;

          auto need_two_bytes = jump_table_element_size == 2;

          auto jump_table_tag = FstOpe::jump_table_tag(need_two_bytes);

          byte_size += jump_table_byte_size;
          address_table_[address_table_.size() - 1] += jump_table_byte_size;
          address_ += jump_table_byte_size;

          if (need_two_bytes) {
            write_jump_table<uint16_t>(os_, jump_table);
          } else {
            write_jump_table<uint8_t>(os_, jump_table);
          }

          vb_encode_value_reverse(jump_table.size(), os_);

          os_.write((char *)&jump_table_tag, 1);
        }
      } else {
        os_ << address_table_.back() << "\t";
        os_ << char_to_string(arc) << "\t";

        os_ << (no_address ? "↑" : " ") << ' ' << (t.final ? '*' : ' ') << ' '
            << (last_transition ? "‾" : " ") << "\t";

        if (!no_address) {
          if (next_address > 0) {
            os_ << next_address;
          } else {
            os_ << "x";
          }
        }

        if (need_output_) {
          os_ << "\t";

          if (!OutputTraits<output_t>::empty(t.output)) { os_ << t.output; }
          os_ << "\t";

          if (!OutputTraits<output_t>::empty(t.state_output)) {
            os_ << t.state_output;
          }
        }

        os_ << "\t" << byte_size << std::endl;
      }
    }

    if (!state.transitions.empty()) {
      record_index_map_[state.id] = address_table_.size() - 1;
    }
  }

private:
  template <typename Input>
  void initialize_char_index_table(const Input &input) {
    char_index_table_.assign(256, 0);

    input([&](const auto &word) {
      char prev = 0;
      for (auto ch : word) {
        char_count_[ch]++;
        bigram_count_[bigram_key(prev, ch)]++;
        prev = ch;
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

    auto index = 1u;
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
    for (auto i = 0u; i < jump_table.size(); i++) {
      table[i] = static_cast<T>(jump_table[i]);
    }
    os_.write((char *)table.data(), table.size() * sizeof(T));
  }

  std::ostream &os_;
  size_t need_output_ = true;
  size_t dump_ = true;
  size_t verbose_ = true;

  std::unordered_map<char, size_t> char_count_;
  std::vector<size_t> char_index_table_;

  uint16_t bigram_key(char prev, char cur) const {
    return static_cast<uint16_t>(prev) << 8 | static_cast<uint16_t>(cur);
  }
  std::unordered_map<uint16_t, size_t> bigram_count_;

  std::unordered_map<size_t, size_t> record_index_map_;

  size_t address_ = 0;
  std::vector<size_t> address_table_;
};

template <typename output_t, typename Input>
inline std::pair<Result, size_t> compile(const Input &input, std::ostream &os,
                                         bool sorted, bool verbose = false) {
  FstWriter<output_t, true> writer(os, true, false, verbose,
                                   [&](const auto &feeder) {
                                     for (const auto &[word, _] : input) {
                                       feeder(word);
                                     }
                                   });
  return build_fst<output_t>(input, writer, true, sorted);
}

template <typename Input>
inline std::pair<Result, size_t> compile(const Input &input, std::ostream &os,
                                         bool need_output, bool sorted,
                                         bool verbose = false) {
  FstWriter<uint32_t, false> writer(os, need_output, false, verbose,
                                    [&](const auto &feeder) {
                                      for (const auto &word : input) {
                                        feeder(word);
                                      }
                                    });
  return build_fst(input, writer, need_output, sorted);
}

template <typename output_t, typename Input>
inline std::pair<Result, size_t> dump(const Input &input, std::ostream &os,
                                      bool sorted, bool verbose = false) {
  FstWriter<output_t, true> writer(os, true, true, verbose,
                                   [&](const auto &feeder) {
                                     for (const auto &[word, _] : input) {
                                       feeder(word);
                                     }
                                   });
  return build_fst<output_t>(input, writer, true, sorted);
}

template <typename Input>
inline std::pair<Result, size_t> dump(const Input &input, std::ostream &os,
                                      bool need_output, bool sorted,
                                      bool verbose = false) {
  FstWriter<uint32_t, true> writer(os, need_output, true, verbose,
                                   [&](const auto &feeder) {
                                     for (const auto &word : input) {
                                       feeder(word);
                                     }
                                   });
  return build_fst(input, writer, need_output, sorted);
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

  void write(const State<output_t> &state, char prev_arc) {
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
        [&](auto arc, const typename State<output_t>::Transition &t) {
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
inline std::pair<Result, size_t> dot(const Input &input, std::ostream &os,
                                     bool sorted) {

  DotWriter<output_t> writer(os);
  return build_fst<output_t>(input, writer, true, sorted);
}

template <typename Input>
inline std::pair<Result, size_t> dot(const Input &input, std::ostream &os,
                                     bool need_output, bool sorted) {
  DotWriter<uint32_t> writer(os);
  return build_fst(input, writer, need_output, sorted);
}

//-----------------------------------------------------------------------------
// get_output_type
//-----------------------------------------------------------------------------

inline OutputType get_output_type(const char *byte_code,
                                  size_t byte_code_size) {
  FstHeader header;
  if (!header.read(byte_code, byte_code_size)) { return OutputType::invalid; }
  return static_cast<OutputType>(header.flags.data.output_type);
}

template <typename T> OutputType get_output_type(const T &byte_code) {
  return get_output_type(byte_code.data(), byte_code.size());
}

//-----------------------------------------------------------------------------
// levenshtein_distance
//-----------------------------------------------------------------------------

inline double cost_replace(std::string_view from, size_t i, std::string_view to,
                           size_t j) {
  auto c1 = from[i];
  auto c2 = to[j];

  if (c1 == c2) return 0;

  // one char similar sound...
  {
    const char *similers[] = {
        "ao",
        "ae",
        "iy",
        "ou",
    };

    for (auto s : similers) {
      if ((c1 == s[0] && c2 == s[1]) || (c1 == s[1] && c2 == s[0])) return 0.25;
    }
  }

  if (i + 1 < from.size() && j + 1 < to.size()) {
    auto cn1 = from[i + 1];
    auto cn2 = to[j + 1];

    // Transposed chars...
    if (c1 == cn2 && c2 == cn1) return 0;

    // two chars similar sound...
    {
      const char *similers[] = {
          "irer",
          "urer",
          "irur",
          "erar",
      };

      for (auto s : similers) {
        if ((c1 == s[0] && cn1 == s[1] && c2 == s[2] && cn2 == s[3]) ||
            (c1 == s[2] && cn1 == s[3] && c2 == s[0] && cn2 == s[1]))
          return 0;
      }
    }
  }

  return 1.0;
}

inline double cost_insert(std::string_view to, size_t j) {
  if (to[j] == to[j + 1]) return 0.5;
  return 1.0;
}

inline double cost_delete(std::string_view from, size_t i) {
  if (from[i] == from[i + 1]) return 0.5;
  return 1.0;
}

inline double levenshtein_distance(std::string_view from, std::string_view to) {
  std::vector<std::vector<double>> m(from.size() + 1);

  for (size_t i = 0; i < m.size(); i++)
    m[i].assign(to.size() + 1, 0);

  for (size_t i = 0; i < m.size(); i++)
    m[i][0] = i;
  for (size_t j = 0; j < m[0].size(); j++)
    m[0][j] = j;

  for (size_t i = 0; i < from.size(); i++)
    for (size_t j = 0; j < to.size(); j++) {
      m[i + 1][j + 1] =
          std::min(m[i][j + 1] + cost_insert(to, j),                  // insert
                   std::min(m[i + 1][j] + cost_delete(from, i),       // delete
                            m[i][j] + cost_replace(from, i, to, j))); // replace
    }

  auto d = m.back().back();
  return 1.0 - (d / std::max<double>(from.size(), to.size()));
}

inline size_t max_range(std::string_view s1, std::string_view s2) {
  return (std::max(s1.length(), s2.length()) / 2) - 1;
}

inline bool common_string(std::string_view s1, std::string_view s2,
                          std::string &cs) {
  auto r = max_range(s1, s2);

  for (size_t i = 0; i < s1.length(); i++) {
    auto beg = std::max<int>(0, (int)i - (int)r);
    auto end = std::min(s2.length(), (i + r));

    auto c1 = s1[i];
    for (size_t j = beg; j <= end; j++) {
      if (c1 == s2[j]) {
        cs += c1;
        break;
      }
    }
  }

  return !cs.empty();
}

inline size_t commn_prefix_len(std::string_view s1, std::string_view s2) {
  auto len = std::min(s1.length(), s2.length());
  size_t i = 0;
  for (; i < len && s1[i] == s2[i]; i++)
    ;
  return i;
}

//-----------------------------------------------------------------------------
// jaro_winkler_distance
//-----------------------------------------------------------------------------

inline double jaro_distance(std::string_view s1, std::string_view s2) {
  std::string cs1;
  if (!common_string(s1, s2, cs1)) return 0;

  std::string cs2;
  if (!common_string(s2, s1, cs2)) return 0;

  double t = 0;
  auto end = std::min(cs1.length(), cs2.length());
  for (size_t i = 0; i < end; i++)
    if (cs1[i] != cs2[i]) t += 1;
  t /= 2;

  auto m = static_cast<double>(cs1.length());

  return ((m / s1.length()) + (m / s2.length()) + ((m - t) / m)) / 3;
}

inline double jaro_winkler_distance(std::string_view s1, std::string_view s2) {
  double dj = jaro_distance(s1, s2);
  if (dj) {
    auto l = static_cast<double>(commn_prefix_len(s1, s2));
    const auto p = 0.1;
    return dj + (l * p * (1.0 - dj));
  }
  return 0.0;
}

//-----------------------------------------------------------------------------
// matcher
//-----------------------------------------------------------------------------

template <typename output_t> class matcher {
public:
  using output_type = output_t;

  matcher(const char *byte_code, size_t byte_code_size)
      : byte_code_(byte_code), byte_code_size_(byte_code_size) {

    if (!header_.read(byte_code, byte_code_size)) { return; }

    if (static_cast<OutputType>(header_.flags.data.output_type) !=
        OutputTraits<output_t>::type()) {
      return;
    }

    is_valid_ = true;
  }

  operator bool() const { return is_valid_; }

  void set_trace(bool on) { trace_ = on; }

  bool contains(std::string_view sv) const {
    return matcher<output_t>::match(sv.data(), sv.size());
  }

protected:
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
    auto output = output_t{};

    auto address = header_.start_address;
    auto i = 0u;
    while (i < len) {
      auto ch = static_cast<uint8_t>(str[i]);
      auto state_output = output_t{};

      auto end = byte_code_ + address;
      auto p = end;

      auto ope = FstOpe(*p--);

      if (ope.has_jump_table()) {
        auto jump_table_element_size = ope.jump_table_element_size();
        size_t jump_table_count = 0;
        auto vb_len = vb_decode_value_reverse(p, jump_table_count);
        p -= vb_len;
        p -= jump_table_count * jump_table_element_size;

        auto jump_table = p;

        auto jump_table_byte_size =
            1 + vb_len + jump_table_count * jump_table_element_size;

        auto base_address = byte_code_ + address - jump_table_byte_size;

        auto get_arc = [&](auto i) -> uint8_t {
          auto p = base_address -
                   lookup_jump_table(jump_table, i, jump_table_element_size);
          auto ope = FstOpe(*p--);
          return read_arc(ope, p);
        };

        auto found = lower_bound_index(0, jump_table_count,
                                       [&](auto i) { return get_arc(i) < ch; });

        if (found < jump_table_count && get_arc(found) == ch) {
          auto offset =
              lookup_jump_table(jump_table, found, jump_table_element_size);
          address -= offset + jump_table_byte_size;
        } else {
          break;
        }
        continue;
      }

      uint8_t arc = read_arc(ope, p);

      auto delta = 0u;
      if (!ope.data.no_address) { p -= vb_decode_value_reverse(p, delta); }

      auto output_suffix = output_t{};
      if (ope.data.has_output) {
        p -= OutputTraits<output_t>::read_byte_value(p, output_suffix);
      }

      if (header_.need_state_output) {
        if (ope.data.has_state_output) {
          p -= OutputTraits<output_t>::read_byte_value(p, state_output);
        }
      }

      auto byte_size = std::distance(p, end);

      auto next_address = 0u;
      if (!ope.data.no_address) {
        if (delta) { next_address = address - byte_size - delta + 1; }
      } else {
        next_address = address - byte_size;
      }

      if (trace_) {
        std::cout << char_to_string(ch) << "\t";
        std::cout << address << "\t";
        std::cout << arc << "\t";
        std::cout << (ope.data.no_address ? "↑" : " ") << ' '
                  << (ope.data.final ? '*' : ' ') << ' '
                  << (ope.data.last_transition ? "‾" : " ") << "\t";

        // Next Address
        if (next_address) {
          std::cout << next_address;
        } else {
          std::cout << "x";
        }
        std::cout << "\t";

        if (ope.data.has_output) { std::cout << output_suffix; }
        std::cout << "\t";

        if (header_.need_state_output) {
          if (ope.data.has_state_output) { std::cout << state_output; }
        }
        std::cout << "\t";

        std::cout << byte_size;
        std::cout << std::endl;
      }

      if (ch == arc) {
        output += output_suffix;
        i++;
        if (ope.data.final) {
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
        if (ope.data.last_transition) { break; }
        address -= byte_size;
      }
    }

    return ret;
  }

  template <typename T, typename U>
  void depth_first_visit(uint32_t address, const std::string &partial_word,
                         const output_t &partial_output, const T &transit,
                         U accept) const {

    while (true) {
      auto state_output = output_t{};

      auto end = byte_code_ + address;
      auto p = end;

      auto ope = FstOpe(*p--);

      if (ope.has_jump_table()) {
        auto jump_table_element_size = ope.jump_table_element_size();
        size_t jump_table_count = 0;
        auto vb_len = vb_decode_value_reverse(p, jump_table_count);
        p -= vb_len;
        p -= jump_table_count * jump_table_element_size;

        address -= std::distance(p, end);
        continue;
      }

      auto arc = read_arc(ope, p);

      auto delta = 0u;
      if (!ope.data.no_address) { p -= vb_decode_value_reverse(p, delta); }

      auto output_suffix = output_t{};
      if (ope.data.has_output) {
        p -= OutputTraits<output_t>::read_byte_value(p, output_suffix);
      }

      if (header_.need_state_output) {
        if (ope.data.has_state_output) {
          p -= OutputTraits<output_t>::read_byte_value(p, state_output);
        }
      }

      auto byte_size = std::distance(p, end);

      auto next_address = 0u;
      if (!ope.data.no_address) {
        if (delta) { next_address = address - byte_size - delta + 1; }
      } else {
        next_address = address - byte_size;
      }

      auto atm = transit; // copy
      atm.step(arc);

      auto word = partial_word + arc;
      auto output = partial_output + output_suffix;

      if (ope.data.final) {
        if (OutputTraits<output_t>::type() != OutputType::none_t) {
          if (OutputTraits<output_t>::empty(state_output)) {
            output += state_output;
          }
        }
        if (atm.is_match()) { accept(word, output); }
      }

      if (!atm.can_match()) { break; }

      if (next_address) {
        depth_first_visit(next_address, word, output, atm, accept);
      }

      if (ope.data.last_transition) { break; }
      address -= byte_size;
    }
  }

  char read_arc(FstOpe ope, const char *&p) const {
    auto index =
        ope.label_index(header_.need_output, header_.need_state_output);
    return index == 0 ? *p-- : header_.char_index[index];
  }

  size_t lookup_jump_table(const char *p, size_t index,
                           size_t element_size) const {
    if (element_size == 2) {
      return reinterpret_cast<const uint16_t *>(p + 1)[index];
    } else {
      return reinterpret_cast<const uint8_t *>(p + 1)[index];
    }
  }

  const char *byte_code_;
  const size_t byte_code_size_;

  FstHeader header_;
  bool is_valid_ = false;
  bool trace_ = false;

  // Suggestion
  template <typename T>
  decltype(auto) suggest_core(std::string_view word, const T &matcher) const {
    using R =
        typename std::conditional<T::has_output,
                                  std::tuple<double, std::string, output_t>,
                                  std::pair<double, std::string>>::type;

    std::vector<R> suggestions;

    auto min_edits = 2;
    auto max_edits = 6;

    for (size_t edits = min_edits; edits <= max_edits; edits++) {
      auto results = matcher.edit_distance_search(word, edits);

      if (results.size() >= 2) {
        for (const auto &result : results) {
          std::string candidate;
          if constexpr (T::has_output) {
            candidate = result.first;
          } else {
            candidate = result;
          }
          if (candidate != word) {
            auto jw = jaro_winkler_distance(word, candidate);
            auto le = levenshtein_distance(word, candidate);
            auto similarity = jw * le;
            if constexpr (T::has_output) {
              suggestions.emplace_back(
                  std::tuple(similarity, candidate, result.second));
            } else {
              suggestions.emplace_back(std::pair(similarity, candidate));
            }
          }
        }

        if (!suggestions.empty()) {
          std::sort(suggestions.begin(), suggestions.end(),
                    [](const auto &a, const auto &b) {
                      return std::get<0>(a) == std::get<0>(b)
                                 ? std::get<1>(a) < std::get<1>(b)
                                 : std::get<0>(a) > std::get<0>(b);
                    });
          break;
        }
      }
    }

    return suggestions;
  }
};

//-----------------------------------------------------------------------------
// LevenshteinAutomaton
//-----------------------------------------------------------------------------

class LevenshteinAutomaton {
public:
  LevenshteinAutomaton(std::string_view sv, size_t max_edits,
                       size_t insert_cost, size_t delete_cost,
                       size_t replace_cost)
      : s_(decode(sv)), max_edits_(max_edits), insert_cost_(insert_cost),
        delete_cost_(delete_cost), replace_cost_(replace_cost) {
    state_.resize(s_.size() + 1);
    std::iota(state_.begin(), state_.end(), 0);
  }

  LevenshteinAutomaton(const LevenshteinAutomaton &rhs) = default;

  void step(char c) {
    u8code_ += c;
    char32_t cp;
    if (!decode_codepoint(u8code_, cp)) { return; }
    u8code_.clear();

    std::vector<size_t> new_state{state_[0] + 1};

    for (size_t i = 0; i < state_.size() - 1; i++) {
      auto cost = (s_[i] == cp) ? 0 : replace_cost_;
      auto edits = std::min({new_state[i] + insert_cost_, state_[i] + cost,
                             state_[i + 1] + delete_cost_});
      new_state.push_back(edits);
    }

    std::transform(new_state.begin(), new_state.end(), state_.begin(),
                   [=](auto edits) { return std::min(edits, max_edits_ + 1); });
  }

  bool is_match() const {
    if (!u8code_.empty()) { return false; }
    return state_.back() <= max_edits_;
  }

  bool can_match() const {
    auto it = std::min_element(state_.begin(), state_.end());
    return *it <= max_edits_;
  }

private:
  std::u32string s_;
  size_t max_edits_;
  size_t insert_cost_;
  size_t delete_cost_;
  size_t replace_cost_; // TODO: better const function is needed?
  std::vector<size_t> state_;
  std::string u8code_;

  bool decode_codepoint(std::string_view s8, char32_t &cp) const {
    auto l = s8.size();
    if (l) {
      uint8_t b = s8[0];
      if ((b & 0x80) == 0) {
        cp = b;
        return true;
      } else if ((b & 0xE0) == 0xC0) {
        if (l >= 2) {
          cp = ((static_cast<char32_t>(s8[0] & 0x1F)) << 6) |
               (static_cast<char32_t>(s8[1] & 0x3F));
          return true;
        }
      } else if ((b & 0xF0) == 0xE0) {
        if (l >= 3) {
          cp = ((static_cast<char32_t>(s8[0] & 0x0F)) << 12) |
               ((static_cast<char32_t>(s8[1] & 0x3F)) << 6) |
               (static_cast<char32_t>(s8[2] & 0x3F));
          return true;
        }
      } else if ((b & 0xF8) == 0xF0) {
        if (l >= 4) {
          cp = ((static_cast<char32_t>(s8[0] & 0x07)) << 18) |
               ((static_cast<char32_t>(s8[1] & 0x3F)) << 12) |
               ((static_cast<char32_t>(s8[2] & 0x3F)) << 6) |
               (static_cast<char32_t>(s8[3] & 0x3F));
          return true;
        }
      }
    }
    return false;
  }

  std::u32string decode(std::string_view s8) const {
    std::u32string out;
    size_t i = 0;
    while (i < s8.size()) {
      auto beg = i++;
      while (i < s8.size() && (s8[i] & 0xc0) == 0x80) {
        i++;
      }
      char32_t cp;
      decode_codepoint(s8.substr(beg, i - beg), cp);
      out += cp;
    }
    return out;
  }
};

//-----------------------------------------------------------------------------
// DummyAutomaton
//-----------------------------------------------------------------------------

struct DummyAutomaton {
  void step(char c) {}
  bool is_match() const { return true; }
  bool can_match() const { return true; }
};

//-----------------------------------------------------------------------------
// map
//-----------------------------------------------------------------------------

template <typename output_t> class map : public matcher<output_t> {
public:
  map(const char *byte_code, size_t byte_code_size)
      : matcher<output_t>(byte_code, byte_code_size) {}

  template <typename T>
  map(const T &byte_code)
      : matcher<output_t>(byte_code.data(), byte_code.size()) {}

  static const bool has_output = true;

  output_t operator[](std::string_view sv) const { return at(sv); }

  output_t operator[](const char *s) const { return at(s); }

  output_t at(std::string_view sv) const {
    auto output = output_t{};
    auto ret = matcher<output_t>::match(sv.data(), sv.size(),
                                        [&](const auto &_) { output = _; });
    if (!ret) { throw std::out_of_range("invalid key..."); }
    return output;
  }

  bool exact_match_search(std::string_view sv, output_t &output) const {
    return matcher<output_t>::match(sv.data(), sv.size(),
                                    [&](const auto &_) { output = _; });
  }

  bool common_prefix_search(
      std::string_view sv,
      std::function<void(size_t, const output_t &)> prefixes) const {
    return matcher<output_t>::match(sv.data(), sv.size(), nullptr, prefixes);
  }

  std::vector<std::pair<size_t, output_t>>
  common_prefix_search(std::string_view sv) const {
    std::vector<std::pair<size_t, output_t>> ret;
    common_prefix_search(sv, [&](size_t length, const output_t &output) {
      ret.emplace_back(std::pair(length, output));
    });
    return ret;
  }

  size_t longest_common_prefix_search(std::string_view sv,
                                      output_t &output) const {
    size_t prefix_len = 0;
    common_prefix_search(sv, [&](size_t len, const auto &_output) {
      prefix_len = len;
      output = _output;
    });
    return prefix_len;
  }

  std::vector<std::pair<std::string, output_t>>
  edit_distance_search(std::string_view sv, size_t max_edits,
                       size_t insert_cost = 1, size_t delete_cost = 1,
                       size_t replace_cost = 1) const {

    std::vector<std::pair<std::string, output_t>> ret;

    if (sv.empty()) { return ret; }

    matcher<output_t>::depth_first_visit(
        matcher<output_t>::header_.start_address, std::string(), output_t{},
        LevenshteinAutomaton(sv, max_edits, insert_cost, delete_cost,
                             replace_cost),
        [&](const auto &word, const auto &output) {
          ret.emplace_back(std::pair(word, output));
        });

    return ret;
  }

  std::vector<std::tuple<double, std::string, output_t>>
  suggest(std::string_view word) const {
    return matcher<output_t>::suggest_core(word, *this);
  }

  template <typename T> void enumerate(T callback) const {
    matcher<output_t>::depth_first_visit(
        matcher<output_t>::header_.start_address, std::string(), output_t{},
        DummyAutomaton(), callback);
  }
};

//-----------------------------------------------------------------------------
// set
//-----------------------------------------------------------------------------

class set : public matcher<none_t> {
public:
  set(const char *byte_code, size_t byte_code_size)
      : matcher<none_t>(byte_code, byte_code_size) {}

  template <typename T>
  set(const T &byte_code)
      : matcher<none_t>(byte_code.data(), byte_code.size()) {}

  static const bool has_output = false;

  bool common_prefix_search(std::string_view sv,
                            std::function<void(size_t)> prefixes) const {
    return matcher<none_t>::match(
        sv.data(), sv.size(), nullptr,
        [&](size_t len, const none_t &) { prefixes(len); });
  }

  std::vector<size_t> common_prefix_search(std::string_view sv) const {
    std::vector<size_t> ret;
    common_prefix_search(sv, [&](size_t length) { ret.push_back(length); });
    return ret;
  }

  size_t longest_common_prefix_search(std::string_view sv) const {
    size_t prefix_len = 0;
    common_prefix_search(sv, [&](size_t len) { prefix_len = len; });
    return prefix_len;
  }

  std::vector<std::string> edit_distance_search(std::string_view sv,
                                                size_t max_edits,
                                                size_t insert_cost = 1,
                                                size_t delete_cost = 1,
                                                size_t replace_cost = 1) const {

    std::vector<std::string> ret;

    if (sv.empty()) { return ret; }

    matcher<none_t>::depth_first_visit(
        matcher<none_t>::header_.start_address, std::string(), none_t{},
        LevenshteinAutomaton(sv, max_edits, insert_cost, delete_cost,
                             replace_cost),
        [&](const auto &word, const auto &) { ret.emplace_back(word); });

    return ret;
  }

  std::vector<std::pair<double, std::string>>
  suggest(std::string_view word) const {
    return matcher<none_t>::suggest_core(word, *this);
  }

  template <typename T> void enumerate(T callback) const {
    matcher<none_t>::depth_first_visit(matcher<none_t>::header_.start_address,
                                       std::string(), none_t{},
                                       DummyAutomaton(), callback);
  }
};

//-----------------------------------------------------------------------------
// decompile
//-----------------------------------------------------------------------------

inline void decompile(const char *byte_code, size_t byte_code_size,
                      std::ostream &out, bool need_output = true) {

  auto type = get_output_type(byte_code, byte_code_size);

  if (type == OutputType::uint32_t) {
    map<uint32_t> matcher(byte_code, byte_code_size);
    if (matcher) {
      matcher.enumerate([&](const auto &word, auto output) {
        if (need_output) {
          out << word << '\t' << output << std::endl;
        } else {
          out << word << std::endl;
        }
      });
    }
  } else if (type == OutputType::uint64_t) {
    map<uint64_t> matcher(byte_code, byte_code_size);
    if (matcher) {
      matcher.enumerate([&](const auto &word, auto output) {
        if (need_output) {
          out << word << '\t' << output << std::endl;
        } else {
          out << word << std::endl;
        }
      });
    }
  } else if (type == OutputType::string) {
    map<std::string> matcher(byte_code, byte_code_size);
    if (matcher) {
      matcher.enumerate([&](const auto &word, auto output) {
        if (need_output) {
          out << word << '\t' << output << std::endl;
        } else {
          out << word << std::endl;
        }
      });
    }
  } else if (type == OutputType::none_t) {
    set matcher(byte_code, byte_code_size);
    if (matcher) {
      matcher.enumerate(
          [&](const auto &word, auto output) { out << word << std::endl; });
    }
  }
}

template <typename T>
inline void decompile(const T &byte_code, std::ostream &out,
                      bool need_output = true) {
  decompile(byte_code.data(), byte_code.size(), out, need_output);
}

} // namespace fst
