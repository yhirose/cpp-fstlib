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
#include <iostream>
#include <list>
#include <map>
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

template <typename Val>
inline size_t vb_encode_value_reverse(Val n, char *out) {
  auto len = vb_encode_value(n, out);
  for (size_t i = 0; i < len / 2; i++) {
    std::swap(out[i], out[len - i - 1]);
  }
  return len;
}

template <typename Val>
inline size_t vb_decode_value(const char *data, Val &n) {
  auto p = (const uint8_t *)data;
  size_t len = 0;
  n = 0;
  size_t cnt = 0;
  while (p[len] < 128) {
    n += (p[len++] << (7 * cnt++));
  }
  n += (p[len++] - 128) << (7 * cnt);
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
// state machine
//-----------------------------------------------------------------------------

template <typename output_t> struct OutputTraits {};

template <> struct OutputTraits<uint32_t> {
  using value_type = uint32_t;

  static value_type initial_value() { return 0; }

  static bool empty(value_type val) { return val == 0; }

  static std::string to_string(value_type val) { return std::to_string(val); }

  static void prepend_value(value_type &base, value_type val) { base += val; }

  static value_type get_suffix(value_type a, value_type b) { return a - b; }

  static value_type get_common_previx(value_type a, value_type b) {
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
    auto vb_len = vb_encode_value_length(val);
    std::vector<char> vb(vb_len, 0);
    vb_encode_value_reverse(val, vb.data());
    os.write(vb.data(), vb.size());
  }

  static size_t read_byte_value(const char *p, value_type &val) {
    return vb_decode_value_reverse(p, val);
  }

  static size_t skip_byte_value(const char *p) {
    value_type val;
    return vb_decode_value_reverse(p, val);
  }
};

template <> struct OutputTraits<std::string> {
  using value_type = std::string;

  static value_type initial_value() { return value_type(); }

  static bool empty(const value_type &val) { return val.empty(); }

  static value_type to_string(const value_type &val) { return val; }

  static void prepend_value(value_type &base, const value_type &val) {
    base.insert(0, val);
  }

  static value_type get_suffix(const value_type &a, const value_type &b) {
    return a.substr(b.size());
  }

  static value_type get_common_previx(const value_type &a,
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
    OutputTraits<uint32_t>::write_byte_value(os, val.size());
  }

  static size_t read_byte_value(const char *p, value_type &val) {
    uint32_t str_len = 0;
    auto vb_len = OutputTraits<uint32_t>::read_byte_value(p, str_len);

    val.resize(str_len);
    memcpy(val.data(), p - vb_len - str_len + 1, str_len);

    return vb_len + str_len;
  }

  static size_t skip_byte_value(const char *p) {
    uint32_t str_len = 0;
    auto vb_len = OutputTraits<uint32_t>::read_byte_value(p, str_len);
    return vb_len + str_len;
  }
};

template <typename output_t, typename Cont>
inline std::string join(const Cont &cont, const char *delm) {
  std::string s;
  for (auto i = 0u; i < cont.size(); i++) {
    if (i != 0) { s += delm; }
    s += OutputTraits<output_t>::to_string(cont[i]);
  }
  return s;
}

template <typename output_t> class State {
public:
  using pointer = State *;

  struct Transition {
    pointer state;
    output_t output;

    bool operator==(const Transition &rhs) const {
      if (this != &rhs) { return state == rhs.state && output == rhs.output; }
      return true;
    }
  };

  class Transitions {
  public:
    std::vector<char> arcs;
    std::vector<Transition> states_and_outputs;
    std::vector<std::string> text;

    bool operator==(const Transitions &rhs) const {
      if (this != &rhs) {
        return arcs == rhs.arcs && states_and_outputs == rhs.states_and_outputs;
      }
      return true;
    }

    size_t size() const { return arcs.size(); }
    bool empty() const { return !size(); }

    int get_index(char arc) const {
      for (size_t i = 0; i < arcs.size(); i++) {
        if (arcs[i] == arc) { return static_cast<int>(i); }
      }
      return -1;
    }

    pointer next_state(const char *start, const char *end,
                       size_t &read_bytes) const {
      auto p = start;
      auto arc = *p++;
      auto idx = get_index(arc);
      if (idx != -1) {
        const auto &s = text[idx];
        auto size = static_cast<size_t>(end - p);
        if (s.empty() || (s.size() <= size && s == std::string(p, s.size()))) {
          read_bytes = s.size() + 1;
          return states_and_outputs[idx].state;
        }
      }
      return nullptr;
    }

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

    template <typename Functor> void for_each_with_text(Functor fn) const {
      for (auto i = 0u; i < arcs.size(); i++) {
        fn(arcs[i], states_and_outputs[i], i, text[i]);
      }
    }

    template <typename Functor> void for_each_reverse(Functor fn) const {
      for (auto i = arcs.size(); i > 0; i--) {
        auto idx = i - 1;
        fn(arcs[idx], states_and_outputs[idx], idx);
      }
    }

    template <typename Functor> void for_each_reverse_text(Functor fn) const {
      for (auto i = arcs.size(); i > 0; i--) {
        auto idx = i - 1;
        fn(arcs[idx], states_and_outputs[idx], idx, text[idx]);
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
      text.clear();
    }

    void set_transition(char arc, pointer state) {
      auto idx = get_index(arc);
      if (idx == -1) {
        idx = static_cast<int>(arcs.size());
        arcs.push_back(arc);
        states_and_outputs.emplace_back(Transition());
        text.push_back(std::string());
      }
      states_and_outputs[idx].state = state;
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

    friend class State;
  };

  size_t id;
  bool final = false;
  Transitions transitions;
  std::vector<output_t> state_outputs;
  size_t parent_count = 0;
  size_t ref_count = 0;

  State(size_t id) : id(id) {}

  pointer next_state(const char *p, const char *end, size_t &read_bytes) const {
    return transitions.next_state(p, end, read_bytes);
  }

  const output_t &output(char arc) const { return transitions.output(arc); }

  bool operator==(const State &rhs) const {
    if (this != &rhs) {
      return final == rhs.final && transitions == rhs.transitions &&
             state_outputs == rhs.state_outputs;
    }
    return true;
  }

  uint64_t hash() const;

  void set_final(bool final) { this->final = final; }

  void set_transition(char arc, pointer state) {
    transitions.set_transition(arc, state);
  }

  void set_output(char arc, const output_t &output) {
    transitions.set_output(arc, output);
  }

  void prepend_suffix_to_output(char arc, const output_t &suffix) {
    transitions.insert_output(arc, suffix);
  }

  void push_to_state_outputs(const output_t &output) {
    if (state_outputs.empty()) {
      // NOTE: The following code makes good performance...
      state_outputs.push_back(0);
    }
    state_outputs.push_back(output);
  }

  void prepend_suffix_to_state_outputs(const output_t &suffix) {
    if (state_outputs.empty()) {
      // NOTE: The following code makes good performance...
      state_outputs.push_back(suffix);
    } else {
      for (auto &output : state_outputs) {
        OutputTraits<output_t>::prepend_value(output, suffix);
      }
    }
  }

  void reuse(size_t state_id) {
    id = state_id;
    set_final(false);
    transitions.clear();
    state_outputs.clear();
    parent_count = 0;
    ref_count = 0;
  }

  static pointer New(std::unordered_set<pointer> &object_pool,
                     size_t state_id = -1) {
    auto p = new State(state_id);
    object_pool.insert(p);
    return p;
  }

private:
  State(const State &) = delete;
  State(State &&) = delete;
};

template <typename output_t> inline uint64_t State<output_t>::hash() const {
  char buff[1024]; // TOOD: large enough?
  size_t buff_len = 0;

  if (final) {
    for (const auto &output : state_outputs) {
      buff_len += OutputTraits<output_t>::write_value(buff, buff_len, output);
      buff[buff_len++] = '\t';
    }
  }

  buff[buff_len++] = '\0';

  transitions.for_each([&](char arc, const State::Transition &t, size_t i) {
    buff[buff_len++] = arc;
    auto val = static_cast<uint32_t>(t.state->id);
    memcpy(&buff[buff_len], &val, sizeof(val));
    buff_len += sizeof(val);
    buff_len += OutputTraits<output_t>::write_value(buff, buff_len, t.output);
    buff[buff_len++] = '\t';
  });

  return MurmurHash64B(buff, buff_len, 0);
}

template <typename output_t> class StateMachine {
public:
  size_t last_id;
  size_t count;
  State<output_t> *root;

  StateMachine(std::unordered_set<State<output_t> *> &&object_pool, size_t id,
               State<output_t> *root)
      : object_pool_(object_pool), last_id(id), count(id), root(root) {}
  ~StateMachine() {
    for (auto p : object_pool_) {
      delete p;
    }
    object_pool_.clear();
  }

private:
  StateMachine(const StateMachine &) = delete;
  StateMachine(StateMachine &&) = delete;
  std::unordered_set<State<output_t> *> object_pool_;
};

template <typename output_t> class Dictionary {
public:
  Dictionary() {}

  Dictionary(std::unordered_set<State<output_t> *> &object_pool)
      : object_pool_(&object_pool) {}

  bool exist(uint64_t key, State<output_t> *state) const {
    auto it = buckets_.find(key);
    if (it != buckets_.end()) {
      auto &l = it->second;
      for (auto [st, dummy] : l) {
        if (*st == *state) { return true; }
      }
    }
    return false;
  }

  State<output_t> *get(uint64_t key, State<output_t> *state) {
    auto it = buckets_.find(key);
    if (it != buckets_.end()) {
      auto &bucket = it->second;
      auto bucket_iter = bucket.begin();
      while (bucket_iter != bucket.end()) {
        auto [st, any_val] = *bucket_iter;
        if (*st == *state) {
          bucket.splice(bucket.begin(), bucket, bucket_iter);
          auto index_iter = std::any_cast<typename Index::iterator>(any_val);
          index_.splice(index_.begin(), index_, index_iter);
          return st;
        }
      }
    }
    return nullptr;
  }

  void put(uint64_t key, State<output_t> *state) {
    static auto dummy = index_.begin();
    auto &bucket = buckets_[key];

    bucket.push_front(std::make_pair(state, dummy));
    auto bucket_iter = bucket.begin();

    index_.push_front(std::make_pair(key, bucket_iter));
    auto index_iter = index_.begin();

    bucket_iter->second = index_iter;

    if (index_.size() > 10000) {
      // TODO: Use rbegin()...
      auto index_iter = index_.end();
      --index_iter;

      size_t i = 0;
      for (;;) {
        const auto &[key, any_value] = *index_iter;
        auto bucket_iter = std::any_cast<typename Bucket::iterator>(any_value);
        auto st = (*bucket_iter).first;

        if (st->ref_count == 0) {
          if (object_pool_) {
            object_pool_->erase(st);
            delete st;

            buckets_[key].erase(bucket_iter);
            index_.erase(index_iter);
          }
          break;
        }

        --index_iter;
        if (index_iter == index_.begin()) { break; }
      }
    }
  }

private:
  using Index = std::list<std::pair<
      uint64_t,
      typename std::list<std::pair<State<output_t> *, std::any>>::iterator>>;
  using Bucket = std::list<std::pair<State<output_t> *, std::any>>;
  using Buckets = std::unordered_map<uint64_t, Bucket>;

  std::unordered_set<State<output_t> *> *object_pool_ = nullptr;
  Buckets buckets_;
  Index index_;
};

template <typename output_t>
inline std::pair<bool, State<output_t> *>
find_minimized(State<output_t> *state, Dictionary<output_t> &dictionary) {
  auto h = state->hash();

  if (dictionary.exist(h, state)) {
    auto st = dictionary.get(h, state);
    st->ref_count++;
    st->transitions.for_each(
        [&](char arc, auto &t, size_t i) { t.state->ref_count--; });
    return std::make_pair(true, st);
  }

  // NOTE: COPY_STATE is very expensive...
  dictionary.put(h, state);
  state->ref_count++;
  state->transitions.for_each(
      [&](char arc, auto &t, size_t i) { t.state->ref_count--; });
  return std::make_pair(false, state);
};

template <typename output_t>
inline void get_common_prefix_and_word_suffix(const output_t &current_output,
                                              const output_t &output,
                                              output_t &common_prefix,
                                              output_t &word_suffix) {
  common_prefix =
      OutputTraits<output_t>::get_common_previx(output, current_output);
  word_suffix = OutputTraits<output_t>::get_suffix(output, common_prefix);
}

template <typename output_t, typename Input>
inline std::shared_ptr<StateMachine<output_t>> make_state_machine(Input input) {
  std::unordered_set<State<output_t> *> object_pool;
  Dictionary<output_t> dictionary;
  size_t next_state_id = 0;

  // Main algorithm ported from the technical paper
  std::vector<State<output_t> *> temp_states;
  std::string previous_word;
  temp_states.push_back(State<output_t>::New(object_pool, next_state_id++));

  input([&](const std::string &current_word, output_t current_output) {
    // The following loop caluculates the length of the longest common
    // prefix of 'current_word' and 'previous_word'
    auto prefix_length = get_prefix_length(previous_word, current_word);

    // We minimize the states from the suffix of the previous word
    State<output_t> *prev_state = nullptr;
    for (auto i = previous_word.size(); i > prefix_length; i--) {
      auto [found, state] =
          find_minimized<output_t>(temp_states[i], dictionary);
      if (found) {
        next_state_id--;
        if (prev_state) { prev_state->parent_count--; }
      } else {
        // Ownership of the object in temp_states[i] has been moved to the
        // dictionary...
        temp_states[i] = State<output_t>::New(object_pool);
      }

      state->parent_count++;
      auto arc = previous_word[i - 1];
      temp_states[i - 1]->set_transition(arc, state);

      prev_state = state;
    }

    // This loop initializes the tail states for the current word
    for (auto i = prefix_length + 1; i <= current_word.size(); i++) {
      assert(i <= temp_states.size());
      if (i == temp_states.size()) {
        temp_states.push_back(
            State<output_t>::New(object_pool, next_state_id++));
      } else {
        temp_states[i]->reuse(next_state_id++);
      }
      auto arc = current_word[i - 1];
      temp_states[i - 1]->set_transition(arc, temp_states[i]);
    }

    if (current_word != previous_word) {
      auto state = temp_states[current_word.size()];
      state->set_final(true);
      // NOTE: The following code causes bad performance...
      // state->push_to_state_outputs("");
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
  });

  // Here we are minimizing the states of the last word
  State<output_t> *prev_state = nullptr;
  for (auto i = static_cast<int>(previous_word.size()); i >= 0; i--) {
    auto [found, state] = find_minimized<output_t>(temp_states[i], dictionary);
    if (found) {
      next_state_id--;
      if (prev_state) { prev_state->parent_count--; }
    }

    if (i > 0) {
      state->parent_count++;
      auto arc = previous_word[i - 1];
      temp_states[i - 1]->set_transition(arc, state);
    }
    prev_state = state;
  }

  return std::make_shared<StateMachine<output_t>>(std::move(object_pool),
                                                  next_state_id, prev_state);
}

template <typename output_t>
inline std::shared_ptr<StateMachine<output_t>>
make_state_machine(const std::vector<std::pair<std::string, output_t>> &input) {
  return make_state_machine<output_t>([&](const auto &add_entry) {
    for (const auto &item : input) {
      add_entry(item.first, item.second);
    }
  });
}

template <class output_t> inline bool connectable(State<output_t> *state) {
  if (state->final) { return false; }
  if (state->transitions.size() > 1) { return false; }
  if (state->parent_count > 1) { return false; }
  return true;
}

template <class output_t>
inline void optimize_core(StateMachine<output_t> &sm, State<output_t> *state,
                          std::set<size_t> &check) {
  auto id = state->id;

  if (check.find(id) != check.end()) { return; }
  check.insert(id);

  for (auto i = 0u; i < state->transitions.arcs.size(); i++) {
    auto &t = state->transitions.states_and_outputs[i];
    while (connectable<output_t>(t.state)) {
      state->transitions.text[i] += t.state->transitions.arcs[0];
      t.state = t.state->transitions.states_and_outputs[0].state;
      sm.count--;
    }

    optimize_core<output_t>(sm, t.state, check);
  }
}

template <typename output_t> inline void optimize(StateMachine<output_t> &sm) {
  std::set<size_t> check;
  optimize_core<output_t>(sm, sm.root, check);
}

//-----------------------------------------------------------------------------
// virtual machine
//-----------------------------------------------------------------------------

const size_t DEFAULT_MIN_ARCS_FOR_JUMP_TABLE = 6;

union Ope {
  enum OpeType { ArcNotFinal = 0, ArcFinal, ArcFinalWithStateOutput, Jmp };

  enum JumpOffsetType { JumpOffsetNone = 0, JumpOffsetZero, JumpOffsetCurrent };

  enum OutputLengthType {
    OutputLengthNone = 0,
    OutputLengthOne,
    OutputLengthTwo,
    OutputLength
  };

  struct {
    unsigned type : 2;
    unsigned has_text : 1;
    unsigned last_transition : 1;
    unsigned output_length_type : 2;
    unsigned jump_offset_type : 2;
  } arc;

  struct {
    unsigned type : 2;
    unsigned need_2byte : 1;
    unsigned reserve : 5;
  } jmp;

  uint8_t byte;
};

inline bool is_ope_arc(Ope ope) { return ope.jmp.type != Ope::Jmp; }

inline bool is_ope_jmp(Ope ope) { return ope.jmp.type == Ope::Jmp; }

inline bool is_ope_final(Ope ope) {
  assert(ope.arc.type != Ope::Jmp);
  return ope.arc.type != Ope::ArcNotFinal;
}

inline bool has_ope_state_outputs(Ope ope) {
  return ope.arc.type == Ope::ArcFinalWithStateOutput;
}

template <typename output_t> struct Command {
  // General
  size_t id = -1;
  size_t next_id = -1;

  bool is_arc;

  // Arc
  bool final;
  bool last_transition;
  char arc;
  std::string text;
  Ope::OutputLengthType output_length_type;
  output_t output;
  std::vector<output_t> state_outputs;
  Ope::JumpOffsetType jump_offset_type;
  size_t jump_offset;
  bool use_jump_table;

  // Jmp
  std::vector<int16_t> arc_jump_offsets;

  size_t output_length(uint32_t output) const {
    if (output > 0xffff) {
      return sizeof(output_t);
    } else if (output > 0xff) {
      return sizeof(uint16_t);
    } else if (output > 0) {
      return sizeof(uint8_t);
    }
    return 0;
  }

  size_t output_length(const std::string &output) const {
    size_t size = 0;
    if (output.size() > 2) { size += sizeof(uint8_t); }
    size += output.size();
    return size;
  }

  size_t state_output_length(uint32_t state_output) const {
    return sizeof(state_output);
  }

  size_t state_output_length(const std::string &state_output) const {
    return sizeof(uint8_t) + state_output.size();
  }

  size_t byte_code_size() const {
    // ope
    auto size = sizeof(uint8_t);

    if (is_arc) {
      // arc
      if (!use_jump_table) { size += sizeof(uint8_t); }

      // text
      if (!text.empty()) {
        size += sizeof(uint8_t);
        size += text.size();
      }

      // output
      size += output_length(output);

      // state_outputs
      if (has_state_outputs()) {
        size += sizeof(uint8_t);
        for (const auto &state_output : state_outputs) {
          size += state_output_length(state_output);
        }
      }

      // jump_offset
      if (jump_offset_type == Ope::JumpOffsetCurrent) {
        size += vb_encode_value_length(jump_offset);
      }
    } else { // type == Jmp
      // arc_jump_offsets
      bool need_2byte;
      size_t start;
      size_t end;
      scan_arc_jump_offsets(need_2byte, start, end);

      if (need_2byte) {
        size += 2 + sizeof(int16_t) * (end - start);
      } else {
        size += 2 + sizeof(uint8_t) * (end - start);
      }
    }

    return size;
  }

  void write_output(std::vector<char> &byte_code, uint32_t output) const {
    if (output > 0xffff) {
      byte_code.insert(byte_code.end(), reinterpret_cast<const char *>(&output),
                       reinterpret_cast<const char *>(&output) +
                           sizeof(output));
    } else if (output > 0xff) {
      uint16_t val = output;
      byte_code.insert(byte_code.end(), reinterpret_cast<const char *>(&val),
                       reinterpret_cast<const char *>(&val) + sizeof(val));
    } else if (output > 0) {
      uint8_t val = output;
      byte_code.insert(byte_code.end(), reinterpret_cast<const char *>(&val),
                       reinterpret_cast<const char *>(&val) + sizeof(val));
    }
  }

  void write_output(std::vector<char> &byte_code,
                    const std::string &output) const {
    if (output.size() > 2) { byte_code.push_back((char)output.size()); }
    byte_code.insert(byte_code.end(), output.begin(), output.end());
  }

  void write_state_output(std::vector<char> &byte_code,
                          uint32_t state_output) const {
    byte_code.insert(
        byte_code.end(), reinterpret_cast<const char *>(&state_output),
        reinterpret_cast<const char *>(&state_output) + sizeof(state_output));
  }

  void write_state_output(std::vector<char> &byte_code,
                          const std::string &state_output) const {
    byte_code.push_back((char)state_output.size());
    byte_code.insert(byte_code.end(), state_output.begin(), state_output.end());
  }

  void write_byte_code(std::vector<char> &byte_code) const {
    auto save_size = byte_code.size();

    if (is_arc) {
      // ope
      assert(final || (!final && !has_state_outputs()));
      auto t = Ope::ArcNotFinal;
      if (final) {
        if (has_state_outputs()) {
          t = Ope::ArcFinalWithStateOutput;
        } else {
          t = Ope::ArcFinal;
        }
      }
      Ope ope = {static_cast<unsigned>(t), !text.empty(), last_transition,
                 static_cast<unsigned>(output_length_type),
                 static_cast<unsigned>(jump_offset_type)};
      byte_code.push_back(ope.byte);

      // arc
      if (!use_jump_table) { byte_code.push_back(arc); }

      // text
      if (!text.empty()) {
        byte_code.push_back((char)text.size());
        byte_code.insert(byte_code.end(), text.begin(), text.end());
      }

      // output
      write_output(byte_code, output);

      // state_outputs
      if (has_state_outputs()) {
        byte_code.push_back((char)state_outputs.size());
        for (const auto &state_output : state_outputs) {
          write_state_output(byte_code, state_output);
        }
      }

      // jump_offset
      if (jump_offset_type == Ope::JumpOffsetCurrent) {
        char vb[9]; // To hold 64 bits value
        auto vb_len = vb_encode_value(jump_offset, vb);
        byte_code.insert(byte_code.end(), vb, vb + vb_len);
      }
    } else { // type == Jmp
      bool need_2byte;
      size_t start;
      size_t end;
      scan_arc_jump_offsets(need_2byte, start, end);

      // ope
      Ope ope = {Ope::Jmp, need_2byte, 0};
      byte_code.push_back(ope.byte);

      // arc_jump_offsets
      auto count = end - start;
      byte_code.push_back((unsigned char)start);
      byte_code.push_back(
          (unsigned char)(count - 1)); // count is stored from 0 to 255
      if (need_2byte) {
        auto p =
            (const char *)arc_jump_offsets.data() + (sizeof(int16_t) * start);
        auto table_size = sizeof(int16_t) * count;
        byte_code.insert(byte_code.end(), p, p + table_size);
      } else {
        for (auto i = start; i < end; i++) {
          auto offset = arc_jump_offsets[i];
          byte_code.push_back((unsigned char)offset);
        }
      }
    }

    assert(byte_code.size() - save_size == byte_code_size());
  }

private:
  bool has_state_outputs() const { return !state_outputs.empty(); }

  void scan_arc_jump_offsets(bool &need_2byte, size_t &start,
                             size_t &end) const {
    need_2byte = false;
    start = -1;
    end = -1;
    for (auto i = 0; i < 256; i++) {
      auto offset = arc_jump_offsets[i];
      if (offset != -1) {
        if (offset >= 0xff) { need_2byte = true; }
        if (start == -1) { start = i; }
        end = i + 1;
      }
    }
  }
};

template <class output_t> using Commands = std::vector<Command<output_t>>;

template <class output_t>
inline size_t compile_core(State<output_t> *state, Commands<output_t> &commands,
                           std::vector<size_t> &state_positions,
                           size_t position, size_t min_arcs_for_jump_table) {
  assert(state->transitions.arcs.size() > 0);

  if (state_positions[state->id]) { return position; }

  struct Helper {
    static size_t output_length(uint32_t output) {
      auto output_len = 0;
      if (output > 0xffff) {
        output_len = sizeof(output_t);
      } else if (output > 0xff) {
        output_len = sizeof(uint16_t);
      } else if (output > 0) {
        output_len = sizeof(uint8_t);
      }
      return output_len;
    }

    static size_t output_length(const std::string &output) {
      return output.size();
    }
  };

  auto arcs_count = state->transitions.arcs.size();

  state->transitions.for_each_reverse(
      [&](char arc, const typename State<output_t>::Transition &t, size_t i) {
        auto next_state = t.state;
        if (next_state->transitions.arcs.size() > 0) {
          position = compile_core(next_state, commands, state_positions,
                                  position, min_arcs_for_jump_table);
        }
      });

  auto use_jump_table = (arcs_count >= min_arcs_for_jump_table);

  size_t arc_positions[256];
  memset(arc_positions, -1, sizeof(arc_positions));

  state->transitions.for_each_reverse_text(
      [&](char arc, const typename State<output_t>::Transition &t, size_t i,
          const std::string &text) {
        auto next_state = t.state;

        Command<output_t> cmd;
        cmd.is_arc = true;
        cmd.id = state->id;
        cmd.next_id = next_state->id;
        cmd.final = next_state->final;
        cmd.last_transition = (i + 1 == arcs_count);
        cmd.arc = arc;
        cmd.text = text;
        cmd.output = t.output;

        auto output_len = Helper::output_length(t.output);
        if (output_len == 0) {
          cmd.output_length_type = Ope::OutputLengthNone;
        } else if (output_len == 1) {
          cmd.output_length_type = Ope::OutputLengthOne;
        } else if (output_len == 2) {
          cmd.output_length_type = Ope::OutputLengthTwo;
        } else {
          cmd.output_length_type = Ope::OutputLength;
        }

        cmd.state_outputs = next_state->state_outputs;

        if (next_state->transitions.arcs.size() == 0) {
          cmd.jump_offset_type = Ope::JumpOffsetNone;
          cmd.jump_offset = 0;
        } else {
          auto offset = position - state_positions[next_state->id];
          if (offset == 0) {
            cmd.jump_offset_type = Ope::JumpOffsetZero;
          } else {
            cmd.jump_offset_type = Ope::JumpOffsetCurrent;
          }
          cmd.jump_offset = offset;
        }

        cmd.use_jump_table = use_jump_table;

        position += cmd.byte_code_size();
        commands.emplace_back(std::move(cmd));
        arc_positions[(uint8_t)arc] = position;
      });

  if (use_jump_table) {
    Command<output_t> cmd;
    cmd.is_arc = false;
    cmd.id = state->id;
    cmd.next_id = -1;
    cmd.arc_jump_offsets.assign(256, -1);
    for (auto i = 0; i < 256; i++) {
      if (arc_positions[i] != -1) {
        auto offset = position - arc_positions[i];
        cmd.arc_jump_offsets[i] = (int16_t)offset;
      }
    }

    position += cmd.byte_code_size();
    commands.emplace_back(std::move(cmd));
  }

  state_positions[state->id] = position;
  return position;
}

template <typename output_t>
inline std::vector<char>
compile(const StateMachine<output_t> &sm,
        size_t min_arcs_for_jump_table = DEFAULT_MIN_ARCS_FOR_JUMP_TABLE) {
  std::vector<char> byte_code;

  Commands<output_t> commands;
  std::vector<size_t> state_positions(sm.last_id);
  compile_core(sm.root, commands, state_positions, 0, min_arcs_for_jump_table);

  auto rit = commands.rbegin();
  while (rit != commands.rend()) {
    rit->write_byte_code(byte_code);
    ++rit;
  }

  return byte_code;
}

template <typename output_t, typename Input>
inline std::vector<char>
build(Input input,
      size_t min_arcs_for_jump_table = DEFAULT_MIN_ARCS_FOR_JUMP_TABLE) {
  auto sm = make_state_machine<output_t>(input);
  optimize(*sm);
  return compile(*sm, min_arcs_for_jump_table);
}

template <typename Input>
inline std::vector<char>
build(Input input,
      size_t min_arcs_for_jump_table = DEFAULT_MIN_ARCS_FOR_JUMP_TABLE) {
  return build<uint32_t>(
      [&](const auto &add_entry) {
        size_t i = 0;
        input([&](const std::string &item) {
          add_entry(item, static_cast<uint32_t>(i++));
        });
      },
      min_arcs_for_jump_table);
}

template <typename output_t>
inline std::vector<char>
build(const std::vector<std::pair<std::string, output_t>> &input,
      size_t min_arcs_for_jump_table = DEFAULT_MIN_ARCS_FOR_JUMP_TABLE) {
  return build<output_t>(
      [&](const auto &add_entry) {
        for (const auto &item : input) {
          add_entry(item.first, item.second);
        }
      },
      min_arcs_for_jump_table);
}

inline std::vector<char>
build(const std::vector<std::string> &input,
      size_t min_arcs_for_jump_table = DEFAULT_MIN_ARCS_FOR_JUMP_TABLE) {
  return build<uint32_t>(
      [&](const auto &add_entry) {
        size_t i = 0;
        for (const auto &item : input) {
          add_entry(item, static_cast<uint32_t>(i++));
        }
      },
      min_arcs_for_jump_table);
}

template <typename output_t> struct Helper_read_byte_code_arc {};

template <> struct Helper_read_byte_code_arc<uint32_t> {
  static const char *read_output_length(const char *p, size_t &output_len) {
    output_len = sizeof(uint32_t);
    return p;
  }

  static const char *skip_state_outputs(const char *p,
                                        size_t state_outputs_size) {
    if (state_outputs_size == 1) {
      p += sizeof(uint32_t);
    } else {
      for (auto i = 0u; i < state_outputs_size; i++) {
        p += sizeof(uint32_t);
      }
    }
    return p;
  }
};

template <> struct Helper_read_byte_code_arc<std::string> {
  static const char *read_output_length(const char *p, size_t &output_len) {
    output_len = (uint8_t)*p;
    return ++p;
  }

  static const char *skip_state_outputs(const char *p,
                                        size_t state_outputs_size) {
    if (state_outputs_size == 1) {
      p += sizeof(uint8_t) + (uint8_t)*p;
    } else {
      for (auto i = 0u; i < state_outputs_size; i++) {
        p += sizeof(uint8_t) + (uint8_t)*p;
      }
    }
    return p;
  }
};

template <typename output_t>
inline const char *
read_byte_code_arc(Ope ope, const char *p, const char *end, size_t &text_len,
                   const char *&text, size_t &output_len, const char *&output,
                   size_t &state_outputs_size, const char *&state_output,
                   Ope::JumpOffsetType &jump_offset_type, size_t &jump_offset) {
  // state_outputs
  text_len = 0;
  // if (!(is_ope_final(ope)) && has_ope_state_outputs(ope)) {
  if (ope.arc.has_text) {
    text_len = (uint8_t)*p++;
    text = p;
    p += text_len;
  }

  // output
  auto output_length_type = (Ope::OutputLengthType)ope.arc.output_length_type;
  if (output_length_type == Ope::OutputLengthNone) {
    output_len = 0;
  } else if (output_length_type == Ope::OutputLengthOne) {
    output_len = 1;
    output = p;
  } else if (output_length_type == Ope::OutputLengthTwo) {
    output_len = 2;
    output = p;
  } else { // Ope::OutputLength
    p = Helper_read_byte_code_arc<output_t>::read_output_length(p, output_len);
    output = p;
  }
  p += output_len;

  // state_outputs
  if (is_ope_final(ope) && has_ope_state_outputs(ope)) {
    state_outputs_size = (uint8_t)*p++;
    state_output = p;
    p = Helper_read_byte_code_arc<output_t>::skip_state_outputs(
        p, state_outputs_size);
  } else {
    state_outputs_size = 0;
  }

  // jump_offset
  jump_offset = 0;
  jump_offset_type = (Ope::JumpOffsetType)ope.arc.jump_offset_type;
  if (jump_offset_type == Ope::JumpOffsetCurrent) {
    p += vb_decode_value(p, jump_offset);
  }

  return p;
}

inline const char *read_byte_code_jmp(Ope ope, uint8_t arc, const char *p,
                                      const char *end, int32_t &jump_offset) {
  auto start = (uint8_t)*p++;
  auto count = ((uint8_t)*p++) + 1; // count is stored from 0 to 255

  if (ope.jmp.need_2byte) {
    if (start <= arc && arc < start + count) {
      jump_offset = *(((const int16_t *)p) + (arc - start));
    } else {
      jump_offset = -1;
    }
    p += sizeof(int16_t) * count;
  } else {
    if (start <= arc && arc < start + count) {
      jump_offset = *(((const uint8_t *)p) + (arc - start));
      if (jump_offset == (uint8_t)-1) { jump_offset = -1; }
    } else {
      jump_offset = -1;
    }
    p += count;
  }

  return p;
}

inline const char *skip_byte_code_jmp(Ope ope, const char *p, const char *end,
                                      std::list<uint8_t> &arcs) {
  auto start = (uint8_t)*p++;
  auto count = ((uint8_t)*p++) + 1; // count is stored from 0 to 255

  for (auto i = 0; i < count; i++) {
    int32_t jump_offset = -1;
    if (ope.jmp.need_2byte) {
      jump_offset = *(((const int16_t *)p) + i);
    } else {
      jump_offset = *(((const uint8_t *)p) + i);
      if (jump_offset == (uint8_t)-1) { jump_offset = -1; }
    }
    if (jump_offset != -1) { arcs.push_back(start + i); }
  }

  if (ope.jmp.need_2byte) {
    p += sizeof(int16_t) * count;
  } else {
    p += count;
  }

  return p;
}

template <typename output_t> struct Helper_run {};

template <> struct Helper_run<uint32_t> {
  static void read_output(uint32_t &buff, const char *output,
                          size_t output_len) {
    if (output_len == 0) {
      ;
    } else if (output_len == 1) {
      auto val = *reinterpret_cast<const uint8_t *>(output);
      buff += val;
    } else if (output_len == 2) {
      auto val = *reinterpret_cast<const uint16_t *>(output);
      buff += val;
    } else {
      assert(output_len == sizeof(uint32_t));
      auto val = *reinterpret_cast<const uint32_t *>(output);
      buff += val;
    }
  }

  static const char *read_state_output(uint32_t &buff,
                                       const char *state_output) {
    buff += *reinterpret_cast<const uint32_t *>(state_output);
    return state_output + sizeof(uint32_t);
  }
};

template <> struct Helper_run<std::string> {
  static void read_output(std::string &buff, const char *output,
                          size_t output_len) {
    if (output_len == 1) {
      buff += *output;
    } else if (output_len > 1) {
      buff.append(output, output_len);
    }
  }

  static const char *read_state_output(std::string &buff,
                                       const char *state_output) {
    size_t state_output_len = (uint8_t)*state_output++;
    buff.append(state_output, state_output_len);
    return state_output + state_output_len;
  }
};

template <typename output_t, typename Begin, typename Value, typename End>
inline void run(const char *byte_code, size_t size, const char *str,
                Begin output_begin, Value output_value, End output_end) {
  auto prefix = OutputTraits<output_t>::initial_value();

  auto p = byte_code;
  auto end = byte_code + size;
  auto pstr = str;
  auto use_jump_table = false;

  while (*pstr && p < end) {
    auto arc = (uint8_t)*pstr;
    Ope ope;
    ope.byte = *p++;

    if (is_ope_arc(ope)) {
      auto expected_arc = use_jump_table ? arc : (uint8_t)*p++;

      size_t text_len;
      const char *text;
      size_t output_len;
      const char *output;
      size_t state_outputs_size;
      const char *state_output;
      Ope::JumpOffsetType jump_offset_type;
      size_t jump_offset;

      p = read_byte_code_arc<output_t>(ope, p, end, text_len, text, output_len,
                                       output, state_outputs_size, state_output,
                                       jump_offset_type, jump_offset);

      if (arc == expected_arc) {
        Helper_run<output_t>::read_output(prefix, output, output_len);

        if (ope.arc.has_text) {
          for (size_t i = 0; i < text_len; i++) {
            pstr++;
            auto arc = (uint8_t)*pstr;
            auto expected_arc = (uint8_t)text[i];
            if (arc != expected_arc) { return; }
          }
        }

        pstr++;
        if (is_ope_final(ope)) {
          output_begin(pstr);

          // NOTE: for better state_outputs compression
          if (state_outputs_size == 0) {
            output_value(prefix);
          } else {
            for (auto i = 0u; i < state_outputs_size; i++) {
              auto final_state_output = prefix;

              state_output = Helper_run<output_t>::read_state_output(
                  final_state_output, state_output);

              output_value(final_state_output);
            }
          }

          if (output_end()) { return; }
        }

        if (jump_offset_type == Ope::JumpOffsetNone) {
          return;
        } else if (jump_offset_type == Ope::JumpOffsetCurrent) {
          p += jump_offset;
        }
      } else {
        if (ope.arc.last_transition) { return; }
      }

      use_jump_table = false;
    } else { // Jmp
      int32_t jump_offset;
      p = read_byte_code_jmp(ope, arc, p, end, jump_offset);
      if (jump_offset == -1) { return; }
      p += jump_offset;

      use_jump_table = true;
    }
  }

  return;
}

template <typename output_t, typename Callback>
inline bool exact_match_search(const char *byte_code, size_t size,
                               const char *str, Callback callback) {
  bool ret = false;

  run<output_t>(
      byte_code, size, str,
      // begin
      [&](const char *pstr) { ret = (*pstr == '\0'); },
      // value
      [&](const output_t &val) {
        if (ret) { callback(val); }
      },
      // end
      [&]() { return ret; });

  return ret;
}

template <typename output_t>
inline std::vector<output_t> exact_match_search(const char *byte_code,
                                                size_t size, const char *str) {
  std::vector<output_t> outputs;
  fst::exact_match_search<output_t>(
      byte_code, size, str,
      [&](const output_t &val) { outputs.emplace_back(val); });
  return outputs;
}

template <typename output_t>
inline bool exact_match_search(const char *byte_code, size_t size,
                               const char *str, output_t &output) {
  return fst::exact_match_search<output_t>(
      byte_code, size, str, [&](const output_t &val) { output = val; });
}

template <typename output_t> struct CommonPrefixSearchResult {
  size_t length;
  std::vector<output_t> outputs;
};

template <typename output_t, typename Callback>
inline void common_prefix_search(const char *byte_code, size_t size,
                                 const char *str, Callback callback) {
  CommonPrefixSearchResult<output_t> result;

  run<output_t>(
      byte_code, size, str,
      // begin
      [&](const char *pstr) {
        result.length = pstr - str;
        result.outputs.clear();
      },
      // value
      [&](const output_t &val) { result.outputs.emplace_back(val); },
      // end
      [&]() {
        callback(result);
        return false;
      });
}

template <typename output_t>
inline std::vector<CommonPrefixSearchResult<output_t>>
common_prefix_search(const char *byte_code, size_t size, const char *str) {
  std::vector<CommonPrefixSearchResult<output_t>> ret;
  fst::common_prefix_search<output_t>(
      byte_code, size, str,
      [&](const auto &result) { ret.emplace_back(result); });
  return ret;
}

template <typename output_t, typename Value>
inline void run_all(const char *byte_code, size_t size, int32_t offset,
                    std::string prefix_key, output_t prefix_output,
                    Value output_value) {
  auto p = byte_code + offset;
  auto end = byte_code + size;

  Ope ope;
  ope.byte = *p++;

  std::list<uint8_t> arcs;
  if (is_ope_jmp(ope)) {
    p = skip_byte_code_jmp(ope, p, end, arcs);
  } else {
    p--;
  }

  while (p < end) {
    ope.byte = *p++;
    assert(is_ope_arc(ope));

    uint8_t arc = 0;
    if (arcs.empty()) {
      arc = (uint8_t)*p++;
    } else {
      arc = arcs.front();
      arcs.pop_front();
    }

    auto prefix_key_new = prefix_key + (char)arc;

    size_t text_len;
    const char *text;
    size_t output_len;
    const char *output;
    size_t state_outputs_size;
    const char *state_output;
    Ope::JumpOffsetType jump_offset_type;
    size_t jump_offset;

    p = read_byte_code_arc<output_t>(ope, p, end, text_len, text, output_len,
                                     output, state_outputs_size, state_output,
                                     jump_offset_type, jump_offset);

    if (ope.arc.has_text) { prefix_key_new.append(text, text_len); }

    auto prefix_output_new = prefix_output;
    Helper_run<output_t>::read_output(prefix_output_new, output, output_len);

    if (is_ope_final(ope)) {
      // NOTE: for better state_outputs compression
      if (state_outputs_size == 0) {
        output_value(prefix_key_new, prefix_output_new);
      } else {
        for (auto i = 0u; i < state_outputs_size; i++) {
          auto final_state_output = prefix_output_new;

          state_output = Helper_run<output_t>::read_state_output(
              final_state_output, state_output);

          output_value(prefix_key_new, final_state_output);
        }
        p = state_output;
      }
    }

    if (jump_offset_type == Ope::JumpOffsetZero) {
      run_all<output_t>(byte_code, size, static_cast<int32_t>(p - byte_code),
                        prefix_key_new, prefix_output_new, output_value);
    } else if (jump_offset_type == Ope::JumpOffsetCurrent) {
      auto p2 = p + jump_offset;
      run_all<output_t>(byte_code, size, static_cast<int32_t>(p2 - byte_code),
                        prefix_key_new, prefix_output_new, output_value);
    }

    if (ope.arc.last_transition) { return; }
  }

  return;
}

template <typename output_t, typename Value>
inline void decompile(const char *byte_code, size_t size, Value output_value) {
  run_all<output_t>(byte_code, size, 0, std::string(),
                    OutputTraits<output_t>::initial_value(), output_value);
}

//-----------------------------------------------------------------------------
// formatter
//-----------------------------------------------------------------------------

template <typename output_t>
inline void
dump(const StateMachine<output_t> &sm, std::ostream &os,
     size_t min_arcs_for_jump_table = DEFAULT_MIN_ARCS_FOR_JUMP_TABLE) {
  Commands<output_t> commands;
  std::vector<size_t> state_positions(sm.last_id);
  compile_core(sm.root, commands, state_positions, 0, min_arcs_for_jump_table);

  os << "Ope\tID\tArc\t\tNextID\tLast\tFinal\tOutput\tStOuts\tAddr\tSize\tJmpOf"
        "f\tNxtAdr\tJpOffTy\tJpOffSz\t\n";
  os << "------\t------\t--------------\t------\t------\t------\t------\t------"
        "\t------\t------\t------\t------\t------\t------\n";

  size_t addr = 0;

  auto rit = commands.rbegin();
  while (rit != commands.rend()) {
    const auto &cmd = *rit;
    auto size = cmd.byte_code_size();
    size_t jump_offset = 0;
    if (cmd.is_arc) {
      size_t next_addr = -1;
      if (cmd.jump_offset_type == Ope::JumpOffsetZero) {
        next_addr = addr + size;
      } else if (cmd.jump_offset_type == Ope::JumpOffsetCurrent) {
        next_addr = addr + size + cmd.jump_offset;
      }

      size_t jump_offset_bytes = 0;
      if (cmd.jump_offset_type == Ope::JumpOffsetCurrent) {
        jump_offset_bytes = vb_encode_value_length(cmd.jump_offset);
        jump_offset = cmd.jump_offset;
      }

      os << "Arc"
         << "\t";
      os << (cmd.id == -1 ? "" : std::to_string(cmd.id)) << "\t";
      os << cmd.arc;
      if (!cmd.text.empty()) { os << cmd.text; }
      os << "\t";
      if (cmd.text.size() < 7) { os << "\t"; }
      os << (cmd.next_id == -1 ? "" : std::to_string(cmd.next_id)) << "\t";
      os << (cmd.last_transition ? "_" : " ") << "\t";
      os << (cmd.final ? "f" : " ") << "\t";
      os << (OutputTraits<output_t>::empty(cmd.output)
                 ? ""
                 : OutputTraits<output_t>::to_string(cmd.output))
         << "\t";
      os << join<output_t>(cmd.state_outputs, "/") << "\t";
      os << addr << "\t";
      os << size << "\t";
      os << (int)jump_offset << "\t";
      os << (int)next_addr << "\t";
      os << cmd.jump_offset_type << "\t";
      os << jump_offset_bytes << std::endl;
    } else { // Jmp
      auto next_addr = addr + size;

      os << "Jmp"
         << "\t";
      os << (cmd.id == -1 ? "" : std::to_string(cmd.id)) << "\t";
      os << "\t\t\t\t\t\t\t";
      os << addr << "\t";
      os << size << "\t\t";
      os << (int)next_addr << std::endl;
    }
    addr += size;
    ++rit;
  }
  std::cerr << "Total size: " << addr << std::endl;
}

template <typename output_t>
inline void dot_core(State<output_t> *state, std::set<size_t> &check,
                     std::ostream &os) {
  auto id = state->id;

  if (check.find(id) != check.end()) { return; }
  check.insert(id);

  if (state->final) {
    auto state_outputs = join<output_t>(state->state_outputs, "|");
    os << "  s" << id << " [ shape = doublecircle, xlabel = \"" << state_outputs
       << "\" ];" << std::endl;
  } else {
    os << "  s" << id << " [ shape = circle ];" << std::endl;
  }

  state->transitions.for_each_with_text(
      [&](char arc, const typename State<output_t>::Transition &t, size_t i,
          const std::string &text) {
        std::string label;
        label += arc;
        if (!text.empty()) { label += text; }
        os << "  s" << id << "->s" << t.state->id << " [ label = \"" << label;
        if (!OutputTraits<output_t>::empty(t.output)) { os << "/" << t.output; }
        os << "\" ];" << std::endl;
      });

  state->transitions.for_each(
      [&](char arc, const typename State<output_t>::Transition &t, size_t i) {
        dot_core<output_t>(t.state, check, os);
      });
}

template <typename output_t>
inline void dot(const StateMachine<output_t> &sm, std::ostream &os) {
  os << "digraph{" << std::endl;
  os << "  rankdir = LR;" << std::endl;
  std::set<size_t> check;
  dot_core<output_t>(sm.root, check, os);
  os << "}" << std::endl;
}

//-----------------------------------------------------------------------------
// state machine interpreter - slow...
//-----------------------------------------------------------------------------

template <typename output_t>
inline std::vector<output_t>
exact_match_search(const StateMachine<output_t> &sm, const std::string s) {
  auto state = sm.root;
  auto prefix = OutputTraits<output_t>::initial_value();

  auto p = s.data();
  auto end = p + s.size();

  while (p < end) {
    auto arc = *p;
    size_t read_bytes = 0;
    auto next_state = state->next_state(p, end, read_bytes);
    if (!next_state) { return std::vector<output_t>(); }
    prefix += state->output(arc);
    state = next_state;
    p += read_bytes;
  }

  if (state->final && p == end) {
    std::vector<output_t> ret;
    if (!state->state_outputs.empty()) {
      for (const auto &suffix : state->state_outputs) {
        ret.push_back(prefix + suffix);
      }
    } else if (!OutputTraits<output_t>::empty(prefix)) {
      ret.push_back(prefix);
    }
    return ret;
  } else {
    return std::vector<output_t>();
  }
}

//-----------------------------------------------------------------------------
// new implementation
//-----------------------------------------------------------------------------

enum class ContainerType { Set, Map };
enum class ValueType { Uint32, String };

template <typename output_t> struct FstTraits {};

template <> struct FstTraits<uint32_t> {
  static ValueType get_value_type() { return ValueType::Uint32; }
};

template <> struct FstTraits<std::string> {
  static ValueType get_value_type() { return ValueType::String; }
};

struct FstHeader {
  static const size_t CHAR_INDEX_SIZE = 8;

  uint8_t container_type = 0;
  uint8_t value_type = 0;
  uint16_t reserved = 0;
  uint32_t start_address = 0;
  char char_index[CHAR_INDEX_SIZE] = {0};

  FstHeader() {}

  FstHeader(ContainerType container_type, ValueType value_type,
            size_t start_address, const std::map<char, size_t> &char_index_map)
      : container_type(static_cast<uint8_t>(container_type)),
        value_type(static_cast<uint8_t>(value_type)),
        start_address(start_address) {
    for (auto [ch, index] : char_index_map) {
      if (index < CHAR_INDEX_SIZE) { char_index[index] = ch; }
    }
  }

  void write(std::ostream &os) {
    os.write(reinterpret_cast<const char *>(this), sizeof(*this));
  }
};

template <typename output_t> struct FstRecord {
  union {
    struct {
      unsigned no_address : 1;
      unsigned last_transition : 1;
      unsigned final : 1;
      unsigned has_output : 1;
      unsigned has_state_output : 1;
      unsigned label_index : 3;
    } data;
    uint8_t byte;
  } flags;

  char label = 0;
  size_t delta = 0;
  const output_t *output = nullptr;
  const output_t *state_output = nullptr;

  size_t byte_size() const {
    size_t sz = 1;
    if (flags.data.label_index == 0) { sz += 1; }
    if (!flags.data.no_address) { sz += vb_encode_value_length(delta); }
    if (flags.data.has_output) {
      sz += OutputTraits<output_t>::get_byte_value_size(*output);
    }
    if (flags.data.has_state_output) {
      sz += OutputTraits<output_t>::get_byte_value_size(*state_output);
    }
    return sz;
  }

  void write(std::ostream &os) {
    if (flags.data.has_state_output) {
      OutputTraits<output_t>::write_byte_value(os, *state_output);
    }
    if (flags.data.has_output) {
      OutputTraits<output_t>::write_byte_value(os, *output);
    }
    if (!flags.data.no_address) {
      auto vb_len = vb_encode_value_length(delta);
      std::vector<char> vb(vb_len, 0);
      vb_encode_value_reverse(delta, vb.data());
      os.write(vb.data(), vb.size());
    }
    if (flags.data.label_index == 0) { os << label; }
    os.write(reinterpret_cast<const char *>(&flags.byte), sizeof(flags.byte));
  }
};

template <typename output_t, typename Input> class ByteCodeBuilder {
public:
  std::ostream &os_;
  bool need_output_ = true;
  size_t trace_ = true;

  std::map<char, size_t> char_index_map_;

  size_t record_index_ = 0;
  std::map<size_t, size_t> record_index_map_;

  size_t address_ = 0;
  std::map<size_t, size_t> address_map_;

  size_t total_byte_size_ = 0;

  ByteCodeBuilder(const Input &input, std::ostream &os, bool need_output,
                  bool trace)
      : os_(os), need_output_(need_output), trace_(trace) {

    intialize_char_index_map_(input);

    if (trace_) {
      std::cout << "Address\tArc\tN F L\tNxtAddr";
      if (need_output_) { std::cout << "\tOutput\tStOuts"; }
      std::cout << std::endl;
      std::cout << "-------\t------\t-----\t-------";
      if (need_output_) { std::cout << "\t------\t------"; }
      std::cout << std::endl;
    }
  }

  ~ByteCodeBuilder() {
    auto container_type =
        need_output_ ? ContainerType::Map : ContainerType::Set;
    auto start_byte_adress = address_map_[record_index_ - 1];

    FstHeader header(container_type, FstTraits<output_t>::get_value_type(),
                     start_byte_adress, char_index_map_);

    header.write(os_);

    if (trace_) {
      std::cout << "# byte code size: " << total_byte_size_ + sizeof(header)
                << std::endl;
    }
  }

  void write(const State<output_t> *state) {
    const auto &transitions = state->transitions;

    transitions.for_each_reverse([&](char arc, const auto &t, size_t i) {
      auto has_address =
          record_index_map_.find(t.state->id) != record_index_map_.end();
      auto last_transition = transitions.size() - 1 == i;
      auto no_address = last_transition && has_address &&
                        record_index_map_[t.state->id] == record_index_ - 1;

      FstRecord<output_t> rec;
      rec.flags.data.no_address = no_address;
      rec.flags.data.last_transition = last_transition;
      rec.flags.data.final = t.state->final;

      auto index = char_index_map_[arc];
      if (index < FstHeader::CHAR_INDEX_SIZE) {
        rec.flags.data.label_index = index;
      } else {
        rec.flags.data.label_index = 0;
        rec.label = arc;
      }

      rec.delta = 0;
      if (!no_address) {
        if (has_address) {
          rec.delta = address_ - address_map_[record_index_map_[t.state->id]];
        }
      }

      rec.flags.data.has_output = false;
      if (need_output_) {
        if (!OutputTraits<output_t>::empty(t.output)) {
          rec.flags.data.has_output = true;
          rec.output = &t.output;
        }
      }

      rec.flags.data.has_state_output = false;
      if (need_output_) {
        if (!t.state->state_outputs.empty()) {
          if (!OutputTraits<output_t>::empty(t.state->state_outputs[0])) {
            rec.flags.data.has_state_output = true;
            rec.state_output = &t.state->state_outputs[0];
          }
        }
      }

      rec.write(os_);

      auto byte_size = rec.byte_size();
      auto accessible_address = address_ + byte_size - 1;

      address_map_[record_index_] = accessible_address;

      if (trace_) {
        // Byte address
        std::cout << address_map_[record_index_] << "\t";

        // Arc
        {
          if (arc < 0x20) {
            std::cout << std::hex << (int)(uint8_t)arc << std::dec;
          } else {
            std::cout << arc;
          }
          std::cout << "\t";
        }

        // Flags
        {
          std::cout << (no_address ? "↑" : " ") << ' '
                    << (t.state->final ? '*' : ' ') << ' '
                    << (last_transition ? "‾" : " ") << "\t";
        }

        // Next Address
        {
          if (!no_address) {
            if (rec.delta > 0) {
              std::cout << address_ - rec.delta;
            } else {
              std::cout << "x";
            }
          }
          std::cout << "\t";
        }

        // Output
        {
          if (need_output_) {
            if (!OutputTraits<output_t>::empty(t.output)) {
              std::cout << t.output;
            }
          }
          std::cout << "\t";
        }

        // State Output
        {
          if (need_output_) {
            if (!t.state->state_outputs.empty()) {
              if (!OutputTraits<output_t>::empty(t.state->state_outputs[0])) {
                std::cout << t.state->id << ":" << t.state->state_outputs[0];
              }
            }
          }
          std::cout << "\t";
        }

        std::cout << std::endl;
      }

      total_byte_size_ += byte_size;

      record_index_ += 1;
      address_ += byte_size;
    });

    if (!state->transitions.empty()) {
      record_index_map_[state->id] = record_index_ - 1;
    }
  }

private:
  void intialize_char_index_map_(const Input &input) {
    char_index_map_.emplace('\0', 0);

    std::map<char, size_t> char_count;
    for (const auto &[word, _] : input) {
      for (auto ch : word) {
        char_count[ch]++;
      }
    }

    struct second_order {
      bool operator()(const std::pair<char, size_t> &x,
                      const std::pair<char, size_t> &y) const {
        return x.second < y.second;
      }
    };

    std::priority_queue<std::pair<char, size_t>,
                        std::vector<std::pair<char, size_t>>, second_order>
        que;

    for (auto x : char_count) {
      que.push(x);
    }

    while (!que.empty()) {
      auto [ch, count] = que.top();
      char_index_map_[ch] = char_index_map_.size();
      que.pop();
    }
  }
};

enum class Result { Success, EmptyKey, UnsortedKey, DuplicateKey };

template <typename output_t, typename Input>
inline std::pair<Result, size_t> make_fst(const Input &input, std::ostream &os,
                                          bool need_output,
                                          bool trace = false) {

  ByteCodeBuilder<output_t, Input> builder(input, os, need_output, trace);

  std::unordered_set<State<output_t> *> object_pool;
  Dictionary<output_t> dictionary(object_pool);
  size_t next_state_id = 0;
  size_t line = 1;
  Result result = Result::Success;

  // Main algorithm ported from the technical paper
  std::vector<State<output_t> *> temp_states;
  std::string previous_word;
  temp_states.push_back(State<output_t>::New(object_pool, next_state_id++));

  for (const auto &[current_word, _current_output] : input) {
    auto current_output = _current_output;

    if (current_word.empty()) {
      result = Result::EmptyKey;
      return std::make_pair(result, line);
    }

    // The following loop caluculates the length of the longest common
    // prefix of 'current_word' and 'previous_word'
    // auto prefix_length = get_prefix_length(previous_word, current_word);
    size_t prefix_length;
    if (!get_prefix_length(previous_word, current_word, prefix_length)) {
      result = Result::UnsortedKey;
      return std::make_pair(result, line);
    }

    if (previous_word.size() == current_word.size() &&
        previous_word == current_word) {
      result = Result::DuplicateKey;
      return std::make_pair(result, line);
    }

    // We minimize the states from the suffix of the previous word
    for (auto i = previous_word.size(); i > prefix_length; i--) {
      auto [found, state] =
          find_minimized<output_t>(temp_states[i], dictionary);

      if (found) {
        next_state_id--;
      } else {
        builder.write(state);

        // Ownership of the object in temp_states[i] has been moved to the
        // dictionary...
        temp_states[i] = State<output_t>::New(object_pool);
      }

      auto arc = previous_word[i - 1];
      temp_states[i - 1]->set_transition(arc, state);
    }

    // This loop initializes the tail states for the current word
    for (auto i = prefix_length + 1; i <= current_word.size(); i++) {
      assert(i <= temp_states.size());
      if (i == temp_states.size()) {
        temp_states.push_back(
            State<output_t>::New(object_pool, next_state_id++));
      } else {
        temp_states[i]->reuse(next_state_id++);
      }
      auto arc = current_word[i - 1];
      temp_states[i - 1]->set_transition(arc, temp_states[i]);
    }

    if (current_word != previous_word) {
      auto state = temp_states[current_word.size()];
      state->set_final(true);
      // NOTE: The following code causes bad performance...
      // state->push_to_state_outputs("");
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
  }

  // Here we are minimizing the states of the last word
  for (auto i = static_cast<int>(previous_word.size()); i >= 0; i--) {
    auto [found, state] = find_minimized<output_t>(temp_states[i], dictionary);

    if (found) {
      next_state_id--;
    } else {
      builder.write(state);
    }

    if (i > 0) {
      auto arc = previous_word[i - 1];
      temp_states[i - 1]->set_transition(arc, state);
    }
  }

  for (auto p : object_pool) {
    delete p;
  }

  return std::make_pair(Result::Success, line);
}

template <typename output_t> class container {
public:
  container(const char *byte_code, size_t byte_code_size, bool need_output)
      : byte_code_(byte_code), byte_code_size_(byte_code_size),
        need_output_(need_output) {

    if (byte_code_size < sizeof(header_)) { return; }

    auto p = byte_code_ + (byte_code_size - sizeof(header_));
    memcpy(reinterpret_cast<char *>(&header_), p, sizeof(header_));

    if (static_cast<ContainerType>(header_.container_type) !=
        (need_output ? ContainerType::Map : ContainerType::Set)) {
      return;
    }

    if (static_cast<ValueType>(header_.value_type) !=
        FstTraits<output_t>::get_value_type()) {
      return;
    }

    is_valid_ = true;
  }

  bool is_valid() const { return is_valid_; }

  void set_trace(bool on) { trace_ = on; }

  bool query(const char *str, size_t len, output_t &value) const {
    auto ret = false;
    auto address = header_.start_address;

    if (trace_) {
      std::cout << "Char\tAddress\tArc\tN F L\tNxtAddr\tOutput\tStOuts"
                << std::endl;
      std::cout << "----\t-------\t---\t-----\t-------\t------\t------"
                << std::endl;
    }

    size_t i = 0;
    while (i < len) {
      auto ch = str[i];
      auto end = byte_code_ + address;
      auto p = end;

      FstRecord<output_t> rec;
      rec.flags.byte = *p--;

      auto index = rec.flags.data.label_index;
      char arc = 0;
      if (index == 0) {
        arc = *p--;
      } else {
        arc = header_.char_index[index];
      }

      size_t delta = 0;
      if (!rec.flags.data.no_address) {
        p -= vb_decode_value_reverse(p, delta);
      }

      auto output = OutputTraits<output_t>::initial_value();
      if (rec.flags.data.has_output) {
        p -= OutputTraits<output_t>::read_byte_value(p, output);
      }

      auto state_output = OutputTraits<output_t>::initial_value();
      if (rec.flags.data.has_state_output) {
        p -= OutputTraits<output_t>::read_byte_value(p, state_output);
      }

      auto byte_size = std::distance(p, end);

      auto next_address = 0;
      if (rec.flags.data.no_address) {
        next_address = address - byte_size;
      } else {
        if (delta) { next_address = address - byte_size - delta + 1; }
      }

      if (trace_) {
        std::cout << ch << "\t";
        std::cout << address << "\t";
        std::cout << arc << "\t";
        std::cout << (rec.flags.data.no_address ? "↑" : " ") << ' '
                  << (rec.flags.data.final ? '*' : ' ') << ' '
                  << (rec.flags.data.last_transition ? "‾" : " ") << "\t";

        // Next Address
        if (!rec.flags.data.no_address) {
          if (delta) {
            std::cout << next_address;
          } else {
            std::cout << "x";
          }
        }
        std::cout << "\t";

        if (rec.flags.data.has_output) { std::cout << output; }
        std::cout << "\t";

        if (rec.flags.data.has_state_output) { std::cout << state_output; }

        std::cout << std::endl;
      }

      if (ch == arc) {
        value += output;
        i++;
        ret = rec.flags.data.final && i == len;
        if (ret) { value += state_output; }
        if (!next_address) { break; }
        address = next_address;
      } else {
        if (rec.flags.data.last_transition) { break; }
        address = address - byte_size;
      }
    }

    return ret;
  }

private:
  const char *byte_code_;
  size_t byte_code_size_;
  bool need_output_ = false;

  FstHeader header_;
  bool is_valid_ = false;
  bool trace_ = false;
};

} // namespace fst

#endif // CPPFSTLIB_FSTLIB_H_
