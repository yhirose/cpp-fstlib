//
//  fstlib.h
//
//  Copyright (c) 2015 Yuji Hirose. All rights reserved.
//  MIT License
//

#ifndef _CPPFSTLIB_FSTLIB_H_
#define _CPPFSTLIB_FSTLIB_H_

#include <algorithm>
#include <cassert>
#include <cstring>
#include <iostream>
#include <map>
#include <memory>
#include <numeric>
#include <set>
#include <string>
#include <unordered_map>
#include <vector>

namespace fst {

//-----------------------------------------------------------------------------
// state machine
//-----------------------------------------------------------------------------

// MurmurHash64B - 64-bit MurmurHash2 for 32-bit platforms
// URL:: https://github.com/aappleby/smhasher/blob/master/src/MurmurHash2.cpp
// License: Public Domain

inline uint64_t MurmurHash64B(const void* key, size_t len, uint64_t seed) {
  const uint32_t m = 0x5bd1e995;
  const size_t r = 24;

  uint32_t h1 = uint32_t(seed) ^ len;
  uint32_t h2 = uint32_t(seed >> 32);

  const uint32_t* data = (const uint32_t*)key;

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
    case 3:
      h2 ^= ((unsigned char*)data)[2] << 16;
    case 2:
      h2 ^= ((unsigned char*)data)[1] << 8;
    case 1:
      h2 ^= ((unsigned char*)data)[0];
      h2 *= m;
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

inline size_t get_prefix_length(const std::string& s1, const std::string& s2) {
  size_t i = 0;
  while (i < s1.length() && i < s2.length() && s1[i] == s2[i]) {
    i++;
  }
  return i;
}

template <typename output_t>
struct OutputTraits {};

template <>
struct OutputTraits<uint32_t> {
  typedef uint32_t value_type;

  static value_type initial_value() { return 0; }

  static bool empty(value_type val) { return val == 0; }

  static std::string to_string(value_type val) { return std::to_string(val); }

  static void prepend_value(value_type& base, value_type val) { base += val; }

  static value_type get_suffix(value_type a, value_type b) { return a - b; }

  static value_type get_common_previx(value_type a, value_type b) {
    return std::min(a, b);
  }

  static void write_value(char* buff, size_t& buff_len, value_type val) {
    memcpy(&buff[buff_len], &val, sizeof(val));
    buff_len += sizeof(val);
  }
};

template <>
struct OutputTraits<std::string> {
  typedef std::string value_type;

  static value_type initial_value() { return value_type(); }

  static bool empty(const value_type& val) { return val.empty(); }

  static value_type to_string(const value_type& val) { return val; }

  static void prepend_value(value_type& base, const value_type& val) {
    base.insert(0, val);
  }

  static value_type get_suffix(const value_type& a, const value_type& b) {
    return a.substr(b.size());
  }

  static value_type get_common_previx(const value_type& a,
                                      const value_type& b) {
    return a.substr(0, get_prefix_length(a, b));
  }

  static void write_value(char* buff, size_t& buff_len, const value_type& val) {
    memcpy(&buff[buff_len], val.data(), val.size());
    buff_len += val.size();
  }
};

template <typename output_t>
class State {
 public:
  typedef State* pointer;

  struct Transition {
    pointer state;
    output_t output;

    bool operator==(const Transition& rhs) const {
      if (this != &rhs) {
        return state == rhs.state && output == rhs.output;
      }
      return true;
    }
  };

  class Transitions {
   public:
    std::vector<char> arcs;
    std::vector<Transition> states_and_outputs;

    bool operator==(const Transitions& rhs) const {
      if (this != &rhs) {
        return arcs == rhs.arcs && states_and_outputs == rhs.states_and_outputs;
      }
      return true;
    }

    int get_index(char arc) const {
      for (size_t i = 0; i < arcs.size(); i++) {
        if (arcs[i] == arc) {
          return i;
        }
      }
      return -1;
    }

    const Transition& state_and_output(char arc) const {
      auto idx = get_index(arc);
      return states_and_outputs[idx];
    }

    pointer next_state(char arc) const { return state_and_output(arc).state; }

    const output_t& output(char arc) const {
      return state_and_output(arc).output;
    }

    template <typename Functor>
    void for_each(Functor fn) const {
      for (auto i = 0u; i < arcs.size(); i++) {
        fn(arcs[i], states_and_outputs[i], i);
      }
    }

    template <typename Functor>
    void for_each_reverse(Functor fn) const {
      for (auto i = arcs.size(); i > 0; i--) {
        auto idx = i - 1;
        fn(arcs[idx], states_and_outputs[idx], idx);
      }
    }

    template <typename Functor>
    void for_each_arc(Functor fn) const {
      for (auto arc : arcs) {
        fn(arc);
      }
    }

   private:
    void clear() {
      states_and_outputs.clear();
      arcs.clear();
    }

    void set_transition(char arc, pointer state) {
      auto idx = get_index(arc);
      if (idx == -1) {
        idx = arcs.size();
        arcs.push_back(arc);
        states_and_outputs.emplace_back(Transition());
      }
      states_and_outputs[idx].state = state;
    }

    void set_output(char arc, const output_t& val) {
      auto idx = get_index(arc);
      states_and_outputs[idx].output = val;
    }

    void insert_output(char arc, const output_t& val) {
      auto idx = get_index(arc);
      auto& output = states_and_outputs[idx].output;
      OutputTraits<output_t>::prepend_value(output, val);
    }

    friend class State;
  };

  size_t id;
  bool in_dictionary;
  bool final;
  Transitions transitions;
  std::vector<output_t> state_outputs;

  State(size_t id) : id(id), in_dictionary(false), final(false) {}

  pointer next_state(char arc) const { return transitions.next_state(arc); }

  const output_t& output(char arc) const { return transitions.output(arc); }

  bool operator==(const State& rhs) const {
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

  void set_output(char arc, const output_t& output) {
    transitions.set_output(arc, output);
  }

  void prepend_suffix_to_output(char arc, const output_t& suffix) {
    transitions.insert_output(arc, suffix);
  }

  void push_to_state_outputs(const output_t& output) {
    if (state_outputs.empty()) {
      // NOTE: The following code makes good performance...
      state_outputs.push_back(0);
    }
    state_outputs.push_back(output);
  }

  void prepend_suffix_to_state_outputs(const output_t& suffix) {
    if (state_outputs.empty()) {
      // NOTE: The following code makes good performance...
      state_outputs.push_back(suffix);
    } else {
      for (auto& state_output : state_outputs) {
        OutputTraits<output_t>::prepend_value(state_output, suffix);
      }
    }
  }

  void reuse(size_t state_id) {
    id = state_id;
    set_final(false);
    transitions.clear();
    state_outputs.clear();
  }

  static pointer New(std::vector<pointer>& objects_, size_t state_id = -1) {
    auto p = new State(state_id);
    objects_.push_back(p);
    return p;
  }

 private:
  State(const State&) = delete;
  State(State&&) = delete;
};

template <typename output_t>
inline uint64_t State<output_t>::hash() const {
  char buff[1024];  // TOOD: large enough?
  size_t buff_len = 0;

  if (final) {
    for (const auto& state_output : state_outputs) {
      OutputTraits<output_t>::write_value(buff, buff_len, state_output);
      buff[buff_len++] = '\t';
    }
  }

  buff[buff_len++] = '\n';

  transitions.for_each([&](char arc, const State::Transition& t, size_t i) {
    buff[buff_len++] = arc;
    uint32_t val = t.state->id;
    memcpy(&buff[buff_len], &val, sizeof(val));
    buff_len += sizeof(val);
    OutputTraits<output_t>::write_value(buff, buff_len, t.output);
    buff[buff_len++] = '\t';
  });

  return MurmurHash64B(buff, buff_len, 0);
}

template <typename output_t>
class StateMachine {
 public:
  size_t count;
  typename State<output_t>::pointer root;

  StateMachine(
      std::vector<typename State<output_t>::pointer>&& objects,
      size_t count,
      typename State<output_t>::pointer root)
      : objects_(objects), count(count), root(root) {}
  ~StateMachine() {
    for (auto p : objects_) {
      delete p;
    }
    objects_.clear();
  }

 private:
  StateMachine(const StateMachine&) = delete;
  StateMachine(StateMachine&&) = delete;
  std::vector<typename State<output_t>::pointer> objects_;
};

// NOTE: unordered_set is not used here, because it uses size_t as hash value
// which becomes 32-bit on 32-bit platforms. But we want to use 64-bit hash
// value.
template <typename output_t>
using Dictionary =
    std::unordered_map<uint64_t, typename State<output_t>::pointer>;

template <typename output_t>
inline typename State<output_t>::pointer find_minimized(
    typename State<output_t>::pointer state, Dictionary<output_t>& dictionary,
    size_t& state_id, bool& found) {
  auto h = state->hash();

  auto it = dictionary.find(h);
  if (it != dictionary.end()) {
    if (*it->second == *state) {
      state_id--;
      found = true;
      return it->second;
    }
  }

  // NOTE: COPY_STATE is very expensive...
  state->in_dictionary = true;
  dictionary[h] = state;
  found = false;
  return state;
};

template <typename output_t>
inline void get_common_prefix_and_word_suffix(const output_t& current_output,
                                              const output_t& output,
                                              output_t& common_prefix,
                                              output_t& word_suffix) {
  common_prefix =
      OutputTraits<output_t>::get_common_previx(output, current_output);
  word_suffix = OutputTraits<output_t>::get_suffix(output, common_prefix);
}

template <typename output_t, typename Input>
inline std::shared_ptr<StateMachine<output_t>> make_state_machine(Input input) {
  Dictionary<output_t> dictionary;
  std::vector<typename State<output_t>::pointer> objects;
  size_t state_id = 0;

  // Main algorithm ported from the technical paper
  std::vector<typename State<output_t>::pointer> temp_states;
  std::string previous_word;
  temp_states.push_back(State<output_t>::New(objects, state_id++));

  input([&](const std::string& current_word, output_t current_output) {
    // The following loop caluculates the length of the longest common
    // prefix of 'current_word' and 'previous_word'
    auto prefix_length = get_prefix_length(previous_word, current_word);

    // We minimize the states from the suffix of the previous word
    for (auto i = previous_word.length(); i > prefix_length; i--) {
      auto arc = previous_word[i - 1];
      bool found;
      auto state =
          find_minimized<output_t>(temp_states[i], dictionary, state_id, found);
      if (!found) {
        // Ownership of the object in temp_states[i] has been moved to the
        // dictionary...
        temp_states[i] = State<output_t>::New(objects);
      }
      temp_states[i - 1]->set_transition(arc, state);
    }

    // This loop initializes the tail states for the current word
    for (auto i = prefix_length + 1; i <= current_word.length(); i++) {
      assert(i <= temp_states.size());
      if (i == temp_states.size()) {
        temp_states.push_back(State<output_t>::New(objects, state_id++));
      } else {
        temp_states[i]->reuse(state_id++);
      }
      auto arc = current_word[i - 1];
      temp_states[i - 1]->set_transition(arc, temp_states[i]);
    }

    if (current_word != previous_word) {
      auto state = temp_states[current_word.length()];
      state->set_final(true);
      // NOTE: The following code makes bad performance...
      // state->push_to_state_outputs("");
    }

    for (auto j = 1u; j <= prefix_length; j++) {
      auto prev_state = temp_states[j - 1];
      auto arc = current_word[j - 1];

      const auto& output = prev_state->output(arc);

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
      auto state = temp_states[current_word.length()];
      state->push_to_state_outputs(current_output);
    } else {
      auto state = temp_states[prefix_length];
      auto arc = current_word[prefix_length];
      state->set_output(arc, current_output);
    }

    previous_word = current_word;
  });

  // Here we are minimizing the states of the last word
  for (auto i = previous_word.length(); i > 0; i--) {
    auto arc = previous_word[i - 1];
    bool found;
    auto state =
        find_minimized<output_t>(temp_states[i], dictionary, state_id, found);
    temp_states[i - 1]->set_transition(arc, state);
  }

  bool found;
  auto root =
      find_minimized<output_t>(temp_states[0], dictionary, state_id, found);
  return std::make_shared<StateMachine<output_t>>(std::move(objects), state_id, root);
}

template <typename output_t>
inline std::shared_ptr<StateMachine<output_t>> make_state_machine(
    const std::vector<std::pair<std::string, output_t>>& input) {
  return make_state_machine<output_t>([&](const auto& add_entry) {
    for (const auto& item : input) {
      add_entry(item.first, item.second);
    }
  });
}

//-----------------------------------------------------------------------------
// virtual machine
//-----------------------------------------------------------------------------

const size_t DEFAULT_MIN_ARCS_FOR_JUMP_TABLE = 6;

template <typename Val>
inline size_t vb_encode_value_length(Val n) {
  size_t len = 0;
  while (n >= 128) {
    len++;
    n >>= 7;
  }
  len++;
  return len;
}

template <typename Val>
inline size_t vb_encode_value(Val n, char* out) {
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
inline size_t vb_decode_value(const char* data, Val& n) {
  auto p = (const uint8_t*)data;
  size_t len = 0;
  n = 0;
  size_t cnt = 0;
  while (p[len] < 128) {
    n += (p[len++] << (7 * cnt++));
  }
  n += (p[len++] - 128) << (7 * cnt);
  return len;
}

union Ope {
  enum OpeType { Arc = 0, Jmp };

  enum JumpOffsetType { JumpOffsetNone = 0, JumpOffsetZero, JumpOffsetCurrent };

  enum OutputLengthType {
    OutputLengthNone = 0,
    OutputLengthOne,
    OutputLengthTwo,
    OutputLength
  };

  struct {
    unsigned type : 1;
    unsigned final : 1;
    unsigned last_transition : 1;
    unsigned output_length_type : 2;
    unsigned has_state_outputs : 1;
    unsigned jump_offset_type : 2;
  } arc;

  struct {
    unsigned type : 1;
    unsigned need_2byte : 1;
    unsigned reserve : 6;
  } jmp;

  uint8_t byte;
};

template <typename output_t>
struct Command {
  // General
  Ope::OpeType type;
  size_t id = -1;
  size_t next_id = -1;

  // Arc
  bool final;
  bool last_transition;
  char arc;
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

  size_t output_length(const std::string& output) const {
    size_t size = 0;
    if (output.length() > 2) {
      size += sizeof(uint8_t);
    }
    size += output.length();
    return size;
  }

  size_t state_output_length(uint32_t state_output) const {
    return sizeof(state_output);
  }

  size_t state_output_length(const std::string& state_output) const {
    return sizeof(uint8_t) + state_output.length();
  }

  size_t byte_code_size() const {
    // ope
    auto size = sizeof(uint8_t);

    if (type == Ope::Arc) {
      // arc
      if (!use_jump_table) {
        size += sizeof(uint8_t);
      }

      // output
      size += output_length(output);

      // state_outputs
      if (has_state_outputs()) {
        size += sizeof(uint8_t);
        for (const auto& state_output : state_outputs) {
          size += state_output_length(state_output);
        }
      }

      // jump_offset
      if (jump_offset > 0) {
        size += vb_encode_value_length(jump_offset);
      }
    } else {  // type == Jmp
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

  void write_output(std::vector<char>& byte_code, uint32_t output) const {
    if (output > 0xffff) {
      byte_code.insert(byte_code.end(), reinterpret_cast<const char*>(&output),
                       reinterpret_cast<const char*>(&output) + sizeof(output));
    } else if (output > 0xff) {
      uint16_t val = output;
      byte_code.insert(byte_code.end(), reinterpret_cast<const char*>(&val),
                       reinterpret_cast<const char*>(&val) + sizeof(val));
    } else if (output > 0) {
      uint8_t val = output;
      byte_code.insert(byte_code.end(), reinterpret_cast<const char*>(&val),
                       reinterpret_cast<const char*>(&val) + sizeof(val));
    }
  }

  void write_output(std::vector<char>& byte_code,
                    const std::string& output) const {
    if (output.length() > 2) {
      byte_code.push_back((char)output.length());
    }
    byte_code.insert(byte_code.end(), output.begin(), output.end());
  }

  void write_state_output(std::vector<char>& byte_code,
                          uint32_t state_output) const {
    byte_code.insert(
        byte_code.end(), reinterpret_cast<const char*>(&state_output),
        reinterpret_cast<const char*>(&state_output) + sizeof(state_output));
  }

  void write_state_output(std::vector<char>& byte_code,
                          const std::string& state_output) const {
    byte_code.push_back((char)state_output.length());
    byte_code.insert(byte_code.end(), state_output.begin(), state_output.end());
  }

  void write_byte_code(std::vector<char>& byte_code) const {
    auto save_size = byte_code.size();

    if (type == Ope::Arc) {
      // ope
      Ope ope = {Ope::Arc,
                 final,
                 last_transition,
                 (unsigned int)output_length_type,
                 has_state_outputs(),
                 (unsigned int)jump_offset_type};
      byte_code.push_back(ope.byte);

      // arc
      if (!use_jump_table) {
        byte_code.push_back(arc);
      }

      // output
      write_output(byte_code, output);

      // state_outputs
      if (has_state_outputs()) {
        byte_code.push_back((char)state_outputs.size());
        for (const auto& state_output : state_outputs) {
          write_state_output(byte_code, state_output);
        }
      }

      // jump_offset
      if (jump_offset > 0) {
        char vb[9];  // To hold 64 bits value
        auto vb_len = vb_encode_value(jump_offset, vb);
        byte_code.insert(byte_code.end(), vb, vb + vb_len);
      }
    } else {  // type == Jmp
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
          (unsigned char)(count - 1));  // count is stored from 0 to 255
      if (need_2byte) {
        auto p =
            (const char*)arc_jump_offsets.data() + (sizeof(int16_t) * start);
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

  void scan_arc_jump_offsets(bool& need_2byte, size_t& start,
                             size_t& end) const {
    need_2byte = false;
    start = -1;
    end = -1;
    for (auto i = 0; i < 256; i++) {
      auto offset = arc_jump_offsets[i];
      if (offset != -1) {
        if (offset >= 0xff) {
          need_2byte = true;
        }
        if (start == -1) {
          start = i;
        }
        end = i + 1;
      }
    }
  }
};

template <class output_t>
using Commands = std::vector<Command<output_t>>;

template <class output_t>
inline size_t compile_core(typename State<output_t>::pointer state,
                           Commands<output_t>& commands,
                           std::vector<size_t>& state_positions,
                           size_t position, size_t min_arcs_for_jump_table) {
  assert(state->transitions.arcs.size() > 0);

  if (state_positions[state->id]) {
    return position;
  }

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

    static size_t output_length(const std::string& output) {
      return output.length();
    }
  };

  auto arcs_count = state->transitions.arcs.size();

  state->transitions.for_each_reverse(
      [&](char arc, const typename State<output_t>::Transition& t, size_t i) {
        auto next_state = t.state;
        if (next_state->transitions.arcs.size() > 0) {
          position = compile_core(next_state, commands, state_positions,
                                  position, min_arcs_for_jump_table);
        }
      });

  auto use_jump_table = (arcs_count >= min_arcs_for_jump_table);

  size_t arc_positions[256];
  memset(arc_positions, -1, sizeof(arc_positions));

  state->transitions.for_each_reverse(
      [&](char arc, const typename State<output_t>::Transition& t, size_t i) {
        auto next_state = t.state;

        Command<output_t> cmd;
        cmd.type = Ope::Arc;
        cmd.id = state->id;
        cmd.next_id = next_state->id;
        cmd.final = next_state->final;
        cmd.last_transition = (i + 1 == arcs_count);
        cmd.arc = arc;
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
    cmd.type = Ope::Jmp;
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
inline std::vector<char> compile(
    const StateMachine<output_t>& sm,
    size_t min_arcs_for_jump_table = DEFAULT_MIN_ARCS_FOR_JUMP_TABLE) {
  std::vector<char> byte_code;

  Commands<output_t> commands;
  std::vector<size_t> state_positions(sm.count);
  compile_core(sm.root, commands, state_positions, 0, min_arcs_for_jump_table);

  auto rit = commands.rbegin();
  while (rit != commands.rend()) {
    rit->write_byte_code(byte_code);
    ++rit;
  }

  return byte_code;
}

template <typename output_t, typename Input>
inline std::vector<char> build(
    Input input,
    size_t min_arcs_for_jump_table = DEFAULT_MIN_ARCS_FOR_JUMP_TABLE) {
  auto sm = make_state_machine<output_t>(input);
  return compile(*sm, min_arcs_for_jump_table);
}

template <typename Input>
inline std::vector<char> build(
    Input input,
    size_t min_arcs_for_jump_table = DEFAULT_MIN_ARCS_FOR_JUMP_TABLE) {
  return build<uint32_t>(
      [&](const auto& add_entry) {
        size_t i = 0;
        input([&](const std::string& item) { add_entry(item, i++); });
      },
      min_arcs_for_jump_table);
}

template <typename output_t>
inline std::vector<char> build(
    const std::vector<std::pair<std::string, output_t>>& input,
    size_t min_arcs_for_jump_table = DEFAULT_MIN_ARCS_FOR_JUMP_TABLE) {
  return build<output_t>(
      [&](const auto& add_entry) {
        for (const auto& item : input) {
          add_entry(item.first, item.second);
        }
      },
      min_arcs_for_jump_table);
}

inline std::vector<char> build(
    const std::vector<std::string>& input,
    size_t min_arcs_for_jump_table = DEFAULT_MIN_ARCS_FOR_JUMP_TABLE) {
  return build<uint32_t>(
      [&](const auto& add_entry) {
        size_t i = 0;
        for (const auto& item : input) {
          add_entry(item, i++);
        }
      },
      min_arcs_for_jump_table);
}

template <typename output_t>
struct Helper_read_byte_code_arc {};

template <>
struct Helper_read_byte_code_arc<uint32_t> {
  static const char* read_output_length(const char* p, size_t& output_len) {
    output_len = sizeof(uint32_t);
    return p;
  }

  static const char* skip_state_outputs(const char* p,
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

template <>
struct Helper_read_byte_code_arc<std::string> {
  static const char* read_output_length(const char* p, size_t& output_len) {
    output_len = (uint8_t)*p;
    return ++p;
  }

  static const char* skip_state_outputs(const char* p,
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
inline const char* read_byte_code_arc(
    uint8_t ope, const char* p, const char* end, size_t& output_len,
    const char*& output, size_t& state_outputs_size, const char*& state_output,
    Ope::JumpOffsetType& jump_offset_type, size_t& jump_offset) {
  // output
  auto output_length_type = (Ope::OutputLengthType)((ope & 0x18) >> 3);
  if (output_length_type == Ope::OutputLengthNone) {
    output_len = 0;
  } else if (output_length_type == Ope::OutputLengthOne) {
    output_len = 1;
    output = p;
  } else if (output_length_type == Ope::OutputLengthTwo) {
    output_len = 2;
    output = p;
  } else {  // Ope::OutputLength
    p = Helper_read_byte_code_arc<output_t>::read_output_length(p, output_len);
    output = p;
  }
  p += output_len;

  // state_outputs
  if (ope & 0x20) {  // has_state_outputs
    state_outputs_size = (uint8_t)*p++;
    state_output = p;
    p = Helper_read_byte_code_arc<output_t>::skip_state_outputs(
        p, state_outputs_size);
  } else {
    state_outputs_size = 0;
  }

  // jump_offset
  jump_offset_type = (Ope::JumpOffsetType)((ope & 0xC0) >> 6);
  if (jump_offset_type == Ope::JumpOffsetCurrent) {
    p += vb_decode_value(p, jump_offset);
  }

  return p;
}

template <typename output_t>
inline const char* read_byte_code_jmp(uint8_t ope, uint8_t arc, const char* p,
                                      const char* end, int32_t& jump_offset) {
  auto start = (uint8_t)*p++;
  auto count = ((uint8_t)*p++) + 1;  // count is stored from 0 to 255

  if (ope & 0x02) {  // need_2byte
    if (start <= arc && arc < start + count) {
      jump_offset = *(((const int16_t*)p) + (arc - start));
    } else {
      jump_offset = -1;
    }
    p += sizeof(int16_t) * count;
  } else {
    if (start <= arc && arc < start + count) {
      jump_offset = *(((const uint8_t*)p) + (arc - start));
      if (jump_offset == (uint8_t)-1) {
        jump_offset = -1;
      }
    } else {
      jump_offset = -1;
    }
    p += count;
  }

  return p;
}

template <typename output_t>
struct Helper_run {};

template <>
struct Helper_run<uint32_t> {
  static void read_output(uint32_t& buff, const char* output,
                          size_t output_len) {
    if (output_len == 0) {
      ;
    } else if (output_len == 1) {
      auto val = *reinterpret_cast<const uint8_t*>(output);
      buff += val;
    } else if (output_len == 2) {
      auto val = *reinterpret_cast<const uint16_t*>(output);
      buff += val;
    } else {
      assert(output_len == sizeof(uint32_t));
      auto val = *reinterpret_cast<const uint32_t*>(output);
      buff += val;
    }
  }

  static const char* read_state_output(uint32_t& buff,
                                       const char* state_output) {
    buff += *reinterpret_cast<const uint32_t*>(state_output);
    return state_output + sizeof(uint32_t);
  }
};

template <>
struct Helper_run<std::string> {
  static void read_output(std::string& buff, const char* output,
                          size_t output_len) {
    if (output_len == 1) {
      buff += *output;
    } else if (output_len > 1) {
      buff.append(output, output_len);
    }
  }

  static const char* read_state_output(std::string& buff,
                                       const char* state_output) {
    size_t state_output_len = (uint8_t)*state_output++;
    buff.append(state_output, state_output_len);
    return state_output + state_output_len;
  }
};

template <typename output_t, typename Begin, typename Value, typename End>
inline void run(const char* byte_code, size_t size, const char* str,
                Begin output_begin, Value output_value, End output_end) {
  auto prefix = OutputTraits<output_t>::initial_value();

  auto p = byte_code;
  auto end = byte_code + size;
  auto pstr = str;
  auto use_jump_table = false;

  while (*pstr && p < end) {
    auto arc = (uint8_t)*pstr;
    auto ope = *p++;

    if ((ope & 0x01) == Ope::Arc) {
      auto arc2 = use_jump_table ? arc : (uint8_t)*p++;

      size_t output_len;
      const char* output;
      size_t state_outputs_size;
      const char* state_output;
      Ope::JumpOffsetType jump_offset_type;
      size_t jump_offset;

      p = read_byte_code_arc<output_t>(ope, p, end, output_len, output,
                                       state_outputs_size, state_output,
                                       jump_offset_type, jump_offset);

      if (arc2 == arc) {
        Helper_run<output_t>::read_output(prefix, output, output_len);

        pstr++;
        if (ope & 0x02) {  // final
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

          if (output_end()) {
            return;
          }
        }

        if (jump_offset_type == Ope::JumpOffsetNone) {
          return;
        } else if (jump_offset_type == Ope::JumpOffsetCurrent) {
          p += jump_offset;
        }
      } else {
        if (ope & 0x04) {  // last_transition
          return;
        }
      }

      use_jump_table = false;
    } else {  // Jmp
      int32_t jump_offset;
      p = read_byte_code_jmp<output_t>(ope, arc, p, end, jump_offset);
      if (jump_offset == -1) {
        return;
      }
      p += jump_offset;

      use_jump_table = true;
    }
  }

  return;
}

template <typename output_t, typename Callback>
inline bool exact_match_search(const char* byte_code, size_t size,
                               const char* str, Callback callback) {
  bool ret = false;

  run<output_t>(byte_code, size, str,
                // begin
                [&](const char* pstr) { ret = (*pstr == '\0'); },
                // value
                [&](const output_t& val) {
                  if (ret) {
                    callback(val);
                  }
                },
                // end
                [&]() { return ret; });

  return ret;
}

template <typename output_t>
inline std::vector<output_t> exact_match_search(const char* byte_code,
                                                size_t size, const char* str) {
  std::vector<output_t> outputs;
  fst::exact_match_search<output_t>(
      byte_code, size, str,
      [&](const output_t& val) { outputs.emplace_back(val); });
  return outputs;
}

template <typename output_t>
inline bool exact_match_search(const char* byte_code, size_t size,
                               const char* str, output_t& output) {
  return fst::exact_match_search<output_t>(
      byte_code, size, str, [&](const output_t& val) { output = val; });
}

template <typename output_t>
struct CommonPrefixSearchResult {
  size_t length;
  std::vector<output_t> outputs;
};

template <typename output_t, typename Callback>
inline void common_prefix_search(const char* byte_code, size_t size,
                                 const char* str, Callback callback) {
  CommonPrefixSearchResult<output_t> result;

  run<output_t>(byte_code, size, str,
                // begin
                [&](const char* pstr) {
                  result.length = pstr - str;
                  result.outputs.clear();
                },
                // value
                [&](const output_t& val) { result.outputs.emplace_back(val); },
                // end
                [&]() {
                  callback(result);
                  return false;
                });
}

template <typename output_t>
inline std::vector<CommonPrefixSearchResult<output_t>> common_prefix_search(
    const char* byte_code, size_t size, const char* str) {
  std::vector<CommonPrefixSearchResult<output_t>> ret;
  fst::common_prefix_search<output_t>(
      byte_code, size, str,
      [&](const auto& result) { ret.emplace_back(result); });
  return ret;
}

//-----------------------------------------------------------------------------
// formatter
//-----------------------------------------------------------------------------

template <typename output_t, typename Cont>
inline std::string join(const Cont& cont, const char* delm) {
  std::string s;
  for (auto i = 0u; i < cont.size(); i++) {
    if (i != 0) {
      s += delm;
    }
    s += OutputTraits<output_t>::to_string(cont[i]);
  }
  return s;
}

template <typename output_t>
inline void print(
    const StateMachine<output_t>& sm, std::ostream& os,
    size_t min_arcs_for_jump_table = DEFAULT_MIN_ARCS_FOR_JUMP_TABLE) {
  Commands<output_t> commands;
  std::vector<size_t> state_positions(sm.count);
  compile_core(sm.root, commands, state_positions, 0, min_arcs_for_jump_table);

  os << "Ope\tArc\tAddr\tNxtAdr\tID\tNextID\tSize\tLast\tFinal\tOutput\tStOuts"
        "\tJpOffTy\tJpOffSz\tJmpOff\n";
  os << "------\t------\t------\t------\t------\t------\t------\t------\t------"
        "\t------\t------\t------\t------\t------\n";

  size_t addr = 0;

  auto rit = commands.rbegin();
  while (rit != commands.rend()) {
    const auto& cmd = *rit;
    auto size = cmd.byte_code_size();
    if (cmd.type == Ope::Arc) {
      size_t next_addr = -1;
      if (cmd.jump_offset_type == Ope::JumpOffsetZero) {
        next_addr = addr + size;
      } else if (cmd.jump_offset_type == Ope::JumpOffsetCurrent) {
        next_addr = addr + size + cmd.jump_offset;
      }
      auto jump_offset_bytes =
          (cmd.jump_offset > 0 ? vb_encode_value_length(cmd.jump_offset) : 0);

      os << "Arc"
         << "\t";
      os << (int)(uint8_t)cmd.arc << "\t";
      os << addr << "\t";
      os << (int)next_addr << "\t";
      os << (cmd.id == -1 ? "" : std::to_string(cmd.id)) << "\t";
      os << (cmd.next_id == -1 ? "" : std::to_string(cmd.next_id)) << "\t";
      os << size << "\t";
      os << cmd.last_transition << "\t";
      os << cmd.final << "\t";
      os << cmd.output << "\t";
      os << join<output_t>(cmd.state_outputs, "/") << "\t";
      os << cmd.jump_offset_type << "\t";
      os << jump_offset_bytes << "\t";
      os << (int)cmd.jump_offset << std::endl;
    } else {  // Jmp
      auto next_addr = addr + size;

      os << "Jmp"
         << "\t\t";
      os << addr << "\t";
      os << (int)next_addr << "\t";
      os << (cmd.id == -1 ? "" : std::to_string(cmd.id)) << "\t";
      os << "\t";
      os << size << std::endl;
    }
    addr += size;
    ++rit;
  }
}

template <typename output_t>
inline void dot_core(typename State<output_t>::pointer state,
                     std::set<size_t>& check, std::ostream& os) {
  auto id = state->id;

  if (check.find(id) != check.end()) {
    return;
  }
  check.insert(id);

  if (state->final) {
    auto state_outputs = join<output_t>(state->state_outputs, "|");
    os << "  s" << id << " [ shape = doublecircle, xlabel = \"" << state_outputs
       << "\" ];" << std::endl;
  } else {
    os << "  s" << id << " [ shape = circle ];" << std::endl;
  }

  state->transitions.for_each(
      [&](char arc, const typename State<output_t>::Transition& t, size_t i) {
        os << "  s" << id << "->s" << t.state->id << " [ label = \"" << arc;
        if (!OutputTraits<output_t>::empty(t.output)) {
          os << "/" << t.output;
        }
        os << "\" ];" << std::endl;
      });

  state->transitions.for_each(
      [&](char arc, const typename State<output_t>::Transition& t, size_t i) {
        dot_core<output_t>(t.state, check, os);
      });
}

template <typename output_t>
inline void dot(const StateMachine<output_t>& sm, std::ostream& os) {
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
inline std::vector<output_t> exact_match_search(
    const StateMachine<output_t>& sm, const std::string s) {
  auto state = sm.root;
  auto prefix = OutputTraits<output_t>::initial_value();

  auto it = s.begin();
  while (it != s.end()) {
    auto arc = *it;
    auto next_state = state->next_state(arc);
    if (!next_state) {
      return std::vector<output_t>();
    }
    prefix += state->output(arc);
    state = next_state;
    ++it;
  }
  if (!state->final || it != s.end()) {
    return std::vector<output_t>();
  } else {
    std::vector<output_t> ret;
    if (!state->state_outputs.empty()) {
      for (const auto& suffix : state->state_outputs) {
        ret.push_back(prefix + suffix);
      }
    } else if (!OutputTraits<output_t>::empty(prefix)) {
      ret.push_back(prefix);
    }
    return ret;
  }
}

}  // namespace fst

#endif
