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

#define USE_JUMP_OFFSET_END

namespace fst {

//-----------------------------------------------------------------------------
// MurmurHash64B - 64-bit MurmurHash2 for 32-bit platforms
//-----------------------------------------------------------------------------

// URL:: https://github.com/aappleby/smhasher/blob/master/src/MurmurHash2.cpp
// License: Public Domain

inline uint64_t MurmurHash64B(const void* key, int len, uint64_t seed) {
  const uint32_t m = 0x5bd1e995;
  const int r = 24;

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

//-----------------------------------------------------------------------------
// fst
//-----------------------------------------------------------------------------

class State {
 public:
  typedef State* pointer;

  struct Transition {
    pointer state;
    std::string output;

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
      assert(idx != -1);
      assert(idx < states_and_outputs.size());
      return states_and_outputs[idx];
    }

    pointer next_state(char arc) const {
      auto idx = get_index(arc);
      return next_state(idx);
    }

    const std::string& output(char arc) const {
      auto idx = get_index(arc);
      assert(idx != -1);
      assert(idx < states_and_outputs.size());
      return states_and_outputs[idx].output;
    }

    template <typename T>
    void for_each(T fn) const {
      for (auto i = 0u; i < arcs.size(); i++) {
        fn(arcs[i], states_and_outputs[i], i);
      }
    }

    template <typename T>
    void for_each_reverse(T fn) const {
      for (auto i = (int)arcs.size() - 1; i >= 0; i--) {
        fn(arcs[i], states_and_outputs[i], i);
      }
    }

    template <typename T>
    void for_each_arc(T fn) const {
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

    void set_output(char arc, const std::string& output) {
      auto idx = get_index(arc);
      assert(idx != -1);
      assert(idx < states_and_outputs.size());
      states_and_outputs[idx].output = output;
    }

    void insert_output(char arc, const std::string& s) {
      auto idx = get_index(arc);
      assert(idx != -1);
      assert(idx < states_and_outputs.size());
      auto& output = states_and_outputs[idx].output;
      output.insert(output.begin(), s.begin(), s.end());
    }

    friend class State;
  };

  size_t id;
  bool in_dictionary;
  bool final;
  Transitions transitions;
  std::vector<std::string> state_outputs;

  State(size_t id) : id(id), in_dictionary(false), final(false) {}

  pointer next_state(char arc) const { return transitions.next_state(arc); }

  const std::string& output(char arc) const { return transitions.output(arc); }

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

  void set_output(char arc, const std::string& output) {
    transitions.set_output(arc, output);
  }

  void prepend_suffix_to_output(char arc, const std::string& suffix) {
    transitions.insert_output(arc, suffix);
  }

  void push_to_state_outputs(const std::string& output) {
    state_outputs.push_back(output);
  }

  void prepend_suffix_to_state_outputs(const std::string& suffix) {
    for (auto& state_output : state_outputs) {
      state_output.insert(0, suffix);
    }
  }

  void reuse(size_t state_id) {
    id = state_id;
    set_final(false);
    transitions.clear();
    state_outputs.clear();
  }

  static pointer New(size_t state_id = -1) {
    auto p = new State(state_id);
    objects_.push_back(p);
    return p;
  }

  static void ClearMemory() {
    for (auto p : objects_) {
      delete p;
    }
    objects_.clear();
  }

 private:
  State(const State&) = delete;
  State(State&&) = delete;

  static std::vector<pointer> objects_;
};

inline uint64_t State::hash() const {
  char buff[1024];  // TOOD: large enough?
  size_t buff_len = 0;

  if (final) {
    for (const auto& state_output : state_outputs) {
      if (!state_outputs.empty()) {
        memcpy(&buff[buff_len], state_output.data(), state_output.size());
        buff_len += state_output.size();
        buff[buff_len++] = '\t';
      }
    }
  }

  buff[buff_len++] = '\n';

  transitions.for_each([&](char arc, const State::Transition& t, int i) {
    buff[buff_len++] = arc;
    uint32_t val = t.state->id;
    memcpy(&buff[buff_len], &val, sizeof(val));
    buff_len += sizeof(val);
    memcpy(&buff[buff_len], t.output.data(), t.output.size());
    buff_len += t.output.size();
    buff[buff_len++] = '\t';
  });

  return MurmurHash64B(buff, buff_len, 0);
}

std::vector<State::pointer> State::objects_;

inline size_t get_prefix_length(const std::string& s1, const std::string& s2) {
  size_t i = 0;
  while (i < s1.length() && i < s2.length() && s1[i] == s2[i]) {
    i++;
  }
  return i;
}

class StateMachine {
 public:
  size_t count;
  State::pointer root;

  StateMachine(size_t count, State::pointer root) : count(count), root(root) {}
  ~StateMachine() { State::ClearMemory(); }

 private:
  StateMachine(const StateMachine&) = delete;
  StateMachine(StateMachine&&) = delete;
};

// NOTE: unordered_set is not used here, because it uses size_t as hash value
// which becomes 32-bit on 32-bit platforms. But we want to use 64-bit hash
// value.
typedef std::unordered_map<uint64_t, State::pointer> Dictionary;

inline State::pointer find_minimized(State::pointer state,
                                     Dictionary& dictionary, size_t& state_id,
                                     bool& found) {
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

template <typename T>
inline std::shared_ptr<StateMachine> make_state_machine(T input) {
  Dictionary dictionary;
  size_t state_id = 0;

  // Main algorithm ported from the technical paper
  std::vector<State::pointer> temp_states;
  std::string previous_word;
  temp_states.push_back(State::New(state_id++));

  input([&](const std::string& current_word, std::string current_output) {
    // The following loop caluculates the length of the longest common
    // prefix of 'current_word' and 'previous_word'
    auto prefix_length = get_prefix_length(previous_word, current_word);

    // We minimize the states from the suffix of the previous word
    for (auto i = previous_word.length(); i > prefix_length; i--) {
      auto arc = previous_word[i - 1];
      bool found;
      auto state = find_minimized(temp_states[i], dictionary, state_id, found);
      if (!found) {
        // Ownership of the object in temp_states[i] has been moved to the
        // dictionary...
        temp_states[i] = State::New();
      }
      temp_states[i - 1]->set_transition(arc, state);
    }

    // This loop initializes the tail states for the current word
    for (auto i = prefix_length + 1; i <= current_word.length(); i++) {
      assert(i <= temp_states.size());
      if (i == temp_states.size()) {
        temp_states.push_back(State::New(state_id++));
      } else {
        temp_states[i]->reuse(state_id++);
      }
      auto arc = current_word[i - 1];
      temp_states[i - 1]->set_transition(arc, temp_states[i]);
    }

    if (current_word != previous_word) {
      auto state = temp_states[current_word.length()];
      state->set_final(true);
      state->push_to_state_outputs("");
    }

    for (auto j = 1u; j <= prefix_length; j++) {
      auto prev_state = temp_states[j - 1];
      auto arc = current_word[j - 1];
      const auto& output = prev_state->output(arc);
      auto common_prefix_length = get_prefix_length(output, current_output);
      auto common_prefix = output.substr(0, common_prefix_length);
      auto word_suffix = output.substr(common_prefix_length);

      prev_state->set_output(arc, common_prefix);

      if (!word_suffix.empty()) {
        auto state = temp_states[j];
        state->transitions.for_each_arc([&](char arc) {
          state->prepend_suffix_to_output(arc, word_suffix);
        });
        if (state->final) {
          state->prepend_suffix_to_state_outputs(word_suffix);
        }
      }
      current_output = current_output.substr(common_prefix_length);
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
    auto state = find_minimized(temp_states[i], dictionary, state_id, found);
    temp_states[i - 1]->set_transition(arc, state);
  }

  bool found;
  auto root = find_minimized(temp_states[0], dictionary, state_id, found);
  return std::make_shared<StateMachine>(state_id, root);
}

inline std::shared_ptr<StateMachine> make_state_machine(
    const std::vector<std::pair<std::string, std::string>>& input) {
  return make_state_machine([&](const auto& feed) {
    for (const auto& item : input) {
      feed(item.first, item.second);
    }
  });
}

//-----------------------------------------------------------------------------
// virtual machine
//-----------------------------------------------------------------------------

namespace {

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
}

union Ope {
  enum OpeType { Arc = 0, Jmp };

  enum JumpOffsetType {
    JumpOffsetNone = 0,
    JumpOffsetZero,
    JumpOffsetCurrent,
    JumpOffsetEnd
  };

  enum OutputType { OutputNone = 0, OutputOne, OutputTwo, OutputLength };

  struct {
    unsigned type : 1;
    unsigned final : 1;
    unsigned last_transition : 1;
    unsigned output_type : 2;
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

struct Command {
  // General
  Ope::OpeType type;
  size_t id = -1;
  size_t next_id = -1;

  // Arc
  bool final;
  bool last_transition;
  char arc;
  Ope::OutputType output_type;
  std::string output;
  std::vector<std::string> state_outputs;
  Ope::JumpOffsetType jump_offset_type;
  int jump_offset;
  bool use_jump_table;

  // Jmp
  std::vector<int16_t> arc_jump_offsets;

  size_t byte_code_size() const {
    // ope
    auto size = sizeof(uint8_t);

    if (type == Ope::Arc) {
      // arc
      if (!use_jump_table) {
        size += sizeof(uint8_t);
      }

      // output
      if (output.length() > 2) {
        size += sizeof(uint8_t);
      }
      size += output.length();

      // state_outputs
      if (has_state_outputs()) {
        size += sizeof(uint8_t);
        for (const auto& state_output : state_outputs) {
          size += sizeof(uint8_t);
          size += state_output.length();
        }
      }

      // jump_offset
      if (jump_offset > 0) {
        size += vb_encode_value_length<uint32_t>(jump_offset);
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

  void write_byte_code(std::vector<char>& byte_code) const {
    auto save_size = byte_code.size();

    if (type == Ope::Arc) {
      // ope
      Ope ope = {Ope::Arc,
                 final,
                 last_transition,
                 (unsigned int)output_type,
                 has_state_outputs(),
                 (unsigned int)jump_offset_type};
      byte_code.push_back(ope.byte);

      // arc
      if (!use_jump_table) {
        byte_code.push_back(arc);
      }

      // output
      if (output.length() > 2) {
        byte_code.push_back((char)output.length());
      }
      for (auto ch : output) {
        byte_code.push_back(ch);
      }

      // state_outputs
      if (has_state_outputs()) {
        byte_code.push_back((char)state_outputs.size());
        for (const auto& state_output : state_outputs) {
          byte_code.push_back((char)state_output.length());
          byte_code.insert(byte_code.end(), state_output.data(),
                           state_output.data() + state_output.length());
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
  // NOTE: for better state_outputs compression
  bool has_state_outputs() const {
    return !(state_outputs.empty() ||
             (state_outputs.size() == 1 && state_outputs.front().empty()));
  }

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

typedef std::vector<Command> Commands;

inline size_t compile_core(State::pointer state, Commands& commands,
                           std::vector<size_t>& state_positions,
                           size_t position, size_t min_arcs_for_jump_table) {
  assert(state->transitions.arcs.size() > 0);

  if (state_positions[state->id]) {
    return position;
  }

  auto arcs_count = state->transitions.arcs.size();

  state->transitions.for_each_reverse(
      [&](char arc, const State::Transition& t, int i) {
        auto next_state = t.state;
        if (next_state->transitions.arcs.size() > 0) {
          position = compile_core(next_state, commands, state_positions,
                                  position, min_arcs_for_jump_table);
        }
      });

  auto use_jump_table = (arcs_count >= min_arcs_for_jump_table);

  size_t arc_positions[256];
  memset(arc_positions, -1, sizeof(arc_positions));

  state->transitions.for_each_reverse([&](char arc, const State::Transition& t,
                                          int i) {
    auto next_state = t.state;

    Command cmd;
    cmd.type = Ope::Arc;
    cmd.id = state->id;
    cmd.next_id = next_state->id;
    cmd.final = next_state->final;
    cmd.last_transition = (i + 1 == arcs_count);
    cmd.arc = arc;
    cmd.output = t.output;
    if (cmd.output.empty()) {
      cmd.output_type = Ope::OutputNone;
    } else if (cmd.output.length() == 1) {
      cmd.output_type = Ope::OutputOne;
    } else if (cmd.output.length() == 2) {
      cmd.output_type = Ope::OutputTwo;
    } else {
      cmd.output_type = Ope::OutputLength;
    }
    cmd.state_outputs = next_state->state_outputs;
    if (next_state->transitions.arcs.size() == 0) {
      cmd.jump_offset_type = Ope::JumpOffsetNone;
      cmd.jump_offset = -1;
    } else {
      auto offset = position - state_positions[next_state->id];
      if (offset == 0) {
        cmd.jump_offset_type = Ope::JumpOffsetZero;
      } else {
#ifdef USE_JUMP_OFFSET_END
        auto cur_byte = vb_encode_value_length(offset);
        auto end_byte = vb_encode_value_length(state_positions[next_state->id]);
        if (end_byte < cur_byte) {
          offset = state_positions[next_state->id];
          cmd.jump_offset_type = Ope::JumpOffsetEnd;
        } else {
          cmd.jump_offset_type = Ope::JumpOffsetCurrent;
        }
#else
        cmd.jump_offset_type = Ope::JumpOffsetCurrent;
#endif
      }
      cmd.jump_offset = offset;
    }
    cmd.use_jump_table = use_jump_table;

    position += cmd.byte_code_size();
    commands.emplace_back(std::move(cmd));
    arc_positions[(uint8_t)arc] = position;
  });

  if (use_jump_table) {
    Command cmd;
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

inline std::vector<char> compile(
    const StateMachine& sm,
    size_t min_arcs_for_jump_table = DEFAULT_MIN_ARCS_FOR_JUMP_TABLE) {
  std::vector<char> byte_code;

  Commands commands;
  std::vector<size_t> state_positions(sm.count);
  compile_core(sm.root, commands, state_positions, 0, min_arcs_for_jump_table);

  auto rit = commands.rbegin();
  while (rit != commands.rend()) {
    rit->write_byte_code(byte_code);
    ++rit;
  }

  return byte_code;
}

template <typename T>
inline std::vector<char> build(
    T input, size_t min_arcs_for_jump_table = DEFAULT_MIN_ARCS_FOR_JUMP_TABLE) {
  auto sm = make_state_machine(input);
  return compile(*sm, min_arcs_for_jump_table);
}

inline std::vector<char> build(
    const std::vector<std::pair<std::string, std::string>>& input,
    size_t min_arcs_for_jump_table = DEFAULT_MIN_ARCS_FOR_JUMP_TABLE) {
  return build(
      [&](const auto& feed) {
        for (const auto& item : input) {
          feed(item.first, item.second);
        }
      },
      min_arcs_for_jump_table);
}

inline const char* read_byte_code_arc(
    uint8_t ope, const char* p, const char* end, size_t& output_len,
    const char*& output, size_t& state_outputs_size, const char*& state_output,
    Ope::JumpOffsetType& jump_offset_type, size_t& jump_offset) {
  // output
  auto output_type = (Ope::OutputType)((ope & 0x18) >> 3);
  if (output_type == Ope::OutputNone) {
    output_len = 0;
  } else if (output_type == Ope::OutputOne) {
    output_len = 1;
    output = p;
  } else if (output_type == Ope::OutputTwo) {
    output_len = 2;
    output = p;
  } else {  // Ope::OutputLength
    output_len = *p++;
    output = p;
  }
  p += output_len;

  // state_outputs
  if (ope & 0x20) {  // has_state_outputs
    state_outputs_size = *p++;
    state_output = p;
    if (state_outputs_size == 1) {
      p += 1 + *p;
    } else {
      for (auto i = 0u; i < state_outputs_size; i++) {
        p += 1 + *p;
      }
    }
  } else {
    state_outputs_size = 0;
  }

  // jump_offset
  jump_offset_type = (Ope::JumpOffsetType)((ope & 0xC0) >> 6);
  if (jump_offset_type == Ope::JumpOffsetCurrent ||
      jump_offset_type == Ope::JumpOffsetEnd) {
    p += vb_decode_value(p, jump_offset);
  }

  return p;
}

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

template <typename Begin, typename Value, typename End>
inline void run(const char* byte_code, size_t size, const char* str,
                Begin output_begin, Value output_value, End output_end) {
  char prefix[BUFSIZ];
  size_t prefix_len = 0;

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

      p = read_byte_code_arc(ope, p, end, output_len, output,
                             state_outputs_size, state_output, jump_offset_type,
                             jump_offset);

      if (arc2 == arc) {
        if (output_len == 1) {
          prefix[prefix_len++] = *output;
        } else if (output_len > 1) {
          memcpy(&prefix[prefix_len], output, output_len);
          prefix_len += output_len;
        }

        pstr++;
        if (ope & 0x02) {  // final
          output_begin(pstr);

          // NOTE: for better state_outputs compression
          if (state_outputs_size == 0) {
            output_value(prefix, prefix_len);
          } else {
            for (auto i = 0u; i < state_outputs_size; i++) {
              size_t state_output_len = *state_output++;
              char final_state_output[BUFSIZ];
              memcpy(&final_state_output[0], prefix, prefix_len);
              memcpy(&final_state_output[prefix_len], state_output,
                     state_output_len);
              output_value(final_state_output, prefix_len + state_output_len);
              state_output += state_output_len;
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
        } else if (jump_offset_type == Ope::JumpOffsetEnd) {
          p = end - jump_offset;
        }
      } else {
        if (ope & 0x04) {  // last_transition
          return;
        }
      }

      use_jump_table = false;
    } else {  // Jmp
      int32_t jump_offset;
      p = read_byte_code_jmp(ope, arc, p, end, jump_offset);
      if (jump_offset == -1) {
        return;
      }
      p += jump_offset;

      use_jump_table = true;
    }
  }

  return;
}

template <typename T>
inline bool exact_match_search(const char* byte_code, size_t size,
                               const char* str, T callback) {
  bool ret = false;

  run(byte_code, size, str,
      // begin
      [&](const char* pstr) { ret = (*pstr == '\0'); },
      // value
      [&](const char* s, size_t l) {
        if (ret) {
          callback(s, l);
        }
      },
      // end
      [&]() { return ret; });

  return ret;
}

inline std::vector<std::string> exact_match_search(const char* byte_code,
                                                   size_t size,
                                                   const char* str) {
  std::vector<std::string> outputs;
  fst::exact_match_search(byte_code, size, str, [&](const char* s, size_t l) {
    outputs.emplace_back(s, l);
  });
  return outputs;
}

struct CommonPrefixSearchResult {
  size_t length;
  std::vector<std::string> outputs;
};

template <typename T>
inline void common_prefix_search(const char* byte_code, size_t size,
                                 const char* str, T callback) {
  CommonPrefixSearchResult result;

  run(byte_code, size, str,
      // begin
      [&](const char* pstr) {
        result.length = pstr - str;
        result.outputs.clear();
      },
      // value
      [&](const char* s, size_t l) { result.outputs.emplace_back(s, l); },
      // end
      [&]() {
        callback(result);
        return false;
      });
}

inline std::vector<CommonPrefixSearchResult> common_prefix_search(
    const char* byte_code, size_t size, const char* str) {
  std::vector<CommonPrefixSearchResult> ret;
  fst::common_prefix_search(byte_code, size, str, [&](const auto& result) {
    ret.emplace_back(result);
  });
  return ret;
}

//-----------------------------------------------------------------------------
// formatter
//-----------------------------------------------------------------------------

namespace {

template <typename Cont>
inline std::string join(const Cont& cont, const char* delm) {
  std::string s;
  for (auto i = 0u; i < cont.size(); i++) {
    if (i != 0) {
      s += delm;
    }
    s += cont[i];
  }
  return s;
}
}

inline void print(
    const StateMachine& sm, std::ostream& os,
    size_t min_arcs_for_jump_table = DEFAULT_MIN_ARCS_FOR_JUMP_TABLE) {
  Commands commands;
  std::vector<size_t> state_positions(sm.count);
  compile_core(sm.root, commands, state_positions, 0, min_arcs_for_jump_table);

  os << "Ope\tArc\tAddr\tNxtAdr\tID\tNextID\tSize\tLast\tFinal\tOutput\tStOuts"
        "\tJpOffSz\tJmpOff\n";
  os << "------\t------\t------\t------\t------\t------\t------\t------\t------"
        "\t------\t------\t------\t------\n";

  size_t end = 0;
  auto rit = commands.rbegin();
  while (rit != commands.rend()) {
    end += rit->byte_code_size();
    ++rit;
  }

  size_t addr = 0;

  rit = commands.rbegin();
  while (rit != commands.rend()) {
    const auto& cmd = *rit;
    auto size = cmd.byte_code_size();
    if (cmd.type == Ope::Arc) {
      size_t next_addr = -1;
      if (cmd.jump_offset_type == Ope::JumpOffsetZero) {
        next_addr = addr + size;
      } else if (cmd.jump_offset_type == Ope::JumpOffsetCurrent) {
        next_addr = addr + size + cmd.jump_offset;
      } else if (cmd.jump_offset_type == Ope::JumpOffsetEnd) {
        next_addr = end - cmd.jump_offset;
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
      os << join(cmd.state_outputs, "/") << "\t";
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

inline void dot_core(State::pointer state, std::set<size_t>& check,
                     std::ostream& os) {
  auto id = state->id;

  if (check.find(id) != check.end()) {
    return;
  }
  check.insert(id);

  if (state->final) {
    auto state_outputs = join(state->state_outputs, "|");
    os << "  s" << id << " [ shape = doublecircle, xlabel = \"" << state_outputs
       << "\" ];" << std::endl;
  } else {
    os << "  s" << id << " [ shape = circle ];" << std::endl;
  }

  state->transitions.for_each([&](char arc, const State::Transition& t, int i) {
    os << "  s" << id << "->s" << t.state->id << " [ label = \"" << arc;
    if (!t.output.empty()) {
      os << "/" << t.output;
    }
    os << "\" ];" << std::endl;
  });

  state->transitions.for_each([&](char arc, const State::Transition& t, int i) {
    dot_core(t.state, check, os);
  });
}

inline void dot(const StateMachine& sm, std::ostream& os) {
  os << "digraph{" << std::endl;
  os << "  rankdir = LR;" << std::endl;
  std::set<size_t> check;
  dot_core(sm.root, check, os);
  os << "}" << std::endl;
}

inline std::vector<std::string> exact_match_search(const StateMachine& sm,
                                                   const std::string s) {
  auto state = sm.root;
  std::string prefix;

  auto it = s.begin();
  while (it != s.end()) {
    auto arc = *it;
    auto next_state = state->next_state(arc);
    if (!next_state) {
      return std::vector<std::string>();
    }
    prefix += state->output(arc);
    state = next_state;
    ++it;
  }
  if (!state->final || it != s.end()) {
    return std::vector<std::string>();
  } else {
    // NOTE: for better state_outputs compression
    std::vector<std::string> ret;
    if (!state->state_outputs.empty()) {
      for (const auto& suffix : state->state_outputs) {
        ret.push_back(prefix + suffix);
      }
    } else if (!prefix.empty()) {
      ret.push_back(prefix);
    }
    return ret;
  }
}

}  // namespace fst

#endif
