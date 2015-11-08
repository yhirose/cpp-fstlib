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
#include <iostream>
#include <list>
#include <map>
#include <memory>
#include <numeric>
#include <set>
#include <string>
#include <vector>
#include <unordered_map>
#include <cstring>

namespace {

template <typename Cont>
inline std::string join(const Cont& cont, const char* delm)
{
    std::string s;
    for (auto i = 0u; i < cont.size(); i++) {
        if (i != 0) {
            s += delm;
        }
        s += cont[i];
    }
    return s;
}

};

namespace fst {

typedef std::string Output;
typedef std::vector<Output> Outputs;

struct CommonPrefixSearchResult
{
    size_t  length;
    Outputs outputs;
};

class State
{
public:
    struct Transition {
        std::shared_ptr<State> state;
        Output                 output;

        bool operator==(const Transition& rhs) const {
            return state == rhs.state && output == rhs.output;
        }
    };
    typedef std::map<char, Transition> Transitions;

    const size_t      id;
    const bool        final = false;
    const Transitions transitions;
    const Outputs     state_outputs;

    State(size_t id = -1) : id(id), final(false), is_hash_prepared(false), hash_value(-1) {}

    State(size_t id, bool final, const Transitions& transitions, const Outputs& state_outputs)
        : id(id)
        , final(final)
        , transitions(transitions)
        , state_outputs(state_outputs)
        , is_hash_prepared(false)
        , hash_value(-1)
    {
    }

    void set_final(bool is_final)
    {
        const_cast<bool&>(final) = is_final;
        set_modified();
    }

    void set_transitions(const Transitions& transitions)
    {
        const_cast<Transitions&>(this->transitions) = transitions;
        set_modified();
    }

    void set_state_outputs(const Outputs& state_outputs)
    {
        const_cast<Outputs&>(this->state_outputs) = state_outputs;
        set_modified();
    }

    std::shared_ptr<State> transition(char arc)
    {
        auto it = transitions.find(arc);
        return it != transitions.end() ? it->second.state : nullptr;
    }

    void set_transition(char arc, std::shared_ptr<State> state)
    {
        const_cast<Transitions&>(transitions)[arc].state = state;
        set_modified();
    }

    const Output& output(char arc)
    {
        return transitions.at(arc).output;
    }

    void set_output(char arc, const Output& output)
    {
        const_cast<Transitions&>(transitions)[arc].output = output;
        set_modified();
    }

    void push_to_state_outputs(const Output& output)
    {
        const_cast<Outputs&>(state_outputs).push_back(output);
        set_modified();
    }

    void prepend_suffix_to_state_outputs(const Output& suffix)
    {
        for (auto& state_output : state_outputs) {
            const_cast<Output&>(state_output).insert(0, suffix);
        }
        set_modified();
    }

    void clear()
    {
        set_final(false);
        const_cast<Transitions&>(transitions).clear();
        const_cast<Outputs&>(state_outputs).clear();
        set_modified();
    }

    std::shared_ptr<State> copy_state(size_t id) const
    {
        return std::make_shared<State>(id, final, transitions, state_outputs);
    }

    void dot(std::ostream& os) const
    {
        os << "digraph{" << std::endl;
        os << "  rankdir = LR;" << std::endl;
        std::set<size_t> check;
        dot_core(os, check);
        os << "}" << std::endl;
    }

    void dot_core(std::ostream& os, std::set<size_t>& check) const
    {
        if (check.find(id) != check.end()) {
            return;
        }
        check.insert(id);

        if (final) {
            os << "  s" << id << " [ shape = doublecircle, xlabel = \"" << join(state_outputs, "|") << "\" ];" << std::endl;
        } else {
            os << "  s" << id << " [ shape = circle ];" << std::endl;
        }

        for (const auto& item : transitions) {
            auto arc = item.first;
            auto t = item.second;
            os << "  s" << id << "->s" << t.state->id << " [ label = \"" << arc;
            if (!t.output.empty()) {
                os << "/" << t.output;
            }
            os << "\" ];" << std::endl;
        }
        for (const auto& item : transitions) {
            auto t = item.second;
            t.state->dot_core(os, check);
        }
    }

    bool operator==(const fst::State& rhs)
    {
        const auto& lhs = *this;

        if (&lhs != &rhs) {
            if (lhs.final != rhs.final ||
                lhs.transitions.size() != rhs.transitions.size() ||
                lhs.state_outputs.size() != rhs.state_outputs.size()) {
                return false;
            }

            auto rit = rhs.transitions.begin();
            for (const auto& l : lhs.transitions) {
                const auto& r = *rit;
                if (l.first != r.first) {
                    return false;
                }
                if (l.second.output != r.second.output) {
                    return false;
                }
                if (l.second.state != r.second.state) {
                    return false;
                }
                ++rit;
            }

            return std::equal(lhs.state_outputs.begin(), lhs.state_outputs.end(), rhs.state_outputs.begin());
        }

        return true;
    }

    size_t hash(size_t next_state_id) const
    {
        if (!is_hash_prepared) {
            is_hash_prepared = true;

            // NOTE: `next_state_id` is used for better hash value
            std::string key = std::to_string(next_state_id);

            key += (final ? "f" : "c");
            if (final) {
                for (const auto& state_output : state_outputs) {
                    key += state_output;
                }
            }
            for (const auto& item : transitions) {
                key += item.first;
                key += item.second.output;
            }

            hash_value = std::hash<std::string>()(key);
        }
        return hash_value;
    }

private:
    State(const State&) = delete;
    State(State&&) = delete;

    void set_modified()
    {
        assert(id == -1);
        is_hash_prepared = false;
    }

    mutable bool is_hash_prepared;
    mutable size_t hash_value;
};

inline size_t get_prefix_length(const std::string& s1, const std::string& s2)
{
    size_t i = 0;
    while (i < s1.length() && i < s2.length() && s1[i] == s2[i]) {
        i++;
    }
    return i;
};

inline std::shared_ptr<State> make_state_machine(
    const std::vector<std::pair<std::string, Output>>& input)
{
    // State dictionary
    std::unordered_map<size_t, std::shared_ptr<State>> dictionary;
    size_t state_id = 0;

    auto find_minimized = [&](std::shared_ptr<State> state, std::shared_ptr<State> next_state) -> std::shared_ptr<State>
    {
        auto h = state->hash(next_state ? next_state->id : -1);

        auto it = dictionary.find(h);
        if (it != dictionary.end() && (*it->second == *state)) {
            return it->second;
        }

        auto r = state->copy_state(state_id++);
        dictionary[h] = r;
        return r;
    };

    // Main algorithm ported from the technical paper
    std::vector<std::shared_ptr<State>> temp_states;
    std::string previous_word;
    temp_states.emplace_back(std::make_shared<State>());

    for (const auto& item: input) {
        const auto& current_word = item.first;
        auto current_output = item.second;

        // The following loop caluculates the length of the longest common
        // prefix of 'current_word' and 'previous_word'
        auto prefix_length = get_prefix_length(previous_word, current_word);

        // We minimize the states from the suffix of the previous word
        std::shared_ptr<State> next_state;
        for (auto i = previous_word.length(); i > prefix_length; i--) {
            auto arc = previous_word[i - 1];
            auto state = find_minimized(temp_states[i], next_state);
            temp_states[i - 1]->set_transition(arc, state);
            next_state = state;
        }

        // This loop initializes the tail states for the current word
        for (auto i = prefix_length + 1; i <= current_word.length(); i++) {
            assert(i <= temp_states.size());
            if (i == temp_states.size()) {
                temp_states.emplace_back(std::make_shared<State>());
            } else {
                temp_states[i]->clear();
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
            auto output = prev_state->output(arc);
            auto common_prefix_length = get_prefix_length(output, current_output);
            auto common_prefix = output.substr(0, common_prefix_length);
            auto word_suffix = output.substr(common_prefix_length);

            prev_state->set_output(arc, common_prefix);

            auto state = temp_states[j];
            for (auto& item: state->transitions) {
                auto arc = item.first;
                const auto& output = state->output(arc);
                state->set_output(arc, word_suffix + output);
            }
            if (state->final) {
                state->prepend_suffix_to_state_outputs(word_suffix);
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
    }

    // Here we are minimizing the states of the last word
    std::shared_ptr<State> next_state;
    for (auto i = previous_word.length(); i > 0; i--) {
        auto arc = previous_word[i - 1];
        auto state = find_minimized(temp_states[i], next_state);
        temp_states[i - 1]->set_transition(arc, state);
        next_state = state;
    }

    return find_minimized(temp_states[0], next_state);
}

union Ope {
    enum Type { Arc = 0, Jmp = 1 };

    struct {
        unsigned tag : 2;
        unsigned final : 1;
        unsigned last_transition : 1;
        unsigned has_output : 1;
        unsigned has_state_outputs : 1;
        unsigned has_jump_offset : 1;
        unsigned is_jump_offset_zero : 1;
    } arc;

    struct {
        unsigned tag : 2;
        unsigned reserve : 6;
    } jmp;

    uint8_t byte;

    Type type() const { return (Type)(byte & 0x3); }
};

struct Command
{
    Ope::Type type;
    size_t    id = -1;
    size_t    next_id = -1;

    // Arc
    bool      final;
    bool      last_transition;
    char      arc;
    Output    output;
    Outputs   state_outputs;
    int       jump_offset;

    // Jmp
    std::vector<int16_t> arc_jump_offsets;

    size_t byte_code_size(bool need_jump_offset = true) const
    {
        // ope
        auto size = sizeof(uint8_t);

        if (type == Ope::Arc) {
            // arc
            size += sizeof(uint8_t);

            // output
            if (!output.empty()) {
                size += sizeof(uint8_t);
                size += output.length();
            }

            // state_outputs
            if (has_state_outputs()) {
                size += sizeof(uint8_t);
                for (const auto& state_output : state_outputs) {
                    size += sizeof(uint8_t);
                    size += state_output.length();
                }
            }

            // jump_offset
            if (need_jump_offset) {
                if (jump_offset > 0) {
                    size += sizeof(uint32_t);
                }
            }
        } else { // type == Jmp
            // arc_jump_offsets
            auto table_size = sizeof(int16_t) * arc_jump_offsets.size();
            size += table_size;
        }

        return size;
    }

    std::vector<char> write_byte_code() const
    {
        std::vector<char> byte_code;

        if (type == Ope::Arc) {
            // ope
            Ope ope = {
                Ope::Arc,
                final,
                last_transition,
                !output.empty(),
                has_state_outputs(),
                jump_offset != -1,
                jump_offset == 0
            };
            byte_code.push_back(ope.byte);

            // arc
            byte_code.push_back(arc);

            // output
            if (!output.empty()) {
                byte_code.push_back((char)output.length());
                for (auto ch: output) {
                    byte_code.push_back(ch);
                }
            }

            // state_outputs
            if (has_state_outputs()) {
                byte_code.push_back((char)state_outputs.size());
                for (const auto& state_output : state_outputs) {
                    byte_code.push_back((char)state_output.length());
                    byte_code.insert(byte_code.end(), state_output.data(), state_output.data() + state_output.length());
                }
            }

            // jump_offset
            if (jump_offset > 0) {
                auto data = (uint32_t)jump_offset;
                const char* p = (const char*)&data;
                byte_code.insert(byte_code.end(), p, p + sizeof(data));
            }
        } else { // type == Jmp
            // ope
            Ope ope = { Ope::Jmp, 0 };
            byte_code.push_back(ope.byte);

            // arc_jump_offsets
            const char* p = (const char*)arc_jump_offsets.data();
            auto table_size = sizeof(int16_t) * arc_jump_offsets.size();
            byte_code.insert(byte_code.end(), p, p + table_size);
        }

        assert(byte_code.size() == byte_code_size());
        return byte_code;
    }

    void print(std::ostream& os) const
    {
        if (type == Ope::Arc) {
            os << "Arc" << "\t";
            os << byte_code_size() << "\t";
            os << (id == -1 ? "" : std::to_string(id)) << "\t";
            os << (next_id == -1 ? "" : std::to_string(next_id)) << "\t";
            os << (int)(uint8_t)arc << "\t";
            os << last_transition << "\t";
            os << final << "\t";
            os << output << "\t";
            os << join(state_outputs, "/") << "\t";
            os << (int)jump_offset << std::endl;
        } else { // Jmp
            os << "Jmp" << "\t";
            os << byte_code_size() << "\t";
            os << (id == -1 ? "" : std::to_string(id)) << std::endl;
        }
    }

private:
    // NOTE: for better state_outputs compression
    bool has_state_outputs() const
    {
        return !(state_outputs.empty() || (state_outputs.size() == 1 && state_outputs.front().empty()));
    }
};

inline size_t compile_core(
    std::shared_ptr<State> state,
    std::list<Command>&    commands,
    std::vector<size_t>&   state_positions,
    size_t                 position)
{
    assert(!state->transitions.empty());

    if (state_positions[state->id]) {
        return position;
    }

    std::vector<char> arcs;
    for (const auto& item : state->transitions) {
        arcs.push_back(item.first);
    }

    for (auto i = (int)arcs.size() - 1; i >= 0; i--) {
        auto arc = arcs[i];
        const auto& trans = state->transitions.at(arc);
        auto next_state = trans.state;

        if (!next_state->transitions.empty()) {
            position = compile_core(next_state, commands, state_positions, position);
        }
    }

    std::vector<int16_t> arc_positions(256, -1);

    for (auto i = (int)arcs.size() - 1; i >= 0; i--) {
        auto arc = arcs[i];
        const auto& trans = state->transitions.at(arc);
        auto next_state = trans.state;

        Command cmd;
        cmd.type = Ope::Arc;
        cmd.id = state->id;
        cmd.next_id = next_state->id;
        cmd.final = next_state->final;
        cmd.last_transition = (i + 1 == arcs.size());
        cmd.output = trans.output;
        cmd.arc = arc;
        cmd.state_outputs = next_state->state_outputs;
        if (next_state->transitions.empty()) {
            cmd.jump_offset = -1;
        } else {
            cmd.jump_offset = position - state_positions[next_state->id];
        }
        commands.push_front(cmd);

        position += cmd.byte_code_size();
        arc_positions[(uint8_t)arc] = position;
    }

    if (arcs.size() >= 16) {
        Command cmd;
        cmd.type = Ope::Jmp;
        cmd.id = state->id;
        cmd.next_id = -1;
        cmd.arc_jump_offsets.assign(arc_positions.size(), -1);
        for (auto i = 0u; i < arc_positions.size(); i++) {
            if (arc_positions[i] != -1) {
                cmd.arc_jump_offsets[i] = position - arc_positions[i];
            }
        }
        commands.push_front(cmd);

        position += cmd.byte_code_size();
    }

    state_positions[state->id] = position;;
    return position;
}

inline std::vector<char> compile(std::shared_ptr<State> state)
{
    std::vector<char> byte_code;
    std::list<Command> commands;
    std::vector<size_t> state_positions(state->id + 1);
    compile_core(state, commands, state_positions, 0);
    for (const auto& cmd: commands) {
        const auto& b = cmd.write_byte_code();
        byte_code.insert(byte_code.end(), b.begin(), b.end());
    }
    return byte_code;
}

inline std::vector<char> build(const std::vector<std::pair<std::string, Output>>& input)
{
    return compile(make_state_machine(input));
}

size_t read_byte_code_arc(
    const char*  byte_code,
    size_t       pos,
    uint8_t&     arc,
    size_t&      output_len,
    const char*& output,
    size_t&      state_outputs_size,
    const char*& state_output,
    size_t&      jump_offset)
{
    auto p = byte_code + pos;

    Ope ope;
    ope.byte = *p++;
    assert(ope.type() == Ope::Arc);

    // arc
    arc = *p++;

    // output
    output_len = 0;
    output = nullptr;
    if (ope.arc.has_output) {
        output_len = *p++;
        output = p;
        p += output_len;
    }

    // state_outputs
    state_outputs_size = 0;
    state_output = nullptr;
    if (ope.arc.has_state_outputs) {
        state_outputs_size = *p++;
        state_output = p;
        if (state_outputs_size == 1) {
            auto len = *p++;
            p += len;
        } else {
            for (auto i = 0u; i < state_outputs_size; i++) {
                auto len = *p++;
                p += len;
            }
        }
    }

    // jump_offset
    jump_offset = -1;
    if (ope.arc.has_jump_offset) {
        if (ope.arc.is_jump_offset_zero) {
            jump_offset = 0;
        } else {
            jump_offset = *(const uint32_t*)p;
            p += sizeof(uint32_t);
        }
    }

    return p - byte_code;
}

size_t read_byte_code_jmp(
    const char*     byte_code,
    size_t          pos,
    const int16_t*& arc_jump_offsets)
{
    auto p = byte_code + pos;

    Ope ope;
    ope.byte = *p++;
    assert(ope.type() == Ope::Jmp);

    arc_jump_offsets = (const int16_t*)p;

    // arc_jump_offsets
    p += sizeof(int16_t) * 256;

    return p - byte_code;
}

inline bool exact_match_search(
    const char* byte_code, size_t size, const std::string& s, Outputs& outputs)
{
    outputs.clear();
    Output prefix;
    size_t pos = 0;

    size_t i = 0;
    while (i < s.size() && pos < size) {
        uint8_t arc = s[i];

        Ope ope;
        ope.byte = *(byte_code + pos);

        if (ope.type() == Ope::Arc) {
            uint8_t     arc2;
            size_t      output_len;
            const char* output;
            size_t      state_outputs_size;
            const char* state_output;
            size_t      jump_offset;

            pos = read_byte_code_arc(
                byte_code, pos,
                arc2, output_len,
                output, state_outputs_size,
                state_output,
                jump_offset);

            if (arc2 == arc) {
                 if (output_len) {
                     prefix.append(output, output_len);;
                 }

                i++;
                if (ope.arc.final && i == s.size()) {
                    // NOTE: for better state_outputs compression
                    if (state_outputs_size) {
                        if (state_outputs_size == 1) {
                            size_t len = *state_output++;
                            auto suffix = Output(state_output, len);
                            outputs.push_back(prefix + suffix);
                        } else {
                            for (auto i = 0u; i < state_outputs_size; i++) {
                                size_t len = *state_output++;
                                auto suffix = Output(state_output, len);
                                outputs.push_back(prefix + suffix);
                                state_output += len;
                            }
                        }
                    } else if (!prefix.empty()) {
                        outputs.push_back(prefix);
                    }
                    return true;
                }

                if (jump_offset == -1) {
                    return false;
                }
                pos += jump_offset;

            } else {
                if (ope.arc.last_transition) {
                    return false;
                }
            }
        } else { // Jmp
            const int16_t* arc_jump_offsets;
            pos = read_byte_code_jmp(byte_code, pos, arc_jump_offsets);

            auto off = arc_jump_offsets[arc];
            if (off == -1) {
                return false;
            }
            pos += off;
        }
    }

    return false;
}

inline Outputs exact_match_search(const std::vector<char>& byte_code, const std::string& s)
{
    Outputs outputs;
    exact_match_search(byte_code.data(), byte_code.size(), s, outputs);
    return outputs;
}

inline std::vector<CommonPrefixSearchResult> common_prefix_search(
    const char* byte_code, size_t size, const std::string& s)
{
    std::vector<CommonPrefixSearchResult> ret;
    Output prefix;
    size_t pos = 0;

    size_t i = 0;
    while (i < s.size() && pos < size) {
        uint8_t arc = s[i];

        Ope ope;
        ope.byte = *(byte_code + pos);

        if (ope.type() == Ope::Arc) {
            uint8_t     arc2;
            size_t      output_len;
            const char* output;
            size_t      state_outputs_size;
            const char* state_output;
            size_t      jump_offset;

            pos = read_byte_code_arc(
                byte_code, pos,
                arc2, output_len,
                output, state_outputs_size,
                state_output,
                jump_offset);

            if (arc2 == arc) {
                prefix.append(output, output_len);
                i++;
                if (ope.arc.final) {
                    // NOTE: for better state_outputs compression
                    CommonPrefixSearchResult result;
                    result.length = i;
                    if (state_outputs_size) {
                        if (state_outputs_size == 1) {
                            size_t len = *state_output++;
                            auto suffix = Output(state_output, len);
                            result.outputs.push_back(prefix + suffix);
                        } else {
                            for (auto i = 0u; i < state_outputs_size; i++) {
                                size_t len = *state_output++;
                                auto suffix = Output(state_output, len);
                                result.outputs.push_back(prefix + suffix);
                                state_output += len;
                            }
                        }
                    } else if (!prefix.empty()) {
                        result.outputs.push_back(prefix);
                    }
                    ret.push_back(result);
                }
                if (jump_offset == -1) {
                    return ret;
                }
                pos += jump_offset;
            } else {
                if (ope.arc.last_transition) {
                    return ret;
                }
            }
        } else { // Jmp
            const int16_t* arc_jump_offsets;
            pos = read_byte_code_jmp(byte_code, pos, arc_jump_offsets);

            auto off = arc_jump_offsets[arc];
            if (off == -1) {
                return ret;
            }
            pos += off;
        }
    }

    return ret;
}

inline std::vector<CommonPrefixSearchResult> common_prefix_search(
    const std::vector<char>& byte_code, const std::string& s)
{
    return common_prefix_search(byte_code.data(), byte_code.size(), s);
}

inline void print_header(std::ostream& os)
{
    os << "Ope" << "\t" << "Size" << "\t" << "ID" << "\t" << "NextID" << "\t" << "Arc" << "\t" << "Last" << "\t" << "Final" << "\t" << "Output" << "\t" << "StOuts" << "\t" << "JmpOff" << std::endl;
    os << "------" << "\t" << "------" << "\t" << "------" << "\t" << "------" << "\t" << "------" << "\t" << "------" << "\t" << "------" << "\t" << "------" << "\t" << "------" << "\t" << "------" << std::endl;
}

inline size_t print(std::shared_ptr<State> state, std::ostream& os)
{
    std::list<Command> commands;
    std::vector<size_t> state_positions(state->id + 1);
    compile_core(state, commands, state_positions, 0);

    size_t pos = 0;
    print_header(os);
    for (const auto& cmd: commands) {
        cmd.print(os);
        pos += cmd.byte_code_size();
    }
    return commands.size();
}

} // namespace fst

#endif
// vim: et ts=4 sw=4 cin cino={1s ff=unix

