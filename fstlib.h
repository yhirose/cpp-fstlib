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

class State
{
public:
    struct Transition {
        std::shared_ptr<State> state;
        std::string            output;

        bool operator==(const Transition& rhs) const {
            return state == rhs.state && output == rhs.output;
        }
    };
    typedef std::map<char, Transition> Transitions;
    typedef std::vector<std::string> StateOutputs;

    const size_t       id;
    const bool         final = false;
    const Transitions  transitions;
    const StateOutputs state_outputs;

    State(size_t id = -1) : id(id), final(false), is_hash_prepared(false), hash_value(-1) {}

    State(size_t id, bool final, const Transitions& transitions, const StateOutputs& state_outputs)
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

    void set_state_outputs(const StateOutputs& state_outputs)
    {
        const_cast<StateOutputs&>(this->state_outputs) = state_outputs;
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

    const std::string& output(char arc)
    {
        return transitions.at(arc).output;
    }

    void set_output(char arc, const std::string& output)
    {
        const_cast<Transitions&>(transitions)[arc].output = output;
        set_modified();
    }

    void push_to_state_outputs(const std::string& output)
    {
        const_cast<StateOutputs&>(state_outputs).push_back(output);
        set_modified();
    }

    void prepend_suffix_to_state_outputs(const std::string& suffix)
    {
        for (auto& state_output : state_outputs) {
            const_cast<std::string&>(state_output).insert(0, suffix);
        }
        set_modified();
    }

    void clear()
    {
        set_final(false);
        const_cast<Transitions&>(transitions).clear();
        const_cast<StateOutputs&>(state_outputs).clear();
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
    const std::vector<std::pair<std::string, std::string>>& input)
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

inline State::StateOutputs search(std::shared_ptr<State> state, const std::string s)
{
    std::string prefix;

    auto it = s.begin();
    while (it != s.end()) {
        auto arc = *it;
        auto next_state = state->transition(arc);
        if (!next_state) {
            return State::StateOutputs();
        }
        prefix += state->output(arc);
        state = next_state;
        ++it;
    }
    if (!state->final || it != s.end()) {
        return State::StateOutputs();
    } else {
        State::StateOutputs ret;
        for (const auto& suffix : state->state_outputs) {
            ret.push_back(prefix + suffix);
        }
        return ret;
    }
}

struct Command
{
    size_t              id = -1;
    size_t              next_id = -1;
    bool                final;
    bool                last_transition;
    char                arc;
    std::string         output;
    State::StateOutputs state_outputs;
    int                 jump_offset;

    union Ope
    {
        struct {
            unsigned final : 1;
            unsigned last_transition : 1;
            unsigned has_output : 1;
            unsigned has_state_outputs : 1;
            unsigned has_jump_offset : 1;
            unsigned is_jump_offset_zero : 1;
            unsigned reserved : 2;
        } bits;

        uint8_t byte;
    };

    Ope ope() const
    {
        return Ope {
            final,
            last_transition,
            !output.empty(),
            has_state_outputs(),
            jump_offset != -1,
            jump_offset == 0
        };
    }

    bool has_state_outputs() const
    {
        // NOTE: for better state_outputs compression
        return !(state_outputs.empty() || (state_outputs.size() == 1 && state_outputs.front().empty()));
    }

    size_t byte_code_size(bool need_jump_offset = true) const
    {
        // ope
        auto size = sizeof(uint8_t);

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

        return size;
    }

    std::vector<char> write_byte_code() const
    {
        std::vector<char> bytes;

        // ope
        bytes.push_back(ope().byte);

        // arc
        bytes.push_back(arc);

        // output
        if (!output.empty()) {
            bytes.push_back((char)output.length());
            for (auto ch: output) {
                bytes.push_back(ch);
            }
        }

        // state_outputs
        if (has_state_outputs()) {
            bytes.push_back((char)state_outputs.size());
            for (const auto& state_output : state_outputs) {
                bytes.push_back((char)state_output.length());
                bytes.insert(bytes.end(), state_output.data(), state_output.data() + state_output.length());
            }
        }

        // jump_offset
        if (jump_offset > 0) {
            auto data = (uint32_t)jump_offset;
            const char* p = (const char*)&data;
            bytes.insert(bytes.end(), p, p + sizeof(data));
        }

        assert(bytes.size() == byte_code_size());
        return bytes;
    }

    size_t read_byte_code(const std::vector<char>& bytes, size_t off)
    {
        Ope ope;
        ope.byte = bytes[off++];

        final = ope.bits.final;
        last_transition = ope.bits.last_transition;

        // arc
        arc = bytes[off++];

        // output
        if (ope.bits.has_output) {
            size_t output_len = bytes[off++];
            output.assign(bytes.data() + off, output_len);
            off += output_len;
        }

        // state_outputs
        if (ope.bits.has_state_outputs) {
            state_outputs.resize(bytes[off++]);
            for (auto i = 0u; i < state_outputs.size(); i++) {
                size_t state_output_len = bytes[off++];
                state_outputs[i].assign(bytes.data() + off, state_output_len);
                off += state_output_len;
            }
        }

        // jump_offset
        jump_offset = -1;
        if (ope.bits.has_jump_offset) {
            if (ope.bits.is_jump_offset_zero) {
                jump_offset = 0;
            } else {
                uint32_t data;
                memcpy(&data, bytes.data() + off, sizeof(data));
                off += sizeof(data);
                jump_offset = data;
            }
        }

        return off;
    }

    void print(std::ostream& os) const
    {
        os << (id == -1 ? "" : std::to_string(id)) << "\t";
        os << (next_id == -1 ? "" : std::to_string(next_id)) << "\t";
        os << arc << "\t";
        os << last_transition << "\t";
        os << final << "\t";
        os << output << "\t";
        os << join(state_outputs, "/") << "\t";
        os << (int)jump_offset << "\t";
        os << byte_code_size() << std::endl;
    }
};

inline size_t compile_core(
    std::shared_ptr<State> state,
    std::list<Command>&    commands,
    std::vector<size_t>&   offsets,
    size_t                 position)
{
    assert(!state->transitions.empty());

    if (offsets[state->id]) {
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
            position = compile_core(next_state, commands, offsets, position);
        }
    }

    for (auto i = (int)arcs.size() - 1; i >= 0; i--) {
        auto arc = arcs[i];
        const auto& trans = state->transitions.at(arc);
        auto next_state = trans.state;

        Command cmd;
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
            auto offset = offsets[next_state->id];
            cmd.jump_offset = position - offset;
        }
        commands.push_front(cmd);

        position += cmd.byte_code_size();
    }

    offsets[state->id] = position;;
    return position;
}

inline std::vector<char> compile(std::shared_ptr<State> state)
{
    std::list<Command> commands;
    std::vector<size_t> offsets(state->id + 1);
    compile_core(state, commands, offsets, 0);
    std::vector<char> byte;
    for (const auto& cmd: commands) {
        const auto& b = cmd.write_byte_code();
        byte.insert(byte.end(), b.begin(), b.end());
    }
    return byte;
}

inline State::StateOutputs search(const std::vector<char>& bytes, const std::string s)
{
    std::string prefix;
    size_t off = 0;

    size_t i = 0;
    while (i < s.size()) {
        auto arc = s[i];

        Command cmd;
        off = cmd.read_byte_code(bytes, off);

        if (cmd.arc == arc) {
            prefix += cmd.output;
            i++;
            if (cmd.final && i == s.size()) {
                // NOTE: for better state_outputs compression
                State::StateOutputs ret;
                if (!cmd.state_outputs.empty()) {
                    for (const auto& suffix : cmd.state_outputs) {
                        ret.push_back(prefix + suffix);
                    }
                } else if (!prefix.empty()) {
                    ret.push_back(prefix);
                }
                return ret;
            }
            if (cmd.jump_offset == -1) {
                return State::StateOutputs();
            }
            off += cmd.jump_offset;
        } else {
            if (cmd.last_transition) {
                return State::StateOutputs();
            }
        }
    }

    return State::StateOutputs();
}

inline void print_header(std::ostream& os)
{
    os << "Pos" << "\t" << "JmpPos" << "\t" << "ID" << "\t" << "NextID" << "\t" << "Arc" << "\t" << "Last" << "\t" << "Final" << "\t" << "Output" << "\t" << "StOuts" << "\t" << "JmpOff" << "\t" << "Size" << std::endl;
    os << "------" << "\t" << "------" << "\t" << "------" << "\t" << "------" << "\t" << "------" << "\t" << "------" << "\t" << "------" << "\t" << "------" << "\t" << "------" << "\t" << "------" << "\t" << "------" << std::endl;
}

inline size_t print(std::shared_ptr<State> state, std::ostream& os)
{
    std::list<Command> commands;
    std::vector<size_t> offsets(state->id + 1);
    compile_core(state, commands, offsets, 0);

    size_t off = 0;
    print_header(os);
    for (const auto& cmd: commands) {
        os << off << "\t";
        if (cmd.jump_offset == -1) {
            os << "-" << "\t";
        } else {
            os << off + cmd.byte_code_size() + cmd.jump_offset << "\t";
        }
        cmd.print(os);
        off += cmd.byte_code_size();
    }
    return commands.size();
}

inline size_t print(const std::vector<char>& bytes, std::ostream& os)
{
    print_header(os);
    size_t count = 0;
    size_t off = 0;
    while (off < bytes.size()) {
        os << off << "\t";
        Command cmd;
        off = cmd.read_byte_code(bytes, off);
        if (cmd.jump_offset == -1) {
            os << "-" << "\t";
        } else {
            os << off + cmd.jump_offset << "\t";
        }
        cmd.print(os);
        count++;
    }
    return count;
}

} // namespace fst

#endif
// vim: et ts=4 sw=4 cin cino={1s ff=unix

