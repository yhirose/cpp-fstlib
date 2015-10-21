//
//  fstlib.h
//
//  Copyright (c) 2015 Yuji Hirose. All rights reserved.
//  MIT License
//

#ifndef _CPPFSTLIB_FSTLIB_H_
#define _CPPFSTLIB_FSTLIB_H_

#include <unordered_map>
#include <unordered_set>
#include <numeric>
//#include <iomanip>

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

    const size_t                         id;
    bool                                 is_final;
    std::unordered_map<char, Transition> transitions;
    std::vector<std::string>             state_outputs;

    State(size_t id = -1) : id(id), is_final(false) {}

    std::shared_ptr<State> transition(char arc)
    {
        auto it = transitions.find(arc);
        return it != transitions.end() ? it->second.state : nullptr;
    }

    void set_transition(char arc, std::shared_ptr<State> state)
    {
        transitions[arc].state = state;
    }

    const std::string& output(char arc)
    {
        return transitions[arc].output;
    }

    void set_output(char arc, const std::string& output)
    {
        transitions[arc].output = output;
    }

    void clear()
    {
        is_final = false;
        transitions.clear();
        state_outputs.clear();
    }

    std::shared_ptr<State> copy_state(size_t id) const
    {
        auto state = std::make_shared<State>(id);
        state->is_final = is_final;
        state->state_outputs = state_outputs;
        state->transitions = transitions;
        return state;
    }

    std::vector<char> sorted_arcs() const
    {
        std::vector<char> arcs;
        for (const auto& item : transitions) {
            arcs.push_back(item.first);
        }
        std::sort(arcs.begin(), arcs.end(), [](char a, char b) { return (uint8_t)a < (uint8_t)b; });
        return arcs;
    }

    void print(std::ostream& os) const
    {
        if (is_final) {
            os << "*** s" << id << " final ***" << std::endl;
            for (const auto& state_output : state_outputs) {
                os << state_output << std::endl;
            }
        } else {
            os << "*** s" << id << " ***" << std::endl;
            auto arcs = sorted_arcs();
            for (auto arc : arcs) {
                os << arc << "/" << transitions.at(arc).output << std::endl;
            }
            for (auto arc : arcs) {
                transitions.at(arc).state->print(os);
            }
        }
    }

private:
    State(const State&) = delete;
    State(State&&) = delete;
};

inline bool operator==(const std::shared_ptr<fst::State> & lptr, const std::shared_ptr<fst::State> & rptr)
{
    const auto& lhs = *lptr;
    const auto& rhs = *rptr;

    if (&lhs != &rhs) {
        if (lhs.is_final != rhs.is_final ||
            lhs.transitions.size() != rhs.transitions.size() ||
            lhs.state_outputs.size() != rhs.state_outputs.size()) {
            return false;
        }

        return
            std::equal(lhs.transitions.begin(), lhs.transitions.end(), rhs.transitions.begin()) &&
            std::equal(lhs.state_outputs.begin(), lhs.state_outputs.end(), rhs.state_outputs.begin());
    }

    return true;
}

} // namespace fst

namespace std {

template <>
struct hash<shared_ptr<fst::State>>
{
    size_t operator()(const shared_ptr<fst::State> & state) const
    {
        string key;
        key += (state->is_final ? "f" : "c");
        if (state->is_final) {
            for (auto state_output: state->state_outputs) {
                key += state_output;
            }
        } else {
            for (const auto& item: state->transitions) {
                key += item.first;
                key += item.second.output;
            }
        }
        return hash<string>()(key);
    }
};

} // namespace std

namespace fst {

inline std::shared_ptr<State> make_state_machine(
    const std::vector<std::pair<std::string, std::string>>& input)
{
    // State dictionary
    std::unordered_set<std::shared_ptr<State>> dictionary;
    size_t state_id = 0;
    auto find_minimized = [&](std::shared_ptr<State> state) -> std::shared_ptr<State>
    {
        auto it = dictionary.find(state);
        if (it == dictionary.end()) {
            auto r = state->copy_state(state_id++);
            dictionary.insert(r);
            return r;
        }
        return *it;
    };

    // Find common prefix length from 2 strings
    auto get_prefix_length = [](const std::string& s1, const std::string& s2)
    {
        size_t i = 0;
        while (i < s1.length() && i < s2.length() && s1[i] == s2[i]) {
            i++;
        }
        return i;
    };

    // Make fst
    std::vector<std::shared_ptr<State>> temp_states;
    std::string previous_word;
    temp_states.emplace_back(std::make_shared<State>());

    for (const auto& item: input) {
        const auto& current_word = item.first;
        auto current_output = item.second;

        // The following loop caluculates the length of the longest common prefix of 'current_word' and 'previous_word'
        auto prefix_length = get_prefix_length(previous_word, current_word);

        // We minimize the states from the suffix of the previous word
        for (auto i = previous_word.length(); i > prefix_length; i--) {
            auto arc = previous_word[i - 1];
            temp_states[i - 1]->set_transition(arc, find_minimized(temp_states[i]));
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
            state->is_final = true;
            state->state_outputs.push_back("");
        }

        for (auto j = 1u; j < prefix_length + 1; j++) {
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
                state->set_output(arc, word_suffix + state->output(arc));
            }
            if (state->is_final) {
                for (auto& state_output: state->state_outputs) {
                    state_output.insert(0, word_suffix);
                }
            }
            current_output = current_output.substr(common_prefix_length);
        }

        if (current_word == previous_word) {
            auto state = temp_states[current_word.length()];
            state->state_outputs.push_back(current_output);
        } else {
            auto state = temp_states[prefix_length];
            auto arc = current_word[prefix_length];
            state->set_output(arc, current_output);
        }

        previous_word = current_word;
    }

    // Here we are minimizing the states of the last word
    for (auto i = previous_word.length(); i > 0; i--) {
        auto state = temp_states[i - 1];
        auto arc = previous_word[i - 1];
        state->set_transition(arc, find_minimized(temp_states[i]));
    }

    return find_minimized(temp_states[0]);
}

inline std::vector<std::string> search(std::shared_ptr<State> state, const std::string s)
{
    std::string prefix;

    auto it = s.begin();
    while (!state->is_final && it != s.end()) {
        auto arc = *it;
        auto next_state = state->transition(arc);
        if (!next_state) {
            return std::vector<std::string>();
        }
        prefix += state->output(arc);
        state = next_state;
        ++it;
    }
    if (!state->is_final || it != s.end()) {
        return std::vector<std::string>();
    } else {
        std::vector<std::string> ret;
        for (const auto& suffix : state->state_outputs) {
            ret.push_back(prefix + suffix);
        }
        return ret;
    }
}

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

struct Command
{
    bool                     is_final;
    bool                     is_last_transition;
    char                     arc;
    std::string              output;
    std::vector<std::string> state_outputs;
    size_t                   jump_offset;

    union Ope
    {
        struct {
            unsigned is_final : 1;
            unsigned is_last_transition : 1;
            unsigned reserved : 6;
        } bits;

        uint8_t byte;
    };

    Ope ope() const
    {
        return Ope{ is_final, is_last_transition };
    }

    size_t byte_code_size() const
    {
        // ope
        auto size = sizeof(uint8_t);

        // arc
        size += sizeof(uint8_t);

        // output
        size += sizeof(uint8_t);
        size += output.length();

        // state_outputs
        size += sizeof(uint8_t);
        for (const auto& state_output: state_outputs) {
            size += sizeof(uint8_t);
            size += state_output.length();
        }

        // jump_offset
        if (!is_final) {
            size += sizeof(uint32_t);
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
        bytes.push_back((char)output.length());
        for (auto ch: output) {
            bytes.push_back(ch);
        }

        // state_outputs
        bytes.push_back((char)state_outputs.size());
        for (const auto& state_output : state_outputs) {
            bytes.push_back((char)state_output.length());
            for (auto ch: state_output) {
                bytes.push_back(ch);
            }
        }

        // jump_offset
        if (!is_final) {
            uint32_t data = jump_offset;
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

        is_final = ope.bits.is_final;
        is_last_transition = ope.bits.is_last_transition;

        // arc
        arc = bytes[off++];

        // output
        size_t output_len = bytes[off++];
        output.assign(bytes.data() + off, output_len);
        off += output_len;

        // state_outputs
        state_outputs.resize(bytes[off++]);
        for (auto i = 0u; i < state_outputs.size(); i++) {
            size_t state_output_len = bytes[off++];
            state_outputs[i].assign(bytes.data() + off, state_output_len);
            off += state_output_len;
        }

        // jump_offset
        jump_offset = -1;
        if (!is_final) {
            uint32_t data;
            memcpy(&data, bytes.data() + off, sizeof(data));
            off += sizeof(data);
            jump_offset = data;
        }

        return off;
    }

    void print(std::ostream& os) const
    {
        os << is_final << "\t";
        os << is_last_transition << "\t";
        os << arc << "\t";
        os << output << "\t";
        os << join(state_outputs, "/") << "\t";
        os << (int)jump_offset << "\t";
        os << byte_code_size() << std::endl;
    }
};

inline size_t compile_core(std::shared_ptr<State> state, std::vector<Command>& commands)
{
    assert(!state->is_final);

    auto arcs = state->sorted_arcs();

    std::vector<Command> current_commands;
    std::vector<Command> child_commands;
    std::vector<size_t> child_commands_size;

    for (auto i = 0u; i < arcs.size(); i++) {
        auto arc = arcs[i];
        const auto& trans = state->transitions.at(arc);

        Command cmd;
        cmd.is_final = trans.state->is_final;
        cmd.is_last_transition = (i + 1 == arcs.size());
        cmd.output = trans.output;
        cmd.arc = arc;
        cmd.state_outputs = trans.state->state_outputs;
        current_commands.push_back(cmd);

        if (!trans.state->is_final) {
            auto size = compile_core(trans.state, child_commands);
            child_commands_size.push_back(size);
        }
    }

    for (auto i = (int)arcs.size() - 1; i >= 0; i--) {
        auto& cmd = current_commands[i];
        const auto& trans = state->transitions.at(arcs[i]);

        if (trans.state->is_final) {
            cmd.jump_offset = -1;
        } else {
            auto current_offset = 0;
            for (auto j = i + 1; j < (int)arcs.size(); j++) {
                current_offset += current_commands[j].byte_code_size();
            }

            auto child_offset = 0;
            for (auto j = 0; j < i; j++) {
                child_offset += child_commands_size[j];
            }

            cmd.jump_offset = current_offset + child_offset;
        }
    }

    commands.insert(commands.end(), current_commands.begin(), current_commands.end());
    commands.insert(commands.end(), child_commands.begin(), child_commands.end());

    size_t current_size = 0;
    for (const auto& cmd: current_commands) {
        current_size += cmd.byte_code_size();
    }

    auto child_size = std::accumulate(child_commands_size.begin(), child_commands_size.end(), 0);

    return current_size + child_size;
}

inline std::vector<char> compile(std::shared_ptr<State> state)
{
    std::vector<Command> commands;
    compile_core(state, commands);

    std::vector<char> bytes;

    for (const auto& cmd: commands) {
        const auto& b = cmd.write_byte_code();
        bytes.insert(bytes.end(), b.begin(), b.end());
    }

    return bytes;
}

inline std::vector<std::string> search(const std::vector<char>& bytes, const std::string s)
{
    std::vector<std::string> ret;
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
            if (cmd.is_final) {
                if (i < s.size()) {
                    return std::vector<std::string>();
                }
                std::vector<std::string> ret;
                for (const auto& suffix : cmd.state_outputs) {
                    ret.push_back(prefix + suffix);
                }
                return ret;
            } else {
                off += cmd.jump_offset;
            }
        } else {
            if (cmd.is_last_transition) {
                return std::vector<std::string>();
            }
        }
    }

    return ret;
}

} // namespace fst

#endif

// vim: et ts=4 sw=4 cin cino={1s ff=unix

