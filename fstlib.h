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

namespace fst {

class State
{
public:
    struct Transition {
        std::shared_ptr<State> state;
        std::string            output;
    };

    const size_t                         id;
    bool                                 is_final;
    std::unordered_map<char, Transition> transitions;
    std::vector<std::string>             state_output;

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
        auto it = transitions.find(arc);
        assert(it != transitions.end());
        return it->second.output;
    }

    void set_output(char arc, const std::string& output)
    {
        transitions[arc].output = output;
    }

    void clear()
    {
        is_final = false;
        transitions.clear();
        state_output.clear();
    }

    std::shared_ptr<State> copy_state(size_t id) const
    {
        auto state = std::make_shared<State>(id);
        state->is_final = is_final;
        state->state_output = state_output;
        state->transitions = transitions;
        return state;
    }

    void print(std::ostream& os) const
    {
        if (is_final) {
            os << "*** s" << id << " final ***" << std::endl;
            for (const auto& output : state_output) {
                std::cout << output << std::endl;
            }
        } else {
            os << "*** s" << id << " ***" << std::endl;
            for (const auto& item : transitions) {
                os << item.first << "/" << item.second.output << std::endl;
            }
            for (const auto& item : transitions) {
                item.second.state->print(os);
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
            lhs.state_output.size() != rhs.state_output.size()) {
            return false;
        }

        {
            auto li = lhs.transitions.begin();
            auto ri = rhs.transitions.begin();
            while (li != lhs.transitions.end()) {
                if (li->first != ri->first) {
                    return false;
                }
                const auto& l = li->second;
                const auto& r = ri->second;
                if (l.output != r.output || l.state != r.state) {
                    return false;
                }
                ++li;
                ++ri;
            }
        }

        {
            auto li = lhs.state_output.begin();
            auto ri = rhs.state_output.begin();
            while (li != lhs.state_output.end()) {
                if (*li != *ri) {
                    return false;
                }
                ++li;
                ++ri;
            }
        }
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
            for (auto output: state->state_output) {
                key += output;
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

const size_t MAX_WORD_SIZE = 1024; // TODO: This limit should be removed.

inline std::shared_ptr<State> make_state_machine(
    const std::vector<std::pair<std::string, std::string>>& input)
{
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

    auto get_prefix_length = [](const std::string& s1, const std::string& s2)
    {
        size_t i = 0;
        while (i < s1.length() && i < s2.length() && s1[i] == s2[i]) {
            i++;
        }
        return i;
    };

    std::vector<std::shared_ptr<State>> temp_states(MAX_WORD_SIZE);
    for (auto i = 0u; i < temp_states.size(); i++) {
        temp_states[i] = std::make_shared<State>();
    }

    std::string previous_word;

    for (const auto& item: input) {
        const auto& current_word = item.first;
        auto current_output = item.second;

        // The following loop caluculates the length of the longest common prefix of 'current_word' and 'previous_word'
        auto prefix_length_plus1 = get_prefix_length(previous_word, current_word) + 1;

        // We minimize the states from the suffix of the previous word
        for (auto i = previous_word.length(); i >= prefix_length_plus1; i--) {
            auto arc = previous_word[i - 1];
            temp_states[i - 1]->set_transition(arc, find_minimized(temp_states[i]));
        }

        // This loop initializes the tail states for the current word
        for (auto i = prefix_length_plus1; i <= current_word.length(); i++) {
            temp_states[i]->clear();
            auto arc = current_word[i - 1];
            temp_states[i - 1]->set_transition(arc, temp_states[i]);
        }

        if (current_word != previous_word) {
            auto state = temp_states[current_word.length()];
            state->is_final = true;
            state->state_output.push_back("");
        }

        for (auto j = 1u; j < prefix_length_plus1; j++) {
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
                for (auto& output: state->state_output) {
                    output.insert(0, word_suffix);
                }
            }
            current_output = current_output.substr(common_prefix_length);
        }

        if (current_word == previous_word) {
            auto state = temp_states[current_word.length()];
            state->state_output.push_back(current_output);
        } else {
            auto state = temp_states[prefix_length_plus1 - 1];
            auto arc = current_word[prefix_length_plus1 - 1];
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
        for (const auto& suffix : state->state_output) {
            ret.push_back(prefix + suffix);
        }
        return ret;
    }
}

inline std::vector<char> compile(std::shared_ptr<State> state)
{
    // TODO:
    return std::vector<char>();
}

inline std::vector<std::string> search(const std::vector<char>& byte_code, const std::string s)
{
    // TODO:
    return std::vector<std::string>();
}

} // namespace fst

#endif

// vim: et ts=4 sw=4 cin cino={1s ff=unix

