//
//  spellcheck.h
//
//  Copyright (c) 2020 Yuji Hirose. All rights reserved.
//  MIT License
//

#pragma once

#include <fstlib.h>

#if !defined(__cplusplus) || __cplusplus < 201703L
#error "Requires complete C++17 support"
#endif

namespace fst {

namespace detail {

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

} // namespace detail

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
      m[i + 1][j + 1] = std::min(
          m[i][j + 1] + detail::cost_insert(to, j),                  // insert
          std::min(m[i + 1][j] + detail::cost_delete(from, i),       // delete
                   m[i][j] + detail::cost_replace(from, i, to, j))); // replace
    }

  auto d = m.back().back();
  return 1.0 - (d / std::max<double>(from.size(), to.size()));
}

namespace detail {

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

} // namespace detail

inline double jaro_distance(std::string_view s1, std::string_view s2) {
  std::string cs1;
  if (!detail::common_string(s1, s2, cs1)) return 0;

  std::string cs2;
  if (!detail::common_string(s2, s1, cs2)) return 0;

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
    auto l = static_cast<double>(detail::commn_prefix_len(s1, s2));
    const auto p = 0.1;
    return dj + (l * p * (1.0 - dj));
  }
  return 0.0;
}

template <typename T>
inline decltype(auto) spellcheck(const T &matcher, std::string_view word) {

  std::vector<std::pair<std::string, double>> candidates;

  if (word.empty() || matcher.contains(word)) {
    return std::pair(true, candidates);
  }

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
          candidates.emplace_back(std::pair(candidate, similarity));
        }
      }

      if (!candidates.empty()) {
        std::sort(candidates.begin(), candidates.end(),
                  [](const auto &a, const auto &b) {
                    return a.second == b.second ? a.first < b.first
                                                : a.second > b.second;
                  });
        break;
      }
    }
  }

  return std::pair(false, candidates);
}

} // namespace fst
