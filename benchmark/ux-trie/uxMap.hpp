/* 
 *  Copyright (c) 2010 Daisuke Okanohara
  * 
 *   Redistribution and use in source and binary forms, with or without
 *   modification, are permitted provided that the following conditions
 *   are met:
 * 
 *   1. Redistributions of source code must retain the above Copyright
 *      notice, this list of conditions and the following disclaimer.
 *
 *   2. Redistributions in binary form must reproduce the above Copyright
 *      notice, this list of conditions and the following disclaimer in the
 *      documentation and/or other materials provided with the distribution.
 *
 *   3. Neither the name of the authors nor the names of its contributors
 *      may be used to endorse or promote products derived from this
 *      software without specific prior written permission.
 */

#ifndef UX_MAP_HPP__
#define UX_MAP_HPP__

#include <vector>
#include <cassert>
#include <iostream>
#include <map>
#include <string>
#include "uxTrie.hpp"

namespace ux{

/**
 * Succict Map using UX
 */
template <class V>
class Map{
public:
  /**
   * Constructor
   */
  Map() : size_(0){}

  /**
   * Destructor
   */
  ~Map() {}

  /**
   * Build a map without values
   * @param keys keys to be associated
   */
  void build(std::vector<std::string>& keys){
    trie_.build(keys);
    vs_.resize(trie_.size());
  }
  
  /**
   * Build a map from std::map
   * @param m A std::map as an input
   */
  void build(const std::map<std::string, V>& m){
    std::vector<std::string> wordList;
    for (typename std::map<std::string, V>::const_iterator it = 
	   m.begin(); it != m.end(); ++it){
      wordList.push_back(it->first);
    }
    trie_.build(wordList);
    vs_.resize(wordList.size());
    for (typename std::map<std::string, V>::const_iterator it = 
	   m.begin(); it != m.end(); ++it){
      const std::string key = it->first;
      if (set(key.c_str(), key.size(), it->second) != 0){
	return;
      }
    }
  }

  /**
   * Build a map from the vector of the pair of a key and a value
   * @param kvs A vector of the pair of a key and vlaue
   */
  void build(const std::vector< std::pair<std::string, V> >& kvs){
    std::vector<std::string> wordList;
    for (size_t i = 0; i < kvs.size(); ++i){
      wordList.push_back(kvs[i].first);
    }

    trie_.build(wordList);
    vs_.resize(wordList.size());

    for (size_t i = 0; i < kvs.size(); ++i){
      const std::string key = kvs[i].first;
      assert(set(key.c_str(), key.size(), kvs[i].second) == 0);
    }
  }

  /**
   * Get a value for a given key
   * @param str the key
   * @param len the length of str
   * @param v An associated value for a key
   * @return 0 on success and -1 if not found
   */
  int get(const char* str, size_t len, V& v) const {
    size_t retLen = 0;
    id_t id = trie_.prefixSearch(str, len, retLen);
    if (id == NOTFOUND){
      return -1;
    } 
    v = vs_[id];
    return 0;
  }

  /**
   * Set a value for a given key
   * @param str the key
   * @param len the length of str
   * @param v  A value to be associated for a key
   * @return 0 on success and -1 if not found
   */
  int set(const char* str, size_t len, const V& v){
    size_t retLen = 0;
    id_t id = trie_.prefixSearch(str, len, retLen);
    if (id == NOTFOUND){
      return -1;
    }
    vs_[id] = v;
    return 0;
  }

  /**
   * Return the longest key that matches the prefix of the query in the dictionary
   * @param str the query
   * @param len the length of the query
   * @param retLen The length of the matched key in the dictionary 
   * @param v The associated value for the key
   * @return 0 if found and -1 if not found
   */
  int prefixSearch(const char* str, size_t len, size_t& retLen, V& v) const {
    id_t id = trie_.prefixSearch(str, len, retLen);
    if (id == NOTFOUND){
      return -1;
    }
    v = vs_[id];
    return 0;
  }

  /** 
   * Return the all associated values that match the prefix of the query in the dictionary
   * @param str the query
   * @param len the length of the query
   * @param vs The returned values associated for the input key
   * @param limit The maximum number of matched keys
   * @return The number of matched keys
   */
  size_t commonPrefixSearch(const char* str, size_t len, std::vector<V>& vs, size_t limit = LIMIT_DEFAULT) const {
    vs.clear();
    std::vector<id_t> retIDs;
    commonPrefixSearch(str, len, retIDs, limit);
    vs.resize(retIDs.size());
    for (size_t i = 0; i < retIDs.size(); ++i){
      vs[i] = vs_[retIDs[i]];
    }
    return vs.size();
  }

  /** 
   * Return the all keys whose their prefixes  match the query 
   * @param str the query
   * @param len the length of the query
   * @param vs The associated values for the input key
   * @param limit The maximum number of matched keys
   * @return The number of matched keys
   */
  size_t predictiveSearch(const char* str, size_t len, std::vector<V>& vs, size_t limit = LIMIT_DEFAULT) const {
    vs.clear();
    std::vector<id_t> retIDs;
    predictiveSearch(str, len, retIDs, limit);
    vs.resize(retIDs.size());
    for (size_t i = 0; i < retIDs.size(); ++i){
      vs[i] = vs_[retIDs[i]];
    }
    return vs.size();
  }

  /**
   * Return the key for the given ID
   * @param id The ID of the key
   * @param ret The key for the given ID or empty if such ID does not exist
   */ 
  void decodeKey(const size_t ind, std::string& ret) const {
    trie_.decodeKey(ind, ret);
  }

  /**
   * Save the map in ostream
   * @param os The ostream as an output 
   * @return 0 on success, -1 on failure
   */
  int save(std::ostream& os) const {
    trie_.save(os);
    size_t vsSize = vs_.size();
    os.write((const char*)&vsSize, sizeof(vsSize));
    os.write((const char*)&vs_[0], sizeof(vs_[0]) * vs_.size());
    if (!os){
      return -1;
    } else {
      return 0;
    }
  }

  /**
   * Load the map from istream
   * @param is The istream as an input
   * @return 0 on success, -1 on failure
   */
  int load(std::istream& is){
    trie_.load(is);
    size_t vsSize = 0;
    is.read((char*)&vsSize, sizeof(vsSize));
    vs_.resize(vsSize);
    is.read((char*)&vs_[0], sizeof(vs_[0]) * vs_.size());
    if (!is){
      return -1;
    } else {
      return 0;
    }
  }

  /**
   * Get the number of keys 
   * @return the number of keys
   */
  size_t size() const {
    return trie_.size();
  }

private:
  Trie trie_;
  std::vector<V> vs_;
  size_t size_;
};


}


#endif // TRIE_MAP_HPP__
