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

#ifndef UX_TRIE_HPP__
#define UX_TRIE_HPP__

#include <string>
#include <vector>
#include <stdint.h>
#include "bitVec.hpp"
#include "rsDic.hpp"

namespace ux{

typedef uint64_t id_t;

enum {
  NOTFOUND      = 0xFFFFFFFFU,
  LIMIT_DEFAULT = 0xFFFFFFFFU
};


/**
 * Succinct Trie Data structure
 */
class Trie {
public:
  /**
   * Constructor
   */
  Trie();

  /**
   * Constructor 
   * @param keyList input key list
   * @param isTailUX use tail compression. 
   */
  Trie(std::vector<std::string>& keyList, bool isTailUX = true);

  /**
   * Destructor
   */
  ~Trie();

  /**
   * Build a dictionary from keyList
   * @param keyList input key list
   * @param isTailUX use tail compression. 
   */
  void build(std::vector<std::string>& keyList, bool isTailUX = true);
  
  /**
   * Save the dictionary in a file
   * @param indexName The file name
   * @return 0 on success, -1 on failure
   */
  int save(const char* indexName) const;

  /**
   * Load the dicitonary from a file
   * @param indexName The filename
   * @return 0 on success, -1 on failure
   */
  int load(const char* indexName);

  /**
   * Save the dictionary in ostream
   * @param os The ostream as an output 
   * @return 0 on success, -1 on failure
   */
  int save(std::ostream& os) const;

  /**
   * Load the dictionary from istream
   * @param is The istream as an input
   * @return 0 on success, -1 on failure
   */
  int load(std::istream& is);

  /**
   * Return the longest key that matches the prefix of the query in the dictionary
   * @param str the query
   * @param len the length of the query
   * @param retLen The length of the matched key in the dictionary 
   * @return The ID of the matched key or NOTFOUND if no key is matched
   */
  id_t prefixSearch(const char* str, size_t len, size_t& retLen) const;

  /** 
   * Return the all keys that match the prefix of the query in the dictionary
   * @param str the query
   * @param len the length of the query
   * @param retIDs The IDs of the matched keys
   * @param limit The maximum number of matched keys
   * @return The number of matched keys
   */
  size_t commonPrefixSearch(const char* str, size_t len, std::vector<id_t>& retIDs, 
			    size_t limit = LIMIT_DEFAULT) const;

  /** 
   * Return the all keys whose their prefixes  match the query 
   * @param str the query
   * @param len the length of the query
   * @param The IDs of the matched keys
   * @param limit The maximum number of matched keys
   * @return The number of matched keys
   */
  size_t predictiveSearch(const char* str, size_t len, std::vector<id_t>& retIDs, 
			  size_t limit = LIMIT_DEFAULT) const;
  
  /**
   * Return the key for the given ID
   * @param id The ID of the key
   * @param ret The key for the given ID or empty if such ID does not exist
   */ 
  void decodeKey(id_t id, std::string& ret) const;

  /**
   * Return the key for the given ID
   * @param id The ID of the key
   * @return The key for the given ID or empty if such ID does not exist
   */ 
  std::string decodeKey(id_t id) const;
  
  /**
   * Return the number of keys in the dictionary
   * @return the number of keys in the dictionary
   */
  size_t size() const;

  /**
   * Clear the internal state
   */
  void clear();
  
  /*
   * Get the allocated memory size
   * @return The size of the allocated memory
   */
  size_t getAllocSize() const;
  
  /*
   * Report the statistics of the memory allocation
   * @param allocSize The initial overhead size
   * @param os The output distination
   */
  void allocStat(size_t allocSize, std::ostream& os) const;

  /*
   * Report the internal statistics 
   * @param os The output distination
   */
  void stat(std::ostream& os) const;

  /**
   * Report the error message for the error ID
   * @param error The error ID
   * @return The error message
   */
  static std::string what(int error);

private:
  void buildTailUX();
  bool isLeaf(uint64_t pos) const;
  void getChild(uint8_t c, uint64_t& pos, uint64_t& zeros) const;
  void getParent(uint8_t& c, uint64_t& pos, uint64_t& zeros) const;
  void traverse(const char* str, size_t len, size_t& retLen, std::vector<id_t>& retIDs, 
		size_t limit) const;

  void enumerateAll(uint64_t pos, uint64_t zeros, std::vector<id_t>& retIDs, size_t limit) const;
  bool tailMatch(const char* str, size_t len, size_t depth,
		 uint64_t tailID, size_t& retLen) const;
  std::string getTail(uint64_t i) const;

  RSDic loud_;
  RSDic terminal_;
  RSDic tail_;

  std::vector<std::string> vtails_;
  Trie* vtailux_;
  std::vector<uint8_t> edges_;
  BitVec tailIDs_;
  size_t tailIDLen_;
  size_t keyNum_;
  bool isReady_;

public:
  /** 
   * Error code.
   */
  enum {
    SUCCESS          = 0, 
    FILE_OPEN_ERROR  = 1,
    FILE_WRITE_ERROR = 2,
    FILE_READ_ERROR  = 3,
    SAVE_ERROR       = 4,
    LOAD_ERROR       = 5
  };
};


}

#endif // UX_TRIE_HPP__
