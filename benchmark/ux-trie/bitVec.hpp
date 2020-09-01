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

#ifndef BIT_VEC_HPP__
#define BIT_VEC_HPP__

#include <stdint.h>
#include <vector>
#include <iostream>
#include "uxUtil.hpp"

namespace ux {

static const uint64_t L_SHIFT = 9;
static const uint64_t L_BLOCK = 1LLU << L_SHIFT;
static const uint64_t S_SHIFT = 6;
static const uint64_t S_BLOCK = 1LLU << S_SHIFT;
static const uint64_t S_RATIO = L_BLOCK / S_BLOCK;

class BitVec {
public:
  BitVec();
  ~BitVec();

  void push_back(const uint8_t b);
  void push_back_with_len(const uint64_t x, const uint64_t len);

  void setBit(const uint64_t pos, const uint8_t b);
  uint8_t getBit(const uint64_t pos) const;
  uint64_t getBits(const uint64_t pos, const uint64_t len) const;
  void save(std::ostream& os) const;
  void load(std::istream& is);
  size_t size() const;
  void clear();
  void print() const;
  size_t getAllocSize() const;
  uint64_t lookupBlock(const size_t ind) const;

private:
  size_t size_;
  std::vector<uint64_t> B_;
};

}



#endif // BIT_VEC_HPP__
