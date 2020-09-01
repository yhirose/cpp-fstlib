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

#ifndef RSDIC_HPP__
#define RSDIC_HPP__

#include <stdint.h>
#include <vector>
#include <iostream>
#include "bitVec.hpp"
#include "uxUtil.hpp"

namespace ux {

class RSDic {
public:
  RSDic();
  ~RSDic();

  void build(BitVec& bv);
  uint64_t rank(uint64_t pos, uint8_t b) const;
  uint64_t select(uint64_t pos, uint8_t b) const;

  void save(std::ostream& os) const;
  void load(std::istream& is);
  size_t getAllocSize() const;
  uint8_t getBit(uint64_t pos) const;
  size_t size() const;
  void clear();

private:
  uint64_t selectOverL(uint64_t pos, uint8_t b, uint64_t& retPos) const;
  
  BitVec bitVec_;
  std::vector<uint64_t> L_;
  size_t size_;
};

}

#endif // RSDIC_HPP__
