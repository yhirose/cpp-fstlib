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

#include <iostream>
#include <cassert>
#include "rsDic.hpp"

using namespace std;

namespace ux {

RSDic::RSDic() : size_(0) {
}

RSDic::~RSDic() {
}

void RSDic::build(BitVec& bv){
  size_ = bv.size();
  swap(bitVec_, bv);
  L_.resize((size_ + L_BLOCK-1) / L_BLOCK);
  size_t sum = 0;
  for (uint64_t il = 0; il < size_; il += L_BLOCK){
    L_[il/L_BLOCK] = sum;
    for (uint64_t is = 0; is < L_BLOCK && il + is < size_; is += S_BLOCK){
      sum += popCount(bitVec_.lookupBlock((il + is)/S_BLOCK));
    }
  }
  L_.push_back(sum);
}

uint64_t RSDic::rank(const uint64_t pos, const uint8_t b) const{
  uint64_t pos1  = pos+1;
  uint64_t rank1 = L_[pos1 >> L_SHIFT];
  uint64_t bpos  = (pos1 >> L_SHIFT) << (L_SHIFT - S_SHIFT);
  uint64_t epos  = pos1 >> S_SHIFT; 
  for (uint64_t i = bpos; i < epos; ++i){
    rank1 += popCount(bitVec_.lookupBlock(i));
  }
  rank1 += popCountMasked(bitVec_.lookupBlock(epos), pos1 % S_BLOCK);
  
  if (b == 1) return rank1;
  else        return pos1 - rank1;
}

uint64_t RSDic::select(const uint64_t pos, const uint8_t b) const{
  uint64_t retPos = 0;
  uint64_t posS   = selectOverL(pos, b, retPos);
  return posS * S_BLOCK + selectBlock(retPos, bitVec_.lookupBlock(posS), b);
}

uint64_t RSDic::selectOverL(const uint64_t pos, const uint8_t b, uint64_t& retPos) const {
  uint64_t left   = 0;
  uint64_t right  = L_.size();
  
  retPos = pos;
  while (left < right){
    uint64_t mid = (left + right)/2;
    assert(mid < L_.size());
    if (getBitNum(L_[mid], L_BLOCK * mid, b) < retPos) left  = mid+1;
    else                                               right = mid;
  }
  uint64_t posL = (left != 0) ? left - 1 : 0;
  uint64_t posS  = posL * S_RATIO;

  assert(retPos >= getBitNum(L_[posL], L_BLOCK * posL, b));

  retPos -= getBitNum(L_[posL], L_BLOCK * posL, b);
  for (;;posS++){
    if (posS >= bitVec_.size()) break;
    uint64_t num = getBitNum(popCount(bitVec_.lookupBlock(posS)), S_BLOCK, b);
    if (retPos <= num) break;
    retPos -= num;
  }
  return posS;
}

void RSDic::save(ostream& ofs) const{
  bitVec_.save(ofs);
}

void RSDic::load(istream& ifs) {
  bitVec_.load(ifs);
  build(bitVec_);
}

size_t RSDic::getAllocSize() const {
  return bitVec_.getAllocSize() + sizeof(L_[0]) * L_.size();
}

uint8_t RSDic::getBit(const uint64_t pos) const{
  return bitVec_.getBit(pos);
}

size_t RSDic::size() const {
  return bitVec_.size();
}

void RSDic::clear() {
  bitVec_.clear();
  L_.clear();
  size_ = 0;
}

}
