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
#include "bitVec.hpp"

using namespace std;

namespace ux {

BitVec::BitVec() : size_(0){
}

BitVec::~BitVec(){
}


void BitVec::push_back(const uint8_t b){
  if (size_ / S_BLOCK >= B_.size()) {
    B_.push_back(0);
  }

  if (b) {
    B_[size_ / S_BLOCK] |= (1ULL << (size_ % S_BLOCK));
  }
  ++size_;
}

void BitVec::push_back_with_len(const uint64_t x, const uint64_t len){
  size_t offset = size_ % S_BLOCK;
  if ((size_ + len - 1) / S_BLOCK >= B_.size()){
    B_.push_back(0);
  }

  B_[size_ / S_BLOCK] |= (x << offset);
  if (offset + len - 1 >= S_BLOCK){
    B_[size_ / S_BLOCK + 1] |= (x >> (S_BLOCK - offset));
  } 
  size_ += len;
}

void BitVec::setBit(const uint64_t pos, const uint8_t b){
  if (b == 0) return;
  B_[pos / S_BLOCK] = 1LLU << (pos % S_BLOCK);
}

uint8_t BitVec::getBit(const uint64_t pos) const{
  return (B_[pos/S_BLOCK] >> (pos % S_BLOCK)) & 1;
}

uint64_t BitVec::getBits(const uint64_t pos, const uint64_t len) const{
  uint64_t blockInd1    = pos / S_BLOCK;
  uint64_t blockOffset1 = pos % S_BLOCK;
  if (blockOffset1 + len <= S_BLOCK){
    return mask(B_[blockInd1] >> blockOffset1, len);
  } else {
    uint64_t blockInd2    = ((pos + len - 1) / S_BLOCK);
    return  mask((B_[blockInd1] >> blockOffset1) + (B_[blockInd2] << (S_BLOCK - blockOffset1)), len);
  }
}

void BitVec::save(ostream& os) const {
  os.write((const char*)&size_, sizeof(size_));
  os.write((const char*)&B_[0],  sizeof(B_[0])*B_.size());
}

void BitVec::load(istream& ifs) {
  ifs.read((char*)&size_, sizeof(size_));
  B_.resize((size_ + S_BLOCK - 1) / S_BLOCK);
  ifs.read((char*)&B_[0],  sizeof(B_[0])*B_.size());
}

size_t BitVec::size() const {
  return size_;
}

void BitVec::clear() {
  B_.clear();
  size_ = 0;
}

void BitVec::print() const {
  for (size_t i = 0; i < size_; ++i){
    if (getBit(i)) cout << "1";
    else           cout << "0";
    if ((i+1)%8 == 0){
      cout << " ";
      if ((i+1)%64 == 0) cout << endl;
    } 
  }
}

size_t BitVec::getAllocSize() const {
  return B_.size() * sizeof(B_[0]);
}

uint64_t BitVec::lookupBlock(const size_t ind) const{
  return B_[ind];
}


}
