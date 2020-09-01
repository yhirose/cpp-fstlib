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

#include "uxUtil.hpp"

namespace ux{

uint64_t lg2(const uint64_t x){
  uint64_t ret = 0;
  while (x >> ret){
    ++ret;
  }
  return ret;
}
  
uint64_t mask(uint64_t x, uint64_t pos){
  return x & ((1LLU << pos) - 1);
}

uint64_t popCount(uint64_t r) {
  r = (r & 0x5555555555555555ULL) +
    ((r >> 1) & 0x5555555555555555ULL);
  r = (r & 0x3333333333333333ULL) +
    ((r >> 2) & 0x3333333333333333ULL);
  r = (r + (r >> 4)) & 0x0f0f0f0f0f0f0f0fULL;
  r = r + (r >>  8);
  r = r + (r >> 16);
  r = r + (r >> 32);
  return (uint64_t)(r & 0x7f);
}

uint64_t popCountMasked(uint64_t x, uint64_t pos){
  return popCount(mask(x, pos));
}

uint64_t selectBlock(uint64_t r, uint64_t x, uint8_t b) {
  if (!b) x = ~x;
  uint64_t x1 = x - ((x & 0xAAAAAAAAAAAAAAAALLU) >> 1);
  uint64_t x2 = (x1 & 0x3333333333333333LLU) + ((x1 >> 2) & 0x3333333333333333LLU);
  uint64_t x3 = (x2 + (x2 >> 4)) & 0x0F0F0F0F0F0F0F0FLLU;
  
  uint64_t pos = 0;
  for (;;  pos += 8){
    uint64_t b = (x3 >> pos) & 0xFFLLU;
    if (r <= b) break;
    r -= b;
  }

  uint64_t v2 = (x2 >> pos) & 0xFLLU;
  if (r > v2) {
    r -= v2;
    pos += 4;
  }

  uint64_t v1 = (x1 >> pos) & 0x3LLU;
  if (r > v1){
    r -= v1;
    pos += 2;
  }

  uint64_t v0  = (x >> pos) & 0x1LLU;
  if (v0 < r){
    r -= v0;
    pos += 1;
  }

  return pos;
}

uint64_t getBitNum(uint64_t oneNum, uint64_t num, uint8_t bit){
   if (bit) return oneNum;
   else     return num - oneNum;
}

}

