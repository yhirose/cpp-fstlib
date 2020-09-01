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

#ifndef UX_UTIL_HPP__
#define UX_UTIL_HPP__

#include <stdint.h>

namespace ux {
  uint64_t lg2(uint64_t x);
  uint64_t mask(uint64_t x, uint64_t pos);
  uint64_t popCount(uint64_t r);
  uint64_t popCountMasked(uint64_t x, uint64_t pos);
  uint64_t selectBlock(uint64_t pos, uint64_t x, uint8_t b);
  uint64_t getBitNum(uint64_t oneNum, uint64_t num, uint8_t bit);
}

#endif // UX_UTIL_HPP__

