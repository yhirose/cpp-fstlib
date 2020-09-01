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

#include <gtest/gtest.h>
#include <vector>
#include <string>
#include "bitVec.hpp"
#include "rsDic.hpp"
#include "uxUtil.hpp"

using namespace std;
using namespace ux;

TEST(bitvec, popcount){
  uint64_t x = 0;
  for (uint64_t i = 0; i < 64; ++i){
    ASSERT_EQ(i, popCount(x));
    x |= (1LLU << i);
  }
}

TEST(bitvec, selectblock){
  uint64_t x = 0;

  for (uint64_t i = 0; i < 64; ++i){
    ASSERT_EQ(i, selectBlock(i+1, x, 0));
  }

  for (uint64_t i = 0; i < 64; ++i){
    x |= (1LLU << i);
  }

  for (uint64_t i = 0; i < 64; ++i){
    ASSERT_EQ(i, selectBlock(i+1, x, 1));
  }
}

TEST(bitvec, trivial_zero){
  BitVec bv;
  for (int i = 0; i < 1000; ++i){
    bv.push_back(0);
  }
  
  RSDic rs;
  rs.build(bv);
  ASSERT_EQ(1000, rs.size());
  for (size_t i = 0; i < rs.size(); ++i){
    ASSERT_EQ(0  , bv.getBit(i));
    ASSERT_EQ(i+1, rs.rank(i, 0));
    ASSERT_EQ(i  , rs.select(i+1, 0));
  }
}

TEST(bitvec, trivial_one){
  BitVec bv;
  for (int i = 0; i < 1000; ++i){
    bv.push_back(1);
  }

  RSDic rs;
  rs.build(bv);
  ASSERT_EQ(1000, rs.size());
  for (size_t i = 0; i < rs.size(); ++i){
    ASSERT_EQ(1  , rs.getBit(i));
    ASSERT_EQ(i+1, rs.rank(i, 1));
    ASSERT_EQ(i  , rs.select(i+1, 1));
  }
}

/*
TEST(bitvec, trivial_interleave){
  RSDic bv;
  for (int i = 0; i < 1000; ++i){
    bv.push_back((i+1)%2);
  }
  bv.build();
  ASSERT_EQ(1000, bv.size());
  for (size_t i = 0; i < bv.size(); ++i){
    ASSERT_EQ(i/2 + 1, bv.rank2(i));
  }
}
*/


TEST(bitvec, random){
  BitVec bv;
  vector<int> B;
  for (int i = 0; i < 100000; ++i){
    int b = rand() % 2;
    bv.push_back(b);
    B.push_back(b);
  }
  
  RSDic rs;
  rs.build(bv);
  ASSERT_EQ(100000, rs.size());
  int sum = 0;
  for (size_t i = 0; i < rs.size(); ++i){
    ASSERT_EQ(B[i]  , bv.getBit(i));
    sum += B[i];
    if (B[i]){
      ASSERT_EQ(sum, rs.rank(i, 1));
      ASSERT_EQ(i,   rs.select(sum, 1));
    } else {
      ASSERT_EQ(i - sum + 1, rs.rank(i, 0));
      ASSERT_EQ(i,           rs.select(i-sum+1, 0));
    }
  }
}

TEST(bitvec, vacuum){
  BitVec bv;
  vector<int> B;
  for (int i = 0; i < 100000; ++i){
    int b = rand() % 2;
    bv.push_back(b);
    B.push_back(b);
  }
  
  RSDic rs;
  rs.build(bv);
}





