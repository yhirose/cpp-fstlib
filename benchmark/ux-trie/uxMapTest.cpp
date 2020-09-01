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
#include "uxMap.hpp"

using namespace std;

TEST(uxmap, trivial){
  ux::Map<int> uxm;
  
  vector<string> wordList;
  vector<int> valueList; 
  uxm.build(wordList);
  ASSERT_EQ(0, uxm.size());
}


TEST(uxmap, simple){
  vector<string> wordList;
  vector<int> valueList;
  wordList.push_back("i");
  valueList.push_back(1);
  wordList.push_back("in");
  valueList.push_back(2);
  wordList.push_back("to");
  valueList.push_back(3);
  wordList.push_back("we");
  valueList.push_back(4);
  wordList.push_back("inn");
  valueList.push_back(5);
  wordList.push_back("tea");
  valueList.push_back(6);
  wordList.push_back("ten");
  valueList.push_back(7);

  vector<string> origWordList = wordList;
  ux::Map<int> uxm;
  uxm.build(wordList);

  for (size_t i = 0; i < origWordList.size(); ++i){
    string key = origWordList[i];
    ASSERT_EQ(0, uxm.set(key.c_str(), key.size(), valueList[i]));
  }
      
  for (size_t i = 0; i < origWordList.size(); ++i){
    string key = origWordList[i];
    int ret = -1;
    ASSERT_EQ(0, uxm.get(key.c_str(), key.size(), ret));
    ASSERT_EQ(valueList[i], ret);
  }
}

TEST(uxmap, pair){
  vector<pair<string, int> > kvs;
  kvs.push_back(make_pair("i",   1));
  kvs.push_back(make_pair("in",  2));
  kvs.push_back(make_pair("to",  3));
  kvs.push_back(make_pair("we",  4));
  kvs.push_back(make_pair("inn", 5));
  kvs.push_back(make_pair("tea", 6));
  kvs.push_back(make_pair("ten", 7));
  
  ux::Map<int> uxm;
  uxm.build(kvs);

  for (size_t i = 0; i < kvs.size(); ++i){
    int ret = -1;
    string key = kvs[i].first;
    ASSERT_EQ(0, uxm.get(key.c_str(), key.size(), ret));
    ASSERT_EQ(kvs[i].second, ret);
  }
}

TEST(uxmap, map){
  map<string, int> kvs;
  kvs[string("i")] = 1;
  kvs[string("in")] = 2;
  kvs[string("to")] = 3;
  kvs[string("we")] = 4;
  kvs[string("inn")] = 5;
  kvs[string("tea")] = 6;
  kvs[string("ten")] = 7;
  
  ux::Map<int> uxm;
  uxm.build(kvs);
  for (map<string, int>::const_iterator it = kvs.begin();
       it != kvs.end(); ++it){
    string key = it->first;
    int ret = -1;
    ASSERT_EQ(0, uxm.get(key.c_str(), key.size(), ret));
    ASSERT_EQ(it->second, ret);
  }
}

TEST(uxmap, save){
  map<string, int> kvs;
  kvs[string("i")] = 1;
  kvs[string("in")] = 2;
  kvs[string("to")] = 3;
  kvs[string("we")] = 4;
  kvs[string("inn")] = 5;
  kvs[string("tea")] = 6;
  kvs[string("ten")] = 7;
  
  ux::Map<int> uxm;
  uxm.build(kvs);

  ostringstream os;
  ASSERT_EQ(0, uxm.save(os));
  istringstream is(os.str());
  ux::Map<int> uxm_load;
  ASSERT_EQ(0, uxm_load.load(is));
  for (map<string, int>::const_iterator it = kvs.begin();
       it != kvs.end(); ++it){
    string key = it->first;
    int ret = -1;
    ASSERT_EQ(0, uxm_load.get(key.c_str(), key.size(), ret));
    ASSERT_EQ(it->second, ret);
  }
}
