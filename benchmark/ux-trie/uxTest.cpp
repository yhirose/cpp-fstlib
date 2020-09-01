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
#include <sstream>
#include <map>
#include "uxTrie.hpp"

using namespace std;

TEST(ux, trivial){
  ux::Trie ux;
  vector<string> wordList;
  ux.build(wordList);
  string q = "hoge";
  size_t retLen = 0;
  ASSERT_EQ(ux::NOTFOUND, ux.prefixSearch(q.c_str(), q.size(), retLen));
}

TEST(ux, simple){
  vector<string> wordList;
  wordList.push_back("i");
  wordList.push_back("in");
  wordList.push_back("to");
  wordList.push_back("we");
  wordList.push_back("inn");
  wordList.push_back("tea");
  wordList.push_back("ten");
  vector<string> origWordList = wordList;
  ux::Trie ux;
  ux.build(wordList);

  for (size_t i = 0; i < origWordList.size(); ++i){
    ASSERT_EQ(origWordList[i], ux.decodeKey(i));
  }
}

TEST(ux, clear){
  vector<string> wordList;
  wordList.push_back("i");
  wordList.push_back("in");
  wordList.push_back("to");
  wordList.push_back("we");
  wordList.push_back("inn");
  wordList.push_back("tea");
  wordList.push_back("ten");
  vector<string> origWordList = wordList;
  ux::Trie ux;
  ux.build(wordList);

  for (size_t i = 0; i < origWordList.size(); ++i){
    ASSERT_EQ(origWordList[i], ux.decodeKey(i));
  }

  ux.clear();
  ux.build(wordList);
  for (size_t i = 0; i < origWordList.size(); ++i){
    ASSERT_EQ(origWordList[i], ux.decodeKey(i));
  }
}


TEST(ux, decodeKey){
  ux::Trie ux;
  vector<string> wordList;
  wordList.push_back("tok");
  wordList.push_back("osak");
  wordList.push_back("okina");
  wordList.push_back("fukush");
  ux.build(wordList);

  ASSERT_EQ("fukush", ux.decodeKey(0));  
  ASSERT_EQ("tok"   , ux.decodeKey(1));
  ASSERT_EQ("okina" , ux.decodeKey(2));
  ASSERT_EQ("osak"  , ux.decodeKey(3));


}

TEST(ux, prefixSearch){
  ux::Trie ux;
  vector<string> wordList;
  wordList.push_back("tea");
  wordList.push_back("top");
  wordList.push_back("bear");
  wordList.push_back("bep");
  wordList.push_back("東京都");
  ux.build(wordList);

  size_t retLen = 0;
  string q1 = "tea";
  ASSERT_NE(ux::NOTFOUND, ux.prefixSearch(q1.c_str(), q1.size(), retLen));
  ASSERT_EQ(3, retLen);
  string q2 = "hoge";
  ASSERT_EQ(ux::NOTFOUND, ux.prefixSearch(q2.c_str(), q2.size(), retLen));
  string q3 = "te";
  ASSERT_EQ(ux::NOTFOUND, ux.prefixSearch(q3.c_str(), q3.size(), retLen));
  string q4 = "東京都";
  ASSERT_NE(ux::NOTFOUND, ux.prefixSearch(q4.c_str(), q4.size(), retLen));
  ASSERT_EQ(9, retLen);
}

TEST(ux, commonPrefixSearch){
  ux::Trie ux;
  vector<string> wordList;
  wordList.push_back("tea");
  wordList.push_back("top");
  wordList.push_back("bear");
  wordList.push_back("bep");
  wordList.push_back("beppu");
  ux.build(wordList);

  vector<ux::id_t> retIDs;
  string q1 = "beppuhaiiyu";
  ASSERT_EQ(2, ux.commonPrefixSearch(q1.c_str(), q1.size(), retIDs));
  ASSERT_EQ("bep", ux.decodeKey(retIDs[0]));
  ASSERT_EQ("beppu", ux.decodeKey(retIDs[1]));
}

TEST(ux, predictiveSearch){
  ux::Trie ux;
  vector<string> wordList;
  wordList.push_back("tea");
  wordList.push_back("top");
  wordList.push_back("bear");
  wordList.push_back("bep");
  wordList.push_back("beppu");
  ux.build(wordList);

  vector<ux::id_t> retIDs;
  string q1 = "be";
  ASSERT_EQ(3, ux.predictiveSearch(q1.c_str(), q1.size(), retIDs));
  ASSERT_EQ("bear",  ux.decodeKey(retIDs[0]));
  ASSERT_EQ("bep",   ux.decodeKey(retIDs[1]));
  ASSERT_EQ("beppu", ux.decodeKey(retIDs[2]));
}

TEST(ux, predictiveSearch2){
  ux::Trie ux;
  vector<string> wordList;
  wordList.push_back("東京都");
  ux.build(wordList);

  vector<ux::id_t> retIDs;
  string q1 = "東";
  ASSERT_EQ(1, ux.predictiveSearch(q1.c_str(), q1.size(), retIDs));
  ASSERT_EQ(1, retIDs.size());
}
  
TEST(ux, save){
  const char* fn = "uxTestSave.ind";
  ux::Trie ux;
  string q1 = "tea";
  string q2 = "top";
  string q3 = "bear";
  string q4 = "bep";
  string q5 = "beppu";

  vector<string> wordList;
  wordList.push_back(q1);
  wordList.push_back(q2);
  wordList.push_back(q3);
  wordList.push_back(q4);
  wordList.push_back(q5);
  ux.build(wordList);
  ASSERT_EQ(0, ux.save(fn));

  ux::Trie ux2;
  ASSERT_EQ(0, ux2.load(fn));
  ASSERT_EQ(0, remove(fn));

  ASSERT_EQ(ux.size(), ux2.size());

  size_t retLen = 0;
  ASSERT_NE(ux::NOTFOUND, ux2.prefixSearch(q1.c_str(), q1.size(), retLen));
  ASSERT_NE(ux::NOTFOUND, ux2.prefixSearch(q2.c_str(), q2.size(), retLen));
  ASSERT_NE(ux::NOTFOUND, ux2.prefixSearch(q3.c_str(), q3.size(), retLen));
  ASSERT_NE(ux::NOTFOUND, ux2.prefixSearch(q4.c_str(), q4.size(), retLen));
  ASSERT_NE(ux::NOTFOUND, ux2.prefixSearch(q5.c_str(), q5.size(), retLen));
}

TEST(ux, large){
  ux::Trie trie;
  vector<string> wordList;
  for (int i = 0; i < 10000; ++i){
    ostringstream os;
    os << i;
    wordList.push_back(os.str());
  }
  
  trie.build(wordList);
  map<int, int> dic;
  for (size_t i = 0; i < wordList.size(); ++i){
    size_t retLen = 0;
    dic[trie.prefixSearch(wordList[i].c_str(), wordList[i].size(), retLen)]++;
  }
  ASSERT_EQ(dic.size(), trie.size());
}

TEST(ux, predictiveTest){
  vector<string> str;
  str.push_back("xx");
  str.push_back("xxy");
  str.push_back("xxxz");
  ux::Trie trie(str);
  
  vector<ux::id_t> v;
  string q = "xxy";
  ASSERT_EQ(1, trie.predictiveSearch(q.c_str(), q.size(), v));
}  

