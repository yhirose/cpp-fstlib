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

#include <algorithm>
#include <queue>
#include <fstream>
#include <cassert>
#include <map>
#include <cmath>
#include "uxTrie.hpp"

using namespace std;

namespace ux{

struct RangeNode{
  RangeNode(size_t _left, size_t _right) :
    left(_left), right(_right) {}
  size_t left;
  size_t right;
};
  
Trie::Trie() : vtailux_(NULL), tailIDLen_(0), keyNum_(0), isReady_(false) {
} 

Trie::Trie(vector<string>& keyList, const bool isTailUX) : vtailux_(NULL), tailIDLen_(0), keyNum_(0), isReady_(false) {
  build(keyList, isTailUX);
} 
  
Trie::~Trie(){
  delete vtailux_;
}
  
void Trie::build(vector<string>& keyList, const bool isTailUX){
  clear();
  sort(keyList.begin(), keyList.end());
  keyList.erase(unique(keyList.begin(), keyList.end()), keyList.end());
  
  keyNum_ = keyList.size();
  
  queue<RangeNode> q;
  queue<RangeNode> nextQ;
  if (keyNum_ != 0){
    q.push(RangeNode(0, keyNum_));
  }

  BitVec terminalBV;
  BitVec tailBV;
  BitVec loudBV;
  loudBV.push_back(0); // super root
  loudBV.push_back(1);
  
  for (size_t depth = 0;;){
    if (q.empty()){
      swap(q, nextQ);
      ++depth;
      if (q.empty()) break;
    }
    RangeNode& rn = q.front();
    const size_t left  = rn.left;
    const size_t right = rn.right;
    q.pop();
    
    string& cur = keyList[left];
    if (left + 1 == right &&
	depth + 1 < cur.size()){ // tail candidate
      loudBV.push_back(1);
      terminalBV.push_back(1);
      tailBV.push_back(1);
      string tail;
      for (size_t i = depth; i < cur.size(); ++i){
	tail += cur[i];
      }
      vtails_.push_back(tail);
      continue;
    } else {
      tailBV.push_back(0);
    }
    
    assert(keyList.size() > left);
    size_t newLeft = left;
    if (depth == cur.size()){
      terminalBV.push_back(1);
      ++newLeft;
      if (newLeft == right){
	loudBV.push_back(1);
	continue;
      }
    } else {
      terminalBV.push_back(0);
    }
    
    size_t  prev  = newLeft;
    assert(keyList[prev].size() > depth);
    uint8_t prevC = (uint8_t)keyList[prev][depth];
    uint64_t degree = 0;
    for (size_t i = prev+1; ; ++i){
      if (i < right && 
	  prevC == (uint8_t)keyList[i][depth]){
	continue;
      }
      edges_.push_back(prevC);
      loudBV.push_back(0);
      degree++;
      nextQ.push(RangeNode(prev, i));
      if (i == right){
	break;
      }
      prev  = i;
      assert(keyList[prev].size() > depth);
      prevC = keyList[prev][depth];
      
    }
    loudBV.push_back(1);
  }
  
  loud_.build(loudBV);
  terminal_.build(terminalBV);
  tail_.build(tailBV);
  
  if (keyNum_ > 0){
    isReady_ = true;
  }
  
  if (isTailUX){
    buildTailUX();
  }
}
  
int Trie::save(const char* fn) const {
  ofstream ofs(fn, ios::binary);
  if (!ofs){
    return FILE_OPEN_ERROR;
  }
  return save(ofs);
}

int Trie::load(const char* fn){
  ifstream ifs(fn, ios::binary);
  if (!ifs){
    return FILE_OPEN_ERROR;
  }
  return load(ifs);
}
  
int Trie::save(std::ostream& os) const {
  loud_.save(os);
  terminal_.save(os);
  tail_.save(os);
  tailIDs_.save(os);
  
  os.write((const char*)&keyNum_, sizeof(keyNum_));
  size_t edgesSize = edges_.size();
  os.write((const char*)&edgesSize, sizeof(edgesSize));
  os.write((const char*)&edges_[0], sizeof(edges_[0]) * edges_.size()); 
  
  int useUX = (vtailux_ != NULL);
  os.write((const char*)&useUX, sizeof(useUX));
  if (useUX){
    int err = 0;
    if ((err = vtailux_->save(os)) != 0){
      return err;
    }
  } else {
    size_t tailsNum  = vtails_.size();
    os.write((const char*)&tailsNum,  sizeof(tailsNum));
    for (size_t i = 0; i < vtails_.size(); ++i){
      size_t tailSize = vtails_[i].size();
      os.write((const char*)&tailSize,  sizeof(tailSize));
      os.write((const char*)&vtails_[i][0], sizeof(vtails_[i][0]) * vtails_[i].size());
    }
  }
  
  if (!os){
    return SAVE_ERROR;
  }
  return 0;
}

int Trie::load(std::istream& is){
  clear();
  loud_.load(is);
  terminal_.load(is);
  tail_.load(is);
  tailIDs_.load(is);
  
  is.read((char*)&keyNum_, sizeof(keyNum_));
  size_t edgesSize = 0;
  is.read((char*)&edgesSize, sizeof(edgesSize));
  edges_.resize(edgesSize);
  is.read((char*)&edges_[0], sizeof(edges_[0]) * edges_.size());
  
  int useUX = 0;
  is.read((char*)&useUX, sizeof(useUX));
  if (useUX){
    vtailux_ = new Trie;
    int err = 0;
    if ((err = vtailux_->load(is)) != 0){
      return err;
    }
    size_t tailNum = vtailux_->size();
    tailIDLen_ = lg2(tailNum); 
    
  } else {
    size_t tailsNum  = 0;
    is.read((char*)&tailsNum,  sizeof(tailsNum));
    vtails_.resize(tailsNum);
    for (size_t i = 0; i < tailsNum; ++i){
      size_t tailSize = 0;
      is.read((char*)&tailSize, sizeof(tailSize));
      vtails_[i].resize(tailSize);
      is.read((char*)&vtails_[i][0], sizeof(vtails_[i][0]) * vtails_[i].size());
    }
  }
  
  if (!is){
    return LOAD_ERROR;
  }
  isReady_ = true;
  return 0;
}
  
id_t Trie::prefixSearch(const char* str, const size_t len, size_t& retLen) const{
  vector<id_t> retIDs;
  traverse(str, len, retLen, retIDs, 0xFFFFFFFF);
  if (retIDs.size() == 0){
    return NOTFOUND;
  }
  return retIDs.back();
}
  
size_t Trie::commonPrefixSearch(const char* str, const size_t len, vector<id_t>& retIDs,
			      const size_t limit) const {
  retIDs.clear();
  size_t lastLen = 0;
  traverse(str, len, lastLen, retIDs, limit);
  return retIDs.size();
}
  
size_t Trie::predictiveSearch(const char* str, const size_t len, vector<id_t>& retIDs, 
			    const size_t limit) const{
  retIDs.clear();
  if (!isReady_) return 0;
  if (limit == 0) return 0;
  
  uint64_t pos       = 2;
  uint64_t zeros     = 2;
  for (size_t i = 0; i < len; ++i){
    uint64_t ones = pos - zeros;
    
    if (tail_.getBit(ones)){
      uint64_t tailID = tail_.rank(ones, 1) - 1;
      string tail = getTail(tailID);
      for (size_t j = i; j < len; ++j){
	if (str[j] != tail[j-i]){
	  return 0;
	}
      }
      retIDs.push_back(terminal_.rank(ones, 1) - 1);

      return retIDs.size();
    }
    getChild((uint8_t)str[i], pos, zeros);
    if (pos == NOTFOUND){
      return 0;
    }
  }
  
  // search all descendant nodes from curPos
  enumerateAll(pos, zeros, retIDs, limit);
  return retIDs.size();
}

void Trie::decodeKey(const id_t id, string& ret) const{
  ret.clear();
  if (!isReady_) return;
  
  uint64_t nodeID = terminal_.select(id+1, 1);
  
  uint64_t pos    = loud_.select(nodeID+1, 1) + 1;
  uint64_t zeros  = pos - nodeID;
  for (;;) { 
    uint8_t c = 0;
    getParent(c, pos, zeros);
    if (pos == 0) break;
    ret += (char)c;
  }
  reverse(ret.begin(), ret.end());
  if (tail_.getBit(nodeID)){
    ret += getTail(tail_.rank(nodeID, 1) - 1);
  }
}
  
string Trie::decodeKey(const id_t id) const {
  std::string ret;
  decodeKey(id, ret);
  return ret;
}
  
size_t Trie::size() const {
  return keyNum_;
}

void Trie::clear() {
  loud_.clear();
  terminal_.clear();
  tail_.clear();
  vtails_.clear();
  delete vtailux_;
  vtailux_ = NULL;
  edges_.clear();
  tailIDs_.clear();
  tailIDLen_ = 0;
  keyNum_ = 0;
  isReady_ = false;
}
  
std::string Trie::what(const int error){
  switch(error) {
  case 0:
    return string("succeeded");
  case FILE_OPEN_ERROR: 
    return string("file open error");
  case FILE_WRITE_ERROR:
    return string("file write error");
  case FILE_READ_ERROR:
    return string("file read error");
  default:
    return string("unknown error");
  }
}

size_t Trie::getAllocSize() const{
  size_t retSize = 0;
  if (vtailux_) {
    retSize += vtailux_->getAllocSize();
    retSize += tailIDs_.getAllocSize();
  } else {
    size_t tailLenSum = 0;
    for (size_t i = 0; i < vtails_.size(); ++i){
      tailLenSum += vtails_[i].size();
    }
    retSize += tailLenSum + tailLenSum / 8; // length bit vector
  }
  return retSize + loud_.getAllocSize() + terminal_.getAllocSize() + 
    tail_.getAllocSize() + edges_.size();
}
  
void Trie::allocStat(size_t allocSize, ostream& os) const{
  if (vtailux_) {
    vtailux_->allocStat(allocSize, os);
    size_t size = tailIDs_.getAllocSize();
    os << "tailIDs:\t" << size << "\t" << (float)size / allocSize << endl;
  } else {
    size_t tailLenSum = 0;
    for (size_t i = 0; i < vtails_.size(); ++i){
      tailLenSum += vtails_[i].size();
    }
    os << "   tails:\t" << tailLenSum << "\t" << (float)tailLenSum / allocSize << endl;
    os << " tailLen:\t" << tailLenSum/8 << "\t" << (float)tailLenSum/8 / allocSize << endl;
  }
  os << "    loud:\t" << loud_.getAllocSize() << "\t" << (float)loud_.getAllocSize() / allocSize << endl;
  os << "terminal:\t" << terminal_.getAllocSize() << "\t" << (float)terminal_.getAllocSize() / allocSize << endl;
  os << "    tail:\t" << tail_.getAllocSize() << "\t" << (float)tail_.getAllocSize() / allocSize << endl;
  os << "    edge:\t" << edges_.size() << "\t" << (float)edges_.size() / allocSize << endl;
}
  
void Trie::stat(ostream & os) const {
  size_t tailslen = 0;
  for (size_t i = 0; i < vtails_.size(); ++i){
    tailslen += vtails_[i].size();
  }
  
  os << "   keyNum\t" << keyNum_ << endl
     << "    loud:\t" << loud_.size()     << endl
     << "terminal:\t" << terminal_.size() << endl
     << "    edge:\t" << edges_.size()    << endl
     << " avgedge:\t" << (float)edges_.size() / keyNum_ << endl
     << "  vtails:\t" << tailslen << endl
     << " tailnum:\t" << vtails_.size() << endl
     << " avgtail:\t" << (float)tailslen / keyNum_ << endl
     << endl;
}

  
void Trie::buildTailUX(){
  vector<string> origTails = vtails_;
  try {
    vtailux_ = new Trie;
  } catch (bad_alloc){
    isReady_ = false;
    return;
  }
  for (size_t i = 0; i < vtails_.size(); ++i){
    reverse(vtails_[i].begin(), vtails_[i].end());
  }
  vtailux_->build(vtails_, false);
  tailIDLen_ = lg2(vtailux_->size());
  
  for (size_t i = 0; i < origTails.size(); ++i){
    size_t retLen = 0;
    reverse(origTails[i].begin(), origTails[i].end());
    id_t id = vtailux_->prefixSearch(origTails[i].c_str(), origTails[i].size(), retLen);
    assert(id != NOTFOUND);
    assert(retLen == origTails[i].size());
    tailIDs_.push_back_with_len(id, tailIDLen_);
  }
  vector<string>().swap(vtails_);
}

void Trie::getChild(const uint8_t c, uint64_t& pos, uint64_t& zeros) const {
  for (;; ++pos, ++zeros){
    if (loud_.getBit(pos)){
      pos = NOTFOUND;
      return;
    }
    assert(zeros >= 2);
    assert(edges_.size() > zeros-2);
    if (edges_[zeros-2] == c){
      pos   = loud_.select(zeros, 1)+1;
      zeros = pos - zeros + 1;
      return;
    }
  }
}

bool Trie::isLeaf(const uint64_t pos) const {
  return loud_.getBit(pos);
}
  
void Trie::getParent(uint8_t& c, uint64_t& pos, uint64_t& zeros) const {
  zeros = pos - zeros + 1;
  pos   = loud_.select(zeros, 0);
  if (zeros < 2) return;
  assert(edges_.size() > zeros-2);
  c     = edges_[zeros-2];
}  
  

void Trie::traverse(const char* str, const size_t len, 
		  size_t& lastLen, std::vector<id_t>& retIDs, const size_t limit) const{
  lastLen = 0;
  if (!isReady_) return;
  if (limit == 0) return;
  
  uint64_t pos   = 2;
  uint64_t zeros = 2;
  for (size_t depth = 0; pos != NOTFOUND; ++depth){
    uint64_t ones = pos - zeros;
    
    if (tail_.getBit(ones)){
      size_t retLen = 0;
      if (tailMatch(str, len, depth, tail_.rank(ones, 1)-1, retLen)){
	lastLen = depth + retLen;
	retIDs.push_back(terminal_.rank(ones, 1) - 1);
      }
      break;
    } else if (terminal_.getBit(ones)){
      lastLen = depth;
      retIDs.push_back(terminal_.rank(ones, 1)-1);
      if (retIDs.size() == limit) {
	break;
      }
    }
    if (depth == len) break;
    getChild((uint8_t)str[depth], pos, zeros);
  }
}
  

void Trie::enumerateAll(const uint64_t pos, const uint64_t zeros, vector<id_t>& retIDs, const size_t limit) const{
  const uint64_t ones = pos - zeros;
  if (terminal_.getBit(ones)){
    retIDs.push_back(terminal_.rank(ones, 1) - 1);
  }
  
  for (uint64_t i = 0; loud_.getBit(pos + i) == 0 &&
	 retIDs.size() < limit; ++i){
    uint64_t nextPos = loud_.select(zeros + i, 1)+1;
    enumerateAll(nextPos, nextPos - zeros - i + 1,  retIDs, limit);
  }
}
  


bool Trie::tailMatch(const char* str, const size_t len, const size_t depth,
		   const uint64_t tailID, size_t& retLen) const{
  string tail = getTail(tailID);
  if (tail.size() > len-depth) {
    return false;
  }
  
  for (size_t i = 0; i < tail.size(); ++i){
    if (str[i+depth] != tail[i]) {
      return false;
    }
  }
  retLen = tail.size();
  return true;
}
  
std::string Trie::getTail(const uint64_t i) const{
  if (vtailux_) {
    string ret;
    vtailux_->decodeKey(tailIDs_.getBits(tailIDLen_ * i, tailIDLen_), ret);
    reverse(ret.begin(), ret.end());
    return ret;
  } else {
    return vtails_[i];
  }
}

}
