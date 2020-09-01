#include <iostream>
#include <fstream>
#include <string>
#include "cmdline.h"
#include "uxTrie.hpp"

using namespace std;

#include <time.h>
#include <sys/time.h>
#include <stdio.h>

double gettimeofday_sec()
{
  struct timeval tv;
  gettimeofday(&tv, NULL);
  return tv.tv_sec + (double)tv.tv_usec*1e-6;
}

void analyzeKeyList(const vector<string>& keyList){
  size_t lcs = 0;
  for (size_t i = 1; i < keyList.size(); ++i){
    const string& s1 = keyList[i-1];
    const string& s2 = keyList[i];
    size_t j = 0;
    for (; j < s1.size() && j < s2.size() && s1[j] == s2[j]; ++j) {};
    lcs += j;
  }
  cout << "  avelcs:\t" << (float)lcs / keyList.size() << endl;
}


size_t allKeySize(const vector<string>& keyList){
  size_t ret = 0;
  for (size_t i = 0; i < keyList.size(); ++i){
    ret += keyList[i].size();
  }
  return ret;
}

void reportStat(const ux::Trie& ux, const vector<string>& keyList){
  ux.allocStat(ux.getAllocSize(), cout);
  ux.stat(cout);
  analyzeKeyList(keyList);
  size_t originalSize = allKeySize(keyList);
  cout << "originalSize:\t" << allKeySize(keyList) << endl
       << "   indexSize:\t" << ux.getAllocSize() << " (" << (float)ux.getAllocSize() / originalSize << ")" << endl
       << "      keyNum:\t" << keyList.size() << endl;
}

void printQuery(const ux::Trie& ux,
		const std::string& query,
		const int limit){
  cout << "query:[" << query << "]" << endl;
  
  // prefixSearch
  size_t retLen = 0;
  cout << "prefixSearch: ";
  ux::id_t id = ux.prefixSearch(query.c_str(), query.size(), retLen);
  if (id == ux::NOTFOUND){
    cout << "not found." << endl;
  } else {
    cout << ux.decodeKey(id) << "\t(id=" << id << ")" << endl;
  }

  vector<ux::id_t> retIDs;  
  // commonPrefixSearch
  ux.commonPrefixSearch(query.c_str(), query.size(), retIDs, (size_t)limit);
  cout << "commonPrefixSearch: " << retIDs.size() << " found." << endl;
  for (size_t i = 0; i < retIDs.size(); ++i){
    cout << ux.decodeKey(retIDs[i]) << "\t(id=" << retIDs[i] << ")" << endl;
  }

  // predictiveSearch
  ux.predictiveSearch(query.c_str(), query.size(), retIDs, (size_t)limit);
  cout << "predictiveSearch: " << retIDs.size() << " found." << endl;
  for (size_t i = 0; i < retIDs.size(); ++i){
    cout << ux.decodeKey(retIDs[i]) << "\t(id=" << retIDs[i] << ")" << endl;
  }
}


int readKeyList(const string& fn, vector<string>& keyList){
  ifstream ifs(fn.c_str());
  if (!ifs){
    cerr << "cannot open " << fn << endl;
    return -1;
  }

  for (string key; getline(ifs, key); ){
    if (key.size() > 0 &&
	key[key.size()-1] == '\r'){
      key = key.substr(0, key.size()-1);
    }
    keyList.push_back(key);
  }
  return 0;
} 

void performanceTest(ux::Trie& ux, vector<string>& keyList){
  random_shuffle(keyList.begin(), keyList.end());
  
  double start = gettimeofday_sec();
  size_t dummy = 0;
  for (size_t i = 0; i < keyList.size() && i < 1000; ++i){
    size_t retLen = 0;
    dummy += ux.prefixSearch(keyList[i].c_str(), keyList[i].size(), retLen);
  }
  double end   = gettimeofday_sec();
  cout << "  query time:\t" << end - start << endl; 
  cout << "  check keys:\t" << min((int)keyList.size(), 1000) << endl;
  
  if (dummy == 777){
    cerr << "luckey" << endl;
  }
}

int buildUX(const string& fn, const string& index, const bool uncompress, const int verbose){
  vector<string> keyList;
  if (readKeyList(fn, keyList) == -1){
    return -1;
  }
  ux::Trie ux;
  double start = gettimeofday_sec();
  ux.build(keyList, !uncompress);
  double elapsedTime = gettimeofday_sec() - start;
  if (verbose >= 1){
    cout << "  index time:\t" << elapsedTime << endl;
    reportStat(ux, keyList);
  }
  if (verbose >= 2){
    performanceTest(ux, keyList);
  }

  if (index == "") return 0;
  int err = ux.save(index.c_str());
  if (err != ux::Trie::SUCCESS){
    cerr << ux.what(err) << " " << index << endl;
    return -1;
  }
  return 0;
}

int searchUX(const string& index, const int limit){
  ux::Trie ux;
  int err = ux.load(index.c_str());
  if (err != ux::Trie::SUCCESS){ 
    cerr << ux.what(err) << " " << index << endl;
    return -1;
  }
  cout << "read:" << ux.size() << " keys" << endl;
  
  string query;
  for (;;){
    putchar('>');
    getline(cin, query);
    if (query.size() == 0){
      break;
    }
    printQuery(ux, query, limit);
  }

  return 0;
}

int listUX(const string& index){
  ux::Trie ux;
  int err = ux.load(index.c_str());
  if (err != ux::Trie::SUCCESS){
    cerr << ux.what(err) << " " << index << endl;
    return -1;
  }
  
  for (size_t i = 0; i < ux.size(); ++i){
    cout << ux.decodeKey(i) <<  endl;
  }
  return 0;
}


int main(int argc, char* argv[]){
  cmdline::parser p;
  p.add<string>("keylist",    'k', "key list", false);
  p.add<string>("index",      'i', "index",     true);
  p.add<int>   ("limit",      'l', "limit at search", false, 10);
  p.add        ("uncompress", 'u', "tail is uncompressed");
  p.add        ("enumerate",  'e', "enumerate all keywords");
  p.add<int>   ("verbose",    'v', "verbose mode", 0);
  p.add("help", 'h', "this message");
  p.set_program_name("ux");

  if (!p.parse(argc, argv) || p.exist("help")){
    cerr << p.usage() << endl;
    return -1;
  }

  if (p.exist("keylist")){
    return buildUX(p.get<string>("keylist"), p.get<string>("index"), p.exist("uncompress"), p.get<int>("verbose"));
  } else if (p.exist("enumerate")){
    return listUX(p.get<string>("index"));
  } else {
    return searchUX(p.get<string>("index"), p.get<int>("limit"));
  }

  return 0; // NOT COME 
}
