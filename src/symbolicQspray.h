#ifndef ___SYMBOLICQSPRAY___
#define ___SYMBOLICQSPRAY___

#include "ratioOfQsprays.h"
using namespace RATIOOFQSPRAYS;

typedef Qspray<RatioOfQsprays<gmpq>>
  SymbolicQspray;
typedef std::unordered_map<powers, RatioOfQsprays<gmpq>, PowersHasher>
  symbolicQspray;


#endif
