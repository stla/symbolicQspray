#ifndef ___SYMBOLICQSPRAY___
#define ___SYMBOLICQSPRAY___

#include "ratioOfQsprays.h"

typedef Qspray<RatioOfQsprays<gmpq>>
  SymbolicQspray;
typedef std::unordered_map<powers, RatioOfQsprays<gmpq>, PowersHasher>
  symbolicQspray;


#endif
