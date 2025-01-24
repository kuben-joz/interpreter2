#include "util.h"

DynamicBitset::DynamicBitset(uint64_t sz)
    : sz(sz), bits((sz + mod - 1) / mod) {}


void DynamicBitset::do_and(const DynamicBitset &other) {
  assert(sz == other.sz && "DynBitset sizes differ");
  for (uint64_t i = 0; i < bits.size(); i++) {
    bits[i] &= other.bits[i];
  }
}

void DynamicBitset::do_or(const DynamicBitset &other) {
  assert(sz == other.sz && "DynBitset sizes differ");
  uint64_t i = 0;
  while(i < bits.size()) {
    bits[i] |= other.bits[i];
    i++;
  }
}

bool DynamicBitset::do_or_with_checks(const DynamicBitset &other) {
  assert(sz == other.sz && "DynBitset sizes differ");
  uint64_t i = 0;
  bool res = false;
  while(!res && i < bits.size()) {
    if(bits[i] ^ other.bits[i]) {
      res = true;
    }
    bits[i] |= other.bits[i];
    i++;
  }
  while(i < bits.size()) {
    bits[i] |= other.bits[i];
    i++;
  }
  return res;
}

void DynamicBitset::set(uint64_t idx) {
  assert(idx < sz && "Bit out of range");
  bits[idx / mod] |= 1ULL << (idx % mod);
}

