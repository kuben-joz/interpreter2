#include "util.h"
#include <cstdint>

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
  while (i < bits.size()) {
    bits[i] |= other.bits[i];
    i++;
  }
}

bool DynamicBitset::do_or_with_checks(const DynamicBitset &other) {
  assert(sz == other.sz && "DynBitset sizes differ");
  uint64_t i = 0;
  bs_type change = 0;
  while (!change && i < bits.size()) {
    change = ~bits[i] & other.bits[i];
    bits[i] |= change;
    i++;
  }
  while (i < bits.size()) {
    bits[i] |= other.bits[i];
    i++;
  }
  return change;
}

bool DynamicBitset::operator==(const DynamicBitset &other) {
  uint64_t i = 0;
  while (i < bits.size() && i < other.bits.size()) {
    if (bits[i] != other.bits[i]) {
      return false;
    }
    i++;
  }
  while (i < bits.size()) {
    if (bits[i] != 0) {
      return false;
    }
    i++;
  }
  while (i < other.bits.size()) {
    if (other.bits[i] != 0) {
      return false;
    }
    i++;
  }
  return true;
}

void DynamicBitset::set(uint64_t idx) {
  assert(idx < sz && "Bit out of range");
  bits[idx / mod] |= 1ULL << (idx % mod);
}

void DynamicBitset::clear() { std::fill(bits.begin(), bits.end(), 0); }

void DynamicBitset::expand(uint64_t new_sz) {
  if (new_sz <= sz) {
    return;
  }
  uint64_t new_cells = (new_sz + mod - 1) / mod;
  bits.resize(new_cells, 0);
  sz = new_sz;
}
