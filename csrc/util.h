#pragma once

#include <cassert>
#include <cstdint>
#include <vector>

class DynamicBitset {
private:
  uint64_t sz;
  uint64_t mod = 64;
  std::vector<uint64_t> bits;

public:
  DynamicBitset(uint64_t sz) : sz(sz), bits((sz + mod - 1) / mod) {}
  uint64_t size();
  void do_and(DynamicBitset &other) {
    uint64_t min_sz = std::min(sz, other.sz);
    uint64_t i = 0;
    for (; i < min_sz; i++) {
      bits[i] &= other.bits[i];
    }
    for (; i < sz; i++) {
      bits[i] = 0;
    }
  }

  void set(uint64_t idx) {
    assert((idx / mod) < bits.size() && "Bit out of range");
    bits[idx / mod] |= 1ULL << (idx % mod);
  }

  void set(std::vector<uint64_t> &idxs) {
    for (auto idx : idxs) {
      set(idx);
    }
  }
};