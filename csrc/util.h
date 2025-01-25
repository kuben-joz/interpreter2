#pragma once

#include <cassert>
#include <cstdint>
#include <vector>

#define bs_type uint64_t

class DynamicBitset {
public:
  uint64_t sz;
  const uint64_t mod = sizeof(bs_type) * 8;
  std::vector<bs_type> bits;

  DynamicBitset(uint64_t sz);

  void do_and(const DynamicBitset &other);
  void do_or(const DynamicBitset &other);
  bool do_or_with_checks(const DynamicBitset &other);

  void set(uint64_t idx);
  void clear();
};