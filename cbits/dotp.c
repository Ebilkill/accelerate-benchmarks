#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

void init_array1(size_t len, float* restrict data) {
  for (size_t i = 0; i < len; ++i) {
    data[i] = i;
  }
}

void init_array2(size_t len, float* restrict data) {
  for (size_t i = 0; i < len; ++i) {
    data[i] = i * 2 + 1;
  }
}

float dot(const float* restrict lhs, const float* restrict rhs, size_t length) {
  float result = 0.0F;
  for (size_t x = 0; x < length; ++x) {
    result += lhs[x] * rhs[x];
  }

  return result;
}

