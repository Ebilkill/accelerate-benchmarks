#include <stdio.h>
#include <stdlib.h>
#include <time.h>

extern float dot(const float* restrict lhs, const float* restrict rhs, size_t length);

int main() {
  size_t size = 100000000;
  float* xs = (float*)malloc(size * 4);
  float* ys = (float*)malloc(size * 4);

  for (size_t i = 0; i < size; ++i) {
    xs[i] = i;
    ys[i] = i * 2 + 1;
  }

  double after, before = clock() / (double)CLOCKS_PER_SEC;
  dot(xs, ys, size);
  after = clock() / (double)CLOCKS_PER_SEC;

  printf("Time taken: %fms\n", (after - before) * 1000.0);

  return 0;
}

