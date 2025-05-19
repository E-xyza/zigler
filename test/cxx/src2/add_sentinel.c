#include <stdio.h>

int count_string(const char* str) {
  int count = 0;
  while (*str) { 
    count++; 
    str += 1;
  }
  return count;
}