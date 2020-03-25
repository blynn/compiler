extern char __heap_base;
void grow_memory_to(unsigned long);
void* malloc(unsigned long n) {
  static unsigned long bump = (unsigned long) &__heap_base;
  unsigned long bump0 = bump;
  bump += n;
  grow_memory_to((bump + 65535) / 65536);
  return (void*) bump0;
}

void *memcpy(void*dest, const void*src, unsigned long n) {
  char *d = dest;
  const char *s = src;
  while(n--) *d++ = *s++;
  return dest;
}
