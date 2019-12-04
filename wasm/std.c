extern char __heap_base;
void* malloc(unsigned long n) {
  static unsigned long bump = (unsigned long) &__heap_base;
  return (void*) ((bump += n) - n);
}

void *memcpy(void*dest, const void*src, unsigned long n) {
  char *d = dest;
  const char *s = src;
  while(n--) *d++ = *s++;
  return dest;
}
