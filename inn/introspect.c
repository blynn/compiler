static void espy_traverse(u n) {
  if (!isAddr(n)) return (void) printf("%d", n);
  u x = mem[n];
  if (x & (1 << 31)) return (void) printf("*");
  mem[n] |= 1 << 31;
  putchar('(');
  espy_traverse(x);
  putchar(' ');
  u y = mem[n + 1];
  if (x == _NUM64) printf("0x%llx", *((unsigned long long*) (mem + n + 2)));
  else if (x == _NUM) printf("%u", y);
  else espy_traverse(y);
  putchar(')');
}
static u pdump;
static void vmdump_traverse(u n) {
  if (!isAddr(n)) return;
  u x = mem[n];
  if (x & (1 << 31)) return;
  mem[n] |= 1 << 31;
  vmdump_traverse(x);
  u y = mem[n + 1];
  if (x != _NUM && x != _NUM64) vmdump_traverse(y);
  altmem[n] = pdump;
  pdump += 2;
  printf("%u ", isAddr(x) ? altmem[x] : x);
  if (x == _NUM64) {
    printf("%u %u %u ", y, mem[n + 2], mem[n + 3]);
    pdump += 2;
  } else printf("%u ", x != _NUM && isAddr(y) ? altmem[y] : y);
}
static void unmark(u n) {
  if (isAddr(n)) {
    u x = mem[n];
    if (x & (1 << 31)) {
      x -= 1 << 31;
      unmark(mem[n] = x);
      if (x != _NUM && x != _NUM64) unmark(mem[n+1]);
    }
  }
}
void espy(u n) { espy_traverse(n); puts(""); unmark(n); }
void vmdump(u n) {
  if (!isAddr(n)) return (void) printf("(%u)\n", n);
  pdump = 128; vmdump_traverse(n); unmark(n); printf("(%u)\n", altmem[n]);
}
