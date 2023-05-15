static void putu(u n) {
  if (!n) {
    putchar('0');
    return;
  }
  u b = 1;
  while (b != 1000000000 && b * 10 <= n) b *= 10;
  for(;;)  {
    u q = n / b;
    putchar(q + '0');
    if (b == 1) return;
    n = n - q * b;
    b /= 10;
  }
}

static u pdump;
static void traverse(u n) {
  if (!isAddr(n)) return;
  u x = mem[n];
  if (x & (1 << 31)) return;
  mem[n] |= 1 << 31;
  altmem[n] = pdump;
  u *p = scratchpad + pdump - 128;
  pdump += 2;
  traverse(x);
  *p++ = isAddr(x) ? altmem[x] : x;
  if (x == _NUM64) pdump += 2;
  u y = mem[n + 1];
  if (x != _NUM && x != _NUM64) traverse(y);
  *p++ = x != _NUM && isAddr(y) ? altmem[y] : y;
}
static void espy_unmark(u n) {
  if (!isAddr(n)) return (void)putu(n);
  u x = mem[n];
  if (!(x & (1 << 31))) return (void)putchar('*');
  putchar('(');
  x -= 1 << 31;
  espy_unmark(mem[n] = x);
  putchar(' ');
  u y = mem[n+1];
  if (x == _NUM) putu(y);
  else if (x == _NUM64) putu(y), putchar(' '), putu(mem[n+2]), putchar(' '), putu(mem[n+3]), putchar(' ');
  else espy_unmark(y);
  putchar(')');
}
static void unmark(u n) {
  if (!isAddr(n)) return;
  u x = mem[n];
  if (!(x & (1 << 31))) return;
  x -= 1 << 31;
  u y = mem[n+1];
  unmark(mem[n] = x);
  if (x != _NUM && x != _NUM64) unmark(y);
}

void espy(u n) { pdump = 128; traverse(n); espy_unmark(n); putchar('\n'); }
u vmdump(u n) {
  if (!isAddr(n)) return n;
  pdump = 128, traverse(n), unmark(n);
  return pdump;
}
u scratch_at(u n) { return scratchpad[n]; }
void scratch_reset() { scratchpadend = scratchpad; }
u unleb128(unsigned char **pp) {
  u n = 0, b = 0;
  for (;;) {
    unsigned char *p = *pp;
    *pp = p + 1;
    if (*p < 128) return n + (*p << b);
    n += (*p - 128) << b;
    b += 7;
  }
}
#ifdef PRECOMPILED
u precompiled() {
  unsigned char *p = precompiled_bytecode;
  for (u lim = unleb128(&p); lim; lim--) {
    u sym_count = unleb128(&p);
    while (sym_count--) *rootend++ = tagcheck(unleb128(&p));
    u mem_count = unleb128(&p);
    scratchpadend = scratchpad;
    while (mem_count--) *scratchpadend++ = unleb128(&p);
    vmheap(scratchpad);
  }
  scratchpadend = scratchpad;
  u hp0 = hp;
  unsigned char *pend = p + sizeof(precompiled_bytecode)/sizeof(*precompiled_bytecode);
  while (p < pend) {
    u x = unleb128(&p);
    if (isAddr(x)) x = x - 128 + hp0;
    mem[hp++] = x;
    u y = unleb128(&p);
    if (x == _NUM64) {
      mem[hp++] = y;
      mem[hp++] = unleb128(&p);
      mem[hp++] = unleb128(&p);
    } else {
      if (isAddr(y) && x != _NUM) y = y - 128 + hp0;
      mem[hp++] = y;
    }
  }
  return hp0;
}
#else
u precompiled() { return 0; }
#endif
