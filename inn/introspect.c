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

void espy(u n) { pdump = 128; traverse(n); espy_unmark(n); putchar('\n');
}
u vmdump(u n) {
  if (!isAddr(n)) return n;
  pdump = 128, traverse(n), unmark(n);
  return pdump;
}
u scratch_at(u n) { return scratchpad[n]; }
void scratch_reset() { scratchpadend = scratchpad; }
int precompiled() {
  u *p = precompiled_bytecode;
  for (u lim = *p++; lim; lim--) {
    u sym_count = *p++;
    while (sym_count--) *rootend++ = tagcheck(*p++);
    u mem_count = *p++;
    scratchpadend = p + mem_count;
    vmheap(p);
    p = scratchpadend;
  }
  scratchpadend = scratchpad;
  u hp0 = hp;
  u *pend = p + sizeof(precompiled_bytecode)/sizeof(*precompiled_bytecode);
  while (p < pend) {
    u x = *p++;
    if (isAddr(x)) x = x - 128 + hp0;
    mem[hp++] = x;
    u y = *p++;
    // TODO: NUM64
    if (isAddr(y) && x != _NUM) y = y - 128 + hp0;
    mem[hp++] = y;
  }
  return hp0;
}
