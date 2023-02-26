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

static void espy_traverse(u n) {
  if (!isAddr(n)) return (void) putu(n);
  u x = mem[n];
  if (x & (1 << 31)) return (void) putchar('*');
  mem[n] |= 1 << 31;
  putchar('(');
  espy_traverse(x);
  putchar(' ');
  u y = mem[n + 1];
  if (x == _NUM64) {
    putu(y), putchar(' ');
    putu(mem[n+2]), putchar(' ');
    putu(mem[n+3]), putchar(' ');
  }
  else if (x == _NUM) putu(y);
  else espy_traverse(y);
  putchar(')');
}
static void espy_unmark(u n) {
  if (!isAddr(n)) return;
  u x = mem[n];
  if (!(x & (1 << 31))) return;
  x -= 1 << 31;
  espy_unmark(mem[n] = x);
  u y = mem[n+1];
  if (x != _NUM && x != _NUM64) espy_unmark(y);
}
static u pdump;
static void vmdump_mark(u n) {
  if (!isAddr(n)) return;
  u x = mem[n];
  if (x & (1 << 31)) return;
  mem[n] |= 1 << 31;
  altmem[n] = pdump;
  pdump += 2;
  vmdump_mark(x);
  if (x == _NUM64) pdump += 2;
  u y = mem[n + 1];
  if (x != _NUM && x != _NUM64) vmdump_mark(y);
}
static void unmark(u n) {
  if (!isAddr(n)) return;
  u x = mem[n];
  if (!(x & (1 << 31))) return;
  x -= 1 << 31;
  putu(isAddr(x) ? altmem[x] : x), putchar(','), putchar(' ');
  u y = mem[n+1];
  if (x == _NUM64) {
    putu(y), putchar(',');
    putu(mem[n+2]), putchar(',');
    putu(mem[n+3]), putchar(',');
  } else putu(x != _NUM && isAddr(y) ? altmem[y] : y), putchar(',');
  unmark(mem[n] = x);
  if (x != _NUM && x != _NUM64) unmark(y);
}

void espy(u n) { espy_traverse(n); putchar('\n'); espy_unmark(n); }
void vmdump(u n) {
  if (!isAddr(n)) putu(n); else pdump = 128, vmdump_mark(n), unmark(n);
  putchar('\n');
}
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
