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
static void unmark(u n) {
  if (!isAddr(n)) return;
  u x = mem[n];
  if (!(x & (1 << 31))) return;
  x -= 1 << 31;
  u y = mem[n+1];
  unmark(mem[n] = x);
  if (x != _NUM && x != _NUM64) unmark(y);
}
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
