int strcmp(const char *, const char *);
void main_compile(void);
void main_comb(void);
void main_type(void);
int main(int argc, char** argv) {
  rts_init();
  if (argc > 1) {
    if (!strcmp(argv[1], "comb")) main_comb();
    else if (!strcmp(argv[1], "type")) main_type();
  } else main_compile();
  return 0;
}
