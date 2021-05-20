__forceinline int PopCnt(uint64_t a)
{
  int c = 1;

  while (a &= a - 1)
    c++;
  return c;
}
#endif

__forceinline int BSR(uint64_t a) 
{
  int v;

  _BitScanReverse64(&v, a);
  return v;
}

__forceinline int BSF(uint64_t a) 
{
  int v;

  _BitScanForward64(&v, a);
  return v;
}
