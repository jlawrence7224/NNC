
#ifndef INTRINSICS_INCLUDED
#define INTRINSICS_INCLUDED 1

#include <intrin.h>
#include <immintrin.h>
namespace NNC {
#pragma intrinsic(_BitScanForward64)
#pragma intrinsic(_BitScanReverse64)
#pragma intrinsic(_pext_u64)
#pragma intrinsic(_pdep_u64)
#pragma intrinsic(_mm_popcnt_u64)
#pragma intrinsic(_mm_prefetch)
#pragma intrinsic(_blsi_u64)
#pragma intrinsic(_blsr_u64)

#define PREFETCH 1
	__forceinline void prefetch(void* addr)
	{
#  if defined(PREFETCH) && (defined(__INTEL_COMPILER) || defined(_MSC_VER))
		_mm_prefetch((char*)addr, _MM_HINT_T0);
#endif
	}
	
	__forceinline uint64_t PopCnt(uint64_t a) {
		return _mm_popcnt_u64(a);
	}

	__forceinline unsigned long BSR(uint64_t a)
	{
		unsigned long v;

		_BitScanReverse64(&v, a);
		return v;
	}

	__forceinline unsigned long BSF(uint64_t a)
	{
		unsigned long v;

		_BitScanForward64(&v, a);
		return v;
	}

#if 0
	__forceinline int Popcnt(uint64_t a)
	{
		int c = 0;
		if (a) do { c++; } while (a &= a - 1);

		return c;
	}

	__forceinline uint64 _pext_u64(uint64 val, uint64 mask)
	{
		uint64 res = 0;
		for (uint64 bb = 1; mask; bb += bb)
		{
			if (val & mask & -mask)
				res |= bb;
			mask &= mask - 1;
		}
		return res;
	}

	__forceinline uint64 _pdep_u64(uint64 val, uint64 mask)
	{
		uint64 res = 0;
		for (uint64 bb = 1; mask; bb += bb)
		{
			if (val & bb)
				res |= mask & -mask;
			mask &= mask - 1;
		}
		return res;
	}
#endif

#define Pext(v,m) _pext_u64((v),(m))
#define Pdep(v,m) _pdep_u64((v),(m))
}

#endif // INTRINSICS_INCLUDED
