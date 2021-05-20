#ifndef INTTYP_INCLUDED
#define INTTYP_INCLUDED 1

// Basic integer types

#include <stdint.h>
#include "intrinsics.h"

typedef unsigned int                    uint;

typedef int8_t				int8;
typedef uint8_t				uint8;
typedef int16_t				int16;
typedef uint16_t			uint16;
typedef int32_t				int32;
typedef uint32_t			uint32;
typedef int64_t				int64;
typedef uint64_t			uint64;

// integer booleans - 0 is false, non-zero true
typedef uint32 bool32;	// 32 bit integer representing a boolean
typedef uint16 bool16;	// 16 bit integer representing a boolean

#define lo8(x) uint8(x)
#define hi8(x) uint8((x)>>8)
#define lo16(x) uint16(x)
#define hi16(x) uint16((x)>>16)

typedef int				index;
typedef int		   	        bindex;	// index for bit shift operations

#ifdef _DEBUG
template<class T>
inline void Assert(T x) { _ASSERT(x); }
#else
#define Assert(x)
#endif
#define DIM(x) (sizeof(x)/sizeof(x[0]))

#if 0
template<class T>
T LSB(T b) { return T(b & (-b)); }

#define ClearSet(xs,b)	(xs ^= (b))
#define ClearLsb(xs)	(xs &= xs-1)
#define GreatestSq(x)   BSR(x)
#define LeastSq(x)      BSF(x)
#define LeastBit(x)	    (blsi(x))
#endif


#endif // INTTYP_INCLUDED
