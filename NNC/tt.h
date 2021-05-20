
#ifndef TT_INCLUDED
#define TT_INCLUDED 1

#include "types.h"

namespace NNC {

	typedef uint32 KM;
	typedef uint16 Key;
	inline KM	makeKM(Hash h, uint16 m) { return uint32(h) ^ (uint32(m) << 16); }

	struct GBD {
		GBD(uint16_t gbd) : GBD(reinterpret_cast<GBD&>(gbd)) {}
		GBD(uint8 g, uint8 b, uint8 d) : GBD((((g << 2) + b) << 8) + d) { Assert(b < 4); }

		uint8	m_depth;
		uint8	m_genbound;

		uint8   gen()				const { return m_genbound >> 2; }
		uint8   bound()				const { return m_genbound & 0x3; }
		uint8	depth()				const { return m_depth; }

		GBD&	incrGen() { m_genbound += 4; return *this; }
		GBD&	setGen(Gen g) { m_genbound = ((g << 2) + bound()); return *this; }
	};

	/// 64 bit TTE      -- Transposition Table Entry:


	struct TTE {
		TTE(uint64_t  m) : TTE(reinterpret_cast<TTE&>(m)) {}
		TTE(KM k, GBD g, Score s) : m_km(k), m_gbd(g), m_score(s) {}
		TTE(KM k, uint8 g, uint8 b, uint8 d, Score s) : m_km(k), m_gbd(g, b, d), m_score(s) {}
		TTE(Hash h, Move m, uint8 d, uint8 g, uint8 b, Score s) : TTE(makeKM(h, uint16(m)), GBD(g, b, d), s) {}

		operator uint64_t() { return reinterpret_cast<uint64_t&>(*this); }

		KM		m_km;
		GBD	    m_gbd;
		int16	m_score;

		// uses this idea http://talkchess.com/forum/viewtopic.php?topic_view=threads&p=733840&t=65327
		TTE&    unhash(Hash hash) { m_km ^= uint32_t(hash); return *this; }
		// bool	moveOk()			const { return MoveOk(move()); }
		bool	match()				const { return (m_km & 0xffff80c0) == 0; }

		Key 	key()				const { return Key(m_km); }
		Move    move()				const { return Move(m_km >> 16); }
		uint8   gen()				const { return m_gbd.m_genbound >> 2; }
		Bound   bound()				const { return Bound(m_gbd.m_genbound & 0x3); }
		uint8	depth()				const { return m_gbd.m_depth; }
		Score	score()				const { return Score(m_score); }

		GBD	    gbd()				const { return m_gbd; }
		TTE&	setGen(Gen g) { m_gbd.setGen(g); return *this; }
		uint16	replacementScore(GBD g) { return (reinterpret_cast<uint16&>(g) - reinterpret_cast<uint16&>(m_gbd)) & ~uint16(0x0400); }
	};


#define NBIT(n)	((1ULL << n) - 1)

	// return greatest power of 2 <= n
	inline size_t trunc2(size_t n) { return size_t(1) << BSR(n); }

	/// A TranspositionTable consists of a power of 2 number of buckets and each
	/// bucket consists of bucketSize number of TTE. Each non-empty entry
	/// contains information of exactly one position. The size of a bucket
	/// evenly divides the cache line size. Each TTE is 8 byte aligned.

	class TranspositionTable {
	public:
		static const int CacheLineSize = 64;
		static const int bucketSize = 4;
		static const int minMbSize = 1;

		typedef struct {
			uint64       entry[bucketSize];
		} bucket_t;

		static		bucket_t*	alloc(size_t bucketCount);	// memory allocator for TT

		static_assert(sizeof(bucket_t) == CacheLineSize / 2, "bucket size incorrect");

		// m_gbd == |gggggg11|11111111| search generation + max bound/depth
		TranspositionTable() : m_table(0), m_bucketCount(0), m_hash_shift(0), m_gbd(0, 3, 255) {}
		~TranspositionTable() { if (m_table) _aligned_free((void*)m_table); }

		// Hi order bits of the hash determine bucket index
		// Lo order bits are used as Key and Move mask
		bucket_t*	bucket_addr(Hash hash) { return &m_table[hash >> m_hash_shift]; }
		TTE			probe(Hash hash, TTE*& replacement);	// Returns 0 for CACHE_MISS
		TTE			store(Hash k, Move m, Score s, Bound b, Depth d, TTE* tptr) { return *tptr = TTE(k, m, d, searchGen(), b, s); }

		// Replacement strategy: prefer entries for replacement sorted on
		// Age ignoring low order bit		-- (Age & ~1) replace oldest first (treating entries from current and previous search as equal age)
		//									-- Age == (searchGen() - tte.gen()), age 0 equivalent to age 1, age 2 equivalent to age 3, etc.
		// Bound (None > Hi > Lo > Exact)	-- replace No and Hi(alpha/fail lo) bounds first, then Lo(beta/fail hi) bounds, then exact bounds
		// Depth							-- compare same (similar) age/bound entries according to depth, replacing shallowest entries first.

		// Highest scoring entry is replaced 
		// m_gbd has bound/depth set to ExactBound/HighestDepth (0x3ff) so the subtraction of those bits
		// never carries into the Age (search gen - tte gen) bits. Consequently Age&~1 can be implemented as below
		Gen			searchGen()						const { return m_gbd.gen(); }
		void		setGen(Gen g) { m_gbd.setGen(g); }
		void		new_search() { m_gbd.incrGen(); Assert(m_gbd.bound() == 3 && m_gbd.depth() == 255); }

		int			hashfull();
		void		resize(size_t mbSize);
		void		clear();

	private:
		bucket_t*		m_table;
		size_t		m_bucketCount;
		Hash	 		m_hash_shift;
		GBD			m_gbd;							// |gggggg11|11111111| search generation + max bound/depth
	};

	extern TranspositionTable TT;
}
#endif // #ifndef TT_INCLUDED
