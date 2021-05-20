

#include <cstring>
#include <iostream>
#include <malloc.h>
#include "NpEngine.h"
#include "pch.h"
#include "tt.h"
#include "assert.h"

namespace NNC {
	// Global transposition table
	// TranspositionTable TT; 

	// TranspositionTable::alloc() allocates bucketCount buckets aligned on a cache line.
	TranspositionTable::bucket_t* TranspositionTable::alloc(size_t bucketCount)
	{
		if (void* tbl = _aligned_malloc(bucketCount * sizeof(bucket_t), CacheLineSize))
		{
			assert(0 == (uintptr_t(tbl) & (CacheLineSize - 1)));
			return (bucket_t*)tbl;
		}
		std::cerr << "Failed to allocate " << bucketCount * sizeof(bucket_t)
			<< "bytes for transposition table." << std::endl;
		exit(EXIT_FAILURE);
	}

	/// TranspositionTable::resize() sets the size of the transposition table,
	/// measured in megabytes. Transposition table consists of a power of 2 number
	/// of buckets and each bucket consists of bucketSize number of TTE.

	void TranspositionTable::resize(size_t mbSize)
	{
		if (mbSize <= minMbSize)
			mbSize = minMbSize;

		size_t bucketCount = trunc2((mbSize * 1024 * 1024) / sizeof(bucket_t));

		if (m_bucketCount == bucketCount && m_table != NULL)
			clear();
		else
		{
			if (m_table)
				free(m_table);

			m_table = alloc(m_bucketCount);
			m_bucketCount = bucketCount;
			m_hash_shift = 64 - BSR(m_bucketCount);
		}
	}

	/// TranspositionTable::clear() overwrites the entire transposition table
	/// with zeros. It is called whenever the table is resized, or when the
	/// user asks the program to clear the table (from the UCI interface).

	void TranspositionTable::clear()
	{
		std::memset(m_table, 0, m_bucketCount * sizeof(bucket_t));
	}


	/// TranspositionTable::probe() looks up the current position in the transposition
	/// table. It returns the TTE if the position is found; otherwise return value is 0.
	/// entry reference points to location to be replaced later. Either current position 
	/// or pointer to least valuable TTE.
#if 0
	TTE TranspositionTable::probe(const Hash hash, TTE*& replacement)
	{
		bucket_t* bucket = bucket_addr(hash);

		int repidx = 0;
		int best = -1;
		TTE repl = 0;
		for (int i = 0; i < bucketSize; ++i)
		{
			TTE tte = bucket->entry[i];
			tte.unhash(hash);
			if (tte.match())
			{
				// hash hit -- hash key cleared by a successful match
				Assert(tte.key() == 0);					// verify
				tte.setGen(searchGen());
				bucket->entry[i] = tte;	// mark entry as hit in current generation
				return replacement = &bucket->entry[i], tte;
			}
			int rscore = tte.replacementScore(m_gbd);
			if (rscore > best)
			{
				repl = tte;
				repidx = i;
				best = rscore;
			}
		}
#if 0
		// Not obvious that this is desirable -- 
		// mark replacement as hit in current generation to
		// encourage children which happen to store into this bucket
		// to replace a different entry.
		bucket->entry[repidx] = repl.setGen(searchGen());
#endif
		return replacement = &bucket->entry[repidx], 0;
	}
#else

	TTE TranspositionTable::probe(const Hash hash, TTE*& replacement)
	{
		bucket_t* bucket = bucket_addr(hash);
		const Hash subhash = uint32_t(hash);	// hi half of hash key set to 0

		for (int i = 0; i < bucketSize; ++i)
		{
			TTE tte = bucket->entry[i] ^ subhash;
			if (tte.match())
			{
				// hash hit -- hash key cleared by a successful match
				Assert(tte.key() == 0);					// verify
				tte.setGen(searchGen());
				bucket->entry[i] = (tte ^ subhash);	// mark entry as hit in current generation
				return replacement = (TTE*)&bucket->entry[i], tte;
			}
		}
		int repidx = 0;
		TTE tte = bucket->entry[0];
		int best = tte.replacementScore(m_gbd);
		for (int i = 1; i < bucketSize; ++i)
		{
			int rscore = tte.replacementScore(m_gbd);
			if (rscore > best)
			{
				repidx = i;
				best = rscore;
			}
		}
		return replacement = (TTE*)&bucket->entry[repidx], 0;
	}
#endif
	/// Returns an approximation of the hashtable occupation during a search. The
	/// hash is x permill full, as per UCI protocol.

	int TranspositionTable::hashfull()
	{
		int cnt = 0;
		Gen g = searchGen();
		const TTE* ttep = (TTE*)&bucket_addr(0)->entry[0];
		for (int i = 0; i < 1000; i++)
		{
			if (ttep[i].gen() == g)
				cnt++;
		}
		return cnt;
	}
}