
// Basic types
#ifndef BITBOARD_INCLUDED
#define BITBOARD_INCLUDED 1

#include <crtdbg.h>
#include "types.h"

namespace NNC {
	//////////////////////////////////////////////////////////////////////////////
	/// Bitboard

	extern Bitboard between[64][64];
	// ray array -- the ray beginning at (but not including) sq1 in the direction of sq2 if sq1 and sq2 are on the same rank/file/diagonal, else 0
	extern Bitboard open_ray[64][64];
	// Piece types for which a move from sq1 to sq2 is PseudoLegal.
	extern uint8    PseudoLegal[128][64];

	extern Bitboard beside[64];
	extern uint64 diagU_BB[64];
	extern uint64 diagD_BB[64];

	// White relative ranks
	constexpr Bitboard R1 = Bitboard(0x00000000000000ffULL), R2 = Bitboard(0x000000000000ff00ULL);
	constexpr Bitboard R3 = Bitboard(0x0000000000ff0000ULL), R4 = Bitboard(0x00000000ff000000ULL);
	constexpr Bitboard R5 = Bitboard(0x000000ff00000000ULL), R6 = Bitboard(0x0000ff0000000000ULL);
	constexpr Bitboard R7 = Bitboard(0x00ff000000000000ULL), R8 = Bitboard(0xff00000000000000ULL);

	constexpr Bitboard FA = Bitboard(0x0101010101010101ULL);	// A file
	constexpr Bitboard FH = Bitboard(0x8080808080808080ULL);	// H file

	inline Bitboard FileOf_bb(Square sq) { return FA << FileOf(sq); }
	inline Bitboard RankOf_bb(Square sq) { return R1 << Rank8(sq); }

	constexpr uint64 rank_BB[2][8] = {
		{ 0x00000000000000ffULL, 0x000000000000ff00ULL, 0x0000000000ff0000ULL, 0x00000000ff000000ULL,
		  0x000000ff00000000ULL, 0x0000ff0000000000ULL, 0x00ff000000000000ULL, 0xff00000000000000ULL },
		  // relative ranks for black
		  { 0xff00000000000000ULL, 0x00ff000000000000ULL, 0x0000ff0000000000ULL, 0x000000ff00000000ULL,
			0x00000000ff000000ULL, 0x0000000000ff0000ULL, 0x000000000000ff00ULL, 0x00000000000000ffULL }
	};

	inline Bitboard rank_bb(Rank_t r) { Assert(0 <= r && r < 8);  return Bitboard(rank_BB[0][r]); }
	// Relative rank
	inline Bitboard rank_bb(Rank_t r, Color c) { return Bitboard(rank_BB[c][r]); }

	template <Color S, int N> constexpr Bitboard bbRank = Bitboard(rank_BB[S][N]); //  S == White ? masks::bbRank<N> : masks::bbRank<7 - N>;

	const uint64 file_BB[8] = {
		0x0101010101010101ULL, 0x0202020202020202ULL, 0x0404040404040404ULL, 0x0808080808080808ULL,
		0x1010101010101010ULL, 0x2020202020202020ULL, 0x4040404040404040ULL, 0x8080808080808080ULL
	};


	inline Bitboard file_bb(int n) { Assert(0 <= n && n < 8); return Bitboard(file_BB[n]); }


	inline Bitboard DiagOf_bb(Square sq, bool UorD) {
		SquareOk(sq); return Bitboard(UorD ? diagU_BB[sq] : diagD_BB[sq]);
#if 0
		int f = FileOf(sq);
		int r = RankOf(sq);
		return Bitboard(UorD ? diagU_BB[7 - f + r] : diagD_BB[f + r]);
#endif
	}


	
	const Bitboard pawnHomeRank[2] = { rank_bb(RANK_2), rank_bb(RANK_7) };
	inline Bitboard between_bb(Square sq1, Square sq2) { SquareOk(sq1); SquareOk(sq2); return between[sq1][sq2]; }

	// Assuming ksq and p are either on same rank or file, compute bitboard of squares between.
	inline Bitboard between_rook_bb(Square ksq, Square p)
	{
		// compute bits between p and ksq
		Square sq0 = ksq, sq1 = ksq;
		Bitboard bbb = FA;
		SqrInt d = p - ksq;
		sq0 = d < 0 ? sq0 : p;	// sq0 = min(ksq,p)
		sq1 = d < 0 ? p : sq1;	// sq1 = max(ksq,p)
		bbb = d & 7 ? R1 : bbb;	// bbb = (p-ksq)&7 ? R1 : FA
		return andn(bbb << sq0, bbb << sq1) ^ Bit(sq0);
	}

	inline Bitboard open_ray_bb(Square sq1, Square sq2) { SquareOk(sq1); SquareOk(sq2); return open_ray[sq1][sq2]; }
	inline Bitboard beside_bb(Square sq) { return sq < 64 ? beside[sq] : NO_SQUARES; }	// ** TO DO ** Avoid conditional
}
#endif
