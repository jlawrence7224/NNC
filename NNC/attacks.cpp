#include "attacks.h"
#include <random>

namespace NNC {

	Bitboard between[64][64];
	// ray array -- the ray beginning at (but not including) sq1 in the direction of sq2 if sq1 and sq2 are on the same rank/file/diagonal, else 0
	Bitboard open_ray[64][64];
	// Piece types for which a move from sq1 to sq2 is PseudoLegal.
	uint8 PseudoLegal[128][64];
	Bitboard beside[64];
	uint64 diagU_BB[64];
	uint64 diagD_BB[64];
	Bitboard bit[64];

	Hash		Zobrist::piece_square_hash[2][6][64];
	Hash		Zobrist::ep_hash[16];
	Hash		Zobrist::castling_rights_hash[16];

	typedef std::mt19937_64 PRNG;
	void Zobrist::init()
	{
		PRNG rng(uint64(123456));
		std::uniform_int_distribution<uint64> dist;

		for (ClrInt c = White; c <= Black; ++c)
			for (PcInt pt = 0; pt < 6; ++pt)
				for (SqrInt s = A1; s <= H8; ++s)
				{
					Hash z = dist(rng);
					Zobrist::piece_square_hash[c][pt][s] = z;
				}

		// En Passant
		for (int i = 0; i < 8; ++i)
			Zobrist::ep_hash[i] = dist(rng);

		// castling rights
		Hash cr_hash[4];
		for (int i = 0; i < 4; ++i)
			cr_hash[i] = rng();

		for (uint64 cr = 0; cr < 16; ++cr)
		{
			Hash k = 0;
			for (auto c : Bitboard(cr))
			{
				Assert(c < 4);
				k ^= cr_hash[c];
			}
			Zobrist::castling_rights_hash[cr] = k;
		}
	}

	CastlingRights CR_RightsLost[64] = {
		CR_QS_WHT,CR_NONE,CR_NONE,CR_NONE,CR_WHITE,CR_NONE,CR_NONE,CR_KS_WHT,
		CR_NONE,CR_NONE,CR_NONE,CR_NONE,CR_NONE,CR_NONE,CR_NONE,CR_NONE,
		CR_NONE,CR_NONE,CR_NONE,CR_NONE,CR_NONE,CR_NONE,CR_NONE,CR_NONE,
		CR_NONE,CR_NONE,CR_NONE,CR_NONE,CR_NONE,CR_NONE,CR_NONE,CR_NONE,
		CR_NONE,CR_NONE,CR_NONE,CR_NONE,CR_NONE,CR_NONE,CR_NONE,CR_NONE,
		CR_NONE,CR_NONE,CR_NONE,CR_NONE,CR_NONE,CR_NONE,CR_NONE,CR_NONE,
		CR_NONE,CR_NONE,CR_NONE,CR_NONE,CR_NONE,CR_NONE,CR_NONE,CR_NONE,
		CR_QS_BLK,CR_NONE,CR_NONE,CR_NONE,CR_BLACK,CR_NONE,CR_NONE,CR_KS_BLK,
	};

	uint64 king_attack(int sq)
	{
		uint64 result = 0ULL;
		int rk = sq / 8, fl = sq % 8;

		if (rk < 7)
		{
			result |= square_bb(rk + 1, fl);
			if (fl > 0) result |= square_bb(rk + 1, fl - 1);
			if (fl < 7) result |= square_bb(rk + 1, fl + 1);
		}
		if (fl > 0) result |= square_bb(rk, fl - 1);
		if (fl < 7) result |= square_bb(rk, fl + 1);
		if (rk > 0)
		{
			result |= square_bb(rk - 1, fl);
			if (fl > 0) result |= square_bb(rk - 1, fl - 1);
			if (fl < 7) result |= square_bb(rk - 1, fl + 1);
		}
		return result;
	}

	uint64 knight_attack(int sq)
	{
		uint64 result = 0ULL;
		int rk = sq / 8, fl = sq % 8;
		if (rk < 7)
		{
			if (fl > 1) result |= square_bb(rk + 1, fl - 2);
			if (fl < 6) result |= square_bb(rk + 1, fl + 2);
		}
		if (rk < 6)
		{
			if (fl > 0) result |= square_bb(rk + 2, fl - 1);
			if (fl < 7) result |= square_bb(rk + 2, fl + 1);
		}
		if (rk > 0)
		{
			if (fl > 1) result |= square_bb(rk - 1, fl - 2);
			if (fl < 6) result |= square_bb(rk - 1, fl + 2);
		}
		if (rk > 1)
		{
			if (fl > 0) result |= square_bb(rk - 2, fl - 1);
			if (fl < 7) result |= square_bb(rk - 2, fl + 1);
		}
		return result;
	}

	uint64 rmask(int sq)
	{
		uint64 result = 0ULL;
		int rk = RankOf(Square(sq)), fl = FileOf(Square(sq)), r, f;
		for (r = rk + 1; r <= 6; r++) result |= square_bb(r, fl);
		for (r = rk - 1; r >= 1; r--) result |= square_bb(r, fl);
		for (f = fl + 1; f <= 6; f++) result |= square_bb(rk, f);
		for (f = fl - 1; f >= 1; f--) result |= square_bb(rk, f);
		return result;
	}

	uint64 ratt(int sq, uint64 block) {
		uint64 result = 0ULL;
		int rk = RankOf(Square(sq)), fl = FileOf(Square(sq)), r, f;
		for (r = rk + 1; r <= 7; r++) {
			result |= square_bb(r, fl);
			if (block & square_bb(r, fl)) break;
		}
		for (r = rk - 1; r >= 0; r--) {
			result |= square_bb(r, fl);
			if (block & square_bb(r, fl)) break;
		}
		for (f = fl + 1; f <= 7; f++) {
			result |= square_bb(rk, f);
			if (block & square_bb(rk, f)) break;
		}
		for (f = fl - 1; f >= 0; f--) {
			result |= square_bb(rk, f);
			if (block & square_bb(rk, f)) break;
		}
		return result;
	}


	uint64 batt(int sq, uint64 block) {
		uint64 result = 0ULL;
		int rk = RankOf(Square(sq)), fl = FileOf(Square(sq)), r, f;
		for (r = rk + 1, f = fl + 1; r <= 7 && f <= 7; r++, f++) {
			result |= square_bb(r, f);
			if (block & (1ULL << (f + r * 8))) break;
		}
		for (r = rk + 1, f = fl - 1; r <= 7 && f >= 0; r++, f--) {
			result |= square_bb(r, f);
			if (block & (1ULL << (f + r * 8))) break;
		}
		for (r = rk - 1, f = fl + 1; r >= 0 && f <= 7; r--, f++) {
			result |= square_bb(r, f);
			if (block & (1ULL << (f + r * 8))) break;
		}
		for (r = rk - 1, f = fl - 1; r >= 0 && f >= 0; r--, f--) {
			result |= square_bb(r, f);
			if (block & (1ULL << (f + r * 8))) break;
		}
		return result;
	}

	uint64 bmask(int sq)
	{
		Bitboard result = NO_SQUARES;
		int rk = RankOf(Square(sq)), fl = FileOf(Square(sq)), r, f;
		for (r = rk + 1, f = fl + 1; r <= 6 && f <= 6; r++, f++) result |= square_bb(r, f);
		for (r = rk + 1, f = fl - 1; r <= 6 && f >= 1; r++, f--) result |= square_bb(r, f);
		for (r = rk - 1, f = fl + 1; r >= 1 && f <= 6; r--, f++) result |= square_bb(r, f);
		for (r = rk - 1, f = fl - 1; r >= 1 && f >= 1; r--, f--) result |= square_bb(r, f);
		return result;
	}

	// The rising (left to right) or falling (left to right) diagonal through sq.
	uint64 dmask(int sq, bool DiagRising)
	{
		Bitboard result = NO_SQUARES;
		int rk = RankOf(Square(sq)), fl = FileOf(Square(sq)), r, f;
		if (DiagRising)
		{
			for (r = rk, f = fl; r <= 7 && f <= 7; r++, f++) result |= square_bb(r, f);
			for (r = rk, f = fl; r >= 0 && f >= 0; r--, f--) result |= square_bb(r, f);
		}
		else
		{
			for (r = rk, f = fl; r >= 0 && f <= 7; r--, f++) result |= square_bb(r, f);
			for (r = rk, f = fl; r <= 7 && f >= 0; r++, f--) result |= square_bb(r, f);
		}
		return result;
	}

	uint16 attack_table[107648];
	struct attack_info att_info[64];

	void initTables()
	{
		// initialize the zobrist keys
		Zobrist::init();

		// bit: square to Bitboard table
		for (bindex n = 0; n < 64; n++)
			bit[n] = Bitboard(1) << n;

		// King and Knight attacks
		for (int sq = 0; sq < 64; sq++)
		{
			att_info[sq].king_att = king_attack(sq);
			att_info[sq].knight_att = knight_attack(sq);
		}
		// Slider attacks
		int aoff = 0;	// offset into attack array
		// ...Bishops
		for (int sq = 0; sq < 64; sq++)
		{
			// Init info for sq
			uint16* data = &attack_table[aoff];
			uint64  mask1 = bmask(sq);
			uint64  mask2 = batt(sq, 0);
			att_info[sq].bdata = data;
			att_info[sq].bocc_mask = mask1;
			att_info[sq].brng_mask = mask2;

			// Init attack data sq
			int n = 1 << PopCnt(mask1);
			for (int idx = 0; idx < n; idx++)
			{
				//    pext    data     pdep
				// occ---->idx---->bits---->att
				//    mask1            mask2
				uint64 occ = Pdep(idx, mask1);
				uint64 att = batt(sq, occ);
				uint16 bits = uint16(Pext(att, mask2));
				data[idx] = bits;
			}
			aoff += n;
		}
		// Rook attacks
		for (int sq = 0; sq < 64; sq++)
		{
			// Init info for sq
			uint16* data = &attack_table[aoff];
			uint64  mask1 = rmask(sq);
			uint64  mask2 = ratt(sq, 0);
			att_info[sq].rdata = data;
			att_info[sq].rocc_mask = mask1;
			att_info[sq].rrng_mask = mask2;

			// Init attack data for sq
			int n = 1 << PopCnt(mask1);
			for (int idx = 0; idx < n; idx++)
			{
				//    pext    data     pdep
				// occ---->idx---->bits---->att
				//    mask1            mask2
				uint64 occ = Pdep(idx, mask1);
				uint64 att = ratt(sq, occ);
				uint16 bits = uint16(Pext(att, mask2));
				data[idx] = bits;
			}
			aoff += n;
		}
		Assert(aoff == DIM(attack_table));

#define PawnCapture 1
		// between array -- the squares between sq1 and sq2 exclusive if sq1 and sq2 are on the same rank/file/diagonal, else 0
		// ray array -- the ray beginning at (but not including) sq1 in the direction of sq2 if sq1 and sq2 are on the same rank/file/diagonal, else 0

		//  ** TO DO ** PseudoLegal for Pawn1
		for (Square sq1 : ALL_SQUARES)
		{

			Bitboard br = bishopRange(sq1);
			Bitboard rr = rookRange(sq1);
			for (Square sq2 : ALL_SQUARES)
			{
				between[sq1][sq2] = NO_SQUARES;
				open_ray[sq1][sq2] = NO_SQUARES;
				PseudoLegal[sq1][sq2] = 0;
				Bitboard occ = sq1 | sq2;
				if (br & sq2)
				{
					Bitboard att2 = bishopAttack(sq2, occ);
					between[sq1][sq2] = bishopAttack(sq1, occ) & att2;
					open_ray[sq1][sq2] = (att2^occ) & br;
					PseudoLegal[sq1][sq2] |= (1 << Queen) | (1 << Bishop);
					if (kingRange(sq1) & sq2)
					{	// 1 square diagonal move
						// King move or a Pawn capture ------------------------------------ color of pawn
						PseudoLegal[sq1][sq2] |= (1 << King);
						Color c = Color(uint8(sq1 > sq2));
						// Regular pawn moves are indexed by color
						Rank_t r = RelativeRank(c, RankOf(sq1));
						if (r != RANK_1 && r != RANK_7)
							PseudoLegal[sq1][sq2] |= (uint8(1) << c);
					}
				}
				else if (rr & sq2)
				{
					Bitboard att2 = rookAttack(sq2, square_bb(sq1));
					between[sq1][sq2] = rookAttack(sq1, square_bb(sq2)) & att2;
					open_ray[sq1][sq2] = (att2^occ) & rr;
					PseudoLegal[sq1][sq2] |= (1 << Queen) | (1 << Rook);
					if (kingRange(sq1) & sq2)
					{
						PseudoLegal[sq1][sq2] |= (1 << King);
						// Omit promotions (special moves) from the 'PseudoLegal' table
						Color c = Color(uint8(sq1 > sq2));
						Rank_t r = RelativeRank(c, RankOf(sq1));
						if (abs(int(sq2) - int(sq1)) == 8 && r != RANK_1 && r != RANK_7)
						{
							// Regular pawn moves are indexed by color
							// Pawn advance ---------------- color of pawn
							PseudoLegal[sq1][sq2] |= (uint8(1) << c);
						}
					}
				}
				else if (knightRange(sq1) & sq2)
					PseudoLegal[sq1][sq2] |= (1 << Knight);
			}
		}

		// beside array -- the (one or) two squares on adjacent files of the same rank.
		// diag arrays -- the squares on a rising/falling diagonal through a square
		for (Square sq : ALL_SQUARES)
		{
			Bitboard bb = NO_SQUARES;
			if ((FA & sq) == 0)
				bb |= (sq - 1);
			if ((FH & sq) == 0)
				bb |= (sq + 1);
			beside[sq] = bb;
			diagU_BB[sq] = dmask(sq, true);
			diagD_BB[sq] = dmask(sq, false);
		}
	}
}
