#ifndef ATTACKS_INCLUDED
#define ATTACKS_INCLUDED 1

#include "types.h"
#include "bitboard.h"
namespace NNC {

	extern const Value val[8]; // = { 0,0,0,1,3,3,5,9 };
	const Bitboard DiagU_0 = A8 | C1 | D2 | E3 | F4 | G5 | H6;
	const Bitboard DiagD_0 = A1 | H1 | G2 | F3 | E4 | D5 | C6 | B7;

	void initTables();

	// 64 byte data structure (64 byte aligned)
	struct attack_info {
		// Bishop attacks
		uint16  *bdata;			// pointer into attack_table
		uint64   bocc_mask;		// bishop occupancy mask
		uint64   brng_mask;		// bishop attack range mask (attacks on empty board)
		// Rook attacks
		uint16  *rdata;			// pointer into attack_table
		uint64   rocc_mask;		// rook occupancy mask
		uint64   rrng_mask;		// rook attack range mask (attacks on empty board)
		// King attacks
		uint64   king_att;
		// Knight attacks
		uint64   knight_att;
	};


	extern uint16 attack_table[107648];
	extern attack_info att_info[64];
#if PDEP_BISHOP_LOOKUP
	inline Bitboard bishopAttack(Square sq, Bitboard occ)
	{
		attack_info *info = &att_info[sq];
		return Bitboard(Pdep(info->bdata[Pext(occ, info->bocc_mask)], info->brng_mask));
	}
#else
	inline Bitboard bishopAttack(Square sq, Bitboard occ)
	{
		attack_info* info = &att_info[sq];
		return Bitboard(info->bdata[Pext(occ, info->bocc_mask)]);
	}
#endif
#if PDEP_ROOK_LOOKUP
	inline Bitboard rookAttack(Square sq, Bitboard occ)
	{
		attack_info *info = &att_info[sq];
		return Bitboard(Pdep(info->rdata[Pext(occ, info->rocc_mask)], info->rrng_mask));
	}
#else
	inline Bitboard rookAttack(Square sq, Bitboard occ)
	{
		attack_info* info = &att_info[sq];
		return Bitboard(info->rdata[Pext(occ, info->rocc_mask)]);
	}
#endif

	inline Bitboard pawnAttack(Square sq, Color c) { ColorOk(c); SquareOk(sq); return beside_bb(sq + advance(c)); }
	inline Bitboard kingAttack(Square sq) { SquareOk(sq); return Bitboard(att_info[sq].king_att); }
	inline Bitboard kingRange(Square sq) { return kingAttack(sq); }
	inline Bitboard knightAttack(Square sq) { SquareOk(sq); return Bitboard(att_info[sq].knight_att); }
	inline Bitboard knightRange(Square sq) { return knightAttack(sq); }
	inline Bitboard bishopRange(Square sq) { SquareOk(sq); return Bitboard(att_info[sq].brng_mask); }
	inline Bitboard rookRange(Square sq) { SquareOk(sq); return Bitboard(att_info[sq].rrng_mask); }
	inline Bitboard queenAttack(Square sq, Bitboard occ) { return rookAttack(sq, occ) | bishopAttack(sq, occ); }
	inline Bitboard queenRange(Square sq) { return rookRange(sq) | bishopRange(sq); }
	inline uint8    valid_move(Piece p, Square from, Square to) { PieceOk(p);  SquareOk(from); SquareOk(to); Assert(p != Pawn); return PseudoLegal[from][to] & (uint8(1) << p); }
	// return the pawn fields: Pawn/PawnCature/PawnColor -- this is nonzero only if this is a PseudoLegal pawn move/capture for one color

	// return true if from/to is a PseudoLegal pawn move for color c. 
	inline bool		valid_pawn_move(Color c, Square f, Square t) { SquareOk(f); SquareOk(t); return PseudoLegal[f][t] & (uint8(1) << c); }

	// Bitboard b has a single set bit (or is zero)
	inline bool singleton(Bitboard b) { return blsr(b) == 0; }

	// Sliders from sets QR and QB which can reach sq
	inline Bitboard slidingAttackers(Square sq, Bitboard occ, Bitboard QR, Bitboard QB)
	{
		return (rookAttack(sq, occ) & QR) | (bishopAttack(sq, occ) & QB);
	}
}
#endif