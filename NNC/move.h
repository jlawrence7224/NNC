
#ifndef MOVE_INCLUDED
#define MOVE_INCLUDED 1

#include "types.h"
namespace NNC{
	extern uint8 PseudoLegal[128][64];

	typedef uint16 mov16;
	/// <summary>
	/// An integer type the same size as mov_t
	/// </summary>
	typedef int32 MoveInt;
	constexpr MoveInt SPECIAL_FLAG	= 0x04000;
	constexpr MoveInt PROMO_FLAG	= 0x1000 | SPECIAL_FLAG;
	constexpr MoveInt CASTLE_FLAG	= 0x2000 | SPECIAL_FLAG;
	constexpr MoveInt EP_FLAG		= 0x0100 | SPECIAL_FLAG;
	constexpr MoveInt EP_ALL		= 0xfD00;

	struct mov_t {
		uint8 m_from;
		uint8 m_to;
		int16 m_score;


		inline			operator uint16_t()		const	{ return uint16_t(reinterpret_cast<const MoveInt&>(*this)); }
		inline mov_t&	operator=(MoveInt x)			{ reinterpret_cast<MoveInt&>(*this) = x; return *this; }
			   mov_t	(Square f, Square t)			: m_from(f), m_to(t), m_score(0) {}
			   mov_t	(MoveInt x)						{ reinterpret_cast<MoveInt&>(*this) = x; }
	};

	typedef mov_t MoveX;
	typedef MoveX Move;
	typedef Move  Move16;

	const  MoveX  MOVE_NONE(0);
	inline bool   MoveOk		(const Move& m)		{ return Square(m.m_from) < SPECIAL && Square(m.m_to) < 2 * SPECIAL; }
	inline Square MoveFromSq	(const MoveX& m)	{ return Square(m.m_from); }
	inline Square MoveToSq		(const MoveX& m)	{ return Square(m.m_to); }

	// Predicates for special moves
	inline bool MoveIsPromotion	(const MoveX& m)	{ return (reinterpret_cast<const MoveInt&>(m) & PROMO_FLAG)  == PROMO_FLAG; }
	inline bool MoveIsCastle	(const MoveX& m)	{ return (reinterpret_cast<const MoveInt&>(m) & CASTLE_FLAG) == CASTLE_FLAG; }
	inline bool MoveIsEP		(const MoveX& m)	{ return (reinterpret_cast<const MoveInt&>(m) & EP_ALL)	== EP_FLAG; }
	inline bool MoveIsEPA		(const MoveX& m)	{ return m.m_to == EPA; }
	inline bool MoveIsEPH		(const MoveX& m)	{ return m.m_to == EPH; }
	inline bool MoveIsAdv2		(const MoveX& m)	{ return m.m_to == ADV2; }

	// Predicate/field extraction for SPECIAL to-squares

	/// <summary>
	/// Extract the "displacement" bits from a SPECIAL square.
	/// The to square of an EP or PROMO move encodes the board to-square
	/// relative to the move from square. The to square is computed as
	/// from+advance+displacement where displacement = -1,0,1.
	/// The displacement value is encoded as {1,2,3} stored in bits 0:1
	/// </summary>
	/// <param name="sq"></param>
	/// <returns></returns>
	inline SqrInt	SqDisp	(Square sq)	{ return SqrInt(sq) & 3 - 2; }
	inline SqrInt	SqCR	(Square sq)	{ return SqrInt(sq) & 15; }
	/// <summary>
	/// Does the move to-square encode a King-side castle?
	/// </summary>
	/// <param name="to_sq">Special to-square with CASTLE flag set</param>
	/// <returns></returns>
	inline SqrInt	SqCRKS	(Square sq)	{ return SqrInt(sq) & CR_KS; }
	inline SqrInt	SqCRQS	(Square sq) { return SqrInt(sq) & CR_QS; }
	inline Piece	SqPromo	(Square sq) { return Piece(_bextr_u32(uint32(sq),2,2) + Knight); }

	inline Move		MoveEP(Square f, Square t)		{ return mov_t(f, FileOf(f) > FileOf(t) ? EPA : EPH); }
	inline Move		MovePromo(Square f, Square t)	{ return mov_t(f, QPF + FileOf(t) - FileOf(f)); }
	inline Move		MoveCastle(Square f, CR ks)		{ return mov_t(f, CASTLE + SqrInt(ks)); }

	inline bool	operator<		(const mov_t& lhs, const mov_t& rhs) { return lhs.m_score < rhs.m_score; }
	inline bool operator>		(const mov_t& lhs, const mov_t& rhs) { return rhs < lhs; }
	inline bool operator==		(const mov_t& lhs, const mov_t& rhs) { return uint16_t(lhs)  == uint16_t(rhs); }

	/// <summary>
	/// Initialize "extended" move structure -- from/to/score
	/// </summary>
	/// <param name="m"></param>
	/// <param name="from"></param>
	/// <param name="to"></param>
	/// <param name="score"></param>
	inline void initMoveX(mov_t& m, Square f, Square t, Score s)
	{
		m.m_score = s;
		m.m_from = f;
		m.m_to = t;
	}
	/// <summary>
	/// Initialize move structure -- from/to (sans score)
	/// </summary>
	/// <param name="m"></param>
	/// <param name="from"></param>
	/// <param name="to"></param>
	inline void initMove(mov_t& m, Square f, Square t)
	{
		m.m_from = f;
		m.m_to = t;
	}

	inline uint8 valid_move(Square f, Square t, Piece p)
	{
		uint8 pl = PseudoLegal[t][f];
		return ((1 << p) & pl) ? pl : 0;
	}
	inline uint8 valid_move(mov_t& m, Piece p)
	{
		return valid_move(MoveFromSq(m), MoveToSq(m), p);
	}

	// std::ostream& operator<<(std::ostream& os, mov_t& m); // { os << MoveFromSq(m) << MoveToSq(m);  }

	// -------------------------------------------------------------------------------
	// enum type synonym for mov_t
	enum mov_enum : MoveInt {
		MOV_NONE = 0,
		MOV_SPECIAL = (SPECIAL << 8),
		MOV_UNDER = 256 + 4	// TO-square of each underpromotion is 4 less than promo above.
	};
	inline MoveInt  MovePromotionUnder(MoveInt m) { return m - MOV_UNDER; }

};
#if 0
namespace NNC {
	///////////////////////////////////////////////////////////////////////////////////////
	// Move structure(s)

	typedef enum move_t : MoveInt {
		MOVE_NONE = 0,
		MOVE_SPECIAL = (SPECIAL << 8),
		MOVE_PROMO = MOVE_SPECIAL + 4,
		MOVE_UNDER = 256,
		MOVE_MAX = MOVE_SPECIAL + (16 << 8)
	};

	typedef MoveX   Move;
	typedef Move    Move16;
	typedef Move    MoveT;

	//inline Square MoveFromSq(MoveX m) { return Square(uint8(MoveInt(m))); }
	//inline Square MoveToSq(MoveX m) { return Square(uint8(MoveInt(m) >> 8)); }
	inline Score  MoveScore(MoveX m) { return MoveInt(m) >> 16; }
	inline Move   MoveGetMove(MoveX m) { return Move(uint16(m)); }
	inline MoveX  MoveSetScore(MoveX m, Score s) { return MoveX((s << 16) + MoveGetMove(m)); }
	std::ostream& operator<<(std::ostream& os, MoveX m); // { os << MoveFromSq(m) << MoveToSq(m);  }

	inline bool   MoveOk(MoveInt m) { return uint16(m) < uint16(MOVE_MAX) && uint8(m) < uint8(SPECIAL); }

	inline MoveX  makeMove(Move m) { return m; }
	inline MoveX  makeMove(Move m, Piece p) { return makeMove(m); }
	inline MoveX  makeMove(Move m, Piece p, Piece cap) { return makeMove(m); }

	inline MoveX  makeMove(Move m, Score s) { return MoveX((s << 16) + MoveInt(m)); }
	inline MoveX  makeMove(Square f, Square t) { return MoveX(f + (t << 8)); }
	inline MoveX  makeMove(Square f, Square t, Score s) { return MoveX((s << 16) + f + (t << 8)); }
	inline MoveX  makeMove(Square f, Square t, Piece p, Score s) { return makeMove(f, t, s); }
	inline MoveX  makeMove(Square f, Square t, Piece p, Piece cap, Score s) { return makeMove(f, t, s); }

	//inline uint8  MoveLoss( MoveX m) { return uint8(MoveScore(m)); }
	//inline Piece  MovePiece(MoveX m) { return Piece(MoveScore(m)); }
	inline bool   MoveIsPromotion(MoveX m) { return uint16(m) >= MOVE_PROMO; }
	inline bool   MoveIsNormal(MoveX m) { return uint16(m) < uint16(MOVE_SPECIAL); }
	inline bool   MoveIsSpecial(MoveX m) { return !MoveIsNormal(m); }
	inline MoveX  makePromotionUnder(MoveX m) { return Move(MoveInt(m) - MOVE_UNDER); }
	inline MoveX  makePromotionOver(MoveX m) { return Move(MoveInt(m) + MOVE_UNDER); }
	inline MoveT  makeMoveTemplate(Piece p, Square from) { return makeMove(from, Square(PcInt(p))); }

	inline bool	  CastleKS(Move16 m) { return MoveToSq(m) == CASTLE_KS; }
	inline bool	  CastleQS(Move16 m) { return MoveToSq(m) == CASTLE_QS; }
	inline CR	  CastleType(MoveType t) { Assert(t == CASTLE_KS || t == CASTLE_QS); return t == CASTLE_KS ? CR_KS : CR_QS; }
	inline Square CastleKingFrom(Move16 m) { return MoveFromSq(m); }
	inline Square CastleKingTo(Move16 m) { return CastleKingFrom(m) + (CastleKS(m) ? 2 : (-2)); }

	inline Square CastleRookFrom(Move16 m) { return CastleKingFrom(m) + (CastleKS(m) ? 3 : (-4)); }
	inline Square CastleRookTo(Move16 m) { return CastleKingFrom(m) + (CastleKS(m) ? 1 : (-1)); }

	inline SqrInt PromoDiff(MoveType t) { Assert(t >= SPECIAL); return (SqrInt(t) >> 2) - (16 + 2); }		// -1--ddpp|ffffff // ---1--dd
	inline Piece  PromoPiece(MoveType t) { return Piece((SqrInt(t) & 0x3) + 4); }
	inline bool   valid_promo(MoveType t) { return t < 16 && t >= 4; }
	inline SqrInt PromoDiff(Move16 m) { return PromoDiff(MoveToSq(m)); }
	inline Piece  PromoPiece(Move16 m) { return PromoPiece(MoveToSq(m)); }
	inline Square PromoToSq(Square f, MoveType t) { return Square(SqrInt(advanceProm(f)) + PromoDiff(t)); }
	inline Square PromoToSq(Move16 m) { return PromoToSq(MoveFromSq(m), MoveToSq(m)); }
	inline bool   MoveIsQueenPromotion(Move16 m) { return MoveIsPromotion(m) && PromoPiece(m) == Queen; }
	inline bool   MoveIsRookPromotion(Move16 m) { return MoveIsPromotion(m) && PromoPiece(m) == Rook; }
	inline bool   MoveIsBishopPromotion(Move16 m) { return MoveIsPromotion(m) && PromoPiece(m) == Bishop; }
	inline bool   MoveIsKnightPromotion(Move16 m) { return MoveIsPromotion(m) && PromoPiece(m) == Knight; }

	inline Square Adv2ToSq(Move16 m) { return advance24(MoveFromSq(m)); }
	inline MoveType MoveSpecialType(Move16 m) { return MoveToSq(m); }
	///////////////////////////////////////////////////////////////////////////////////////
	// Constructors for special moves

	inline MoveX makeCastleMove(Score s, MoveType ksqs, Square ksq) { return makeMove(ksq, ksqs, s); }
	inline MoveX makeADV2Move(Score s, Square from) { return makeMove(from, ADV2, s); }
	inline MoveX makeEpMove(Score s, Square from) { return makeMove(from, EP, s); }
	inline Move16 makePromo16(PcInt promo, SqrInt from, SqrInt to) { return makeMove(Square(from), Square(SPECIAL + (promo_displacement(from, to) * 4 + (promo - 4)))); }
	inline MoveX makePromotionMove(Score s, Piece promo, Square from, Square to) { return makeMove(makePromo16(promo, from, to), s); }
	inline MoveX makePromotionMove(Score s, Piece promo, Piece cap, Square from, Square to) { return makeMove(makePromo16(promo, from, to), s); }

	// Convert a Q/R/B promotion to an under promotion to the next most valuable piece
	// inline MoveX  makePromotionUnder(MoveX m) { return MoveX(reinterpret_cast<int32_t&>(m) - 256); }

	inline uint8  MoveLoss(MoveX m) { return uint8(MoveScore(m)); }
	inline Piece  MovePiece(MoveX m) { return Piece(PcInt(MoveToSq(m))); }
}
#endif OLD_MOVE
#endif // #ifndef MOVE_INCLUDED
