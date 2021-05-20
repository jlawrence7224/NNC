
// Basic types
#ifndef TYPES_INCLUDED
#define TYPES_INCLUDED

#include <crtdbg.h>
#include "inttyp.h"
#include <iostream>

namespace NNC {

	template<typename T>
	inline T& operator+=(T& d1, T d2) { return d1 = d1 + d2; }
	template<typename T>
	inline T& operator+=(T& d1, int d2) { return d1 = d1 + d2; }
	template<typename T>
	inline T& operator-=(T& d1, T d2) { return d1 = d1 - d2; }
	template<typename T>
	inline T& operator-=(T& d1, int d2) { return d1 = d1 - d2; }

	template<typename T>
	inline T& operator|=(T& d1, T d2) { return d1 = d1 | d2; }
	template<typename T>
	inline T& operator&=(T& d1, T d2) { return d1 = d1 & d2; }
	template<typename T>
	inline T& operator^=(T& d1, T d2) { return d1 = d1 ^ d2; }
	template<typename T>
	inline T& operator<<=(T& d1, int n) { return d1 = d1 << n; }
	template<typename T>
	inline T& operator>>=(T& d1, int n) { return d1 = d1 >> n; }

	//////////////////////////////////////////////////////////////////////////////
	/// Integer types

	// typedef uint32   MoveInt;

	typedef int32	 Score;

	typedef Score				Value;
	typedef uint64				Hash;	// 64 bit Zobrist hash
	typedef uint8				Depth;
	typedef uint8				Gen;

	namespace Eval {
		const Score DrawScore = 0;			// someday, contempt
		const Score MateScore = 1000;		// ** TO DO **
	};
	//////////////////////////////////////////////////////////////////////////////
	/// Color
	typedef int32 ClrInt;
	typedef enum clr_t : ClrInt { White = 0, Black = 1 } Color;
	inline  void   ColorOk(Color  c) { Assert(uint32(c) < 2); }

	// operator ~ flips a Color: White<=>Black
	inline Color operator~  (Color c) { return Color(ClrInt(c) ^ 1); }

	std::ostream& operator<<(std::ostream& os, Color c);

	//////////////////////////////////////////////////////////////////////////////
	/// Square 

	typedef int32 SqrInt;	// must be signed type

	typedef enum sqr_t : SqrInt {
		A1 = 0, B1, C1, D1, E1, F1, G1, H1,
		A2, B2, C2, D2, E2, F2, G2, H2,
		A3, B3, C3, D3, E3, F3, G3, H3,
		A4, B4, C4, D4, E4, F4, G4, H4,
		A5, B5, C5, D5, E5, F5, G5, H5,
		A6, B6, C6, D6, E6, F6, G6, H6,
		A7, B7, C7, D7, E7, F7, G7, H7,
		A8, B8, C8, D8, E8, F8, G8, H8,

		// Special to-square encodings used by mov_t

		SPECIAL		= 64,
		ADV2		= SPECIAL + 0,		// double pawn advancement which sets EP state
		EPA			= SPECIAL + 1,		// EP capture toward A file
		EPH			= SPECIAL + 3,		// EP capture toward H file
		CASTLE		= SPECIAL + 32,		// CASTLE + CS-flag
		PROMO		= SPECIAL + 16,		// Least promotion
		QPROMO		= SPECIAL + 28,		// Queen promotion base
		QPA			= QPROMO  + 1,		// Queen promotion capturing toward A file
		QPF			= QPROMO  + 2,		// Queen promotion forward
		QPH			= QPROMO  + 3,		// Queen promotion capturing toward H file
		MaxToSq		= 128				// All TO square encodings are < MaxToSq
	} Square;

	// A MoveType is a ToSquare with a "special" value >= 64
	// encoding one of the special moves: ADV2,EP,CASTLE,QPROMO
	typedef Square MoveType;
	enum ScoreType
	{
		NONE,
		QUIET,
		CAPTURE,
		CS,	// castle
	};

	std::ostream& operator<<(std::ostream& os, Square sq);


	inline bool		SquareIsSpecial(Square sq) { return sq >= SPECIAL; }

	constexpr SqrInt RankBits = 0x38;
	constexpr SqrInt FileBits = 0x07;

	inline  void   SquareOk(Square sq) { Assert(sq < SPECIAL); }
	// A PseudoLegal square for the Zobrist hash of EP positions -- the position of an EP-capturable Pawn
	inline  void   EPOk(Square sq) { Assert(sq >= A4 && sq <= H5); }

	// Arithmetic on squares

	inline Square   operator+(Square sq, int  offset) { return Square(SqrInt(sq) + offset); }
	inline Square   operator-(Square sq, int  offset) { return Square(SqrInt(sq) - offset); }
	inline Square   operator+(Square sq, uint offset) { return Square(SqrInt(sq) + offset); }
	inline Square   operator-(Square sq, uint offset) { return Square(SqrInt(sq) - offset); }
	inline SqrInt   operator-(Square to, Square from) { return SqrInt(to) - SqrInt(from); }

	// Bit twiddling on Squares
	inline Square	operator^(Square sq, SqrInt bit) { return Square(SqrInt(sq) ^ bit); }
	inline Square	operator|(Square sq, SqrInt bit) { return Square(SqrInt(sq) | bit); }

	//////////////////////////////////////////////////////////////////////////////
	/// rank_t and file_t
	typedef SqrInt RankInt;
	typedef enum rank_t : RankInt {
		RANK_1 = 0,
		RANK_2 = 1,
		RANK_3 = 2,
		RANK_4 = 3,
		RANK_5 = 4,
		RANK_6 = 5,
		RANK_7 = 6,
		RANK_8 = 7
	} Rank_t;

	inline  void    RankOk(SqrInt r) { Assert(r < 8); }
	std::ostream& operator<<(std::ostream& os, Rank_t r);
	// typedef RankInt rank8_t;	// Descriptive type name for 8*Rank

	typedef SqrInt FileInt;
	typedef enum file_t : FileInt {
		FILE_A = 0,
		FILE_B = 1,
		FILE_C = 2,
		FILE_D = 3,
		FILE_E = 4,
		FILE_F = 5,
		FILE_G = 6,
		FILE_H = 7
	} File_t;

	inline  void    FileOk(SqrInt f) { Assert(f < 8); }
	std::ostream& operator<<(std::ostream& os, File_t sq);

	//////////////////////////////////////////////////////////////////////////////
	/// Bitboard interpreted as Set of Squares

	typedef enum bitboard_t : uint64_t {
		NO_SQUARES = 0,
		ALL_SQUARES = ~uint64_t(0)
	} Bitboard;

	extern Bitboard bit[64];
	//inline Bitboard Bit(SqrInt n) { Assert(uint32(n) < 64); return bit[n]; }

	inline Bitboard Bit(SqrInt n) { Assert(0 <= n && n < 64); return Bitboard(uint64(1) << n); }

	// Isolate lowest set bit -- compiler intrinsic for (b&(-b))
	inline Bitboard  blsi		(Bitboard  b)	{ return Bitboard(_blsi_u64(b)); }
	// Remove lowest set bit  -- compiler intrinsic for b&(b-1)
	inline Bitboard  blsr		(Bitboard  b)	{ return Bitboard(_blsr_u64(b)); }


	/// x `and not` y : x & ~y
	/// [Note intrinsic negates first argument whereas our convention negates the second.]
	inline Bitboard  andn		(Bitboard  x, Bitboard y) { return Bitboard(_andn_u64(uint64(y),uint64(x))); }

	// define iteration protocol over a Bitboard enum as a set of Squares
	// for( Square sq : <Bitboard expression> ) { ...sq... }
	inline Bitboard  begin		(Bitboard  set)	{ return set; }
	inline Bitboard  end		(Bitboard  set)	{ return NO_SQUARES; }
	inline Square    operator*  (Bitboard  it)	{ return Square(BSF(it)); }
	inline Bitboard& operator++ (Bitboard& it)	{ return it = blsr(it); }	// 	{ return it = Bitboard(uint64_t(it) & (uint64_t(it) - 1)); }

	inline Bitboard operator&	(Bitboard b1, Bitboard b2)	{ return Bitboard(uint64(b1) & uint64(b2)); }
	inline Bitboard operator|	(Bitboard b1, Bitboard b2)	{ return Bitboard(uint64(b1) | uint64(b2)); }
	inline Bitboard operator^	(Bitboard b1, Bitboard b2)	{ return Bitboard(uint64(b1) ^ uint64(b2)); }
	inline Bitboard operator<<	(Bitboard b1, int n)		{ return Bitboard(uint64(b1) << n); }
	inline Bitboard operator>>	(Bitboard b1, int n)		{ return Bitboard(uint64(b1) >> n); }
	inline Bitboard operator~	(Bitboard b)				{ return Bitboard(~uint64(b)); }


	inline uint8    member		(Bitboard b, Square sq)		{ return _bittest64(reinterpret_cast<const int64*>(&b), uint64(sq)); }
	inline Bitboard operator&	(Bitboard b, Square sq)		{ return b & Bit(sq); }
	inline Bitboard operator|	(Bitboard b, Square sq)		{ return b | Bit(sq); }
	inline Bitboard operator|	(Square x, Square y)		{ return Bit(x) | Bit(y); }
	inline Bitboard operator^	(Bitboard b, Square sq)		{ return b ^ Bit(sq); }

	inline Bitboard& operator&=	(Bitboard& b, Square sq)	{ return b = b & Bit(sq); }
	inline Bitboard& operator|=	(Bitboard& b, Square sq)	{ return b = b | Bit(sq); }
	inline Bitboard& operator^=	(Bitboard& b, Square sq)	{ return b = b ^ Bit(sq); }

	std::ostream& operator<<(std::ostream& os, Bitboard bb);

	constexpr Bitboard all = ALL_SQUARES; //  Bitboard(~uint64_t(0));

	//////////////////////////////////////////////////////////////////////////////
	/// Rank/File/Square/Bitboard functions

	inline Square	sqr(RankInt r, FileInt f) { Assert(r < 8 && f < 8); return Square(r * 8 + f); }
	inline Square	sqr(rank_t rank, file_t file) { return sqr(RankInt(rank), FileInt(file)); }
	inline Square	sqr(Bitboard sq) { return *sq; }
	// inline Square	sqr8(rank8_t r8, file_t file) { return Square(r8 + file); }
	inline SqrInt	Rank8(Square sq) { return sq & RankBits; }
	inline rank_t	RankOf(Square sq) { return rank_t(sq >> 3); }
	inline file_t	FileOf(Square sq) { return file_t(sq & FileBits); }

	inline int      advance(Color c) { return c ? -8 : 8; }	// **TO DO** verify this is compiled as conditional move.
	// inline int      advance(Color c) { return 8 - 16 * c; }	// +/- 8
	// pawn_capture: Assuming f->t is a PseudoLegal pawn move, test whether it is a capture
	inline bool		pawn_capture(Square f, Square t) { return (f - t) & 0x7; }
	// pawn_displacement: The file displacement of a pawn move: (to-from)-advance(c)
	// -1 for a capture toward A file, 0 for single square advance, +1 for a capture toward H file
	inline int		pawn_displacement(Color c, Square from, Square to) { return (to - from) - advance(c); }
	inline bool		valid_displacement(int diff) { return uint(diff + 1) <= 2; }
	// file displacement + 2 assuming from-to is a PseudoLegal pawn move, thus 1=A-Capture,2=Advance,3=H-capture
	inline int		promo_displacement(SqrInt from, SqrInt to) { SqrInt disp = (to - from + 1) & SqrInt(7); Assert(disp <= 2); return disp + 1; }
	// PseudoLegal displacement: test that 

	inline rank_t	PromotionRank(Color c) { return rank_t((-ClrInt(c))&ClrInt(7)); }
	inline rank_t	RelativeRank(Color c, rank_t r) { return rank_t(((-RankInt(c))&RankInt(7)) ^ RankInt(r)); }
	inline Square	RelativeSquare(Color c, Square s) { return Square(((-(signed)SqrInt(c))&RankBits) ^ SqrInt(s)); }

	// if'less, color'less mappings between certain related squares
	// xor with 8  swaps squares with: RANK_1<->RANK_2, RANK_3<->RANK_4, RANK_5<->RANK_6, RANK_7<->RANK_8
	// xor with 16 swaps squares with: RANK_1<->RANK_3, RANK_2<->RANK_4, RANK_5<->RANK_7, RANK_6<->RANK_8

	// The square behind (1 relative rank smaller) than a doubly advanced pawn (a pawn subject to EP capture)
	inline Square	retreatEP(Square epsq) { Assert(RankOf(epsq) == RANK_4 || RankOf(epsq) == RANK_5); return Square(SqrInt(8) ^ epsq); }
	// The square ahead (1 relative rank larger) than a RANK_3 pawn -- the square of the pawn captured by EP capture
	inline Square	advanceEP(Square epto) { Assert(RankOf(epto) == RANK_3 || RankOf(epto) == RANK_6); return Square(SqrInt(8) ^ epto); }
	// Exchange RANK_2<->RANK_3 and RANK_7<->RANK_6
	inline Square	advance23(Square sq) { Assert(RankOf(sq) == RANK_2 || RankOf(sq) == RANK_7); return Square(SqrInt(0x018) ^ sq); }

	// Exchange the a square on relative rank 7 with the square on relative  rank 8 (the same functions, different assertions).
	inline Square	advanceProm(Square psq) { Assert(RankOf(psq) == RANK_2 || RankOf(psq) == RANK_7); return Square(SqrInt(8) ^ psq); }
	inline Square	retreatProm(Square psq) { Assert(RankOf(psq) == RANK_1 || RankOf(psq) == RANK_8); return Square(SqrInt(8) ^ psq); }
	// Exchange the a square on relative rank 2 with the square on relative  rank 4 (advance/retreat the same functions, different assertions).
	inline Square	advance24(Square sq) { Assert(RankOf(sq) == RANK_2 || RankOf(sq) == RANK_7); return Square(SqrInt(16) ^ sq); }
	inline Square	retreat24(Square sq) { Assert(RankOf(sq) == RANK_4 || RankOf(sq) == RANK_5); return Square(SqrInt(16) ^ sq); }

	inline Bitboard advance(Color c, Bitboard b, int n) { ColorOk(c);  Assert(0 <= n && n < 64); return c == White ? (b << n) : (b >> n); }
	inline Bitboard retreat(Color c, Bitboard b, int n) { ColorOk(c);  Assert(0 <= n && n < 64); return c == White ? (b >> n) : (b << n); }
	inline Bitboard advance(Color c, Bitboard b) { return advance(c, b, 8); }
	inline Bitboard retreat(Color c, Bitboard b) { return retreat(c, b, 8); }

	inline Bitboard square_bb(Square sq) { SquareOk(sq);  return Bit(sq); }
	inline Bitboard square_bb(rank_t r, file_t f) { return square_bb(sqr(r, f)); }
	inline Bitboard square_bb(int r, int f) { return square_bb(sqr(r, f)); }

	//////////////////////////////////////////////////////////////////////////////
	/// Piece

	// This enum is used to represent piece types (King, Queen, etc.)
	typedef int PcInt;
	typedef enum piece_t : PcInt {
		NoPiece = 0,
		BlkPawn = 1,
		WhtPawn = 2,
		Pawn = 2,
		Knight = 3,	
		Bishop = 4,
		Rook = 5,	
		Queen = 6, 
		King = 7,

		MinPieceType = 1,
		MaxPieceType = 7,

	} Piece;
	extern const char* pieceName[8];
	// assert p is a valid piece type
	inline	void	PieceOk(Piece  p)		{ Assert(p >= MinPieceType && p <= MaxPieceType); }
	inline  PcInt	PieceIsSlider(Piece p)	{ PieceOk(p); return p & 0x4; }
	// piece type of Pawn of color c -- c ? BlkPawn : WhtPawn
	inline	Piece	PawnClr(Color c)		{ return Piece(int(Pawn) - int(c)); }
	std::ostream&	operator<<(std::ostream& os, Piece p);

#if 1
	// piece type combined with a color as a piece on the chess board, aka Wood.
	// used by mailbox chessboard inside the Board representation .
	typedef enum piece_clr_t : PcInt {
		PC_EMPTY = 0,
		PC_WHITE = 0,
		PC_BLACK = 1,
		PC_COLOR = 1,
	} PieceClr;
	typedef PieceClr Wood;

	inline	Color   PieceColor(Wood p) { return Color(p&PC_COLOR); }
	inline	Piece	PieceType(Wood p) { return Piece(p >> 1); }
	inline	bool	NoWood(Wood p) { return p == PC_EMPTY; }
	inline	Wood	ColoredPiece(Piece p, Color c) { PieceOk(p); ColorOk(c); return PieceClr(2 * PcInt(p) + ClrInt(c)); }

	inline	Wood	operator|(Piece p, Color c) { return ColoredPiece(p, c); }

	inline  void	WoodOk(Wood p) { PieceOk(PieceType(p)); }
#endif

	//////////////////////////////////////////////////////////////////////////////
	/// Score bound type information

	typedef uint8 BndInt;

	typedef enum bound_t : BndInt {
		BOUND_NONE = 0,
		BOUND_UPPER = 1,
		BOUND_LOWER = 2,
		BOUND_EXACT = BOUND_LOWER | BOUND_UPPER,
	} Bound;

	inline  void   BoundOk(Bound  b) { Assert(b < 4); }

	//////////////////////////////////////////////////////////////////////////////
	/// CastlingRights

	// 4 bits: QS_BLK,QS_WHT,KS_BLK,KS_WHT
	typedef uint32 CRInt;
	typedef enum cr_t : CRInt {
		CR_NONE = 0,

		CR_KS_WHT = 0x01,
		CR_KS_BLK = 0x02,
		CR_QS_WHT = 0x04,
		CR_QS_BLK = 0x08,

		CR_WHITE = CRInt(CR_KS_WHT) | CRInt(CR_QS_WHT), // 0b0101
		CR_BLACK = CRInt(CR_KS_BLK) | CRInt(CR_QS_BLK),

		CR_KS = CRInt(CR_KS_WHT) | CRInt(CR_KS_BLK),
		CR_QS = CRInt(CR_QS_WHT) | CRInt(CR_QS_BLK),

		CR_ALL = 0x0f
	} CastlingRights;

	typedef CastlingRights CR;
	inline CastlingRights operator^(CR oldCR, CR newCR) { return CR(CRInt(oldCR) ^ CRInt(newCR)); }
	inline CastlingRights operator&(CR cr1, CR cr2) { return CR(CRInt(cr1)&CRInt(cr2)); }
	inline CastlingRights operator|(CR cr1, CR cr2) { return CR(CRInt(cr1) | CRInt(cr2)); }

	/// CR_remove: remove rights in 'del' from rights in 'cr' -- cr\del
	inline CastlingRights CR_remove(CastlingRights cr, CastlingRights del) { return CastlingRights(~del&cr); }
	inline CastlingRights CR_rights(Color c) { return CR(CRInt(CR_WHITE) << ClrInt(c)); }

	// Castling Rights lost if the square is either moved TO or FROM
	extern CastlingRights CR_RightsLost[64];
	std::ostream& operator<<(std::ostream& os, CastlingRights cr);
	namespace Zobrist
	{
		constexpr Hash	sideToMove = uint64(-1);
		extern Hash		piece_square_hash[2][6][64];
		extern Hash		ep_hash[16];
		extern Hash		castling_rights_hash[16];
		void init();


		inline Hash*	H(Color ct, Piece pt) { PieceOk(pt); return piece_square_hash[ct][pt - King]; }
		inline Hash		H(Color ct, Piece pt, Square sq) { PieceOk(pt); return piece_square_hash[ct][pt - King][sq]; }
		inline Hash		HashMove(Color ct, Piece pt, Square from, Square to) { return H(ct, pt, from) ^ H(ct, pt, to); }
		inline Hash		HashCapture(Color ct, Piece pt, Square from, Square to, Piece capture) { return H(ct, pt, from) ^ H(ct, pt, to) ^ H(~ct, capture, to); }
		inline Hash		HashPromotion(Color ct, Piece promoted_type, Square from, Square to) { return H(ct, Pawn, from) ^ H(ct, promoted_type, to); }
		inline Hash		HashCastlingRights(CastlingRights cr) { return castling_rights_hash[cr]; }
		inline Hash		HashEP(Square ep) { EPOk(ep);  return ep_hash[ep - A4]; }
	};


}
#endif
