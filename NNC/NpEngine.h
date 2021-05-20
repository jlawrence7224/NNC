
#pragma once

// Basic types
#include <assert.h>
#include <algorithm>
#include <string>
#include "options.h"
#include "intrinsics.h"
#include "types.h"
#include "attacks.h"
#include "move.h"
#include "tt.h"

namespace NNC {

	class MoveGen;
	enum GenStage {
		UNINITIALIZED,
		GEN_QUIET,
		GEN_CAPTURES,
		GEN_KILLERS,
		GEN_LOSERS,
		GEN_DONE,

		PLAY_QUIET,
		PLAY_CAPTURES,
		PLAY_KILLERS,
		PLAY_LOSERS,

		INVALID_POSITION,	// A King capture was generated
	};

	// Board representation


	struct Board
	{
	private:
#define PieceBB 0
#define OccBB	bb[White][1]
#define nColor	2
#define nPiece	8

		Bitboard	bb[nColor][nPiece];
		Piece	sqocc[64];

	protected:
		void	clear() { memset(this, 0, sizeof(*this)); }
		void	init() { clear(); }

	public:
		Piece	board(Square sq)			const { SquareOk(sq); return sqocc[sq]; }
		Piece&	board(Square sq)				  { SquareOk(sq); return sqocc[sq]; }
		Piece	board(Square sq, Piece w)		  { SquareOk(sq); return sqocc[sq] = w; }

		Bitboard Pieces()					const { return OccBB; }
		Bitboard Pieces(Color ct)			const { ColorOk(ct); return bb[ct][PieceBB]; }
		Bitboard Enemy(Color ct)			const { return Pieces(~ct); }

		Bitboard Pieces(Color c, Piece pc)	const { PieceOk(pc); ColorOk(c); return bb[c][pc]; }
		Bitboard Pieces(Color c, Piece p1, Piece p2)	const { return Pieces(c, p1) | Pieces(c, p2); }
		Bitboard Pieces(Piece pc)			const { return Pieces(White, pc) | Pieces(Black, pc); }
		Bitboard Pieces(Piece p1, Piece p2)	const { return Pieces(p1) | Pieces(p2); }

		Bitboard Kings(Color c)				const { return Pieces(c, King); }
		Square	 KingSq(Color c)			const { return sqr(Kings(c)); }

		Bitboard Pawns(Color c)				const { return Pieces(c, Pawn); }
		Bitboard Knights(Color c)			const { return Pieces(c, Knight); }
		Bitboard Bishops(Color c)			const { return Pieces(c, Bishop); }
		Bitboard Rooks(Color c)				const { return Pieces(c, Rook); }
		Bitboard Queens(Color c)			const { return Pieces(c, Queen); }

		bool	 Empty(Square sq)			const { return NoPiece == board(sq); }

		Piece	 PieceTypeOnSqr(Square sq)	const { return board(sq); }
		// Color	 PieceColorOnSqr(Square sq)	const { Assert(Pieces() & sq);  return (Pieces(Black) & sq) ? Black : White; }
		Color	 PieceColorOnSqr(Square sq)	const { Assert(Pieces() & sq);  return Color(member(Pieces(White), sq)); }

		void setOccBB() { OccBB = Pieces(White) | Pieces(Black); }

		void inner_place_piece(Color c, Piece p, Square sq)
		{
			ColorOk(c); PieceOk(p);
			Assert(Empty(sq));
			Assert((Pieces(White) & sq) == 0);
			Assert((Pieces(Black) & sq) == 0);

			Bitboard update = Bit(sq);
			board(sq, p);
			bb[c][p] |= update;
			bb[c][PieceBB] |= update;
		}
		void place_piece(Color c, Piece p, Square sq) { inner_place_piece(c, p, sq); setOccBB(); }

		void inner_remove_piece(Color c, Piece p, Square sq)
		{
			ColorOk(c); PieceOk(p);
			Assert(board(sq) == p);
			Assert(Pieces(c, p) & sq);
			Assert(Pieces(c) & sq);

			Bitboard update = Bit(sq);
			board(sq, NoPiece);
			bb[c][p] ^= update;
			bb[c][PieceBB] ^= update;
		}
		void remove_piece(Color c, Piece p, Square sq) { inner_remove_piece(c, p, sq); setOccBB(); }
		#if 0
		void inner_move_piece(Color c, Piece p, Square from, Square to)
		{
			inner_remove_piece(c, p, from);
			inner_place_piece(c, p, to);
		}
		void move_piece(Color c, Piece p, Square from, Square to) { inner_move_piece(c, p, from, to); setOccBB(); }
		#else
		void move_piece(Color c, Piece p, Square from, Square to)
		{
			ColorOk(c); PieceOk(p);
			Assert(board(from) == p);
			Assert(Pieces(c, p) & from);
			Assert(Pieces(c) & from);

			Bitboard update = Bit(to) ^ Bit(from);
			Bitboard cbb = bb[c][PieceBB] ^ update;
			Bitboard pbb = bb[c][p] ^ update;

			OccBB = Pieces(~c) | cbb;
			board(from) = NoPiece;
			board(to) = p;
			
			bb[c][PieceBB] = cbb;
			bb[c][p] = pbb;
		}
		#endif


		#if 1
		void capture_piece(Color c, Piece p, Square from, Square to, Piece cap)
		{
			Color enemy = ~c;
			ColorOk(c); PieceOk(p); PieceOk(cap);
			Assert(board(from) == p);
			Assert(board(to) == cap);
			Assert(Pieces(c, p) & from);
			Assert(Pieces(c) & from);
			Assert(Pieces(enemy, cap) & to);
			Assert(Pieces(enemy) & to);

			Bitboard update = Bit(to);
			Bitboard ecbb = bb[enemy][PieceBB] ^ update;
			Bitboard epbb = bb[enemy][cap] ^ update;

			update |= Bit(from);
			Bitboard cbb = bb[c][PieceBB] ^ update;
			Bitboard pbb = bb[c][p] ^ update;

			board(from) = NoPiece;
			board(to) = p;
			bb[enemy][cap] = epbb;
			bb[enemy][PieceBB] = ecbb;
			bb[c][p] = pbb;
			bb[c][PieceBB] = cbb;
			OccBB = ecbb | cbb;
		}
		#else
		void capture_piece(Color c, Piece p, Square from, Square to, Piece cap) { inner_remove_piece(~c, cap, to); move_piece(c, p, from, to); }
		#endif

		/////////////////////////////////////////////////////////////////////////////
		/// Board analysis
		//__declspec(noinline)
		Bitboard PotentialPinners(Color c, Square sq) const
		{
			Bitboard Q = Pieces(c, Queen);  return (rookRange(sq) & (Q | Pieces(c, Rook))) | (bishopRange(sq) & (Q | Pieces(c, Bishop)));
		}
		/// Returns subset of occ which are the only piece between sq and one of pp
		Bitboard CalcPinned(Square sq, Bitboard potential_pinners, Bitboard occ) const
		{
			Bitboard pinned = NO_SQUARES;
			for (Square psq : potential_pinners)
			{
				Bitboard btw = between_bb(sq, psq) & occ;
				if (singleton(btw))
					pinned |= btw;
			}
			return pinned;
		}
		Bitboard AbsolutePinned(Color c) const {
			Square ksq = KingSq(c);  return CalcPinned(ksq, PotentialPinners(~c, ksq), Pieces()) & Pieces(c);
		}

		Bitboard PinnedDiscovered(Color c, Bitboard occ) const {
			Square ksq = KingSq(c);  return CalcPinned(ksq, occ & PotentialPinners(~c, ksq), occ);
		}

		///////////////////////////////////////////////////////////////////////////////////////
		/// Board validation/debugging
#ifdef RELEASE
		bool ValidBoard()														const {}
		void AssertBoard()														const {}
#else
#define Verify(b) if(!(b)) return false;
		bool ValidBoard() const
		{
			Verify((Pieces(White) & Pieces(Black)) == 0);
			Verify((Pieces(White) | Pieces(Black)) == Pieces());
			Bitboard wbb = NO_SQUARES;
			Bitboard bbb = NO_SQUARES;
			for (int i = MinPieceType; i <= MaxPieceType; i++)
			{
				Piece p = Piece(i);
				Bitboard pw = Pieces(White, p);
				Verify((pw & wbb) == 0);
				wbb |= pw;
				for (Square s : pw)
					Verify(board(s) == p);
				Bitboard pb = Pieces(Black, p);
				Verify((pb & bbb) == 0);
				bbb |= pb;
				for (Square s : pb)
					Verify(board(s) == p);
			}
			Verify(wbb == Pieces(White) && bbb == Pieces(Black));
			Verify(Kings(White) && Kings(Black));
			Verify(singleton(Kings(White)) && singleton(Kings(Black)));
			return true;
		}
		void AssertBoard()														const { return; } // { Assert(ValidBoard()); }
#endif

	};



	struct PLY
	{
		void init(PLY& prev, Move m)
		{
			stackNext = prev.stackNext;
			castling_rights = prev.castling_rights;
			reversible50 = prev.reversible50 + 1;

			ep = Square(0);
			killer[0] = MOVE_NONE;
			killer[1] = MOVE_NONE;
			// curr_capture = NoPiece;
			curr_move = m;
		}

		void init(MoveX* stack)
		{
			/* stackBase = */stackNext = stack;
			castling_rights = CR_NONE;
			reversible50 = 0;
			ep = Square(0);
			hash = 0;
			killer[0] = MOVE_NONE;
			killer[1] = MOVE_NONE;
			curr_move = MOVE_NONE;
			curr_capture = NoPiece;
		}
		//Hash    pawnKey;
		//Hash    materialKey;
		//Value  nonPawnMaterial[COLOR_NB];
		//int    pliesFromNull;

		void*			stackNext;
		// En Passant square -- the square with the pawn, not the capturing square
		Square			ep;				// zero or a square on the 4th of 5th rank
		Hash			hash;			// hash key of this position

		CastlingRights  castling_rights;
		int32			reversible50;	// # consecutive reversible moves


		Move	killer[2];				// C:\Users\Owner\Desktop\Math Documents\chessKillerHeuristic.html
		// Move	hash_move;				// Move from Transposition table
		Move	curr_move;				// The current move leaving this position

		Piece   curr_capture;
	};


	// Data local to a search thread
	struct Tree : public Board
	{
		static const int MaxPly = 256;
		static const int MaxMoveStack = 2048;
		PLY		plyStack[MaxPly];	// Initial position
		MoveX   moveStack[MaxMoveStack];

		PLY*	root;				// plys above root have been played (are fixed). Root and below are being searched.


		void	clear() { Board::clear(); }
		void	init()
		{
			Board::init();
			root = plyStack;
			root->init(moveStack);

		}
	};

	// Search state -- a specific location in Tree structure
	struct Position : public Tree
	{
		PLY*		current_ply;
		Color		sideToMove;
		MoveX*		root_moves;
		MoveX*		root_end;

		const int MOVE_ILLEGAL = 1;
		const int MOVE_PLAYED = 0;

		int set(const std::string& fenStr);

		// Check to see if we have searched enough nodes
		// that it is time to peek at how much time has been used,
		// or if is time to check for operator keyboard input.
		bool  TimeCheck() { return false; }
		void  ScoreRootMoves();
		Move  RootSearch( int maxdepth );
		Score Evaluate();
		Score quiesce(Score alpha, Score beta);
		Score pvs(int depth, Score alpha, Score beta, int ply, bool do_null);
		Score Search(int depth, Score alpha, Score beta, int ply, bool do_null)
		{
			return (depth > 0) ? pvs(depth, alpha, beta, ply, do_null) : quiesce(alpha, beta);
		}
		Move Go(const int depth,
			const int movestogo = 0,
			const uint64_t movetime = 0,
			const uint64_t wtime = 0, const uint64_t winc = 0,
			const uint64_t btime = 0, const uint64_t binc = 0,
			Move* ponder = NULL);

		size_t		gameDepth()				const { return current_ply - plyStack; }
		PLY&		gameHistory(int n) { Assert(n < gameDepth()); return current_ply[-n]; }
		PLY*		Ply() { return current_ply; }
		PLY*		Ply(int n) { return current_ply - n; }
		PLY*        prev_ply() { return Ply(1); }
		void*		stackBase() { return current_ply[-1].stackNext; }
		void*&		stackNext() { return current_ply->stackNext; }
		size_t		searchDepth()			const { return current_ply - root; }
		Hash		hash()					const { return current_ply->hash; }
		Hash		hash(int n) { return gameHistory(n).hash; }
		// Set hash key and prefetch
		Hash		setHash(Hash key) { /* prefetch((void*)TT.bucket_addr(key));  */ current_ply->hash = key; return key; }

		/* ***************************************************************************** */
		// ** TO DO ** killers
		Move		killer1()				const { return MOVE_NONE; }
		Move		killer2()				const { return MOVE_NONE; }

		// Make a move m if it is fully legal and return MOVE_PLAYED
		// otherwise return MOVE_ILLEGAL
		// If MOVE_ILLEGAL is returned the Board structure has not been modified
		// and current_ply is unchanged.

		void	doLegalMove(Move& m);
		int doMove(Move& m)
		{
			if (!legal_move(m))
				return MOVE_ILLEGAL;
			doLegalMove(m);
			return MOVE_PLAYED;
		}
		void	undoMove(Move& m);
		inline
			Move	makeNextMove(MoveGen& gen);
		inline
			Move	makeNextGoodCapture(MoveGen& gen, Score delta);

#if 0
		//------------------------------------------------------------------------
		// normal moves via template
		// Make and score move using a template
		template<ScoreType MOV> MoveX make(Piece p, Square from, Square to);

		template<>
		MoveX make<QUIET>(Piece p, Square from, Square to)
		{
			Assert(Empty(to));
			return makeMove(from, to, p, scoreQuiet(p, from, to));
		}

		template<>
		MoveX make<CAPTURE>(Piece p, Square from, Square to)
		{
			Piece cap = PieceTypeOnSqr(to);
			return makeMove(from, to, p, cap, scoreCapture(p, cap));
		}

		template<>
		MoveX make<NORMAL>(Piece p, Square from, Square to) { return Empty(to) ? make<QUIET>(p, from, to) : make<CAPTURE>(p, from, to); }


		// normal moves to a target set
		template<ScoreType MOV> __forceinline
			MoveX* makeMovesFrom(MoveX* ml, Piece p, Square from, Bitboard dest)
		{
			for (Square to : dest)
				*ml++ = make<MOV>(p, from, to);
			return ml;
		}
		template<ScoreType MOV> __forceinline
			MoveX* makeMovesFrom(MoveX* ml, MoveT tmpl, Bitboard dest) { return makeMovesFrom<MOV>(ml, MovePiece(tmpl), MoveFromSq(tmpl), dest); }

		//------------------------------------------------------------------------

		// Pawn moves -- make and score
		MoveX makePawnAdv2(Square to, int offset) { Square from = to - offset;  return makeADV2Move(scoreQuiet(Pawn, from, to), from); }
		MoveX makePawnAdv(Square to, int offset)
		{
			Square from = to - offset;
			Assert(Empty(to));
			return makeMove(from, to, Pawn, scoreQuiet(Pawn, from, to));
		}
		MoveX makePawnCap(Square to, int offset)
		{
			Square from = to - offset;
			Piece cap = PieceTypeOnSqr(to);
			return makeMove(from, to, Pawn, cap, scoreCapture(Pawn, cap, 0));
		}
		MoveX makePawnEP(Square from, Square to) { return makeEpMove(scoreCapture(Pawn, Pawn, 0), from); }

		MoveX makePawnPromo(Piece promotion, Square to, int offset)
		{
			Assert(PieceTypeOnSqr(to) == NoPiece);
			Square from = to - offset;
			return makePromotionMove(scorePromo(promotion), promotion, NoPiece, from, to);
		}
		MoveX makePawnPromoCap(Piece promotion, Square to, int offset)
		{
			Piece c = PieceTypeOnSqr(to);
			Assert(c != NoPiece);
			Square from = to - offset;
			return makePromotionMove(scorePromoCap(promotion, c), promotion, c, from, to);
		}
#endif

		Bitboard attackers(Square sq, Color c, Bitboard occ);

		/* ***************************************************************************** */


		// repetition() -- test whether this position a repetition of a previous position
		//                 or the 100th irreversible half move (50 move rule) been made.
		bool		repetition()
		{
			int n = reversible();
			if (n < 4)
				return false;
			if (n >= 100) 		// Draw by 50 move rule?
				return true;
			int rep = 0;
			Hash h = hash();
			int i = 4;
			do {
				if (h == hash(i))
					return true;
				i += 2;
			} while (i < n);
			return false;
		}

		// Set move currently being searched.
		Move        setCurrMove(Move m) { return current_ply->curr_move = m; }
		// move currently being searched.
		Move        curr_move()			const { return current_ply->curr_move; }
		// moves from prior Plys
		Move        curr_move(int d)	const { Assert(d < gameDepth()); return current_ply[-d].curr_move; }
		Piece       curr_capture()		const { return current_ply->curr_capture; }
		Piece       SetCapture(Piece cap) { return current_ply->curr_capture = cap; }

		// --------------------------------------------------------------------------------------
		// EP status
		void        clearEP() { current_ply->ep = Square(0); }
		// Set EP field -- the square now containing the pawn just moved
		Square		setEP(Square sq) { Assert(sq >= A4 && sq <= H5); return current_ply->ep = sq; }

		// epCapture is the square with the pawn to be captured/removed from board -- relative rank 4
		Square		epCapture()			const { return current_ply->ep; }
		// epTo is the destination of the capturing pawn
		Square		epTo()				const { return retreatEP(epCapture()); }
		// epTarget is Bitboard of destination square
		Bitboard	epTarget()			const { return Bit(epTo()); }
		// Is EP capture available
		bool		epSet()				const { return epCapture() != Square(0); }
		// --------------------------------------------------------------------------------------

		Piece		GetCapture(Move&   m) { return PieceTypeOnSqr(MoveToSq(m)); }
		Piece		GetPiece(Move&   m)	const { return PieceTypeOnSqr(MoveFromSq(m)); }

		int8		reversible()		const { return current_ply->reversible50; }
		int8		reversible(int8 n) { return current_ply->reversible50 = n; }

		CastlingRights castling_rights() const { return current_ply->castling_rights; }
		CastlingRights castling_rights(Color c) const { return CR_rights(c)  & castling_rights(); }
		CastlingRights castling_rights(Color c, CR kq) const { return castling_rights(c) & kq; }

		// Set castling_rights field
		CastlingRights setCR(CastlingRights cr) { return current_ply->castling_rights = cr; }

		void		init() { Tree::init(); sideToMove = White; current_ply = root; }
		void		clear() { init(); }


		/// pieces of color c attacking sq
		Bitboard PawnAttacksTo(Square sq, Color c) const { return pawnAttack(sq, ~c) & Pawns(c); }
		Bitboard attacksTo(Square sq, Color c, Bitboard occ) const
		{
			return
				(rookAttack(sq, occ)	& Pieces(c, Queen, Rook)) |
				(bishopAttack(sq, occ)	& Pieces(c, Queen, Bishop)) |
				PawnAttacksTo(sq, c) |
				(knightAttack(sq)		& Knights(c)) |
				(kingAttack(sq)			& Kings(c));
		}
		Bitboard attacksTo(Square sq, Color c) const { return attacksTo(sq, c, Pieces()); }

		bool	pseudo_legal(Color c, Move m)			const;
		bool	pseudo_legal(Move m)					const { return pseudo_legal(sideToMove, m); }
		bool	verify_ttm	(Move m)					const { return pseudo_legal(m); }

		// All generated normal piece moves are fully legal except King moves.
		bool legal_normal_move(Color c, Piece p, Square f, Square t) const
		{
			return (p != King) || attacksTo(t, ~c) == 0;
		}

		bool legal_king_move(Move& m)
		{
			Square to = MoveToSq(m);
			Color c = sideToMove;
			if (to < SPECIAL) return attacksTo(to, ~c) == 0;
			else
			{
				Square ksq = KingSq(c);
				return SqCRKS(to)	? attacksTo(ksq + 1, ~c) == 0 && attacksTo(ksq + 2, ~c) == 0 
									: attacksTo(ksq - 1, ~c) == 0 && attacksTo(ksq - 2, ~c) == 0;

			}
			return true;
		}
		bool legal_move(Move& m)
		{
			return (GetPiece(m) != King) || legal_king_move(m);
		}

		// unset castling rights set in 'del' -- return Hash difference
		Hash    update_castling(CastlingRights oldCR, CastlingRights del)
		{
			CR newCR = setCR(CR_remove(oldCR, del));
			return Zobrist::HashCastlingRights(oldCR^newCR);
		}
		Hash    update_castling(CastlingRights del)
		{
			return update_castling(castling_rights(), del);
		}

		// set the ep flag and return corresponding EP hash key
		Hash hashEP(Square cap) { setEP(cap); return Zobrist::HashEP(cap); }
		// remove the EP componentent of the hash key for the current ply
		Hash unhashEP() { Assert(epSet());  Square cap = epCapture();  clearEP(); return setHash(hash() ^ Zobrist::HashEP(cap)); }

	private:
		// Initialize next ply on the stack and adjust PLY pointer -- return the previous ply
		// __declspec(noinline)
		PLY*	pushPly(Move m) {
			Assert(gameDepth() < MaxPly);
			PLY* prev = current_ply;
			PLY* next_ply = current_ply + 1;
			next_ply->init(*current_ply, m);
			current_ply = next_ply;
			return prev;
		}
		// Pop bottom PLY off stack -- adjust PLY pointer.
		PLY*	popPly() { Assert(gameDepth() > 0);  return --current_ply; }

		__forceinline
			void	doNormalMove(Color c, Hash k, Square f, Square t);
		__forceinline
			void	doSpecialMove(Color c, Hash k, Square f, Square to_type);
		//__declspec(noinline)
		__forceinline
			void	undoNormalMove(Color c, Square f, Square t);
		//__declspec(noinline)
		__forceinline
			void	undoSpecialMove(Color c, Square f, Square t);

		Hash hash_place_piece(Color c, Piece p, Square sq) { return Zobrist::H(c, p, sq); }
		Hash hash_remove_piece(Color c, Piece p, Square sq) { Assert(p != King); return p ? Zobrist::H(c, p, sq) : 0; }
		Hash hash_move_piece(Color c, Piece p, Square from, Square to) { return Zobrist::HashMove(c, p, from, to); }
		Hash hash_capture_piece(Color c, Piece p, Square from, Square to, Piece cap) { return Zobrist::HashCapture(c, p, from, to, cap); }
		Hash hash_normal_move(Color c, Piece p, Square from, Square to, Piece cap) { return cap ? hash_capture_piece(c, p, from, to, cap) : hash_move_piece(c, p, from, to); }

		// inline int classifyHit(TTE tte, int depth, bool do_null, int null_depth, Score alpha, Score beta, Score& value);
		inline	Score	BetaCutoffExit(Score value, Move m);
		inline	Score	NoMoveExit();
		inline	void	improvedAlpha(Move m, int n, GenStage stg);

	public:
		std::ostream& ShowBoard(std::ostream& os) const;

		uint64		perft(int depth);
		// perft divided
		std::ostream& divide(std::ostream&, int depth);
		std::ostream & search(std::ostream & os, int depth);
		// Utility to decode algebraic notation into From/To format returned as a Move16
		static Move parseMoveFT(const char* ms);
		// Utility to decode algebraic notation into Move format
		// verify pseudo_legality and correctly encoding special moves
		Move parseMove(const char* ms);
		/// <summary>
		/// Convert EP/Promo/Castle moves expressed in from/to notation
		/// to use the SPECIAL flag.
		/// </summary>
		/// <param name="depth"></param>
		/// <returns></returns>
		Move convertFt2Special(Move m);
		std::ostream& fen(std::ostream& os) const;

	};

	typedef Position& POS;

}
