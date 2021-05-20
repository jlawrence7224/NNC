#include <iostream>
#include <string>
#include "NpEngine.h"
#include "pch.h"
#include <time.h>

namespace NNC { 

#if 1
	inline void moveSort(mov_t* beg, mov_t* end) {
		std::sort(beg, end, std::greater<mov_t>());
	}
	inline void movePartialSort(MoveX* beg, MoveX*end, int n) {
#if 0
		moveSort(beg, end);
#else
		if (end - beg <= n) { moveSort(beg, end); return; }
		std::partial_sort((int32_t*)beg, (int32_t*)beg + n, (int32_t*)end, std::greater<int32_t>());
#endif
	}
#endif

	struct KingPawnInfo
	{
		Bitboard m_rook_pins;
		Bitboard m_rook_tgt;
		Bitboard m_bishop_pins;
		Bitboard m_bishop_tgt;

		Bitboard P1, P2, PA, PH;	// Pawn advances/attacks

		Bitboard m_kingMobility;	// pseudo legal king moves
		Bitboard m_target;			// In check targets squares (when #checkers == 1)
		Bitboard m_checkers;		// All checkers.
		Bitboard m_PE;				// Squares attacked by enemy pawns

		void initKingPawnInfo(Color c, POS pos);
	};

	class MoveGen : KingPawnInfo
	{
		POS		pos;

	#define KingInCheck InCheck()
	// #define lcapBeg (MoveX*)attEnd;
	#define clr pos.sideToMove
	#define pseudo_legal (stage != UNINITIALIZED)

		typedef Bitboard Att;	// attack bitboards

		void*&	stackPtr;		// reference to stack ptr maintained by POS
		void*   stackBase;		// our starting stack pointer
		MoveX*	moveBeg;		// start of stack region containing Move structures
		MoveX*  moveNext;		// pointer to next generated move
		MoveX*	moveEnd;		// (past) end of stack region containing Moves
		// MoveX*  lcapBeg;		// start of stack region containing losing captures (== moveBeg)
		MoveX*	lcapEnd;		// (past) end of stack region containing losing captures
		Att*	attack;			// start of computed attack Bitboards stored on stack
		#if COUNT_SORT
		union {
			char count[8];
			uint64 ucount;
		};
		#endif
		char	from_att[16];	// from-squares associated with each attack bitboard
		int		n_att;			// # from/attack pairs


		// staged move generation state
		GenStage stage;
		Move	ttmove;
		Move	killer1;
		Move	killer2;
		CR		cr_rights;

	public:

		// returns true if position is PseudoLegal, ie no King capture is possible
		bool initGen() { stageCaptures(); return stage != INVALID_POSITION; }
		

		MoveGen(POS state, Move ttm, Move killer1, Move killer2, bool init) : pos(state), ttmove(ttm), killer1(killer1), killer2(killer2), stackPtr(pos.stackNext()), ucount(0), attack(0)
		{
			// Set pointers into Move stack
			stackBase = stackPtr;
			cr_rights = state.castling_rights(clr);
			if (init)
				initGen();
		}
		MoveGen(POS state, Move ttm, Move killer1, Move killer2) : MoveGen(state, ttm, killer1, killer2, true) {}
		MoveGen(POS state) : MoveGen(state, MOVE_NONE, MOVE_NONE, MOVE_NONE, true) {}
		MoveGen(POS state, bool init) : MoveGen(state, MOVE_NONE, MOVE_NONE, MOVE_NONE, init) {}

		// Restore position's stack pointer
		~MoveGen() { stackPtr = stackBase; }


		Move* nextMove();					// Return all moves except the ttmove
		Move nextGoodCapture(Score delta);	// Moves for Qsearch with delta pruning
		void genAll(MoveX*& first, MoveX*& end); // Generates all moves. Returns moveBeg..moveEnd
		uint64 bulk_count(Color c);			// for perft

		GenStage	Stage() { return stage; }
		bool		InCheck() { return m_checkers; }

		//Value SeeFull2(Bitboard occ, Square to, Value at_stake, Value captured);
		//Value SeeFull(Bitboard occ, Square to, Value at_stake, Value captured);

		// Is Sign(m) < 0
		//bool SeeSign_old(Bitboard occ, Square to, Value at_stake, Value captured);
		bool SeeSign(Bitboard occ, Square to, Value at_stake, Value captured);
		bool SeeSign(Square to, Square from, Value at_stake, Value captured)
		{
			// Quick exits
			if (at_stake <= captured)
				return false;

			return SeeSign(pos.Pieces() ^ from, to, at_stake, captured);
		}
		bool SeeSign(Square toSq, Piece target, Square frSq, Piece aPiece) { return SeeSign(toSq, frSq, val[aPiece], val[target]); }
	private:
		// move_next() subroutines
		bool losingCapture(MoveX& m);

	private:
		//------------------------------------------------------------------------
		// Tests for Legal killer moves

		// Test whether m is a Legal killer move (must be quiet).
		// If so, remove move from the attack set.
		bool verify_killer_move(Move m);


		////////////////////////////////////////////////////////////////////
		// Compute attack bitboards
		void initAttackStack();
		void initMoveStack();
		__forceinline void genPawnAttacks(Color c, Bitboard P, Bitboard pinned);
		void genPawnAttacksInCheck(Color c, Bitboard P, Bitboard pinned, Bitboard target);
		// __forceinline
		void pushAttFrom(Piece p, Square from, Bitboard att);
		void genPieceAttacks(Color c, Bitboard occ);
		void genPieceAttacksInCheck(Color c, Bitboard occ, Bitboard target);
		void gen_attacks(Color c);

		////////////////////////////////////////////////////////////////////
		// Move generation
		template<ScoreType S>
		Score score(Piece p, Square from, Square to) { return Score(0); }

		// QUIET moves scored with history scoring
		template<>
		Score score<QUIET>(Piece p, Square from, Square to) { return Score(0); }

		// Castle gets scored as King:ksq->32+CS_FLAG (looked up in history table).
		template<>
		Score score<CS>(Piece p, Square from, Square to) { return score<QUIET>(p, from, to ^ 64); }

		// Captures are scored MVV/LVA but only the the captured piece type is stored in score field.
		// Moves are generated in LVA order so a stable sort produces MVV/LVA order using captured type as key.
		#if COUNT_SORT
		template<>
		Score score<CAPTURE>(Piece p, Square from, Square to) { Score s = pos.board(to); count[s]++; return s; }
		#else
		template<>
		Score score<CAPTURE>(Piece p, Square from, Square to) { return pos.board(to); }
		#endif

		/// <summary>
		/// Sort captures from moveNext to end.
		/// Point (moveNext,moveEnd) at the sorted moves
		/// </summary>
		/// <param name="end"></param>
		/// <returns></returns>
		bool captureSort();
		void quietSort() {
			movePartialSort(moveNext, moveEnd, 4);
		}

		//------------------------------------------------------------------------
		// Score a move and construct a scored move on the move stack
		template<ScoreType S> 
		void enq_move(MoveX& m, Piece p, Square from, Square to) { return initMoveX(m, from, to, score<S>(p,from,to)); }


		template<ScoreType S>
		MoveX* enq_king_moves(MoveX* ml, Bitboard target)
		{
			// King moves
			if (Bitboard km = m_kingMobility & target)
			{
				Color c = clr, enemy = ~c;
				Square ksq = pos.KingSq(c);
				Bitboard occ = andn(pos.Pieces(), Bit(ksq));
				Bitboard QR = pos.Pieces(enemy, Queen, Rook), QB = pos.Pieces(enemy, Queen, Bishop), N = pos.Knights(enemy);
				for (auto kto : km)
				{
					// Squares attacked by enemy King or Pawns have already been removed from 
					// m_kingMobility (by initKingPawnInfo), attacks by Q,R,B,N are checked here. 
					int incr = ((rookAttack(kto, occ) & QR) | (bishopAttack(kto, occ) & QB) | (knightAttack(kto) & N)) 
								== NO_SQUARES;
					enq_move<S>(*ml, King, ksq, kto);	// speculatively initialize move structure awaiting legality check
					ml += incr;
				}
			}
			return ml;
		}

		/// <summary>
		/// enqueue piece moves to target squares scored by specified score type
		/// </summary>
		/// <param name="ml"></param>
		/// <param name="target"></param>
		/// <returns></returns>
		template<ScoreType S>
		MoveX* enq_piece_moves(MoveX* ml, Bitboard target)
		{
			int n = n_att;
			for (int i = 0; i < n; ++i)
			{
				Square from = Square(from_att[i]);
				Piece p = pos.board(from);
				for (Square to : attack[i] & target)
					enq_move<S>(*ml++, p, from, to);
			}
			return ml;
		}

		bool ep_fifth_rank_pin(Square from, Square to);

		// enqueue EP captures
		// Delete ep target square from PA/PH sets
		// Check for the special 5th rank EP pin

		// In the rare case that there is an EP target 
		// but no EP captures are possible because of pins
		// then rehash the position.
		__forceinline
			MoveX* enq_ep_captures(MoveX* ml, int adv);

#if REMOVE
		__forceinline
		void enq_pawn_advance(MoveX& ml, Square to, int offset)
		{
			Square from = to - offset;
			setQuiet(ml, from, to, scoreQuiet(Pawn, from, to));
		}
		__forceinline
			void enq_pawn_advance2(MoveX& ml, Square to)
		{
			Square from = retreat24(to);
			setQuiet(ml, from, ADV2, scoreQuiet(Pawn, from, to));
		}
#endif
		MoveX* enq_pawn_promotions(MoveX* ml, int adv, Bitboard rank8);
		MoveX* enq_pawn_caps_and_promotions(Color c, MoveX* ml);

		// enque non-promoting pawn advances
		MoveX* enq_pawn_advances(Color c, MoveX* ml);
		MoveX* enq_castling_moves(MoveX* ml, Color c);

		MoveX* enqQuiet(Color c, MoveX* ml);
		MoveX* enqCaptures(Color c, MoveX* ml);

		void stageCaptures();
		void stageKillers();
		void stageQuiet();
		void stageLosers();

		////////////////////////////////////////////////////////////////////
		// SEE -- static exchange evaluation

		// Sign of Static Exchange Evaluation
		// return See < 0 -- ie losing captures return true, See >= 0 returns false
		// Bishops and knights considered equal
		// -- Blind version -- return true when HxL and toSq has non-pinned defenders

		//_declspec(noinline) 
		//__forceinline
			//bool losingCapture(Move m);
#if 0
		{
			const int val[8] = { 0,0,10,1,3,3,5,9 };
			// Treat bishops and knights as equal
			Piece aPiece = MovePiece(m);
			Piece target = MoveCapture(m);
			if (val[target] >= val[aPiece])
				return false;
			return pos.Blind(clr, MoveToSq(m), target, MoveFromSq(m), aPiece);
		}
#endif
	}; // End of MoveGen
}
