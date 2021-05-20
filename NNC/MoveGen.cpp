#include <iostream>
#include <string>
#include "NpEngine.h"
#include "pch.h"
#include <time.h>
#include "MoveGen.h"

namespace NNC {
	//------------------------------------------------------------------------
	// Tests for Legal killer moves

	// Test whether m is a legal killer move (must be quiet).
	// If so, remove move from the attack set.
	bool MoveGen::verify_killer_move(Move m)
	{
		Square f = MoveFromSq(m), t = MoveToSq(m);
		Piece p = pos.board(f);

		// killers come only from sibling nodes
		if( member(pos.Pieces(clr), f) )
		{ 
			if( t > CASTLE )
			{ 
				SqrInt cr = SqCR(t);
				cr_rights = CR(cr_rights & ~cr);
				return cr & cr_rights;
			}
			Bitboard to = Bit(t);
			if (p > Pawn)
			{
				for (int i = 0; i < n_att; ++i)
				{
					if (from_att[i] == f)
					{
						attack[i] = andn(attack[i], to);
						return member(attack[i], t);
					}
				}
				if (p == King)
				{
					m_kingMobility = andn(m_kingMobility, to);
					return member(m_kingMobility, t);
				}
				return false;
			}
			P1 = andn(P1, to); P2 = andn(P2, to);
			return (P1 | P2) & to;
		}
	}

	////////////////////////////////////////////////////////////////////
	// Compute attack bitboards
#if 0
	__forceinline
		void MoveGen::genPawnAttacks(Color c, Bitboard P, Bitboard pinned)
	{
		Bitboard PR, PL, PF;
		PR = PL = PF = P;
		Bitboard enemy = pos.Pieces(~c) | pos.epTarget();	// Include EP target square as a capture target
		Bitboard empty = ~pos.Pieces();

		if (P & pinned)
		{
			Square ksq = pos.KingSq(c);
			Bitboard unpinned = ~pinned;
			PF &= FileOf_bb(ksq) | unpinned;			// Pawns which can legally advance along its file
			PR &= (DiagOf_bb(ksq, true) | unpinned);	// Pawns which can legally capture along LL/UR diagonal
			PL &= (DiagOf_bb(ksq, false) | unpinned);	// Pawns which can legally capture along LR/UL diagonal
		}

		// Advance White Pawns Up (greater square number) and to the right or left -- Black pawns down (lesser squares)
		if (c == White)
		{
			P1 = (PF << 8) & empty;
			P2 = (P1 << 8) & (empty & rank_bb(RANK_4));
			PH = ((PR & (~FH)) << 9) & enemy;
			PA = ((PL & (~FA)) << 7) & enemy;
		}
		else /*Black */
		{
			P1 = (PF >> 8) & empty;
			P2 = (P1 >> 8) & (empty & rank_bb(RANK_5));
			PH = ((PL & (~FH)) >> 7) & enemy;
			PA = ((PR & (~FA)) >> 9) & enemy;
		}
	}
#endif
	void NNC::KingPawnInfo::initKingPawnInfo(Color c,POS pos)
	{
		std::memset(this, 0, sizeof(*this));
		Square   ksq		= pos.KingSq(c);
		Bitboard enemy		= pos.Pieces(~c);
		Bitboard self		= pos.Pieces(c);

		Bitboard Rook_pp	= rookAttack(ksq, enemy) & pos.Pieces(c, Queen, Rook);
		Bitboard Bishop_pp	= bishopAttack(ksq, enemy) & pos.Pieces(c, Queen, Bishop);
		m_kingMobility		= andn(andn(kingAttack(ksq), kingAttack(pos.KingSq(~c))), self);
		m_checkers			= knightAttack(ksq) & pos.Knights(~c);

		Bitboard PF = NO_SQUARES;

		// These loops find pinned pieces and sliding checkers simultaneously
		for (auto it = begin(Rook_pp); it != end(Rook_pp); ++it)
		{
			Bitboard btw	= between_bb(ksq, *it);
			Bitboard blockers = self & btw;
			Bitboard p		= blsi(it);
			PF				= pos.Pawns(c) & FileOf_bb(ksq);

			// Branchless updates to pin and checking sets
			m_rook_tgt		|= (btw | p);	// Ok to add this to target set even if p is not pinner

			btw				= blockers ? NO_SQUARES : btw;
			p				= blockers ? NO_SQUARES : p;
			blockers		= blsr(blockers) ? blockers : NO_SQUARES;

			m_rook_pins		|= blockers;
			m_target		|= btw;
			m_checkers		|= p;
			m_kingMobility	 = andn(m_kingMobility, btw);
		}

		Bitboard PR = NO_SQUARES, PL = NO_SQUARES;
		for (auto it = begin(Bishop_pp); it != end(Bishop_pp); ++it)
		{
			Bitboard btw	= between_bb(ksq, *it);
			Bitboard blockers = self & btw;
			Bitboard p		= blsi(it);
			PR				= pos.Pawns(c) & DiagOf_bb(ksq, true);
			PL				= pos.Pawns(c) & DiagOf_bb(ksq, false);

			// Branchless updates to pin and checking sets
			m_bishop_tgt	|= (btw | p);	// Ok to add this to target set even if p is not pinner

			btw				= blockers ? NO_SQUARES : btw;
			p				= blockers ? NO_SQUARES : p;
			blockers		= blsr(blockers) ? blockers : NO_SQUARES;

			m_bishop_pins	|= blockers;
			m_target		|= btw;
			m_checkers		|= p;
			m_kingMobility	 = andn(m_kingMobility, btw);
		}

		Bitboard PP = andn(pos.Pawns(c), m_bishop_pins | m_rook_pins);	// unpinned pawns
		PF |= PP;	// Pawns which can legally advance
		PR |= PP;	// Pawns which can legally capture along LL/UR diagonal
		PL |= PP;	// Pawns which can legally capture along LR/UL diagonal

		Bitboard empty	 = ~(self | enemy);
		Bitboard PE		 = pos.Pawns(~c);
		enemy			|= pos.epTarget();	// Add EP target square to capture target

		// Advance White Pawns Up (greater square number) and to the right or left
		// Black pawns down (lesser squares)
		if (c == White)
		{
			m_checkers |= (PE & (beside_bb(ksq) << 8));	// enemy pawns attacking King
			Bitboard PEatt = ((PE & (~FH)) >> 7) | ((PE & (~FA)) >> 9);

			P1 = (PF << 8) & empty;
			P2 = (P1 << 8) & (empty & rank_bb(RANK_4));
			PH = ((PR & (~FH)) << 9) & enemy;
			PA = ((PL & (~FA)) << 7) & enemy;

			m_kingMobility = andn(m_kingMobility, PEatt);
			m_PE = PEatt;
		}
		else /* Black */
		{
			m_checkers |= (PE & (beside_bb(ksq) >> 8));	// enemy pawns attacking King
			Bitboard PEatt = ((PE & (~FH)) << 9) | ((PE & (~FA)) << 7);

			P1 = (PF >> 8) & empty;
			P2 = (P1 >> 8) & (empty & rank_bb(RANK_5));
			PH = ((PL & (~FH)) >> 7) & enemy;
			PA = ((PR & (~FA)) >> 9) & enemy;

			m_kingMobility = andn(m_kingMobility, PEatt);
			m_PE = PEatt;
		}
	}

	/// <summary>
	/// Trim the pawn move/capture sets according to the target set.
	/// </summary>
	/// <param name="c"></param>
	/// <param name="P"></param>
	/// <param name="checkers"></param>
	/// <param name="target"></param>
	void MoveGen::genPawnAttacksInCheck(Color c, Bitboard P, Bitboard checkers, Bitboard target)
	{
		P1 &= target;
		P2 &= target;
		if (member(checkers,pos.epCapture()) && pos.epCapture() != Square(0) )
		{
			// (epCapture() is 0 when there is no EP capturable pawn, thus that case is excluded.)

			// An EP capture is an evasion only if the pawn to be captured is giving check 
			// -- the epTarget square will never be in the target set because occupying the
			// -- rank3 square cannot block a discovered check that results from opponents rank2->rank4 advance
			target |= pos.epTo();
		}
		PA &= target;
		PH &= target;
	}

	// __forceinline
	void MoveGen::pushAttFrom(Piece p, Square from, Bitboard att)
	{
		from_att[n_att] = from;
		attack[n_att++] = att;
	}

	void MoveGen::genPieceAttacks(Color c, Bitboard occ)
	{
		Bitboard bp = m_bishop_pins, rp = m_rook_pins;
		Bitboard unpinned = ~(bp|rp);
		Bitboard N = pos.Knights(c);
		Bitboard B = pos.Bishops(c);
		Bitboard R = pos.Rooks(c);
		Bitboard Q = pos.Queens(c);

		for (Square from : N  & unpinned)
			pushAttFrom(Knight, from, knightAttack(from));
		for (Square from : B  & unpinned)
			pushAttFrom(Bishop, from, bishopAttack(from, occ));
		for (Square from : B  & bp)
			pushAttFrom(Bishop, from, bishopAttack(from, occ) & m_bishop_tgt);
		for (Square from : R  & unpinned)
			pushAttFrom(Rook,   from, rookAttack(from, occ));
		for (Square from : R  & rp)
			pushAttFrom(Rook,   from, rookAttack(from, occ)   & m_rook_tgt);
		for (Square from : Q  & unpinned)
			pushAttFrom(Queen,  from, queenAttack(from, occ));
		for (Square from : Q  & bp)
			pushAttFrom(Queen,  from, bishopAttack(from, occ) & m_bishop_tgt);
		for (Square from : Q  & rp)
			pushAttFrom(Queen,  from, rookAttack(from, occ)   & m_rook_tgt);
	}

	void MoveGen::genPieceAttacksInCheck(Color c, Bitboard occ, Bitboard target)
	{
		Bitboard unpinned = ~(m_bishop_pins|m_rook_pins);
		Bitboard N = pos.Knights(c);
		Bitboard B = pos.Bishops(c);
		Bitboard R = pos.Rooks(c);
		Bitboard Q = pos.Queens(c);

		for (Square from : N & unpinned)
			pushAttFrom(Knight, from, knightAttack(from)      & target);
		for (Square from : B & unpinned)
			pushAttFrom(Bishop, from, bishopAttack(from, occ) & target);
		for (Square from : R & unpinned)
			pushAttFrom(Rook, from,   rookAttack(from, occ)   & target);
		for (Square from : Q & unpinned)
			pushAttFrom(Queen, from,  queenAttack(from, occ)  & target);
	}

	constexpr uintptr_t align_mask = alignof(Bitboard) - 1;

	void MoveGen::initAttackStack() 
	{ 
		stackBase = stackPtr = pos.stackNext();
		// align attack pointer on 8 byte boundary
		// Note: _andn_64(x,y) negates first arg: (~x)&y
		attack = (Att*)_andn_u64(align_mask, uintptr_t(stackBase) + align_mask);
		n_att = 0;
	}
	void MoveGen::initMoveStack()
	{
		// increment stack ptr past end of attack bitboards
		stackPtr = &attack[n_att];
		// Initialize move array pointers
		lcapEnd = moveEnd = moveNext = moveBeg = (MoveX*)stackPtr;
	}

	/// <summary>
	/// Generate piece attack bitboards.
	/// Pawn attacks were computed in initKingPawnInfo 
	/// but are adjusted here when king is in check.
	/// </summary>
	/// <param name="c">side to move</param>
	void MoveGen::gen_attacks(Color c)
	{
		// setup stack for attack generation
		initAttackStack();
		if (Bitboard checkers = m_checkers)	
		{
			// King in check
			if (singleton(checkers))
			{
				// compute piece/pawn evasion targets
				Bitboard target = m_target | checkers;
				genPawnAttacksInCheck(c, pos.Pawns(c), checkers, target);
				genPieceAttacksInCheck(c, pos.Pieces(), target);
			}	
		}
		else
		{
			// compute piece attacks when king not in check
			genPieceAttacks(c, pos.Pieces());
		}
		// setup stack for move generation
		initMoveStack();
	}

	////////////////////////////////////////////////////////////////////
	// Move generation

	bool MoveGen::ep_fifth_rank_pin(Square from, Square to)
	{
		// Check for EP pin on the 5th rank
		Square cap = advanceEP(to);							// Square with the pawn to be captured
		Square ksq = pos.KingSq(clr);
		if (RankOf(cap) == RankOf(ksq))						// if King is on the 5th rank -- not likely
		{													// along with an enemy Q or R -- even less likely
			if (Bitboard pp = rank_bb(RankOf(cap)) & pos.Pieces(~clr, Queen, Rook))
			{
				// Test whether King will be in check after an EP capture
				Bitboard occ = pos.Pieces() ^ cap ^ from;
				for (Square p : pp)
				{
					if (!(between_bb(ksq, p) & occ))		// Nothing between King and major piece after the pawns have moved
						return true;
				}
			}
		}
		return false;
	}
	// enqueue EP captures
	// Delete ep target square from PA/PH sets
	// Check for the special 5th rank EP pin

	// In the rare case that there is an EP target 
	// but no EP captures are possible because of pins
	// then rehash the position.
	__forceinline
		MoveX* MoveGen::enq_ep_captures(MoveX* ml, int adv)
	{
		if (Bitboard ept = pos.epTarget())
		{
			Square to = pos.epTo(), from;
			MoveX* orig = ml; // Save original ml

			// generate the moves
			if (ept & PA)
			{
				PA ^= ept;
				from = to - (adv - 1);
				initMoveX(*ml++, from, EPA, Pawn);
			}
			if (ept & PH)
			{
				PH ^= ept;
				from = to - (adv + 1);
				initMoveX(*ml++, from, EPH, Pawn);
			}
			if (ml == orig || ep_fifth_rank_pin(from, to))
			{
				// In the rare case that there is an EP target 
				// but no EP captures are possible because of pins
				// then rehash the position.
				pos.unhashEP();
				return orig;
			}

		}
		return ml;
	}
	MoveX* MoveGen::enq_pawn_promotions(MoveX* ml, int adv, Bitboard rank8)
	{
		MoveInt* promo = reinterpret_cast<MoveInt*>(ml);
		// enqueue pawn promotions
		// promoting captures are scored as xQ; promoting non-captures are scored xR
		// Promotions are the first moves generated on the move stack, so a (non-capture)
		// promotion will sort below an actual Queen capture but above any Rook captures

		int offset = adv - 1;
		for (Square to : PA & rank8)
			initMoveX(*ml++, to - offset, QPA, Queen);
			offset = adv + 1;
		for (Square to : PH & rank8)
			initMoveX(*ml++, to - offset, QPH, Queen);
		for (Square to : P1 & rank8)
			initMoveX(*ml++, to - adv, QPF, Rook);

		// Generate underpromotions directly on the losing capture stack
		// Move the queen promotions after the under promotions
		// [This shuffle accomodates the rare case of more than one promotion, 
		//  otherwise we would generate them in the correct position.]

		// n == #promotions
		size_t n = (reinterpret_cast<MoveInt*>(ml) - promo);

		// allow 3*n slots on the losing capture stack
		// The n queen promotions already generated will be moved past that point
		MoveInt* Qpromo = &promo[3 * n];	// Queen promotions moved here
		MoveInt* under = Qpromo;			// under promotions stored below here
		for (int i = 0; i < n; i++)
		{
			MoveInt p = Qpromo[i] = promo[i];
			*--under = p = MovePromotionUnder(p);
			*--under = p = MovePromotionUnder(p);
			*--under = p = MovePromotionUnder(p);
		}
		// Adjust pointers into the stack
		ml = moveNext = lcapEnd = reinterpret_cast<MoveX*>(Qpromo);
		// Advance ml past the Queen promotions just generated.
		ml += n;
		return ml;
	}

	MoveX* MoveGen::enq_pawn_caps_and_promotions(Color c, MoveX* ml)
	{
		int adv = advance(c);
		Bitboard rank8 = rank_bb(RANK_8, c);

		if ((PA | PH | P1) & rank8)
			ml = enq_pawn_promotions(ml, adv, rank8);

		// enqueue Pawn EP captures. Delete EP captures from PA/PH sets
		ml = enq_ep_captures(ml, adv);	

		// enqueue Pawn (non EP) captures
		int offset = adv - 1;
		for (Square to : andn(PA, rank8))
			enq_move<CAPTURE>(*ml++, Pawn, to - offset, to);
			offset = adv + 1;
		for (Square to : andn(PH,rank8))
			enq_move<CAPTURE>(*ml++, Pawn, to - offset, to);

		return ml;
	}

	/// <summary>
	/// enque non-promoting 1 and 2 square pawn advances
	/// </summary>
	/// <param name="c"></param>
	/// <param name="ml"></param>
	/// <returns></returns>
	MoveX* MoveGen::enq_pawn_advances(Color c, MoveX* ml)
	{
		int adv = advance(c);
		Bitboard rank8 = rank_bb(RANK_8, c);
		for (Square to : P2)
			enq_move<QUIET>(*ml++, Pawn, retreat24(to), to);
		for (Square to : andn(P1,rank8))
			enq_move<QUIET>(*ml++, Pawn, to-adv, to);

		return ml;
	}

	const Bitboard cs_btw[2] = { F1 | G1, F8 | G8 };
	MoveX* MoveGen::enq_castling_moves(MoveX* ml, Color c)
	{
		if (CR cr = cr_rights) //   pos.castling_rights(c))
		{
			if (!KingInCheck)
			{
				Square ksq = pos.KingSq(c);
				Bitboard occ = pos.Pieces();
				Bitboard btw = cs_btw[c];
				if (cr & CR_KS)
				{
					if (!(btw & occ))
						// Test for king moves through
						// or into check occurs in doMove
						if (pseudo_legal || (pos.attacksTo(ksq + 2, ~c) == 0 && pos.attacksTo(ksq + 1, ~c) == 0))
							enq_move<CS>(*ml++, King, ksq, CASTLE | SqrInt(cr & CR_KS));
				}
				if (cr & CR_QS)
				{
					if (!((btw >> 3) & occ))
						// Test for king moves through
						// or into check occurs in doMove
						if (pseudo_legal || (pos.attacksTo(ksq - 2, ~c) == 0 && pos.attacksTo(ksq - 1, ~c) == 0))
							enq_move<CS>(*ml++, King, ksq, CASTLE | SqrInt(cr & CR_QS));
				}
			}
		}
		return ml;
	}

	uint64 SumCounts(uint64 c)
	{
		uint64 s = (c << 8) + (c << 16);
		return (c << 40) + s + (s << 16);
	}

	bool MoveGen::captureSort() {
#if COUNT_SORT
		if (count[King] != 0)
			return true;
		ucount = SumCounts(ucount);
		MoveX* beg = moveNext;
		MoveX* end = moveEnd;
		int n = int(end - beg);

		for (int i = 0; i < n; ++i)
		{
			end[count[beg[i].m_score]++] = beg[i];
		}
		moveNext += n;
		moveEnd += n;
		stackPtr = moveEnd;		// advance stack pointer
		return false;
#else
		std::sort(moveNext, moveEnd, std::greater<mov_t>());
		stackPtr = moveEnd;		// advance stack pointer
		// King capture?
		return (moveNext->m_score == King);
#endif
	}

	MoveX* MoveGen::enqCaptures(Color c, MoveX* ml)
	{
		ml = enq_pawn_caps_and_promotions(c, ml);
		ml = enq_piece_moves<CAPTURE>(ml, pos.Pieces(~c));
		ml = enq_king_moves<CAPTURE>(ml, pos.Pieces(~c));
		return ml;
	}
	//__declspec(noinline)
	void MoveGen::stageCaptures()
	{
		stage = PLAY_CAPTURES;
		gen_attacks(clr);
		moveEnd = enqCaptures(clr,moveNext);
		if (captureSort())
		{
			// abort if king capture
			// Pop moves from stack
			stackPtr = moveEnd = moveNext = moveBeg;
			stage = INVALID_POSITION;
		}
	}

	void MoveGen::stageKillers()
	{
		// pop the captures off the stack,
		// push the killers after the losing captures

		if (killer1)
		{
			MoveX* ml = moveNext = lcapEnd;
			if (verify_killer_move(killer1))
			{
				// If killer1 is ttmove then skip it
				if (ttmove == killer1)
					ttmove = 0;
				else
					*ml++ = killer1;
			}
			if (killer2 && verify_killer_move(killer2))
			{
				// If killer2 is ttmove then skip it
				if (ttmove == killer2)
					ttmove = 0;
				else
					*ml++ = killer2;
			}
			stackPtr = moveEnd = ml;
			stage = GEN_QUIET;	// Next gen stage
			return;
		}
		stageQuiet();
	}

	Move* MoveGen::enqQuiet(Color c, MoveX* ml)
	{
		ml = enq_pawn_advances(c, ml);
		ml = enq_piece_moves<QUIET>(ml, ~pos.Pieces());
		ml = enq_castling_moves(ml, c);
		return ml;
	}
	//__declspec(noinline)
	void MoveGen::stageQuiet()
	{
		// Generate the quiet moves. The move list is pushed on the stack 
		// following the losing captures/underpromotions list
		MoveX* ml = moveNext = lcapEnd;
		if (ttmove) 
		{ 
			// ttmove wasn't a capture or killer
			// ** TO DO ** Oh, crap. ttmove could be an underpromotion
			verify_killer_move(ttmove); 
			ttmove = 0; 
		}
		stackPtr = moveEnd = enqQuiet(clr, ml);
		// moveNext..moveEnd enumerates all quiet moves
		quietSort();		// sort in history order
		stage = GEN_LOSERS;	// Next gen stage
	}

	void MoveGen::genAll(MoveX*& beg, MoveX*& end)
	{
		Assert(stage == UNINITIALIZED);
		gen_attacks(clr);
		MoveX* ml = beg = moveNext = moveBeg;
		ml = enqCaptures(clr, ml);
		stackPtr = moveEnd = enqQuiet(clr, ml);
		moveNext = moveBeg = beg;
		end = moveEnd;
	}

	void MoveGen::stageLosers()
	{
		// Adjust the move list to point at the losing captures/underpromotions
		moveNext = moveBeg;
		stackPtr = moveEnd = lcapEnd;
		stage = GEN_DONE;
	}

	// Assuming the move is legal a capture by the King is always winning.
	// Thus Kings receive value 0 for HxL calculation.

	bool MoveGen::losingCapture(MoveX& m)
	{
		Square from = MoveFromSq(m);
		return SeeSign(MoveToSq(m), Piece(m.m_score), from, pos.board(from));
	}


	const Score qval[8] = { 0,100,100,300,300,500,900, 0 };
	Move MoveGen::nextGoodCapture(Score delta)
	{
		Assert(stage == PLAY_CAPTURES);

		while (moveNext < moveEnd)
		{
			Move& m = *moveNext++;
			// Futility pruning -- aka "delta" pruning.
			// https://www.chessprogramming.org/Delta_Pruning
			if (MoveIsPromotion(m))
				delta -= 800;
			if (qval[pos.GetCapture(m)] < delta)
				continue;
			// losing captures are moved to the losing captures queue (stackBeg..lcapEnd).
			if (losingCapture(m))
				continue;
			return m;
		}
		return MOVE_NONE;
	}


	// Return all moves except the ttmove
	__forceinline
		Move* MoveGen::nextMove()
	{
		if (stage == PLAY_CAPTURES)
		{
			// play only =/+ captures, queen promotions
			play_captures:
			while (moveNext < moveEnd)
			{
				MoveX& m = *moveNext++;
				if (ttmove == m)
				{
					ttmove = MOVE_NONE;
					continue;
				}
				// losing captures are moved to the losing captures queue (stackBeg..lcapEnd).
				if (losingCapture(m))
				{
					*lcapEnd++ = m;
					continue;
				}
				return &m;
			}
			stageKillers();
		}
		play_moves:
		if (moveNext < moveEnd)
			return moveNext++;

		switch (stage)
		{
		case GEN_QUIET:
			stageQuiet(); goto play_moves;
		case GEN_LOSERS:
			stageLosers(); goto play_moves;
		case GEN_CAPTURES:
			stageCaptures(); goto play_captures;
		case GEN_KILLERS:
			stageKillers(); goto play_moves;
		default:
			return 0;
		}
	}

	Bitboard Position::attackers(Square sq, Color c, Bitboard occ)
	{
		Square ksq = KingSq(c);
		Bitboard QR = Pieces(c, Queen, Rook);
		Bitboard QB = Pieces(c, Queen, Bishop);
		Bitboard qr = Pieces(~c, Queen, Rook);
		Bitboard qb = Pieces(~c, Queen, Bishop);
		Bitboard RA = rookAttack(sq, occ);
		Bitboard BA = bishopAttack(sq, occ);
		return ~CalcPinned(ksq, occ & PotentialPinners(~c, ksq), occ);
	}

	uint64 MoveGen::bulk_count(Color c)
	{
		gen_attacks(c);
		// Pawn moves 
		uint64 cnt = PopCnt(P1) + PopCnt(P2) + PopCnt(PA) + PopCnt(PH);
		if (rank_bb(RANK_7, c) & pos.Pawns(c))
		{
			Bitboard P = rank_bb(RANK_8, c);
			cnt += 3 * (PopCnt(PA&P) + PopCnt((P1 | PH)&P));
		}
		Square ksq = pos.KingSq(c);
		if (pos.epCapture() && (rank_bb(RANK_5, c) & pos.Kings(c)))
		{
			// test for 5th rank e.p.pin.
			for (Square pp : rank_bb(RANK_5, c) & pos.Pieces(~c, Queen, Rook))
				if (PopCnt(between_bb(pp, ksq) & pos.Pieces()) == 2)
				{
					cnt--;
					break;
				}
		}
		Bitboard target = ~pos.Pieces(c);
		// King moves
		for (auto kto : m_kingMobility & target)
			if (pos.attacksTo(kto, ~c) == 0)
				cnt++;

		// Castling moves
		if (!KingInCheck)
		{
			if (CR cr = cr_rights)
			{
				Bitboard occ = pos.Pieces();
				if (cr & CR_KS)
				{
					if (!(between_bb(ksq, ksq + 3) & occ))
						if (pos.attacksTo(ksq + 2, ~c) == 0 && pos.attacksTo(ksq + 1, ~c) == 0)
							cnt++;
				}
				if (cr & CR_QS)
				{
					if (!(between_bb(ksq, ksq - 4) & occ))
						if (pos.attacksTo(ksq - 2, ~c) == 0 && pos.attacksTo(ksq - 1, ~c) == 0)
							cnt++;
				}
			}
		}
		// Piece moves
		for (int i = 0; i < n_att; ++i )
			cnt += PopCnt(attack[i] & target);

		return cnt;
	}

	uint64 Position::perft(int depth)
	{
		uint64 cnt = 0;

		if (depth == 0)
			return 1;
		MoveGen gen(*this, false);
#if 0
		// Bulk counting
		if (depth == 1)
		{
			return gen.bulk_count(sideToMove);
		}
#endif
		int d = depth - 1;
#if 1
		MoveX* beg;
		MoveX* end;
		gen.genAll(beg, end);
#if 0
		if (d == 0)
			return end - beg;
#endif
		while (beg < end)
		{
			Move m = *beg++;
#if 0
			if (d == 0)
			{
#if 0
				if (legal_move(m)) cnt++;
#else
				cnt++;
#endif
			}
			else
#endif
#if 0
				if (doMove(m) == MOVE_PLAYED)
				{
					cnt += perft(d);
					undoMove(m);
				}
#else
			{
				doLegalMove(m);
				cnt += perft(d);
				undoMove(m);
			}
#endif
		}
#else
		gen.initGen();
		while (Move* m = gen.nextMove())
		{
#if 0
			if (d == 0)
			{
				if (legal_move(m)) cnt++;
			}
			else
#endif
				if (doMove(m) == MOVE_PLAYED)
				{
					cnt += perft(d);
					undoMove(m);
				}
		}
#endif
		return cnt;
	}
#if 0
	uint64 Position::perft(int depth)
	{
		uint64 cnt = 0;

		if (depth == 0)
			return 1;
		MoveGen gen(*this, false);
#if 0
		// Bulk counting
		if (depth == 1)
		{
			return gen.bulk_count(sideToMove);
		}
#endif
		int d = depth - 1;
#if 1
		MoveX* beg;
		MoveX* end;
		gen.genAll(beg, end);
#if 0
		if (d == 0)
			return end - beg;
#endif
		while (beg < end)
		{
			Move m = *beg++;
#if 0
			if (d == 0)
			{
#if 0
				if (legal_move(m)) cnt++;
#else
				cnt++;
#endif
			}
			else
#endif
#if 0
				if (doMove(m) == MOVE_PLAYED)
				{
					cnt += perft(d);
					undoMove(m);
				}
#else
			{
				doLegalMove(m);
				cnt += perft(d);
				undoMove(m);
			}
#endif
		}
#else
		gen.initGen();
		while (Move* m = gen.nextMove())
		{
#if 0
			if (d == 0)
			{
				if (legal_move(m)) cnt++;
			}
			else
#endif
				if (doMove(m) == MOVE_PLAYED)
				{
					cnt += perft(d);
					undoMove(m);
				}
		}
#endif
		return cnt;
	}
#endif
	// std::ostream& fmtMove(std::ostream& os, Move16 m) { return os << m; }
	std::ostream& Position::divide(std::ostream& os, int depth)
	{
		if (depth <= 0)
			return os << 1 << std::endl;
		clock_t t = clock();
		int d = depth - 1;
		int64_t cnt = 0;
		MoveGen gen(*this);

		while (Move* m = gen.nextMove())
		{
			if (doMove(*m) == MOVE_PLAYED)
			{
				uint64 c = perft(d);
				cnt += c;
				os << *m << ": " << c << std::endl;
				undoMove(*m);
			}
		}
		t = clock() - t;
		double sec = 1.0*t / CLOCKS_PER_SEC;
		return os << cnt << "(" << sec << ")" << std::endl;
	}

	std::ostream& Position::search(std::ostream& os, int depth)
	{
		return os;
	}
}