#include <iostream>
#include <string>
#include "NpEngine.h"
#include "pch.h"
#include <time.h>
#include "MoveGen.h"

namespace NNC {

	void Position::ScoreRootMoves()
	{
		// Score the moves by doing a q-search on each
		Score alpha = -Eval::MateScore;
		Score beta =   Eval::MateScore;
		for (MoveX* m = root_moves; m < root_end; m++)
		{
			doLegalMove(*m);
			Score s = quiesce(alpha, beta);
			undoMove(*m);
			*m = MoveSetScore(*m, s);
		}
		moveSort(root_moves, root_end);
	}

	Move Position::RootSearch(int maxdepth)
	{
		//  Initialize statistical counters and such.
		
		//  Check for EGTB result

		//  Generate the root move list. 

		MoveGen gen(*this, false);
		gen.genAll(root_moves, root_end);
		int nmoves = root_end - root_moves;
		// Test for check/stale mate -- shouldn't happen with UCI?
		if (nmoves == 0)
			return MOVE_NONE;
		// Only 1 legal move -- immediately return when not pondering
		if (nmoves == 1)
			return root_moves[0];
		
		//  Score/Sort the root move list. 
		ScoreRootMoves();

		// Set the initial search bounds based on the last search  
		// or default values.               
		Score alpha = -Eval::MateScore;
		Score beta = Eval::MateScore;

		// Enter the iterative deepening loop

		for (int iteration = 2; iteration <= maxdepth; iteration++)
		{
			Score s = Search(iteration, alpha, beta, 1, true);
		}
		// Now install the old PV into the hash table so that      
		// these moves will be followed first.  We do this since   
		// the last iteration done could have overwritten the PV   
		// as the last few root moves were searched.               
		return *beg;
	}
	//--------------------------------------------------------------------------
	//! \brief Execute search on current position to find best move
	//! \param[in] depth Maximum number of half-moves (plies) to search
	//! \param[in] movestogo Number of moves remaining until next time control
	//! \param[in] movetime Maximum milliseconds to spend on this move
	//! \param[in] wtime Milliseconds remaining on white's clock
	//! \param[in] winc White increment per move in milliseconds
	//! \param[in] btime Milliseconds remaining on black's clock
	//! \param[in] binc Black increment per move in milliseconds
	//! \param[out] ponder If not NULL set to the move engine should ponder next
	//! \return Best move in coordinate notation (e.g. "e2e4", "g8f6", "e7f8q")
	Move Position::Go(const int depth,
		const int movestogo,
		const uint64_t movetime,
		const uint64_t wtime, const uint64_t winc,
		const uint64_t btime, const uint64_t binc,
		Move* ponder)
	{
		*ponder = MOVE_NONE;
		return MOVE_NONE;
	}

	static int counterfeit = 0;	// # TT hits falsified by move verification
	const Score deltamargin = 20;

	Score Position::Evaluate() { return Score(0); }

	inline
		Move Position::makeNextMove(MoveGen& gen)
	{
		Move m = gen.nextMove();
		while (m && !doMove(m))
			m = gen.nextMove();
		return m;
	}
	inline
		Move Position::makeNextGoodCapture(MoveGen& gen, Score delta)
	{
		Move m = gen.nextGoodCapture(delta);
		if (doMove(m))
			return m;
		do { m = gen.nextGoodCapture(delta); } while (!doMove(m));
		return m;
	}

	Score Position::quiesce(Score alpha, Score beta)
	{
		// ** TO DO ** refactor InCheck processing
		if (InCheck())
			return pvs(1, alpha, beta, 0, false);
		Score basevalue = Evaluate();
		if (basevalue > alpha)
		{
			if (basevalue >= beta)
				return basevalue;
			alpha = basevalue;
		}
		// Futility pruning -- aka "delta" pruning.
		// https://www.chessprogramming.org/Delta_Pruning
		int delta = alpha - basevalue - deltamargin;
		MoveGen gen(*this);
		while (Move m = makeNextGoodCapture(gen, delta))
		{
			Score value = quiesce(alpha, beta);
			if (value > alpha)
			{
				if (value >= beta)
					return value;
				alpha = value;
			}
			undoMove(m);
		}
		return alpha;
	}


#define TT_HIT		1				// usable score
#define TT_MISS		0				// must search
	constexpr auto DO_NULL = true;

	__forceinline
		int classifyHit(TTE tte, int depth, bool& do_null, int null_depth, Score alpha, Score beta, Score& val)
	{
		Score s = val = tte.score();
		if (Bound bnd = tte.bound())
		{
			if (tte.depth() >= depth)
			{
				if (bnd & BOUND_LOWER) {
					if ((beta < s) || (bnd & BOUND_UPPER)) return TT_HIT; 	// Exact or fail hi
				}
				else if (s <= alpha) return TT_HIT;							// Upper bound / fail lo
			}
			else if (tte.depth() >= null_depth)
				if ((bnd & BOUND_UPPER) && (s < beta))
					return do_null = false, TT_MISS;							// skip null move and search
		}
		else /* a null-bound */ if (do_null && tte.depth() >= null_depth && s >= beta)
			// Previous null search failed high with score above beta
			return TT_HIT;	// fail hi

		return TT_MISS;
	}



	inline
		Score Position::BetaCutoffExit(Score value, Move m)
	{
		return value;
	}

	inline
		Score Position::NoMoveExit()
	{
		// KingInCheck ? MateScore : DrawScore;
		return Eval::MateScore;
	}
	void Position::improvedAlpha(Move m, int n, GenStage stg)
	{
		return;
	}

	Score Position::pvs(int depth, Score alpha, Score beta, int ply, bool do_null)
	{
		Assert(depth > 0);	// Call 'PVS' for depth/quiesce dispatch
		Move hash_move = MOVE_NONE;
		// Determine if we have searched enough nodes
		// to check how much time has been used,
		// or look for keyboard input.
		if (TimeCheck())
			return Score(0);

		// Check for draw by repetition and 50 move rule -- skip this test at root position
		if (ply != 1 && repetition())
			return Eval::DrawScore;

		/*
		 ************************************************************
		 *                                                          *
		 *  Check TT
		 *
		 *  If an entry is retrieved from TT verify the move.
		 *  Move verification gives 6-7 more bits of key assurance.

		 *  classifyHit checks whether data in an entry suffices
		 *  to determine the search return value.
		 *
		 *  If classifyHit returns true (TT_HIT) then value stored
		 *  in TT entry is returned as the value of the node.
		 *  Failing that the entry may establish that a null-search
		 *  will fail lo, in which case do_null is set to false.
		 *                                                          *
		 ************************************************************
		 */
		Score value;
		int null_depth = depth;
		TTE* replacement;				// Address to store updated record
		TTE  tte = TT.probe(hash(), replacement);
		if (tte)
		{	// cache hit
			if (Move16 m = tte.move())
			{
				if (Move ttm = verify_ttm(m))
					hash_move = ttm;	// verified hit -- a PseudoLegal hash move
				else { counterfeit++; goto no_tte; }
			}
			else
				/*	No hash move, but non-conterfeit entry.
					It is either fail lo with no move and no previous move
					or previous null move cutoff. */
				;
			if (classifyHit(tte, depth, do_null, null_depth, alpha, beta, value))
				return value;
		}
	no_tte:

		/*
		 ************************************************************
		 *                                                          *
		 *  EGTB.  Try a probe into the endgame tablebase files.	*
		 *  Null Move.
		 *                                                          *
		 ************************************************************
		*/

		// *********************************************************
		// Pvs search
		int move_count = 0;
		Move best_move = MOVE_NONE;
		Score orig_alpha = alpha;

		// ---------------------------------------------------------
		// First move -- search with open window
		// try the hash move
		if (hash_move && doMove(hash_move))
		{
			move_count++;
			value = -Search(depth - 1, -beta, -alpha, ply + 1, DO_NULL);
			undoMove(hash_move);
			if (value >= beta)
				return BetaCutoffExit(value, hash_move);
		}
#if defined(USE_IID)
		else // Internal Iterative Deepening
		{
		}
#endif
		// no hash move cutoff -- generate moves
		MoveGen gen(*this, hash_move, killer1(), killer2());
		if (move_count == 0)
		{
			if (Move m = makeNextMove(gen))
			{
				move_count++;
				value = -Search(depth - 1, -beta, -alpha, ply + 1, DO_NULL);
				undoMove(m);
				if (value >= beta)
					return BetaCutoffExit(value, m);
				if (value > alpha)
				{
					improvedAlpha(m, move_count, gen.Stage());
					best_move = m;
					alpha = value;
				}
			}
			else // No legal move -- this is checkmate or stalemate
				return NoMoveExit();
		}

		// ---------------------------------------------------------
		// Zero Window search
		// moves after the first one are searched with Zero Window

		while (Move m = makeNextMove(gen))
		{
			move_count++;
			value = -Search(depth - 1, -alpha - 1, -alpha, ply + 1, DO_NULL);
			if (value > alpha && value < beta)
			{
				// Zero-window fail hi -- repeat search with open window
				value = -Search(depth - 1, -beta, -alpha, ply + 1, DO_NULL);
			}
			undoMove(m);
			if (value >= beta)
				return BetaCutoffExit(value, m);
			if (value > alpha)
			{
				improvedAlpha(m, move_count, gen.Stage());
				best_move = m;
				alpha = value;
			}
#if defined(USE_LMR)
			if (gen.Stage() >= LATE_MOVE)
				break;
#endif
		}
#if defined(USE_LMR)
#endif


		/*
		 ************************************************************
		 *                                                          *
		 *  Step 7c.  Now it's time to try to reduce the search     *
		 *  depth if the move appears to be "poor".  To reduce the  *
		 *  search, the following requirements must be met:         *
		 *                                                          *
		 *  (1) We must be in the HISTORY_MOVES (or later) part of  *
		 *      the move ordering, so that we have nearly given up  *
		 *      on failing high on any move.                        *
		 *                                                          *
		 *  (2) We must not be too close to the horizon (this is    *
		 *      the LMR_remaining_depth value).                     *
		 *                                                          *
		 *  (3) The current move must not be a checking move and    *
		 *      the side to move can not be in check.               *
		 *                                                          *
		 *  If we meet those criteria, then we reduce the search.   *
		 *  The reduction is variable and is done via a table look- *
		 *  up that uses a function based on remaining depth (more  *
		 *  depth remaining, the larger the reduction) and the      *
		 *  number of moves searched (more moves searched, the      *
		 *  larger the reduction).  The "shape" of this reduction   *
		 *  formula is user-settable via the "lmr" command.         *
		 *                                                          *
		 ************************************************************
		 */

		 /*
		  ************************************************************
		  *                                                          *
		  *  Step 7d.  We have determined whether the depth is to    *
		  *  be changed by an extension or a reduction.  If we get   *
		  *  to this point, then the move is not being pruned.  So   *
		  *  off we go to a recursive search/quiescence call to work *
		  *  our way toward a terminal node.                         *
		  *                                                          *
		  *  There is one special-case to handle.  If the depth was  *
		  *  reduced, and Search() returns a value >= beta then      *
		  *  accepting that is risky (we reduced the move as we      *
		  *  thought it was bad and expected it to fail low) so we   *
		  *  repeat the search using the original (non-reduced)      *
		  *  depth to see if the fail-high happens again.            *
		  *                                                          *
		  ************************************************************
		  */


		  /*
		   ************************************************************
		   *                                                          *
		   *  Step 7d.  We have determined whether the depth is to    *
		   *  be changed by an extension or a reduction.  If we get   *
		   *  to this point, then the move is not being pruned.  So   *
		   *  off we go to a recursive search/quiescence call to work *
		   *  our way toward a terminal node.                         *
		   *                                                          *
		   *  There is one special-case to handle.  If the depth was  *
		   *  reduced, and Search() returns a value >= beta then      *
		   *  accepting that is risky (we reduced the move as we      *
		   *  thought it was bad and expected it to fail low) so we   *
		   *  repeat the search using the original (non-reduced)      *
		   *  depth to see if the fail-high happens again.            *
		   *                                                          *
		   ************************************************************
		   */

		   /*
			************************************************************
			*                                                          *
			*  Step 7e.  This is the PVS re-search code.  If we reach  *
			*  this point and value > alpha and value < beta, then     *
			*  this can not be a null-window search.  We have to re-   *
			*  search the position with the original beta value (not   *
			*  alpha+1 as is the usual case in PVS) to see if it still *
			*  fails high before we treat this as a real fail-high and *
			*  back up the value to the previous ply.                  *
			*                                                          *
			*  Special case:  ply == 1.                                *
			*                                                          *
			*  In this case, we need to clean up and then move the     *
			*  best move to the top of the root move list, and return  *
			*  back to Iterate() to let it produce the usual informa-  *
			*  tive output and re-start the search with a new beta     *
			*  value.  We also reset the failhi_delta back to 16,      *
			*  since an earlier fail-high or fail low in this          *
			*  iteration could have left it at a large value.          *
			*                                                          *
			*  Last step is to build a usable PV in case this move     *
			*  fails low on the re-search, because we do want to play  *
			*  this move no matter what happens.                       *
			*                                                          *
			************************************************************
			*/


			/*
			 ************************************************************
			 *                                                          *
			 *  Step 7f.  We have completed the search/re-search and we *
			 *  we have the final score.  Now we need to check for a    *
			 *  fail-high which terminates this search instantly since  *
			 *  no further searching is required.  On a fail high, we   *
			 *  need to update the killer moves, and hash table before  *
			 *  we return.                                              *
			 *                                                          *
			 *  If ply == 1, we call Output() which will dump the new   *
			 *  PV.  But but we need to back up the PV to ply=0 so that *
			 *  it will be available to tell main() which move to make. *
			 *                                                          *
			 ************************************************************
			 */



			 /*
			  ************************************************************
			  *                                                          *
			  *  Step 8.  All moves have been searched.  If none were    *
			  *  legal, return either MATE or DRAW depending on whether  *
			  *  the side to move is in check or not.                    *
			  *                                                          *
			  ************************************************************
			  */

		return Score(0);
	}


}