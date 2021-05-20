// NpEngine.cpp : Defines the exported functions for the DLL application.
//
#include "NpEngine.h"
#include "pch.h"



// Quit Search for "pragmatic" reasons -- too much time or exceeding resources.
#if 0
void Tree::QuitSearch(int nply)
{
	// Too deep
	if ( nply > MaxPly )
	{
		AbortSearch("Exceeded MAXPLY");
	}

	//  Check to see if we have searched enough nodes  
	//  that it is time to peek at how much time has been used, 
	//  or if is time to check for operator keyboard input.     
	//  This is usually enough nodes to force a time/input      
	//  check about once per second, except when the target     
	//  time per move is very small, in which case we try to    
	//  check the time more frequently.                         
	//                                                          
	//  Note that we check input or time-out in thread 0.  This 
	//  makes the code simpler and eliminates some problematic  
	//  race conditions.                                        
	AbortSearchOnTime();
}

bool Tree::TerminalPosition( Score& score, Color wtm )
{
	    if (depth > 6 && TotalAllPieces <= EGTB_use && !Castle(ply, white) &&
        !Castle(ply, black) && (CaptureOrPromote(tree->curmv[ply - 1]) ||
            ply < 3)) {
      int egtb_value;

      tree->egtb_probes++;
      if (EGTBProbe(tree, ply, wtm, &egtb_value)) {
        tree->egtb_probes_successful++;
        alpha = egtb_value;
        if (MateScore(alpha))
          alpha += (alpha > 0) ? -ply + 1 : ply;
        else if (alpha == 0) {
          alpha = DrawScore(wtm);
          if (Material > 0)
            alpha += (wtm) ? 1 : -1;
          else if (Material < 0)
            alpha -= (wtm) ? 1 : -1;
        }
        if (alpha < beta)
          SavePV(tree, ply, 2);
        return alpha;
      }
    }
}


inline bool Tree::NullMove(Score& value, Score alpha, Score beta, int nply, int depth, Color wtm, int do_null, int in_check, int verify )
{
	const int R = 3; /* the depth reduction factor */


	/* if verify = true, and depth = 1, null-move search is not conducted, since
	* verification will not be possible */
	if (!in check() && (!verify || depth > 1)) 
	{
		make_null_move();
		/* null-move search with minimal window around beta */
		value = -Search(-beta, -beta + 1, depth - R - 1, verify);
		if (value >= beta) /* fail-high? */
		{
			if (verify) {
			depth--; /* reduce the depth by one ply */
			/* turn verification off for the sub-tree */
			verify = false;
			/* mark a fail-high flag, to detect zugzwangs later*/
			fail_high = true;
			}
			else /* cutoff in a sub-tree with fail-high report */
			return value;
		}
	}
	re_search: /* if a zugzwang is detected, return here for re-search */
	/* do regular NegaScout/PVS search */
	/* search() is called with current value of “verify” */
	. . .
	/* if there is a fail-high report, but no cutoff was found, the position
	* is a zugzwang and has to be re-searched with the original depth */
	if(fail high && best < beta) {
	depth++;
	fail high = false;
	verify = true;
	goto re search;
	}
}
// Search the current  
Score Tree::Search(Score alpha, Score beta, int nply, int depth, Color wtm, int do_null, int in_check, int verify ) 
{
	Assert(depth > 0 );
	// Quit Search if too much time or exceeding resources (too deep).
	QuitSearch(nply);

	//  Check for draw by repetition or 50 move draws.  
	if( DrawByRepetition(nply) )
		return DRAW_SCORE;

	// Lookup position in transposition table
	HashRecord hrec = 0;
	switch (HashProbe(alpha, beta, nply, depth, wtm, hrec)) 
	{
		case HASH_HIT:
			return hrec.score;
		case AVOID_NULL_MOVE:
			do_null = 0;
	}

	// position in endgame database
	Score tscore;
	if( TerminalPosition(tscore,wtm) )
		return tscore;

	// Verified Null move pruning
	if( do_null && NullMove(tscore,verify) )
		return tscore;



	// using fail soft with negamax: 
	bestscore = -PVS(-beta,-alpha,depth-1); 
	if( bestscore > alpha ) { 
	if( bestscore >= beta ) 
		return bestscore; 
	alpha = bestscore; 
	} 

	for( all remaining moves ) { 
	score = -PVS(-alfa-1,-alfa,depthleft-1); 
	if( score > alfa && score < beta ) { 
		// research with window [alfa;beta] 
		score = -PVS(-beta,-alfa,depthleft-1); 
		if( score > alfa ) 
		alfa = score; 
	} 

	if( score > bestscore ) { 
		if( score >= beta ) 
		return score; 
		bestscore = score; 
	} 
	return bestscore;
	}


}

#if COMMENT
Call from root: 
    rootscore = PVS(-infinite,infinite,depthleft); 


int PVS(alfa,beta,depthleft) { 
  if( depthleft <= 0 ) return qsearch(alfa,beta); 

  // using fail soft with negamax: 
  bestscore = -PVS(-beta,-alfa,depthleft-1); 
  if( bestscore > alfa ) { 
    if( bestscore >= beta ) 
      return bestscore; 
    alfa = bestscore; 
  } 

  for( all remaining moves ) { 
    score = -PVS(-alfa-1,-alfa,depthleft-1); 
    if( score > alfa && score < beta ) { 
      // research with window [alfa;beta] 
      score = -PVS(-beta,-alfa,depthleft-1); 
      if( score > alfa ) 
        alfa = score; 
    } 

    if( score > bestscore ) { 
      if( score >= beta ) 
        return score; 
      bestscore = score; 
    } 
  } 
  return bestscore; 
}

http://www.stmintz.com/ccc/index.php?id=271055

Hi Uri,

OK you have pawnBB[2] with white pawns pawnBB[WHITE] and black pawns
pawnBB[BLACK]. "allPawns" combines both sets by or.

Some pawn-patterns on the fly (I'm at work and can't look to my sources):

pawnAttacksAH[WHITE]  = (pawnBB[WHITE] << 9) & 0xfefefefefefefefe;
pawnAttacksHA[WHITE]  = (pawnBB[WHITE] << 7) & 0x7f7f7f7f7f7f7f7f;
pawnAttacks[WHITE]    = pawnAttacksAH[WHITE] | pawnAttacksHA[WHITE];
pawnDblAttacks[WHITE] = pawnAttacksAH[WHITE] & pawnAttacksHA[WHITE];

hebel[WHITE]          = pawnAttacks[WHITE] & pawnBB[BLACK];
widder[WHITE]         = (pawnBB[WHITE] << 8) & pawnBB[BLACK]
                        /* & ~hebel[WHITE] */;
defended[WHITE]       = pawnAttacks[WHITE] & pawnBB[WHITE];
duo[WHITE]            = ((pawnBB[WHITE]<<8)& pawnAttacks[WHITE]
                                           & ~pawnDblAttacks[WHITE]) >> 8;

Some helpfull routines:

BitBoard fillUp(BitBoard bb)
{
	bb |= (bb<<8);
        bb |= (bb<<16);
        bb |= (bb<<32);
	return bb;
}

BitBoard fillDown(BitBoard bb)
{
	bb |= (bb>>8);
        bb |= (bb>>16);
        bb |= (bb>>32);
	return bb;
}

openPawns[WHITE] = pawnBB[WHITE] & ~filldown( fillup(pawnBB[WHITE]) & allPawns);
stop = allPawns | pawnAttacks[BLACK];
passedPawns[WHITE] = pawnBB[WHITE] & ~filldown( fillup(pawnBB[WHITE]) & stop)
                   & ~hebel[WHITE];
notDefendable[WHITE] = pawnBB[WHITE] & fillup (filldown(pawnAttacks[WHITE]));
....

oups... no more time, may be more tomorrow.

But the principle with bitboards is to get pattern sets of pawns with a desired
property rather than to determine the property for a single pawn.

Regards,
Gerd


Rein Halbersma wrote:

OK, if I understand it correctly: if the null-window search fails high, then using the fail-soft score as a bound in the re-search can give a fail-low if one uses fancy pruning tricks. That makes sense, but don't such search instabilities also occur if you research with alpha as a bound, or do they just occur less often? And if they do, how does one treat such search instabilities? Yet another re-search?


Your understanding is correct. 

One point to note: in the original post, the window of -beta, -score doesn't allow you to determine, for a result of "score", whether this is a true score or a bound. Regardless of pruning etc this can cause some additional issues, such as not having a PV. So even if you use that, you want -score+1. 

If you use a window of alpha, beta, and the score comes back outside alpha, you will propagate the fail high upwards and perform the research at that level. This wouldn't be the case when the result would be "score".

OK, some time was needed to process your arguments. So the fail-high returning v on the [a,a+1] search can be triggered by spurious null move prunings 2 plies from the root. Re-searching with [a,b] will remove most such false null move triggers. Then the re-search will get either a fail-low or get a new pv. Either case, all is well. 

OTOH, if you had re-searched with [v,b], the spurious null moves still would have been removed and the search might have returned a little cheaper (because of the tighter v bound). However, now you could introduce a spurious null move at 1 ply from the root (again because of the tighter v bound). Even worse, if a+1 < v < b, you can get a fail-low at the root with value w below the re-search window but above the old null window! The (theoretically) slightly quicker re-search probably doesn't outweigh the hassle of this annoying case. 

So you convinced me that the research needs to be done with [a,b]. Thanks for the help!


Oups, sorry, not so easy...

those were wrong again:

openPawns[WHITE] = pawnBB[WHITE] & ~filldown(allPawns);
passedPawns[WHITE] = pawnBB[WHITE] & ~filldown(allPawns|pawnAttacks[BLACK]);
notDefendable[WHITE] = pawnBB[WHITE] & fillup(pawnAttacks[WHITE]);

Both openPawns and  passedPawns statements produce an empty set due to
"allPawns" as filldown parameter, which is member of the filldown result.

I'll hope this is it finally ;-)

openPawns[WHITE] = pawnBB[WHITE] & ~filldown(allPawns >> 8);
passedPawns[WHITE] = openPawns[WHITE]
                   & ~filldown(pawnBB[BLACK]|pawnAttacks[BLACK]);


The notDefendable-Statement was implemented as isDefendable of course.

notDefendable[WHITE] = pawnBB[WHITE] & ~fillup(pawnAttacks[WHITE]);

Gerd


#pragma once 

#ifdef _WIN32 

#include <windows.h> 

class Event { 
  HANDLE hEvent; 
public: 
  inline Event() {}; 

  inline void init() { 
    hEvent = CreateEvent(NULL, false, false, NULL); 
  } 

  inline void destroy() { 
    CloseHandle(hEvent); 
  } 

  inline void signal() { 
    SetEvent(hEvent); 
  } 

  inline void wait_for() { 
    WaitForSingleObject(hEvent, INFINITE); 
  } 

  inline bool wait_for(int timeout) { 
    return (WaitForSingleObject(hEvent, timeout) == WAIT_TIMEOUT); 
  } 

}; 

#define ThreadProcResult DWORD WINAPI 
typedef LPVOID ThreadProcParam; 
typedef ThreadProcResult ThreadProc(ThreadProcParam); 

inline void start_thread(ThreadProc proc, ThreadProcParam param) { 
  DWORD tid; 
  CloseHandle(CreateThread(NULL, 0, proc, param, 0, &tid)); 
} 

typedef CRITICAL_SECTION Lock; 

#define lock_init(lock) InitializeCriticalSection(&(lock)) 
#define lock_acquire(lock) EnterCriticalSection(&(lock)) 
#define lock_release(lock) LeaveCriticalSection(&(lock)) 
#define lock_delete(lock) DeleteCriticalSection(&(lock)) 

t_chess_value calc_evaluation(struct t_board *board, struct t_chess_eval *eval) { 

   t_chess_value score; 
//-- Are we in the middle game, endgame or somewhere in between 
   calc_game_phase(board, eval); 

    
   //-- Return the rights score 
    score = (1 - 2 * board->to_move) * (((eval->middlegame * eval->game_phase) + (eval->endgame * (256 - eval->game_phase))) / 256); 

   //-- Store the score 
    eval->static_score = score; 
    return score; 

} 

inline void calc_game_phase(struct t_board *board, struct t_chess_eval *eval) { 
    //-- Phase of the game 
    eval->game_phase = 2 * popcount(board->piecelist[WHITEPAWN] ^ board->piecelist[BLACKPAWN]); 
    eval->game_phase += 44 * popcount(board->piecelist[WHITEQUEEN] ^ board->piecelist[BLACKQUEEN]); 
    eval->game_phase += 16 * popcount(board->piecelist[WHITEROOK] ^ board->piecelist[BLACKROOK]); 
    eval->game_phase += 12 * popcount(board->piecelist[WHITEBISHOP] ^ board->piecelist[BLACKBISHOP]); 
    eval->game_phase += 6 * popcount(board->piecelist[WHITEKNIGHT] ^ board->piecelist[BLACKKNIGHT]); 
}


I have something simpler in my projects: 
Only white pieces, no pawns 
Code:
    
fase += (pos->pceNum[wQueen]*10); 
fase += (pos->pceNum[wRook]*5); 
fase += (pos->pceNum[wBishop]*3); 
fase += (pos->pceNum[wNkingth]*3); 
score = ((fase * OPEN + (32 - fase) * ENDG) / 32);

#endif
#endif
#endif

