
#include <iostream>
#include <string>
#include "NpEngine.h"
#include "pch.h"
#include "MoveGen.h"

namespace NNC {
	// Assuming the move is legal a King capture is always winning.
	// Thus Kings receive value 0 for this calculation.
	const Value val[8] = { 0,1,1,3,3,5,9,0 };
#if 0
	//  Adapted from code by Joost Buijs http://talkchess.com/forum3/viewtopic.php?f=7&t=72285&start=10
	Value Position::SeeFull2(Bitboard occ, Square to, Value at_stake, Value captured)
	{
		Color side = sideToMove;
		Value exchange[32];
#if 0
		int at_stake = piece_value(Type(pieces[move->from]));
		int captured = piece_value(Type(pieces[move->to]));
		occ ^= Bit(move->from); // remove agressor

		if (move->type & mtEnpc)
		{
			int csq = move->to ^ 8;
			captured = piece_value(Pawn);
			occ ^= Bit(csq); // remove victim
		}

		if (move->type & mtProm)
		{
			captured += piece_value(move->info) - 2 * piece_value(Pawn);
			at_stake = piece_value(move->info);
		}

		if (!(state.attacked & Bit(move->to))) return captured; // early exit
#endif
		int n = 0;

		exchange[n++] = captured;

		while (true)
		{
			// swap sides
			side = ~side;

			// get the least valued attacker
			Bitboard att = occ & PawnAttacksTo(to, side);
			if (att)
			{
				exchange[n] = at_stake - exchange[n - 1]; n++;
				at_stake = val[Pawn];
				occ ^= blsi(att);
				continue;
			}

			att = Knights(side) & occ & knightAttack(to);
			if (att)
			{
				exchange[n] = at_stake - exchange[n - 1]; n++;
				at_stake = val[Knight];
				occ ^= blsi(att);
				continue;
			}

			Bitboard batt = bishopAttack(to, occ);
			att = Bishops(side) & occ & batt;
			if (att)
			{
				exchange[n] = at_stake - exchange[n - 1]; n++;
				at_stake = val[Bishop];
				occ ^= blsi(att);
				continue;
			}

			Bitboard ratt = rookAttack(to, occ);
			att = Rooks(side) & occ & ratt;
			if (att)
			{
				exchange[n] = at_stake - exchange[n - 1]; n++;
				at_stake = val[Rook];
				occ ^= blsi(att);
				continue;
			}

			att = Queens(side) & occ & (batt | ratt);
			if (att)
			{
				exchange[n] = at_stake - exchange[n - 1]; n++;
				at_stake = val[Queen];
				occ ^= blsi(att);
				continue;
			}

			att = Kings(side) & occ & kingAttack(to);
			if (att)
			{
				exchange[n] = at_stake - exchange[n - 1]; n++;
				at_stake = 1000; // Infinite val[King];
				occ ^= blsi(att);
				continue;
			}

			break;
		}

		// negamax
		while (--n) if (exchange[n] > -exchange[n - 1]) exchange[n - 1] = -exchange[n];

		return exchange[0];
	}

	Value Position::SeeFull(Bitboard occ, Square to, Value at_stake, Value captured)
	{
		Color side = sideToMove;
		Value exchange[32];
		enum Stage {
			P,	// Pawns may attack to-square
			N,	// Knights, but not pawns, may attack to-square
			S	// Slider, but not Pawns or Knights, may attack to-square
		} stage[2] = { P, P };

		int n = 0;

		exchange[n++] = captured;

		bool bcomputed = false;
		Bitboard att, batt;
		// Pawns and Knight captures may affect bishop attacks, 
		// but never affect rook attacks
		Bitboard ratt = rookAttack(to, occ);

		while (true)
		{
			// swap sides
			side = ~side;
			exchange[n] = at_stake - exchange[n - 1]; n++;
			switch (stage[side])
			{
			case P:
				// get the least valued attacker
				att = occ & PawnAttacksTo(to, side);
				if (att)
				{
					at_stake = val[Pawn];
					occ ^= blsi(att);
					if (bcomputed)
						batt = bishopAttack(to, occ);
					continue;
				}
				stage[side] = N;	// No more pawns for this side
			case N:
				att = Knights(side) & occ & knightAttack(to);
				if (att)
				{
					at_stake = val[Knight];
					occ ^= blsi(att);
					continue;
				}
				stage[side] = S;	// No more knights for this side

				if (!bcomputed)
				{
					batt = bishopAttack(to, occ);
					bcomputed = true;
				}

			default: // case S:
				att = Bishops(side) & occ & batt;
				if (att)
				{
					at_stake = val[Bishop];
					occ ^= blsi(att);
					batt = bishopAttack(to, occ);
					continue;
				}
				att = Rooks(side) & occ & ratt;
				if (att)
				{
					at_stake = val[Rook];
					occ ^= blsi(att);
					ratt = rookAttack(to, occ);
					continue;
				}
				Bitboard Q = Queens(side) & occ;
				att = Q & ratt;
				if (att)
				{
					at_stake = val[Queen];
					occ ^= blsi(att);
					ratt = rookAttack(to, occ);
					continue;
				}
				att = Q & batt;
				if (att)
				{
					at_stake = val[Queen];
					occ ^= blsi(att);
					batt = bishopAttack(to, occ);
					continue;
				}
				att = Kings(side) & occ & kingAttack(to);
				if (att)
				{
					at_stake = 1000; // Infinite -- val[King];
					occ ^= blsi(att);
					continue;
				}
				--n; // No capture -- remove last exchange from list
				break;
			}
		}

		// negamax
		while (--n) if (exchange[n] > -exchange[n - 1]) exchange[n - 1] = -exchange[n];

		return exchange[0];
	}

	// Sign of Static Exchange Evaluation
	// return See < 0 -- ie losing captures return true, See >= 0 returns false
	// Bishops and knights considered equal
	bool Position::SeeSign_old(Bitboard occ, Square to, Value at_stake, Value captured)
	{
		Color side = sideToMove;
		Value exchange[32];
		enum Stage {
			P,	// Pawns may attack to-square
			N,	// Knights, but not pawns, may attack to-square
			S	// Slider, but not Pawns or Knights, may attack to-square
		} stage[2] = { P, P };

		// HxL where target defended by pawn is a loss.
		if (occ & PawnAttacksTo(to, ~side))
			return true;
		stage[~side] = N;
		// Pawn attackers, no pawn defenders, not that far behind => at least even
		if (at_stake - captured <= 2)
		{
			if (occ & PawnAttacksTo(to, side))
				return false;
			stage[side] = N;
		}

		int n = 0;

		exchange[n++] = captured;

		bool bcomputed = false;
		Bitboard att, batt;
		// Pawns and Knight captures may affect bishop attacks, 
		// but never affect rook attacks
		Bitboard ratt = rookAttack(to, occ);

		while (true)
		{
			// swap sides
			side = ~side;
			exchange[n] = at_stake - exchange[n - 1]; n++;
			switch (stage[side])
			{
			case P:
				// get the least valued attacker
				att = occ & PawnAttacksTo(to, side);
				if (att)
				{
					at_stake = val[Pawn];
					occ ^= blsi(att);
					if (bcomputed)
						batt = bishopAttack(to, occ);
					continue;
				}
				stage[side] = N;	// No more pawns for this side
			case N:
				att = Knights(side) & occ & knightAttack(to);
				if (att)
				{
					at_stake = val[Knight];
					occ ^= blsi(att);
					continue;
				}
				stage[side] = S;	// No more knights for this side

				if (!bcomputed)
				{
					batt = bishopAttack(to, occ);
					bcomputed = true;
				}

			default: // case S:
				att = Bishops(side) & occ & batt;
				if (att)
				{
					at_stake = val[Bishop];
					occ ^= blsi(att);
					batt = bishopAttack(to, occ);
					continue;
				}
				att = Rooks(side) & occ & ratt;
				if (att)
				{
					at_stake = val[Rook];
					occ ^= blsi(att);
					ratt = rookAttack(to, occ);
					continue;
				}
				Bitboard Q = Queens(side) & occ;
				att = Q & ratt;
				if (att)
				{
					at_stake = val[Queen];
					occ ^= blsi(att);
					ratt = rookAttack(to, occ);
					continue;
				}
				att = Q & batt;
				if (att)
				{
					at_stake = val[Queen];
					occ ^= blsi(att);
					batt = bishopAttack(to, occ);
					continue;
				}
				att = Kings(side) & occ & kingAttack(to);
				if (att)
				{
					at_stake = 1000; // Infinite -- val[King];
					occ ^= blsi(att);
					continue;
				}
				--n; // No capture -- remove last exchange from list
				break;
			}
		}

		// negamax
		while (--n) if (exchange[n] > -exchange[n - 1]) exchange[n - 1] = -exchange[n];

		return exchange[0] < 0;
	}

#endif


	/// <summary>
	/// Sign of Static Exchange Evaluation
	/// return true if See < 0 -- ie is losing capture return false if See >= 0
	/// Bishops and knights considered equal
	/// </summary>
	/// <param name="occ"></param>
	/// <param name="to"></param>
	/// <param name="at_stake"></param>
	/// <param name="captured"></param>
	/// <returns></returns>
	bool MoveGen::SeeSign(Bitboard occ, Square to, Value at_stake, Value captured)
	{
		Assert(at_stake > captured);
		// Is captured piece defended by enemy pawns?
		// HxL where target defended by pawn is a loss.
		if (member(PE, to))
			return true;

		Color side = clr;
		Value exchange[32];
		enum Stage {
			P,	// Pawns may attack to-square
			N,	// Knights, but not pawns, may attack to-square
			S	// Slider, but not Pawns or Knights, may attack to-square
		} stage[2] = { P, N };

		// enemy has no pawn defenders

		// If our side has a Pawn attackers, enemy has no pawn defenders 
		// and at_stake <= captured+2 (consequently capture is Rx{B,N})
		// then exchange is at least even. Eg. RxB, NxR, PxN, ANYxP, stand pat
		if (at_stake <= captured + 2)
		{
			if (member(PA|PH,to))
				return false;
			stage[side] = N;
		}

		int n = 0;

		exchange[n++] = captured;

		bool bcomputed = false;
		Bitboard att, batt;
		// Pawns captures may affect bishop attacks, 
		// but not rook attacks
		Bitboard ratt = rookAttack(to, occ);

		while (true)
		{
			// swap sides
			side = ~side;
			exchange[n] = at_stake - exchange[n - 1]; n++;
			switch (stage[side])
			{
			case P:
				// side is necessarily our side and at_stake > capture + 2
				// so RxP or QxL. Get a Pawn attacker
				if (member(PA | PH, to))
				{
					att = occ & (PA | PH);
					if (att)
					{
						at_stake = val[Pawn];
						occ ^= blsi(att);
						if (bcomputed)
							batt = bishopAttack(to, occ);
						continue;
					}
				}
				stage[side] = N;	// No more pawns for this side
			case N:
				att = pos.Knights(side) & occ & knightAttack(to);
				if (att)
				{
					at_stake = val[Knight];
					occ ^= blsi(att);
					continue;
				}
				stage[side] = S;	// No more knights for this side

				if (!bcomputed)
				{
					batt = bishopAttack(to, occ);
					bcomputed = true;
				}

			default: // case S:
				Assert(bcomputed);
				att = pos.Bishops(side) & occ & batt;
				if (att)
				{
					at_stake = val[Bishop];
					occ ^= blsi(att);
					batt = bishopAttack(to, occ);
					continue;
				}
				att = pos.Rooks(side) & occ & ratt;
				if (att)
				{
					at_stake = val[Rook];
					occ ^= blsi(att);
					ratt = rookAttack(to, occ);
					continue;
				}
				Bitboard Q = pos.Queens(side) & occ;
				att = Q & ratt;
				if (att)
				{
					at_stake = val[Queen];
					occ ^= blsi(att);
					ratt = rookAttack(to, occ);
					continue;
				}
				att = Q & batt;
				if (att)
				{
					at_stake = val[Queen];
					occ ^= blsi(att);
					batt = bishopAttack(to, occ);
					continue;
				}
				att = pos.Kings(side) & occ & kingAttack(to);
				if (att)
				{
					at_stake = 1000; // Infinite -- val[King];
					occ ^= blsi(att);
					continue;
				}
				--n; // No capture -- remove last exchange from list
				break;
			}
		}

		// negamax
		while (--n) if (exchange[n] > -exchange[n - 1]) exchange[n - 1] = -exchange[n];

		return exchange[0] < 0;
	}
#if 0
	bool MoveGen::See(Move m)
	{
		Square f = MoveFromSq(m), t = MoveToSq(m);
		Bitboard occ = Pieces() ^ f;

		if (SquareIsSpecial(t))	// special move encoding
		{
			switch (t)
			{
			case ADV2: { return SeeFull(occ, advance24(f), val[Pawn], val[NoPiece]); }
			case EP: { t = epTo(); occ ^= advanceEP(t); return SeeFull(occ, t, val[Pawn], val[Pawn]); }
			case CASTLE_KS:	case CASTLE_QS: return true;
			default: // Promotions
				Value va = val[PromoPiece(t)];
				t = PromoToSq(f, t);	// decode actual to square
				// value of promotion is less than value of promoted piece type.
				Value vb = val[board(t)] + va - 2 * val[Pawn];
				return SeeFull(occ, t, va, vb);
			}
		}
		return SeeFull(occ, t, val[board(f)], val[board(t)]);
	}
#endif
}