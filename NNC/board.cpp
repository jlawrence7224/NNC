// board.cpp : Defines the exported functions of Board class
//
#include "NpEngine.h"
#include "pch.h"
#include <sstream>
#include <iterator>
#include <vector>

namespace NNC {
	/// Determine checkers/sliding checkers and pinned pieces


	__forceinline
		void Position::doNormalMove(Color c, Hash k, Square f, Square t)
	{
		Piece  p = board(f), cap = board(t);	// Piece types on from/to squares

		if (CR cr = castling_rights())
			if (CR del = (CR_RightsLost[f] | CR_RightsLost[t]))
				k ^= update_castling(cr, del);

		if ( SetCapture(cap) )
		{
			// update hash field and prefetech
			setHash(k ^ hash_capture_piece(c, p, f, t, cap));

			capture_piece(c, p, f, t, cap);
			reversible(0);		// a capture -- reset count of reversible moves
			#if TODO
			// Update Piece Square table --
			if (cap == Pawn || p == Pawn)
			{
				// update Pawn Hash
			}
			// update Material Hash
			#endif
		}
		else
		{
			setHash(k ^ hash_move_piece(c, p, f, t));
			move_piece(c, p, f, t);
			// Update Piece Square table --
			if (p == Pawn)
			{
				reversible(0);	// pawn move -- reset count of reversible moves
#if !ADV2_SPECIAL
				// double advance beside an opposing pawn?
				if (t == advance24(f) && (beside_bb(t) & Pieces(~c, Pawn)))
					k ^= hashEP(t);	// xor EP hash into key and set the ep flag		
#endif
				// update Pawn Hash
			}
		}
		// return MOVE_PLAYED;
	}

	void Position::undoNormalMove(Color c, Square f, Square t)
	{
		Board::move_piece(c, PieceTypeOnSqr(t), t, f);
		if (Piece cap = curr_capture())
		{
			PieceOk(cap);
			Board::place_piece(~c, cap, t);
		}
	}

	__forceinline
		void Position::doSpecialMove(Color c, Hash k, Square fsq, Square to_type)
	{
		Square ksq, kto, rsq, rto, tsq;
#if ADV2_SPECIAL
		if (to_type == ADV2) // double pawn advance
		{
			tsq = advance24(fsq);
			// moved beside an opposing pawn?
			if (beside_bb(tsq) & Pieces(~c, Pawn))
				k ^= hashEP(tsq);	// xor EP hash into key and set the ep flag			

			setHash(k ^ hash_move_piece(c, Pawn, fsq, tsq));
			move_piece(c, Pawn, fsq, tsq);
			// **TO DO** update psq, pawn hash
			return;
		}
		else 
#endif
		if (to_type <= EPH) // EnPassant capture
		{
			Square epsq = fsq + SqDisp(to_type);	// The square of the pawn being removed from the board
			tsq = retreatEP(epsq);					// Square the pawn is moving to/capturing on
			setHash(k ^ hash_move_piece(c, Pawn, fsq, tsq) ^ hash_remove_piece(~c, Pawn, epsq));
			inner_remove_piece(~c, Pawn, epsq);
			move_piece(c, Pawn, fsq, tsq);

			// **TO DO** update psq, pawn hash
			return;
		}
		else if (to_type < CASTLE) // Pawn promotion
		{
			tsq = advanceProm(fsq) + SqDisp(to_type);
			Piece  q = SqPromo(to_type);
			k ^= hash_remove_piece(c, Pawn, fsq) ^ hash_place_piece(c, q, tsq);
			if (Piece cap = SetCapture(PieceTypeOnSqr(tsq)))
			{
				// A promoting capture may affect castling rights
				// if it captures an unmoved rook.
				if (CR cr = castling_rights())
					if (CR del = CR_RightsLost[tsq])
						k ^= update_castling(cr, del);
				setHash(k ^ hash_remove_piece(~c, cap, tsq));
				Board::remove_piece(~c, cap, tsq);
			}
			else
				setHash(k);
			inner_remove_piece(c, Pawn, fsq);
			place_piece(c, q, tsq);
			// update psq, pawn hash, material hash
			return;
		}
		else
		{
			if (SqCRKS(to_type)) // CASTLE_KS
			{
				ksq = fsq;		// King From square
				kto = ksq + 2;	// King To square
				rsq = ksq + 3;	// Rook From square H-Rook
				rto = ksq + 1;	// Rook To square
			}
			else // CASTLE_QS
			{
				ksq = fsq;		// King From square
				kto = ksq - 2;	// King To square
				rsq = ksq - 4;	// Rook From square A-Rook
				rto = ksq - 1;	// Rook To square
			}
			// do castle update
			setHash(update_castling(CR_rights(c)) ^ hash_move_piece(c, King, ksq, kto) ^ hash_move_piece(c, Rook, rsq, rto));	// delete castling rights for color c
			move_piece(c, King, ksq, kto);		// King moves 2 squares
			move_piece(c, Rook, rsq, rto);		// rook moves to square beside king
			// **TO DO** Update psq
			return;
		}
	}

	__forceinline
		void Position::undoSpecialMove(Color c, Square fsq, Square to_sq)
	{
		Square ksq, kto, rsq, rto, tsq;

#if ADV2_SPECIAL
		if (to_sq == ADV2)	// double pawn advance
		{
			move_piece(c, Pawn, advance24(fsq), fsq);
		}
		else
#endif
		if( to_sq <= EPH )							// EnPassant capture
		{
			Square epsq = fsq + SqDisp(to_sq);		// Original square of the pawn removed from the board
			tsq = retreatEP(epsq);					// Square the capturing pawn moved to
			inner_place_piece(~c, Pawn, epsq);		// replace captured pawn on appropriate square
			move_piece(c, Pawn, tsq, fsq);			// move our pawn back to its from square
		}
		else if (to_sq < CASTLE)					// Pawn promotion	
		{
			tsq = advanceProm(fsq) + SqDisp(to_sq);	// Promotion square
			Piece  q = SqPromo(to_sq);				// Promoted piece type
			inner_remove_piece(c, q, tsq);			// remove promoted piece from promotion square
			if (Piece cap = curr_capture())			// Did the promotion capture an enemy piece?
			{
				inner_place_piece(~c, cap, tsq);	// replace captured piece on promotion square
			}
			place_piece(c, Pawn, fsq);				// replace our pawn on its starting square
		}
		else
		{
			if (SqCRKS(to_sq))
			{
				ksq = fsq;		// King From square
				kto = ksq + 2;	// King To square
				rsq = ksq + 3;	// Rook From square H-Rook
				rto = ksq + 1;	// Rook To square
			}
			else
			{
				ksq = fsq;		// King From square
				kto = ksq - 2;	// King To square
				rsq = ksq - 4;	// Rook From square A-Rook
				rto = ksq - 1;	// Rook To square
			}
			// do_castle
			move_piece(c, King, kto, ksq);			// King moves 2 squares
			move_piece(c, Rook, rto, rsq);			// rook moves to square beside king
		}
	}

	//__declspec(noinline)
	void Position::doLegalMove(Move& m)
	{
		Color c = sideToMove;
		sideToMove = ~c;

		Assert(pseudo_legal(c, m));

		Hash k = ~hash(); //  curr_hash ^ Zobrist::sideToMove;
		// Unset EP hash of current ply
		if (epSet())
			k ^= Zobrist::HashEP(epCapture());

		// initialize the next ply from the current one
		// make the next ply the current_ply
		pushPly(m);
		// make the move on the board
		Square f = MoveFromSq(m), t = MoveToSq(m);
		if (t < SPECIAL)
			doNormalMove(c, k, f, t);
		else
			doSpecialMove(c, k, f, t);
	}

	//__declspec(noinline)
	void Position::undoMove(Move& m)
	{
		Square f = MoveFromSq(m), t = MoveToSq(m);
		sideToMove = ~sideToMove;
		Color c = sideToMove;

		// undo the move on the board
		if (t < SPECIAL)
			undoNormalMove(c, f, t);
		else
			undoSpecialMove(c, f, t);
		popPly();
	}


#define KingInCheck m_checkers

	/// <summary>
	/// pseudo_legal() takes a Move not necessarily generated for this position 
	/// (hash move) and tests whether it is pseudo_legal. 
	/// </summary>
	/// <param name="c"></param>
	/// <param name="m"></param>
	/// <returns></returns>
	bool Position::pseudo_legal(Color c, Move m) const
	{
		Square from = MoveFromSq(m), to = MoveToSq(m);
		uint8 pl = PseudoLegal[to][from];
		Piece p = board(from);
		if (member(Pieces(c), from) && (pl & (1 << p)))
		{
			if (to < SPECIAL)
			{
				return member(~Pieces(c), to)
					&& between_bb(to, from) == 0
					&& ((p > Pawn) || (pl&uint8(1)) == member(Pieces(~c), to));
			}
#if ADV2_SPECIAL
			else if( to == ADV2 )
			{
				return member(Pieces(), advance24(from))==0 && member(Pieces(), advance23(from))==0;
			}
#endif
			else if (to > CASTLE)
			{
				return SqCR(to) & CR_rights(c);
			}
			else // PROMO or EP
			{
				SqrInt disp = SqDisp(to);
				if (to > PROMO) {
					Square t = advanceProm(from) + disp;  
					return (pl & uint8(1)) == member(Pieces(~c), t);
				}
				else {	// EP
					return from + disp == epCapture();
				}
			}
		}
		return false;
	}



	std::string PieceNames = "**KPNBRQ**kpnbrq";

	PieceClr CharToPiece(char c)
	{
		size_t idx = PieceNames.find(c);
		if (idx != std::string::npos)
			return ColoredPiece(Piece(idx & 7), Color(idx >> 3));
		else
			return PC_EMPTY;
	}

	char PieceToChar(Piece p, Color c) { return PieceNames[p + 8 * c]; }
	char PieceToChar(PieceClr p) { return PieceToChar(PieceType(p), PieceColor(p)); }

	// Returns 0 for success else error code.
	int Position::set(const std::string& fenStr)
	{
		clear();

		char token;
		int f = 0;
		int r = 7;

		std::istringstream ss(fenStr);
		ss >> std::noskipws;

		/*
		1) Piece placement(from white's perspective). Each rank is described, starting
		with rank 8 and ending with rank 1. Within each rank, the contents of each
			square are described from file A through file H.Following the Standard
			Algebraic Notation(SAN), each piece is identified by a single letter taken
			from the standard English names.White pieces are designated using upper - case
			letters("PNBRQK") whilst Black uses lowercase("pnbrqk").Blank squares are
			noted using digits 1 through 8 (the number of blank squares), and "/"
			separates ranks.
			*/
		while ((ss >> token) && !isspace(token))
		{
			if (isdigit(token))
			{
				f += token - '0'; // Advance the given number of files
			}
			else if (token == '/')
			{
				if (--r < 0)
					return 1; // Invalid rank
				f = 0;
			}
			else if (f < 8)
			{
				PieceClr p = CharToPiece(token);
				if (p == PC_EMPTY)
					return 3; // Invalid piece code
				place_piece(PieceColor(p), PieceType(p), sqr(rank_t(r), file_t(f)));
				f++;
			}
			else {
				// Invalid file
				return 2;
			}
		}

		// 2. Active color. "w" means white moves next, "b" means black.
		ss >> token;
		token = tolower(token);
		if (token == 'w')
			sideToMove = White;
		else if (token == 'b')
			sideToMove = Black;
		else
			// Invalid color
			return 2;

		// 3. Castling availability.
		CR cr = CR_NONE;
		do { ss >> token; } while (isspace(token));
		if (token != '-')
		{
			do {
				switch (token)
				{
				case 'K': cr |= CR_KS_WHT; break;
				case 'k': cr |= CR_KS_BLK; break;
				case 'Q': cr |= CR_QS_WHT; break;
				case 'q': cr |= CR_QS_BLK; break;
				default:
					return 4; // Invalid castling option
				}
			} while ((ss >> token) && !isspace(token));
		}
		setCR(cr);

		/*
		4) En passant target square(in algebraic notation).If there's no en passant
			target square, this is "-".If a pawn has just made a 2 - square move, this
			is the position "behind" the pawn. This MAY BE set regardless of whether
			there is a pawn in position to make an en passant capture.

			Ignore if no pawn capture is possible
			*/
		do { ss >> token; } while (isspace(token));
		clearEP();
		if (token != '-')
		{
			char col = tolower(token), row;
			if ((col >= 'a' && col <= 'h')
				&& ((ss >> row) && (row == '3' || row == '6')))
			{
				// Valid syntax -- check whether EP capture is possible
				Square epPawn = advanceEP(sqr(Rank_t(row - '1'), File_t(col - 'a')));
				// There must be a pawn of color ~sideToMove on this square
				if (Pawns(~sideToMove) & epPawn)
				{
					if (beside_bb(epPawn) & Pawns(sideToMove))
						setEP(epPawn);
				}
			}
			else return 5; // Invalid EP target syntax		
		}

		// 5-6. Halfmove clock and fullmove number
		int move50, fullmove;
		ss >> std::skipws >> move50 >> fullmove;

		// Convert from fullmove starting from 1 to ply starting from 0,
		// handle also common incorrect FEN with fullmove = 0.
		// -- not this --
		// ply(std::max(2 * (fullmove - 1), 0) + (sideToMove == Black));

		AssertBoard();
		return 0;
	}

}