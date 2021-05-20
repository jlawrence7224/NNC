
#include "NpEngine.h"
#include "pch.h"
#include "types.h"
#include "move.h"
#  include <time.h>

namespace NNC {
	// returns bytes written
	int sqrName(char* buf, size_t bufsiz, Square sq)
	{
		if (SquareIsSpecial(sq))
		{
			switch (sq)
			{
			case ADV2: return sprintf_s(buf, bufsiz, "%s", "<ADV2>");
			case EPA: return sprintf_s(buf, bufsiz, "%s", "<EPA>");
			case EPH: return sprintf_s(buf, bufsiz, "%s", "<EPH>");
			default:
				if (sq < CASTLE )
					return sprintf_s(buf, bufsiz, "%s", "<PROMO>");
				if (SqCRKS(sq))
					return sprintf_s(buf, bufsiz, "%s", "<O-O>");
				if (SqCRQS(sq))
					return sprintf_s(buf, bufsiz, "%s", "<O-O-O>");
			}
		}
		else
		{
			File_t f = FileOf(sq); Rank_t r = RankOf(sq); return sprintf_s(buf, bufsiz, "%c%c", 'a' + char(f), '0' + char(r));
		}
	}

	// Utility to decode algebraic notation into From/To format returned as a Move16
	// This does not correctly format special moves.
	Move Position::parseMoveFT(const char* ms)
	{
		char ff, fr, tf, tr;
		if (strlen(ms) != 4)
			return MOVE_NONE;
		if (4 == sscanf(ms, "%c%c%c%c", &ff, &fr, &tf, &tr))
		{
			int FF = ff - 'a', FR = fr - '1', TF = tf - 'a', TR = tr - '1';
			Square f = sqr(FR, FF), t = sqr(TR, TF);
			Move m(f, t);
			if (!MoveOk(m))
				return MOVE_NONE;
			return m;
		}
		return MOVE_NONE;
	}
	Move Position::convertFt2Special(Move m16)
	{
		Square f = MoveFromSq(m16);
		Square t = MoveToSq(m16);

		// Check for special moves which must be re-encoded
		switch (PieceTypeOnSqr(f))
		{
		case Pawn:
#if ADV2_SPECIAL
			if (t == advance24(f))
				return mov_t(f, ADV2);
			else
#endif
				if (RelativeRank(sideToMove, RankOf(t)) == RANK_8)
					return MovePromo(f, t);
				else if (t == epTo())
					return MoveEP(f, t);
			break;
		case King:
			if (f == KingSq(sideToMove))
			{
				if (t == f + 2) 
					return MoveCastle(f, castling_rights(sideToMove, CR_KS));
				else if (t == f - 3 || t == f - 2) 
					return MoveCastle(f, castling_rights(sideToMove, CR_QS));
			}
			break;
		}
		return m16;
	}

	Move Position::parseMove(const char* ms)
	{
		Move m16 = convertFt2Special(parseMoveFT(ms));

		if (Move m = pseudo_legal(m16))
			return m;
		return MOVE_NONE;
	}

	std::ostream& operator<<(std::ostream& os, Color c) { return os << ((c == White) ? "w" : "b"); }
	std::ostream& operator<<(std::ostream& os, Rank_t r) { return os << char('1' + char(r)); }
	std::ostream& operator<<(std::ostream& os, File_t f) { return os << char('a' + char(f)); }
	std::ostream& operator<<(std::ostream& os, Square sq) { return os << FileOf(sq) << RankOf(sq); }
	std::ostream& operator<<(std::ostream& os, CastlingRights cr)
	{
		if (cr == CR_NONE)
			return os << "-";
		if (cr & CR_KS_WHT)
			os << "K";
		if (cr & CR_QS_WHT)
			os << "Q";
		if (cr & CR_KS_BLK)
			os << "k";
		if (cr & CR_QS_BLK)
			os << "q";
		return os;
	}

	const char* pieceName[8] = { "<none>","<unused>","King","Pawn","Knight","Bishop","Rook","Queen" };
	std::ostream& operator<<(std::ostream& os, Piece p) { return os << ((p <= MaxPieceType) ? pieceName[p] : "<invalid>"); }

	std::ostream& operator<<(std::ostream& os, MoveX m)
	{
		Square f = MoveFromSq(m), t = MoveToSq(m);
		if (t < SPECIAL )
			return os << f << t;
#if ADV2_SPECIAL
		if( t == ADV2 )
			return os << f << advance24(f);
#endif
		if( t <= EPH )
			// EP capture
			return os << f << f + advanceEP(f) + SqDisp(t) << "(ep)";
		if (t < CASTLE)
			// Promotion
			return os << f << f + advanceProm(f) + SqDisp(t) << "(" << SqPromo(t) << ")";
		// Castle
		return os << (SqCRKS(t) ? "O-O" : "O-O-O");

	}
	std::ostream& operator<<(std::ostream& os, Bitboard bb)
	{
		uint64_t b = bb;
		for (int i = 56; i >= 0; i -= 8)
		{
			uint64_t x = (b >> i) & 255;
			for (uint64_t j = 1; j < 256; j <<= 1)
				if (x & j)
					os << 'x';
				else
					os << '-';
			os << std::endl;
		}
		return os;
	}

	uint64_t atoiKMB(char *input) {
		uint64_t size;

		size = atoi(input);
		if (strchr(input, 'K') || strchr(input, 'k'))
			size *= 1 << 10;
		if (strchr(input, 'M') || strchr(input, 'm'))
			size *= 1 << 20;
		if (strchr(input, 'B') || strchr(input, 'b') || strchr(input, 'G') ||
			strchr(input, 'g'))
			size *= 1 << 30;
		return size;
	}

	/*
	 *******************************************************************************
	 *                                                                             *
	 *   DisplayArray() prints array data either 8 or 16 values per line, and also *
	 *   reverses the output for arrays that overlay the chess board so that the   *
	 *   'white side" is at the bottom rather than the top.  this is mainly used   *
	 *   from inside Option() to display the many evaluation terms.                *
	 *                                                                             *
	 *******************************************************************************
	 */

	void DisplayArray(int *array, int size) {
		int i, j, len = 16;

		if (abs(size) % 10 == 0)
			len = 10;
		else if (abs(size) % 8 == 0)
			len = 8;
		if (size > 0 && size % 16 == 0 && len == 8)
			len = 16;
		if (size > 0) {
			printf("    ");
			for (i = 0; i < size; i++) {
				printf("%3d ", array[i]);
				if ((i + 1) % len == 0) {
					printf("\n");
					if (i < size - 1)
						printf("    ");
				}
			}
			if (i % len != 0)
				printf("\n");
		}
		if (size < 0) {
			for (i = 0; i < 8; i++) {
				printf("    ");
				for (j = 0; j < 8; j++) {
					printf("%3d ", array[(7 - i) * 8 + j]);
				}
				printf(" | %d\n", 8 - i);
			}
			printf("    ---------------------------------\n");
			printf("      a   b   c   d   e   f   g   h\n");
		}
	}

	/*
	 *******************************************************************************
	 *                                                                             *
	 *   DisplayArray() prints array data either 8 or 16 values per line, and also *
	 *   reverses the output for arrays that overlay the chess board so that the   *
	 *   'white side" is at the bottom rather than the top.  this is mainly used   *
	 *   from inside Option() to display the many evaluation terms.                *
	 *                                                                             *
	 *******************************************************************************
	 */
	void DisplayArrayX2(int *array, int *array2, int size) {
		int i, j;

		if (size == 256) {
			printf("    ----------- Middlegame -----------   ");
			printf("    ------------- Endgame -----------\n");
			for (i = 0; i < 8; i++) {
				printf("    ");
				for (j = 0; j < 8; j++)
					printf("%3d ", array[(7 - i) * 8 + j]);
				printf("  |  %d  |", 8 - i);
				printf("  ");
				for (j = 0; j < 8; j++)
					printf("%3d ", array2[(7 - i) * 8 + j]);
				printf("\n");
			}
			printf
			("    ----------------------------------       ---------------------------------\n");
			printf("      a   b   c   d   e   f   g   h        ");
			printf("      a   b   c   d   e   f   g   h\n");
		}
		else if (size == 32) {
			printf("    ----------- Middlegame -----------   ");
			printf("    ------------- Endgame -----------\n");
			printf("    ");
			for (i = 0; i < 8; i++)
				printf("%3d ", array[i]);
			printf("  |     |");
			printf("  ");
			for (i = 0; i < 8; i++)
				printf("%3d ", array2[i]);
			printf("\n");
		}
		else if (size <= 20) {
			size = size / 2;
			printf("    ");
			for (i = 0; i < size; i++)
				printf("%3d ", array[i]);
			printf("  |<mg    eg>|");
			printf("  ");
			for (i = 0; i < size; i++)
				printf("%3d ", array2[i]);
			printf("\n");
		}
		else if (size > 128) {
			printf("    ----------- Middlegame -----------   ");
			printf("    ------------- Endgame -----------\n");
			for (i = 0; i < size / 32; i++) {
				printf("    ");
				for (j = 0; j < 8; j++)
					printf("%3d ", array[(7 - i) * 8 + j]);
				printf("  |  %d  |", 8 - i);
				printf("  ");
				for (j = 0; j < 8; j++)
					printf("%3d ", array2[(7 - i) * 8 + j]);
				printf("\n");
			}
		}
		else
			printf("ERROR, invalid size = -%d in packet\n", size);
	}

	/*
	 *******************************************************************************
	 *                                                                             *
	 *   DisplayBitBoard() is a debugging function used to display bitboards in a  *
	 *   more visual way.  they are displayed as an 8x8 matrix oriented as the     *
	 *   normal chess board is, with a1 at the lower left corner.                  *
	 *                                                                             *
	 *******************************************************************************
	 */
	void DisplayBitBoard(uint64_t board)
	{
		int i, j, x;

		for (i = 56; i >= 0; i -= 8) {
			x = (board >> i) & 255;
			for (j = 1; j < 256; j = j << 1)
				if (x & j)
					printf("X ");
				else
					printf("- ");
			printf("\n");
		}
	}

	/*
	 *******************************************************************************
	 *                                                                             *
	 *   Display2BitBoards() is a debugging function used to display bitboards in  *
	 *   a more visual way.  they are displayed as an 8x8 matrix oriented as the   *
	 *   normal chess board is, with a1 at the lower left corner.  this function   *
	 *   displays 2 boards side by side for comparison.                            *
	 *                                                                             *
	 *******************************************************************************
	 */
	void Display2BitBoards(uint64_t board1, uint64_t board2)
	{
		int i, j, x, y;

		for (i = 56; i >= 0; i -= 8) {
			x = (board1 >> i) & 255;
			for (j = 1; j < 256; j = j << 1)
				if (x & j)
					printf("X ");
				else
					printf("- ");
			printf("    ");
			y = (board2 >> i) & 255;
			for (j = 1; j < 256; j = j << 1)
				if (y & j)
					printf("X ");
				else
					printf("- ");
			printf("\n");
		}
	}

	/*
	 *******************************************************************************
	 *                                                                             *
	 *   DisplayChessBoard() is used to display the board since it is kept in      *
	 *   both the bit-board and array formats, here we use the array format which  *
	 *   is nearly ready for display as is.                                        *
	 *                                                                             *
	 *******************************************************************************
	 */
#if 1

	std::ostream& Position::fen(std::ostream& os) const {
		static const char* display_string[2][8] =
		{ " ", " ", "K", "P", "N", "B", "R", "Q",
		  " ", " ", "k", "p", "n", "b", "r", "q",
		};
		for (int i = 7; i >= 0; i--)
		{
			Square sq0 = Square(i * 8);
			for (auto sq : rank_bb(Rank_t(i)) & Pieces())
			{
				if (sq > sq0)
					os << int(sq - sq0);
				os << display_string[PieceColorOnSqr(sq)][PieceTypeOnSqr(sq)];
				sq0 = sq + 1;
			}
			if (sq0 < (i * 8 + 8))
				os << ((i * 8 + 8) - sq0);

			os << ((i > 0) ? "/" : " ");
		}
		os << sideToMove << " ";
		os << castling_rights() << " ";
		if (epTarget())
			os << epTo() << " ";
		else
			os << "- ";
		os << int(reversible()) << " ";	// 50 move counter
		os << 1; // (depth() / 2 + 1);		// full move counter
		os << std::endl;
		return os;
	}

	std::ostream& Position::ShowBoard(std::ostream& os) const
	{
		const char* display_board[64];
		static const char* display_string[2][8] =
		{ " . ", "ZZZ", "<K>", "<P>", "<N>", "<B>", "<R>", "<Q>",
		  " . ", "ZZZ", " k ", " p ", " n ", " b ", " r ", " q ",
		};
		for (int i = 0; i < 64; i++)
			display_board[i] = " . ";
		for (auto sq : Pieces(White))
			display_board[sq] = display_string[White][PieceTypeOnSqr(sq)];
		for (auto sq : Pieces(Black))
			display_board[sq] = display_string[Black][PieceTypeOnSqr(sq)];

		os << std::endl << "+---+---+---+---+---+---+---+---+" << std::endl;
		for (int i = 56; i >= 0; i -= 8)
		{
			os << " ";
			for (int j = 0; j < 8; j++)
				os << display_board[i + j] << " ";
			os << std::endl;
		}
		os << "+---+---+---+---+---+---+---+---+" << std::endl;
		return os;
	}

#endif
	/*
	 *******************************************************************************
	 *                                                                             *
	 *   DisplayEvaluation() is used to convert the evaluation to a string that    *
	 *   can be displayed.  The length is fixed so that screen formatting will     *
	 *   look nice and aligned.                                                    *
	 *                                                                             *
	 *******************************************************************************
	 */
#if 0
	extern bool MateScore(int value);
	char *DisplayEvaluation(int value, int wtm)
	{
		static char out[10];
		int tvalue;

		tvalue = (wtm) ? value : -value;
		if (!MateScore(value))
			sprintf(out, "%7.2f", ((float)tvalue) / 100.0);
		else if (abs(value) > MATE) {
			if (tvalue < 0)
				sprintf(out, " -infnty");
			else
				sprintf(out, " +infnty");
		}
		else if (value == MATE - 2 && wtm)
			sprintf(out, "   Mate");
		else if (value == MATE - 2 && !wtm)
			sprintf(out, "  -Mate");
		else if (value == -(MATE - 1) && wtm)
			sprintf(out, "  -Mate");
		else if (value == -(MATE - 1) && !wtm)
			sprintf(out, "   Mate");
		else if (value > 0 && wtm)
			sprintf(out, "  Mat%.2d", (MATE - value) / 2);
		else if (value > 0 && !wtm)
			sprintf(out, " -Mat%.2d", (MATE - value) / 2);
		else if (wtm)
			sprintf(out, " -Mat%.2d", (MATE - abs(value)) / 2);
		else
			sprintf(out, "  Mat%.2d", (MATE - abs(value)) / 2);
		return out;
	}

#endif
	/*
	 *******************************************************************************
	 *                                                                             *
	 *   DisplayHHMMSS is used to convert integer time values in 1/100th second    *
	 *   units into a traditional output format for time, hh:mm:ss rather than     *
	 *   just nnn.n seconds.                                                       *
	 *                                                                             *
	 *******************************************************************************
	 */
	char *DisplayHHMMSS(unsigned int time) {
		static char out[10];

		time = time / 100;
		sprintf(out, "%3u:%02u:%02u", time / 3600, time / 60, time % 60);
		return out;
	}

	/*
	 *******************************************************************************
	 *                                                                             *
	 *   DisplayHHMM is used to convert integer time values in 1/100th second      *
	 *   units into a traditional output format for time, mm:ss rather than just   *
	 *   nnn.n seconds.                                                            *
	 *                                                                             *
	 *******************************************************************************
	 */
	char *DisplayHHMM(unsigned int time) {
		static char out[10];

		time = time / 6000;
		sprintf(out, "%3u:%02u", time / 60, time % 60);
		return out;
	}

	/*
	 *******************************************************************************
	 *                                                                             *
	 *   DisplayKMB() takes an integer value that represents nodes per second, or  *
	 *   just total nodes, and converts it into a more compact form, so that       *
	 *   instead of nps=57200931, we get nps=57.2M.  We use units of "K", "M",     *
	 *   "B" and "T".  If type==0, K=1000, etc.  If type=1, K=1024, etc.           *
	 *                                                                             *
	 *******************************************************************************
	 */
#define PRIu64 "llu"
	char *DisplayKMB(uint64_t val, int type) {
		static char out[10];

		if (type == 0) {
			if (val < 1000)
				sprintf(out, "%" PRIu64, val);
			else if (val < 1000000)
				sprintf(out, "%.1fK", (double)val / 1000);
			else if (val < 1000000000)
				sprintf(out, "%.1fM", (double)val / 1000000);
			else
				sprintf(out, "%.1fB", (double)val / 1000000000);
		}
		else {
			if (val > 0 && !(val & 0x000000003fffffffULL))
				sprintf(out, "%dG", (int)(val / (1 << 30)));
			else if (val > 0 && !(val & 0x00000000000fffffULL))
				sprintf(out, "%dM", (int)(val / (1 << 20)));
			else if (val > 0 && !(val & 0x00000000000003ffULL))
				sprintf(out, "%dK", (int)(val / (1 << 10)));
			else
				sprintf(out, "%" PRIu64, val);
		}
		return out;
	}

	/*
	 *******************************************************************************
	 *                                                                             *
	 *   DisplayTime() is used to display search times, and shows times in one of  *
	 *   two ways depending on the value passed in.  If less than 60 seconds is to *
	 *   be displayed, it is displayed as a decimal fraction like 32.7, while if   *
	 *   more than 60 seconds is to be displayed, it is converted to the more      *
	 *   traditional mm:ss form.  The string it produces is of fixed length to     *
	 *   provide neater screen formatting.                                         *
	 *                                                                             *
	 *******************************************************************************
	 */
	char *DisplayTime(unsigned int time) {
		static char out[10];

		if (time < 6000)
			sprintf(out, "%6.2f", (float)time / 100.0);
		else {
			time = time / 100;
			sprintf(out, "%3u:%02u", time / 60, time % 60);
		}
		return out;
	}
#if 0
	/*
	 *******************************************************************************
	 *                                                                             *
	 *   Display2Times() is used to display search times, and shows times in one   *
	 *   of two ways depending on the value passed in.  If less than 60 seconds is *
	 *   to be displayed, it is displayed as a decimal fraction like 32.7, while   *
	 *   if more than 60 seconds is to be displayed, it is converted to the more   *
	 *   traditional mm:ss form.  The string it produces is of fixed length to     *
	 *   provide neater screen formatting.                                         *
	 *                                                                             *
	 *   The second argument is the "difficulty" value which lets us display the   *
	 *   target time (as modified by difficulty) so that it is possible to know    *
	 *   roughly when the move will be announced.                                  *
	 *                                                                             *
	 *******************************************************************************
	 */
	char *Display2Times(unsigned int time) {
		static char out[20], tout[10];
		int ttime;
		int c, spaces;

		if (time < 6000)
			sprintf(out, "%6.2f", (float)time / 100.0);
		else {
			time = time / 100;
			sprintf(out, "%3u:%02u", time / 60, time % 60);
		}
		if (search_time_limit)
			ttime = search_time_limit;
		else
			ttime = difficulty * time_limit / 100;
		if (ttime < 360000) {
			if (ttime < 6000)
				sprintf(tout, "%6.2f", (float)ttime / 100.0);
			else {
				ttime = ttime / 100;
				sprintf(tout, "%3u:%02u", ttime / 60, ttime % 60);
			}
			c = strspn(tout, " ");
			strcat(out, "/");
			strcat(out, tout + c);
		}
		spaces = 13 - strlen(out);
		for (c = 0; c < spaces; c++)
			strcat(out, " ");
		return out;
	}

	/*
	 *******************************************************************************
	 *                                                                             *
	 *   ReadClock() is a procedure used to read the elapsed time.  Since this     *
	 *   varies from system to system, this procedure has several flavors to       *
	 *   provide portability.                                                      *
	 *                                                                             *
	 *******************************************************************************
	 */
#endif
#if 0
	unsigned int ReadClock(void)
	{
		HANDLE hThread;
		FILETIME ftCreate, ftExit, ftKernel, ftUser;
		uint64_t tUser64;

		return (unsigned int)GetTickCount() / 10;
	}


	/*
	 *******************************************************************************
	 *                                                                             *
	 *   InvalidPosition() is used to determine if the position just entered via a *
	 *   FEN-string or the "edit" command is legal.  This includes the expected    *
	 *   tests for too many pawns or pieces for one side, pawns on impossible      *
	 *   squares, and the like.                                                    *
	 *                                                                             *
	 *******************************************************************************
	 */
	int InvalidPosition(TREE * RESTRICT tree) {
		int error = 0;
		int wp, wn, wb, wr, wq, bp, bn, bb, br, bq;

		wp = PopCnt(Pawns(white));
		wn = PopCnt(Knights(white));
		wb = PopCnt(Bishops(white));
		wr = PopCnt(Rooks(white));
		wq = PopCnt(Queens(white));
		bp = PopCnt(Pawns(black));
		bn = PopCnt(Knights(black));
		bb = PopCnt(Bishops(black));
		br = PopCnt(Rooks(black));
		bq = PopCnt(Queens(black));
		if (wp > 8) {
			Print(4095, "illegal position, too many white pawns\n");
			error = 1;
		}
		if (wp + wn > 10) {
			Print(4095, "illegal position, too many white knights\n");
			error = 1;
		}
		if (wp + wb > 10) {
			Print(4095, "illegal position, too many white bishops\n");
			error = 1;
		}
		if (wp + wr > 10) {
			Print(4095, "illegal position, too many white rooks\n");
			error = 1;
		}
		if (wp + wq > 10) {
			Print(4095, "illegal position, too many white queens\n");
			error = 1;
		}
		if (KingSQ(white) > 63) {
			Print(4095, "illegal position, no white king\n");
			error = 1;
		}
		if (wp + wn + wb + wr + wq > 15) {
			Print(4095, "illegal position, too many white pieces\n");
			error = 1;
		}
		if (Pawns(white) & (rank_mask[RANK1] | rank_mask[RANK8])) {
			Print(4095, "illegal position, white pawns on first/eighth rank(s)\n");
			error = 1;
		}
		if (bp > 8) {
			Print(4095, "illegal position, too many black pawns\n");
			error = 1;
		}
		if (bp + bn > 10) {
			Print(4095, "illegal position, too many black knights\n");
			error = 1;
		}
		if (bp + bb > 10) {
			Print(4095, "illegal position, too many black bishops\n");
			error = 1;
		}
		if (bp + br > 10) {
			Print(4095, "illegal position, too many black rooks\n");
			error = 1;
		}
		if (bp + bq > 10) {
			Print(4095, "illegal position, too many black queens\n");
			error = 1;
		}
		if (KingSQ(black) > 63) {
			Print(4095, "illegal position, no black king\n");
			error = 1;
		}
		if (bp + bn + bb + br + bq > 15) {
			Print(4095, "illegal position, too many black pieces\n");
			error = 1;
		}
		if (Pawns(black) & (rank_mask[RANK1] | rank_mask[RANK8])) {
			Print(4095, "illegal position, black pawns on first/eighth rank(s)\n");
			error = 1;
		}
		if (error == 0 && Check(!game_wtm)) {
			Print(4095, "ERROR side not on move is in check!\n");
			error = 1;
		}
		return error;
	}
#endif

	/*
	 *******************************************************************************
	 *                                                                             *
	 *   ParseTime() is used to parse a time value that could be entered as s.ss,  *
	 *   mm:ss, or hh:mm:ss.  It is converted to Crafty's internal 1/100th second  *
	 *   time resolution.                                                          *
	 *                                                                             *
	 *******************************************************************************
	 */
	int ParseTime(char *string) {
		int time = 0;
		int minutes = 0;

		while (*string) {
			switch (*string) {
			case '0':
			case '1':
			case '2':
			case '3':
			case '4':
			case '5':
			case '6':
			case '7':
			case '8':
			case '9':
				minutes = minutes * 10 + (*string) - '0';
				break;
			case ':':
				time = time * 60 + minutes;
				minutes = 0;
				break;
			default:
				printf("illegal character in time, please re-enter\n");
				break;
			}
			string++;
		}
		return time * 60 + minutes;
	}
}