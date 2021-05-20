
//#include <iostream>
//#include <string>
//#include "NpEngine.h"
#include "pch.h"
#include "tt.h"
#include "UCIAdapter.h"
#include "EngineOption.h"
#include <sstream>

#define UCI_MAIN 1

namespace NNC {

	std::string engine_info() { return "NP 0.0"; }
	const char* StartFEN = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
	TranspositionTable TT;
	Position pos;

#ifdef UCI_MAIN
	class EngineMain : public senjo::ChessEngine
	{
		bool is_initialized;

		EngineMain() : senjo::ChessEngine(), is_initialized(false) {}
		EngineMain(int argc, char** argv) : EngineMain() {}

		//--------------------------------------------------------------------------
		//! \brief Get the engine name
		//! \return The engine name
		//--------------------------------------------------------------------------
		std::string GetEngineName() const { return engine_info(); }

		//--------------------------------------------------------------------------
		//! \brief Get the engine version (e.g. "major.minor.build" e.g. "1.0.0")
		//! \return The engine version
		//--------------------------------------------------------------------------
		virtual std::string GetEngineVersion() const { return "0.0"; }

		//--------------------------------------------------------------------------
		//! \brief Get the engine author name(s)
		//! \return The engine author name(s)
		//--------------------------------------------------------------------------
		virtual std::string GetAuthorName() const { return "John Lawrence"; }

		//--------------------------------------------------------------------------
		//! \brief Get email address(es) for use with this engine
		//! Return an empty string if you don't wish to report an email address.
		//! \return An email address(es) for use with this engine
		//--------------------------------------------------------------------------
		virtual std::string GetEmailAddress() const { return "jlawrence7224@gmail.com"; }

		//--------------------------------------------------------------------------
		//! \brief Get the name of the country this engine originates from
		//! Return an empty string if you don't wish to report a country
		//! \return The name of the country this engine originates from
		//--------------------------------------------------------------------------
		virtual std::string GetCountryName() const { return "USA"; }

		//--------------------------------------------------------------------------
		//! \brief Get options supported by the engine, and their current values
		//! \return A list of the engine's options and their current values
		//--------------------------------------------------------------------------
		// ** TO DO ** 
		virtual std::list<senjo::EngineOption> GetOptions() const { return std::list<senjo::EngineOption>(); }

		//--------------------------------------------------------------------------
		//! \brief Set a particular option to a given value
		//! Option value may be empty, particularly if the option type is Button
		//! \param[in] optionName The option name
		//! \param[in] optionValue The new option value
		//! \return false if the option name or value is invalid
		//--------------------------------------------------------------------------
		// ** TO DO **
		virtual bool SetEngineOption(const std::string& optionName,
			const std::string& optionValue) {
			return false;
		}

		//--------------------------------------------------------------------------
		//! \brief Initialize the engine
		//--------------------------------------------------------------------------
		virtual void Initialize() {
			TT.resize(64);
			initTables();
			pos.init();
			is_initialized = true;
		}

		//--------------------------------------------------------------------------
		//! \brief Is the engine initialized?
		//! \return true if the engine is initialized
		//--------------------------------------------------------------------------
		virtual bool IsInitialized() const { return is_initialized; }

		//--------------------------------------------------------------------------
		//! \brief Set the board position according to a given FEN string
		//! The engine should use Output() to report errors in the FEN string.
		//! Only use position info from the given FEN string, don't process any moves
		//! or other data present in the FEN string.
		//! \param[in] fen The FEN string
		//! \return Position in \p fen after last char needed to load the position,
		//!         NULL if the FEN string does not contain a PseudoLegal position
		//--------------------------------------------------------------------------
		virtual const char* SetPosition(const char* fen) { return pos.set(fen) == 0 ? fen : 0; }

		//--------------------------------------------------------------------------
		//! \brief Execute a single move on the current position
		//! Determine whether the first word in the given string is a PseudoLegal move
		//! and if it is apply the move to the current position.  Moves should be
		//! in coordinate notation (e.g. "e2e4", "g8f6", "e7f8q").
		//! \param[in] str A string containing one or more moves or anything else
		//! \return Position in \p str after the first move,
		//!         NULL if the first word isn't a PseudoLegal move
		//--------------------------------------------------------------------------
		virtual const char* MakeMove(const char* str) {
			if (Move m = pos.pseudo_legal(pos.parseMove(str)))
				pos.doMove(m);
		}

		//--------------------------------------------------------------------------
		//! \brief Get a FEN string representation of the current board position
		//! \return A FEN string representation of the current board postiion
		//--------------------------------------------------------------------------
		virtual std::string GetFEN() const {
			std::ostringstream ss;
			pos.fen(ss);
			return ss.str();
		}

		//--------------------------------------------------------------------------
		//! \brief Output a text representation of the current board position
		//--------------------------------------------------------------------------
		virtual void PrintBoard() const { pos.ShowBoard(std::cout); }

		//--------------------------------------------------------------------------
		//! \brief Is it white to move in the current position?
		//! \return true if it is white to move in the current position
		//--------------------------------------------------------------------------
		virtual bool WhiteToMove() const { return pos.sideToMove == White; }

		//--------------------------------------------------------------------------
		//! \brief Clear any engine data that can persist between searches
		//! Examples of search data are the transposition table and killer moves.
		//--------------------------------------------------------------------------
		virtual void ClearSearchData() {
			// More ** TO DO **
			TT.clear();
		}

		//--------------------------------------------------------------------------
		//! \brief The last ponder move was played
		//! The Go() method may return a ponder move which is the expected response
		//! to the bestmove returned by Go().  If pondering is enabled the UCI
		//! adapter may tell the engine to ponder this move, e.g. start searching
		//! for a reply to the ponder move.  If, while the engine is pondering, the
		//! ponder move is played this method will be called.  In general the engine
		//! should make what it has learned from its pondering available for the next
		//! Go() call.
		//--------------------------------------------------------------------------
		virtual void PonderHit() {
			// ** TO DO ** 
			return;
		}

		//--------------------------------------------------------------------------
		//! \brief Get statistics from the last (or current) search
		//! \param[out] depth The maximum depth achieved
		//! \param[out] seldepth The maximum selective depth achieved
		//! \param[out] nodes The number of nodes searched
		//! \param[out] qnodes The number of \p nodes that were in quiescence search
		//! \param[out] msecs The number of milliseconds spent searching
		//! \param[out] movenum The last/current move number searched
		//! \param[out] move The last/current move searched (e.g. "e2e4")
		//! \param[in] movelen The size of the \p move parameter
		//--------------------------------------------------------------------------


		virtual void GetStats(int* depth,
			int* seldepth = NULL,
			uint64_t* nodes = NULL,
			uint64_t* qnodes = NULL,
			uint64_t* msecs = NULL,
			int* movenum = NULL,
			char* move = NULL,
			const size_t movelen = 0) const {
			// ** TO DO **
			return;
		}

		//--------------------------------------------------------------------------
		//! \brief Get a guess of how many moves remaining until game end
		//! This is used in the Go() method when the movestogo value is not given
		//! by the UCI caller.  It is generally not a good idea to return anything
		//! less than 10 or over 40 if the time control or move count is unknown.
		//! \return A guess of how many moves remaining until game end
		//--------------------------------------------------------------------------
		virtual int MovesToGo() const { return 20; }

		//--------------------------------------------------------------------------
		//! \brief Use the built-in timer thread for timeouts and periodic updates?
		//! \return true To use the built-in timer thread
		//--------------------------------------------------------------------------
		virtual bool UseTimer() const { return true; }

		//--------------------------------------------------------------------------
		//! \brief How long should the timer thread wait between outputs?
		//! \return Number of milliseconds to wait between outputs
		//--------------------------------------------------------------------------
		virtual unsigned int TimerOutputInterval() const { return 1000; }

		//--------------------------------------------------------------------------
		//! \brief Do performance test on the current position
		//! Perft is useful for determining the speed of you move generator.
		//! Perft is vital for validating whether your move generator is
		//! producing the correct moves.  In any given position, a perft search to
		//! a specific depth should always visit the same number of leaf nodes.
		//! For example, Perft(6) from STARTPOS should yield 119060324.
		//! \param[in] depth How many half-moves (plies) to search
		//! \return The number of leaf nodes visited at \p depth
		//--------------------------------------------------------------------------
		virtual uint64_t MyPerft(const int depth) { return pos.perft(depth); }
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
		//--------------------------------------------------------------------------
		virtual std::string MyGo(const int depth,
			const int movestogo = 0,
			const uint64_t movetime = 0,
			const uint64_t wtime = 0, const uint64_t winc = 0,
			const uint64_t btime = 0, const uint64_t binc = 0,
			std::string* ponder = NULL) = 0;

	};
#endif
#ifdef UCI_MAIN
int main(int argc, char** argv)
{
	try {
		NNC::EngineMain engine(argc,argv);
		senjo::UCIAdapter adapter(engine);

		std::string line;
		line.reserve(16384);

		while (std::getline(std::coin, line)) {
			if (!adapter.doCommand(line)) {
				break;
			}
		}

		return 0;
	}
	catch (const std::exception& e) {
		senjo::Output() << "ERROR: " << e.what();
		return 1;
	}
}
#else

int main(int argc, char* argv[])
{
	using namespace NPC;
	std::cout << engine_info() << std::endl;

	//UCI::init(Options);
	initTables();
	pos.init();
	TT.resize(64);

	int depth = 4;
	if (argc > 1)
		depth = atoi(argv[1]);
	const char* fen = argc > 2 ? (!strcmp(argv[2], "start") ? StartFEN : argv[2]) : StartFEN;
	if (pos.set(fen) == 0)
	{
		pos.AssertBoard();
		// std::cout << fen << std::endl << "perft " << depth << ": " << pos.perft(depth) << std::endl;
		for (int i = 3; i < argc; i++)
		{
			if (Move m = pos.pseudo_legal(pos.parseMove(argv[i])))
				pos.doMove(m);
			else
			{
				std::cout << "bad move: " << argv[i] << std::endl;
				exit(-1);
			}
		}
		pos.AssertBoard();
		pos.ShowBoard(std::cout);
		pos.fen(std::cout);
	}
	else
	{
		std::cout << "invalid board position";
		exit(-1);
	}
	// std::cout << fen << std::endl << "perft " << depth << ": " << pos.perft(depth) << std::endl;
	pos.divide(std::cout, depth);
}
#endif