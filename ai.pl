/*
* Functions used for the pit selection, done by the bot.
*/

:- module(ai, [find_best_move/3]).
:- use_module(basic_rules).

% List of IDs of all pits on the board
find_best_move(PlayerBoard, board(OpponentPits, OpponentHouse), SelectedPit) :-
  boardSize(BoardSize),
  consecutive(BoardSize, InitialList),
  all_available_pits(InitialList, OpponentPits, Pits).


% all_available_pits([all pits], OpponentBoard, Pits)
% Pits contains a list of all available pits, that can be chosen by the bot
% (which are not empty and place on the bot's board part)
all_available_pits([X|Xs], [O|Os], Pits) :-!.

all_available_pits([], _, Pits) :- !.
