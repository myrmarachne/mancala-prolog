/*
* Functions used to distribute seeds into pits.
*/

:- module(basic_rules, [
  switch_player/2,
  seeds_number/3,
  more_turns/2,
  game_over/1,
  sow_seeds/4,
  seeds_number/3,
  boardSize/1
  ]).

:- use_module(io_functions).

boardSize(6).

% switch_player(GameState0, GameState1)
switch_player(
    game_state(PlayerBoard, OpponentBoard, player),
    game_state(OpponentBoard, PlayerBoard, bot)
).
switch_player(
    game_state(PlayerBoard, OpponentBoard, bot),
    game_state(OpponentBoard, PlayerBoard, player)
).

% seeds_number(N, Board, SeedsNumber) - for the given Board (list of 6 Pits)
% it returns the number of seeds in the Nth pit and stores it under SeedsNumber
seeds_number(N, board(Board, _), SeedsNumber) :- nth0(N, Board, SeedsNumber).

% sow_seeds(N, SeedsNumber0, GameState0, GameState2)
% Distribute the seeds from Nth pit to the consecutive pits on player and opponent
% boards (the seeds distribution direction should be counter-clockwise).
sow_seeds(Pit, SeedsNumber0, GameState0, GameState2) :-
  boardSize(BoardSize),
  SeedsNumber0 =< (2*BoardSize-Pit),
  sow_seeds_player_side(Pit, SeedsNumber0, GameState0, SeedsNumber1, GameState1),
  sow_seeds_opponent_side(SeedsNumber1, GameState1, _, GameState2).

sow_seeds(Pit, SeedsNumber0, GameState0, GameState3) :-
  boardSize(BoardSize),
  SeedsNumber0 > (2*BoardSize-Pit),
  sow_seeds_player_side(Pit, SeedsNumber0, GameState0, SeedsNumber1, GameState1),
  sow_seeds_opponent_side(SeedsNumber1, GameState1, SeedsNumber2, GameState2),
  sow_seeds(-1, SeedsNumber2, GameState2, GameState3).

% sow_seeds_player_side(Pit, SeedsNumber0, GameState0, SeedsNumber1, GameState1)
% Distribute the seeds to consecutive pits and the house.
% If the last seed was placed in an empty pit - collect it and all seeds from
% the opposite pit and place them in player's house.
% If the last seed was placed in the house - there should be another turn for the player.

% The opponent board would not change in this particular case
sow_seeds_player_side(Pit, SeedsNumber0, GameState0, SeedsNumber1, GameState1) :-
    GameState0 = game_state(board(PlayerPits0, PlayerHouse0), OpponentBoard, CurrentPlayer),
    GameState1 = game_state(board(PlayerPits2, PlayerHouse1), OpponentBoard, CurrentPlayer),
    boardSize(BoardSize),
    SeedsNumber0 > (BoardSize-Pit),
    collect_seeds(Pit, PlayerPits0, PlayerPits1),
    NextPit is Pit+1,
    distribute_seeds(NextPit, SeedsNumber0, PlayerPits1, PlayerPits2),
    PlayerHouse1 is PlayerHouse0+1, % Add a seed to the house
    SeedsNumber1 is SeedsNumber0-BoardSize+Pit.

% In this case it is possible that the opponent's board may change (in case of
% opponent's seeds capturing).
% Set the number of IntermediateSeedsNumber equal to 0.
sow_seeds_player_side(Pit, SeedsNumber, GameState0, 0, GameState2) :-
    GameState0 = game_state(board(PlayerPits0, PlayerHouse0), board(OpponentPits0, OpponentHouse), CurrentPlayer),
    GameState1 = game_state(board(PlayerPits2, PlayerHouse0), board(OpponentPits0, OpponentHouse), CurrentPlayer),
    boardSize(BoardSize),
    SeedsNumber < (BoardSize-Pit),
    collect_seeds(Pit, PlayerPits0, PlayerPits1),
    NextPit is Pit+1,
    distribute_seeds(NextPit, SeedsNumber, PlayerPits1, PlayerPits2),
    check_if_beatable(Pit, SeedsNumber, GameState1, GameState2).

sow_seeds_player_side(Pit, SeedsNumber, GameState0, 0, GameState1) :-
    GameState0 = game_state(board(PlayerPits0, PlayerHouse0), OpponentBoard, CurrentPlayer),
    GameState1 = game_state(board(PlayerPits2, PlayerHouse1), OpponentBoard, CurrentPlayer),
    boardSize(BoardSize),
    SeedsNumber =:= BoardSize-Pit,
    collect_seeds(Pit, PlayerPits0, PlayerPits1),
    NextPit is Pit+1,
    distribute_seeds(NextPit, SeedsNumber, PlayerPits1, PlayerPits2),
    PlayerHouse1 is PlayerHouse0+1. % Add a seed to the house

% sow_seeds_opponent_side(SeedsNumber0, GameState0, SeedsNumber1, GameState1)
% Distribute the seeds to consecutive pits, skipping the house.
% If the sow_seeds_player_side finished in empty pit - collect the seeds from
% "opposite" pit and place them in player's house.
sow_seeds_opponent_side(0, GameState, 0, GameState) :- !.
sow_seeds_opponent_side(SeedsNumber, GameState0, 0, GameState1) :-
    GameState0 = game_state(PlayerBoard, board(OpponentPits0, OpponentHouse), CurrentPlayer),
    GameState1 = game_state(PlayerBoard, board(OpponentPits1, OpponentHouse), CurrentPlayer),
    boardSize(BoardSize),
    SeedsNumber =< BoardSize,
    distribute_seeds(0, SeedsNumber, OpponentPits0, OpponentPits1).

sow_seeds_opponent_side(SeedsNumber, GameState0, SeedsNumber1, GameState1) :-
    GameState0 = game_state(PlayerBoard, board(OpponentPits0, OpponentHouse), CurrentPlayer),
    GameState1 = game_state(PlayerBoard, board(OpponentPits1, OpponentHouse), CurrentPlayer),
    boardSize(BoardSize),
    SeedsNumber > BoardSize,
    distribute_seeds(0, SeedsNumber, OpponentPits0, OpponentPits1),
    boardSize(BoardSize),
    SeedsNumber1 is SeedsNumber-BoardSize.

% collect_seeds(Pit, PlayerBoard0, PlayerBoard1)
% Pick up the all the seed from the given Pit and return the new Board.
% If the Pit ID equals -1 - do not collect any seed from the board.
collect_seeds(-1, Board, Board) :- !.
collect_seeds(0, [_|T], [0|T]) :- !.
collect_seeds(N, [H|T], [H|T1]) :- N > 0, N1 is N-1,  collect_seeds(N1, T, T1).

% distribute_seeds(Pit, Seeds, Board0, Board1)
% Universal function for seeds distribution (only for the pits, does not include house)
% If N is the number of selected Pit, distribute only 6-N seeds to consecutive pits.
distribute_seeds(-1, _, Board, Board) :- !. % used in case if seeds would circle back
distribute_seeds(_, 0, Board, Board) :- !.
distribute_seeds(_, _, [], []) :- !.
distribute_seeds(N, Seeds, [H|T], [H|T1]) :- N > 0, N1 is N-1, distribute_seeds(N1, Seeds, T, T1).
distribute_seeds(0, Seeds, [H|T], [H1|T1]) :- H1 is H+1, Seeds1 is Seeds-1, distribute_seeds(0, Seeds1, T, T1).

% check_if_beatable(Pit, SeedsNumber, GameState0, GameState1)
% Check if it is possible to beat the opponent and collect opponent's seeds from the
% opposite pit. To do this, check if the number of seeds in the end pit is equal to 1.
check_if_beatable(Pit, SeedsNumber, GameState0, GameState1) :-
  GameState0 = game_state(board(PlayerPits0, PlayerHouse0), board(OpponentPits0, OpponentHouse), CurrentPlayer),
  GameState1 = game_state(board(PlayerPits1, PlayerHouse1), board(OpponentPits1, OpponentHouse), CurrentPlayer),
  EndPit is Pit+SeedsNumber,
  nth0(EndPit, PlayerPits0, 1), % check if the EndPit containts 1 seeds
  boardSize(BoardSize),
  OppositePit is BoardSize - 1 - EndPit,
  nth0(OppositePit, OpponentPits0, SeedsOnOppositePit), % check if the opposite pit contains any seeds
  SeedsOnOppositePit > 0, !,
  collect_seeds(EndPit, PlayerPits0, PlayerPits1), % collect the one seed from the EndPit
  collect_seeds(OppositePit, OpponentPits0, OpponentPits1), % collect the seeds from the oppononest opposite pits
  PlayerHouse1 is PlayerHouse0 + 1 + SeedsOnOppositePit.

check_if_beatable(_, _, GameState, GameState) :- !.

% Check if the number of moves would be 1 or more (at least 2)
more_turns(Pit, SeedsNumber) :-
  boardSize(BoardSize),
  0 is mod((SeedsNumber-BoardSize+Pit), (2*BoardSize+1)).

% Checks if PlayerPits or OpponentPits is empty (contains only zeroes) and determiens the winner.
game_over(GameState0) :-
  either_side_is_empty(GameState0),
  move_seeds_to_houses(GameState0, GameState1),
  winner(GameState1, Winner),
  writeln('\nGame over!'),
  display_board(GameState1),
  display_winner_information(Winner), !.

% Checks if the list contains anything else then 0
pits_are_empty([]).
pits_are_empty([0|T]) :- pits_are_empty(T).

% Checks if either of the players has empty pits
either_side_is_empty(GameState) :-
  GameState = game_state(board(PlayerPits, _), _, _),
  pits_are_empty(PlayerPits).
either_side_is_empty(GameState) :-
  GameState = game_state(_, board(OpponentPits, _), _),
  pits_are_empty(OpponentPits).

% At the end of the game, moves all seeds from a part of the board to owner's house.
% move_seeds_to_house(Pits, House, FinalHouse)
move_seeds_to_house([], House, House) :- !.
move_seeds_to_house([H|T], House, FinalHouse) :-
  House1 is House + H,
  move_seeds_to_house(T, House1, FinalHouse).

move_seeds_to_houses(GameState0, GameState1) :-
  GameState0 = game_state(board(PlayerPits, PlayerHouse0), board(OpponentPits, OpponentHouse0), CurrentPlayer),
  GameState1 = game_state(board([0,0,0,0,0,0], PlayerHouse1), board([0,0,0,0,0,0], OpponentHouse1), CurrentPlayer),
  move_seeds_to_house(OpponentPits, OpponentHouse0, OpponentHouse1),
  move_seeds_to_house(PlayerPits, PlayerHouse0, PlayerHouse1).

% Returns the Winner
winner(GameState, tie) :-
  GameState = game_state(board(_, PlayerHouse), board(_, OpponentHouse), _),
  PlayerHouse =:= OpponentHouse.

winner(GameState, CurrentPlayer) :-
  GameState = game_state(board(_, PlayerHouse), board(_, OpponentHouse), CurrentPlayer),
  PlayerHouse > OpponentHouse.

winner(GameState, NextPlayer) :-
  switch_player(GameState, game_state(_, _, NextPlayer)).
