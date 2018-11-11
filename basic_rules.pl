/*
* Functions used to distribute seeds into pits.
*/

:- module(basic_rules, [
  switch_player/2,
  seeds_number/3,
  more_turns/2,
  game_over/3,
  sow_seeds/6,
  seeds_number/3,
  boardSize/1,
  first_non_empty_pit/3 %TODO usunac pozniej
  ]).

:- use_module(io_functions).

boardSize(6).

% switch_player(CurrentPlayer, NextPlayer)
switch_player(bot, player).
switch_player(player, bot).

% seeds_number(N, Board, SeedsNumber) - for the given Board (list of 6 Pits)
% it returns the number of seeds in the Nth pit and stores it under SeedsNumber
seeds_number(N, board(Board, _), SeedsNumber) :- nth0(N, Board, SeedsNumber).

% sow_seeds(N, SeedsNumber, PlayerBoard, OpponentBoard, NewPlayerBoard, NewOpponentBoard)
% Distribute the seeds from Nth pit to the consecutive pits on player and opponent
% boards (the seeds distribution direction should be counter-clockwise).
sow_seeds(Pit, SeedsNumber, PlayerBoard, OpponentBoard, FinalPlayerBoard, FinalOpponentBoard) :-
  boardSize(BoardSize),
  SeedsNumber =< (2*BoardSize-Pit),
  sow_seeds_player_side(Pit,
    SeedsNumber, PlayerBoard, OpponentBoard,
    IntermediateSeedsNumber, IntermediatePlayerBoard, IntermediateOpponentBoard),
  sow_seeds_opponent_side(
    IntermediateSeedsNumber, IntermediatePlayerBoard, IntermediateOpponentBoard,
    FinalSeedsNumber, FinalPlayerBoard, FinalOpponentBoard).

sow_seeds(Pit, SeedsNumber, PlayerBoard, OpponentBoard, FinalPlayerBoard, FinalOpponentBoard) :-
  boardSize(BoardSize),
  SeedsNumber > (2*BoardSize-Pit),
  sow_seeds_player_side(Pit,
    SeedsNumber, PlayerBoard, OpponentBoard,
    IntermediateSeedsNumber, IntermediatePlayerBoard, IntermediateOpponentBoard),
  sow_seeds_opponent_side(
    IntermediateSeedsNumber, IntermediatePlayerBoard, IntermediateOpponentBoard,
    NewSeedsNumber, NewPlayerBoard, NewOpponentBoard),
  sow_seeds(-1, NewSeedsNumber, NewPlayerBoard, NewOpponentBoard, FinalPlayerBoard, FinalOpponentBoard).

% sow_seeds_player_side(Pit,
%                       SeedsNumber, PlayerBoard, OpponentBoard,
%                       IntermediateSeedsNumber, IntermediatePlayerBoard, IntermediateOpponentBoard),
% Distribute the seeds to consecutive pits and the house.
% If the last seed was placed in an empty pit - collect it and all seeds from
% the opposite pit and place them in player's house.
% If the last seed was placed in the house - there should be another turn for the player.

% The opponent board would not change in this particular case
sow_seeds_player_side(Pit,
  SeedsNumber, board(PlayerPits, PlayerHouse), OpponentBoard,
  IntermediateSeedsNumber, board(IntermediatePlayerPits, IntermediatePlayerHouse),
  OpponentBoard) :-
    boardSize(BoardSize),
    SeedsNumber > (BoardSize-Pit),
    collect_seeds(Pit, PlayerPits, PlayerPits1),
    NextPit is Pit+1,
    distribute_seeds(NextPit, SeedsNumber, PlayerPits1, IntermediatePlayerPits),
    IntermediatePlayerHouse is PlayerHouse+1, % Add a seed to the house
    IntermediateSeedsNumber is SeedsNumber-BoardSize+Pit.

% In this case it is possible that the opponent's board may change (in case of
% opponent's seeds capturing).
% Set the number of IntermediateSeedsNumber equal to 0.
sow_seeds_player_side(Pit,
  SeedsNumber, board(PlayerPits, PlayerHouse), board(OpponentPits, OpponentHouse),
  0, board(IntermediatePlayerPits, IntermediatePlayerHouse), board(IntermediateOpponentPits, OpponentHouse)) :-
    boardSize(BoardSize),
    SeedsNumber < (BoardSize-Pit),
    collect_seeds(Pit, PlayerPits, PlayerPits1),
    NextPit is Pit+1,
    distribute_seeds(NextPit, SeedsNumber, PlayerPits1, PlayerPits2),
    check_if_beatable(Pit, SeedsNumber, PlayerPits2, PlayerHouse, OpponentPits,
      IntermediatePlayerPits, IntermediatePlayerHouse, IntermediateOpponentPits).

sow_seeds_player_side(Pit,
  SeedsNumber, board(PlayerPits, PlayerHouse), OpponentBoard,
  0, board(IntermediatePlayerPits, IntermediatePlayerHouse),
  OpponentBoard) :-
    boardSize(BoardSize),
    SeedsNumber =:= BoardSize-Pit,
    collect_seeds(Pit, PlayerPits, PlayerPits1),
    NextPit is Pit+1,
    distribute_seeds(NextPit, SeedsNumber, PlayerPits1, IntermediatePlayerPits),
    IntermediatePlayerHouse is PlayerHouse+1. % Add a seed to the house

% sow_seeds_opponent_side(SeedsNumber, PlayerBoard, OpponentBoard, NewPlayerBoard, NewOpponentBoard).
% Distribute the seeds to consecutive pits, skipping the house.
% If the sow_seeds_player_side finished in empty pit - collect the seeds from
% "opposite" pit and place them in player's house.
sow_seeds_opponent_side(0, PlayerBoard, OpponentBoard, 0, PlayerBoard, OpponentBoard) :- !.
sow_seeds_opponent_side(SeedsNumber, PlayerBoard, board(OpponentPits, OpponentHouse), 0,
  PlayerBoard, board(FinalOpponentPits, OpponentHouse)) :-
    boardSize(BoardSize),
    SeedsNumber =< BoardSize,
    distribute_seeds(0, SeedsNumber, OpponentPits, FinalOpponentPits).

sow_seeds_opponent_side(SeedsNumber, PlayerBoard, board(OpponentPits, OpponentHouse), FinalSeedsNumber,
  PlayerBoard, board(FinalOpponentPits, OpponentHouse)) :-
    SeedsNumber > 6,
    distribute_seeds(0, SeedsNumber, OpponentPits, FinalOpponentPits),
    boardSize(BoardSize),
    FinalSeedsNumber is SeedsNumber-BoardSize.

% collect_seeds(Pit, PlayerBoard, NewPlayerBoard)
% Pick up the all the seed from the given Pit and return the new Board.
% If the Pit ID equals -1 - do not collect any seed from the board.
collect_seeds(-1, Board, Board) :- !.
collect_seeds(0, [_|T], [0|T]) :- !.
collect_seeds(N, [H|T], [H|T1]) :- N > 0, N1 is N-1,  collect_seeds(N1, T, T1).

% distribute_seeds(Pit, Seeds, Board, NewBoard)
% Universal function for seeds distribution (only for the pits, does not include house)
% If N is the number of selected Pit, distribute only 6-N seeds to consecutive pits.
distribute_seeds(-1, _, Board, Board) :- !. % used in case if seeds would circle back
distribute_seeds(_, 0, Board, Board) :- !.
distribute_seeds(_, _, [], []) :- !.
distribute_seeds(N, Seeds, [H|T], [H|T1]) :- N > 0, N1 is N-1, distribute_seeds(N1, Seeds, T, T1).
distribute_seeds(0, Seeds, [H|T], [H1|T1]) :- H1 is H+1, Seeds1 is Seeds-1, distribute_seeds(0, Seeds1, T, T1).

% check_if_beatable(Pit, SeedsNumber, PlayerBoard, OpponentBoard, IntermediatePlayerBoard, IntermediateOpponentBoard)
% Check if it is possible to beat the opponent and collect opponent's seeds from the
% opposite pit. To do this, check if the number of seeds in the end pit is equal to 1.
check_if_beatable(Pit, SeedsNumber, PlayerPits, PlayerHouse, OpponentPits, FinalPlayerPits, FinalPlayerHouse, FinalOpponentPits) :-
  EndPit is Pit+SeedsNumber,
  nth0(EndPit, PlayerPits, 1), % check if the EndPit containts 1 seeds
  boardSize(BoardSize),
  OppositePit is BoardSize - 1 - EndPit,
  nth0(OppositePit, OpponentPits, SeedsOnOppositePit), % check if the opposite pit contains any seeds
  SeedsOnOppositePit > 0, !,
  collect_seeds(EndPit, PlayerPits, FinalPlayerPits), % collect the one seed from the EndPit
  collect_seeds(OppositePit, OpponentPits, FinalOpponentPits), % collect the seeds from the oppononest opposite pits
  FinalPlayerHouse is PlayerHouse + 1 + SeedsOnOppositePit.

check_if_beatable(_, _, PlayerPits, PlayerHouse, OpponentPits, PlayerPits, PlayerHouse, OpponentPits) :- !.

% check if the number of moves would be 1 or more (at least 2)
more_turns(Pit, SeedsNumber) :-
  boardSize(BoardSize),
  0 is mod((SeedsNumber-BoardSize+Pit), (2*BoardSize+1)).

% True if PlayerPits or OpponentPits is empty (contains only zeroes).
game_over(CurrentPlayer, board(PlayerPits, PlayerHouse), board(OpponentPits, OpponentHouse)) :-
  some_pits_are_empty(PlayerPits, OpponentPits),
  move_seeds_to_house(OpponentPits, OpponentHouse, FinalOpponentHouse),
  move_seeds_to_house(PlayerPits, PlayerHouse, FinalPlayerHouse),
  winner(CurrentPlayer, FinalPlayerHouse, FinalOpponentHouse, Winner),
  display_winner_information(Winner), !.

% Checks if the list contains anything else then 0
pits_are_empty([]).
pits_are_empty([0|T]) :- pits_are_empty(T).

% Checks if either of the players has empty pits
some_pits_are_empty(Pits1, _) :- pits_are_empty(Pits1).
some_pits_are_empty(_, Pits2) :- pits_are_empty(Pits2).

%first_non_empty_pit((0|T), Pit) :- Pit is Pit1+1, first_non_empty_pit(T, Pit1).
first_non_empty_pit(PlayerBoard, MaxPit, Pit) :-
  MaxPit > 0,
  MaxPit1 is MaxPit - 1,
  first_non_empty_pit(PlayerBoard, MaxPit1, Pit).
first_non_empty_pit(PlayerBoard, MaxPit, Pit) :-
  seeds_number(MaxPit, PlayerBoard, SeedsNumber),
  SeedsNumber > 0,
  Pit is MaxPit.

% At the end of the game, moves all seeds from a part of the board to owner's house.
% move_seeds_to_house(Pits, House, FinalHouse)
move_seeds_to_house([], House, House) :- !.
move_seeds_to_house([H|T], House, FinalHouse) :-
  House1 is House + H,
  move_seeds_to_house(T, House1, FinalHouse).

% Returns the Winner
winner(_, PlayerPoints, OpponentPoints, tie) :-
  PlayerPoints =:= OpponentPoints.

winner(Player, PlayerPoints, OpponentPoints, Player) :-
  PlayerPoints > OpponentPoints.

winner(Player, _, _, NextPlayer) :-
  switch_player(Player, NextPlayer).
