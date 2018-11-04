/*
* Functions used to distribute seeds into pits.
*/

:- [io_functions].

% switch_player(CurrentPlayer, NextPlayer)
switch_player(bot, player).
switch_player(player, bot).

% seeds_amount(N, Board, SeedsAmount) - for the given Board (list of 6 Pits)
% it returns the amount of seeds in the Nth pit and stores it under SeedsAmount
seeds_amount(N, board(Board, House), SeedsAmount) :- nth0(N, Board, SeedsAmount).

% sow_seeds(N, SeedsAmount, PlayerBoard, OpponentBoard, NewPlayerBoard, NewOpponentBoard)
% Distribute the seeds from Nth pit to the consecutive pits on player and opponent
% boards (the seeds distribution direction should be counter-clockwise).
sow_seeds(Pit, SeedsAmount, PlayerBoard, OpponentBoard, FinalPlayerBoard, FinalOpponentBoard) :-
  SeedsAmount =< (12-Pit),
  sow_seeds_player_side(Pit,
    SeedsAmount, PlayerBoard, OpponentBoard,
    IntermediateSeedsAmount, IntermediatePlayerBoard, IntermediateOpponentBoard),
  sow_seeds_opponent_side(
    IntermediateSeedsAmount, IntermediatePlayerBoard, IntermediateOpponentBoard,
    FinalSeedsAmount, FinalPlayerBoard, FinalOpponentBoard).

sow_seeds(Pit, SeedsAmount, PlayerBoard, OpponentBoard, FinalPlayerBoard, FinalOpponentBoard) :-
  SeedsAmount > (12-Pit),
  sow_seeds_player_side(Pit,
    SeedsAmount, PlayerBoard, OpponentBoard,
    IntermediateSeedsAmount, IntermediatePlayerBoard, IntermediateOpponentBoard),
  sow_seeds_opponent_side(
    IntermediateSeedsAmount, IntermediatePlayerBoard, IntermediateOpponentBoard,
    NewSeedsAmount, NewPlayerBoard, NewOpponentBoard),
  sow_seeds(-1, NewSeedsAmount, NewPlayerBoard, NewOpponentBoard, FinalPlayerBoard, FinalOpponentBoard).

% sow_seeds_player_side(Pit,
%                       SeedsAmount, PlayerBoard, OpponentBoard,
%                       IntermediateSeedsAmount, IntermediatePlayerBoard, IntermediateOpponentBoard),
% Distribute the seeds to consecutive pits and the house.
% If the last seed was placed in an empty pit - collect it and all seeds from
% the opposite pit and place them in player's house.
% If the last seed was placed in the house - there should be another turn for the player.

% The opponent board would not change in this particular case
sow_seeds_player_side(Pit,
  SeedsAmount, board(PlayerPits, PlayerHouse), OpponentBoard,
  IntermediateSeedsAmount, board(IntermediatePlayerPits, IntermediatePlayerHouse),
  OpponentBoard) :-
    SeedsAmount > (6-Pit),
    collect_seeds(Pit, PlayerPits, PlayerPits1),
    NextPit is Pit+1,
    distribute_seeds(NextPit, SeedsAmount, PlayerPits1, IntermediatePlayerPits),
    IntermediatePlayerHouse is PlayerHouse+1, % Add a seed to the house
    IntermediateSeedsAmount is SeedsAmount-6+Pit.

% In this case it is possible that the opponent's board may change (in case of
% opponent's seeds capturing).
% Set the amount of IntermediateSeedsAmount equal to 0.
sow_seeds_player_side(Pit,
  SeedsAmount, board(PlayerPits, PlayerHouse), board(OpponentPits, OpponentHouse),
  0, board(IntermediatePlayerPits, IntermediatePlayerHouse), board(IntermediateOpponentPits, OpponentHouse)) :-
    SeedsAmount < (6-Pit),
    collect_seeds(Pit, PlayerPits, PlayerPits1),
    NextPit is Pit+1,
    distribute_seeds(NextPit, SeedsAmount, PlayerPits1, PlayerPits2),
    check_if_beatable(Pit, SeedsAmount, PlayerPits2, PlayerHouse, OpponentPits,
      IntermediatePlayerPits, IntermediatePlayerHouse, IntermediateOpponentPits).

sow_seeds_player_side(Pit,
  SeedsAmount, board(PlayerPits, PlayerHouse), OpponentBoard,
  0, board(IntermediatePlayerPits, IntermediatePlayerHouse),
  OpponentBoard) :-
    SeedsAmount =:= 6-Pit,
    collect_seeds(Pit, PlayerPits, PlayerPits1),
    NextPit is Pit+1,
    distribute_seeds(NextPit, SeedsAmount, PlayerPits1, IntermediatePlayerPits),
    IntermediatePlayerHouse is PlayerHouse+1. % Add a seed to the house

% sow_seeds_opponent_side(SeedsAmount, PlayerBoard, OpponentBoard, NewPlayerBoard, NewOpponentBoard).
% Distribute the seeds to consecutive pits, skipping the house.
% If the sow_seeds_player_side finished in empty pit - collect the seeds from
% "opposite" pit and place them in player's house.
sow_seeds_opponent_side(0, PlayerBoard, OpponentBoard, 0, PlayerBoard, OpponentBoard) :- !.
sow_seeds_opponent_side(SeedsAmount, PlayerBoard, board(OpponentPits, OpponentHouse), 0,
  PlayerBoard, board(FinalOpponentPits, OpponentHouse)) :-
    SeedsAmount =< 6,
    distribute_seeds(0, SeedsAmount, OpponentPits, FinalOpponentPits).

sow_seeds_opponent_side(SeedsAmount, PlayerBoard, board(OpponentPits, OpponentHouse), FinalSeedsAmount,
  PlayerBoard, board(FinalOpponentPits, OpponentHouse)) :-
    SeedsAmount > 6,
    distribute_seeds(0, SeedsAmount, OpponentPits, FinalOpponentPits),
    FinalSeedsAmount is SeedsAmount-6.

% collect_seeds(Pit, PlayerBoard, NewPlayerBoard)
% Pick up the all the seed from the given Pit and return the new Board.
% If the Pit ID equals -1 - do not collect any seed from the board.
collect_seeds(-1, Board, Board) :- !.
collect_seeds(0, [H|T], [0|T]) :- !.
collect_seeds(N, [H|T], [H|T1]) :- N > 0, N1 is N-1,  collect_seeds(N1, T, T1).

% distribute_seeds(Pit, Seeds, Board, NewBoard)
% Universal function for seeds distribution (only for the pits, does not include house)
% If N is the number of selected Pit, distribute only 6-N seeds to consecutive pits.
distribute_seeds(-1, _, Board, Board) :- !. % used in case if seeds would circle back
distribute_seeds(_, 0, Board, Board) :- !.
distribute_seeds(_, _, [], []) :- !.
distribute_seeds(N, Seeds, [H|T], [H|T1]) :- N > 0, N1 is N-1, distribute_seeds(N1, Seeds, T, T1).
distribute_seeds(0, Seeds, [H|T], [H1|T1]) :- H1 is H+1, Seeds1 is Seeds-1, distribute_seeds(0, Seeds1, T, T1).

% check_if_beatable(Pit, SeedsAmount, PlayerBoard, OpponentBoard, IntermediatePlayerBoard, IntermediateOpponentBoard)
% Check if it is possible to beat the opponent and collect opponent's seeds from the
% opposite pit. To do this, check if the number of seeds in the end pit is equal to 1.
check_if_beatable(Pit, SeedsAmount, PlayerPits, PlayerHouse, OpponentPits, FinalPlayerPits, FinalPlayerHouse, FinalOpponentPits) :-
  EndPit is Pit+SeedsAmount,
  nth0(EndPit, PlayerPits, 1), % check if the EndPit containts 1 seeds
  OppositePit is 5 - EndPit,
  nth0(OppositePit, OpponentPits, SeedsOnOppositePit), % check if the opposite pit contains any seeds
  SeedsOnOppositePit > 0, !,
  collect_seeds(EndPit, PlayerPits, FinalPlayerPits), % collect the one seed from the EndPit
  collect_seeds(OppositePit, OpponentPits, FinalOpponentPits), % collect the seeds from the oppononest opposite pits
  FinalPlayerHouse is PlayerHouse + 1 + SeedsOnOppositePit.

check_if_beatable(Pit, SeedsAmount, PlayerPits, PlayerHouse, OpponentPits, PlayerPits, PlayerHouse, OpponentPits) :- !.

% check if the number of moves would be 1 or more (at least 2)
more_turns(Pit, SeedsAmount) :- 0 is mod((SeedsAmount-6+Pit), 13).

% True if PlayerPits or OpponentPits is empty (contains only zeroes).
game_over(CurrentPlayer, board(PlayerPits, PlayerHouse), board(OpponentPits, OpponentHouse)) :-
  pits_are_empty(PlayerPits),
  move_seeds_to_house(OpponentPits, OpponentHouse, FinalOpponentHouse),
  winner(CurrentPlayer, PlayerHouse, FinalOpponentHouse, Winner),
  display_winner_information(Winner), !.

game_over(CurrentPlayer, board(PlayerPits, PlayerHouse), board(OpponentPits, OpponentHouse)) :-
  pits_are_empty(OpponentPits),
  move_seeds_to_house(PlayerPits, PlayerHouse, FinalPlayerHouse),
  winner(CurrentPlayer, FinalPlayerHouse, OpponentHouse, Winner),
  display_winner_information(Winner), !.

% Checks if the list contains anything else then 0
pits_are_empty([]).
pits_are_empty([0|T]) :- pits_are_empty(T).

% At the end of the game, moves all seeds from a part of the board to owner's house.
% move_seeds_to_house(Pits, House, FinalHouse)
move_seeds_to_house([], House, House) :- !.
move_seeds_to_house([H|T], House, FinalHouse) :-
  House1 is House + H,
  move_seeds_to_house(T, House1, FinalHouse).

% Returns the Winner
winner(_, PlayerPoints, OpponentPoints, remis) :-
  PlayerPoints =:= OpponentPoints.

winner(Player, PlayerPoints, OpponentPoints, Player) :-
  PlayerPoints > OpponentPoints.

winner(Player, PlayerPoints, OpponentPoints, NextPlayer) :-
  switch_player(Player, NextPlayer).
