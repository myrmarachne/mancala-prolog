/*
* Functions used to distribute seeds into pits.
*/

:- [io_functions].

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
  SeedsNumber0 =< (12-Pit),
  sow_seeds_player_side(Pit, SeedsNumber0, GameState0, SeedsNumber1, GameState1),
  sow_seeds_opponent_side(SeedsNumber1, GameState1, _, GameState2).

sow_seeds(Pit, SeedsNumber0, GameState0, GameState3) :-
  SeedsNumber0 > (12-Pit),
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
    SeedsNumber0 > (6-Pit),
    collect_seeds(Pit, PlayerPits0, PlayerPits1),
    NextPit is Pit+1,
    distribute_seeds(NextPit, SeedsNumber0, PlayerPits1, PlayerPits2),
    PlayerHouse1 is PlayerHouse0+1, % Add a seed to the house
    SeedsNumber1 is SeedsNumber0-6+Pit.

% In this case it is possible that the opponent's board may change (in case of
% opponent's seeds capturing).
% Set the number of IntermediateSeedsNumber equal to 0.
sow_seeds_player_side(Pit, SeedsNumber, GameState0, 0, GameState2) :-
    GameState0 = game_state(board(PlayerPits0, PlayerHouse0), board(OpponentPits0, OpponentHouse), CurrentPlayer),
    GameState1 = game_state(board(PlayerPits2, PlayerHouse0), board(OpponentPits0, OpponentHouse), CurrentPlayer),
    SeedsNumber < (6-Pit),
    collect_seeds(Pit, PlayerPits0, PlayerPits1),
    NextPit is Pit+1,
    distribute_seeds(NextPit, SeedsNumber, PlayerPits1, PlayerPits2),
    check_if_beatable(Pit, SeedsNumber, GameState1, GameState2).

sow_seeds_player_side(Pit, SeedsNumber, GameState0, 0, GameState1) :-
    GameState0 = game_state(board(PlayerPits0, PlayerHouse0), OpponentBoard, CurrentPlayer),
    GameState1 = game_state(board(PlayerPits2, PlayerHouse1), OpponentBoard, CurrentPlayer),
    SeedsNumber =:= 6-Pit,
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
    SeedsNumber =< 6,
    distribute_seeds(0, SeedsNumber, OpponentPits0, OpponentPits1).

sow_seeds_opponent_side(SeedsNumber, GameState0, SeedsNumber1, GameState1) :-
    GameState0 = game_state(PlayerBoard, board(OpponentPits0, OpponentHouse), CurrentPlayer),
    GameState1 = game_state(PlayerBoard, board(OpponentPits1, OpponentHouse), CurrentPlayer),
    SeedsNumber > 6,
    distribute_seeds(0, SeedsNumber, OpponentPits0, OpponentPits1),
    SeedsNumber1 is SeedsNumber-6.

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
  OppositePit is 5 - EndPit,
  nth0(OppositePit, OpponentPits0, SeedsOnOppositePit), % check if the opposite pit contains any seeds
  SeedsOnOppositePit > 0, !,
  collect_seeds(EndPit, PlayerPits0, PlayerPits1), % collect the one seed from the EndPit
  collect_seeds(OppositePit, OpponentPits0, OpponentPits1), % collect the seeds from the oppononest opposite pits
  PlayerHouse1 is PlayerHouse0 + 1 + SeedsOnOppositePit.

check_if_beatable(_, _, GameState, GameState) :- !.

% Check if the number of moves would be 1 or more (at least 2)
more_turns(Pit, SeedsNumber) :- 0 is mod((SeedsNumber-6+Pit), 13).

% True if PlayerPits or OpponentPits is empty (contains only zeroes).
game_over(GameState) :-
  GameState = game_state(board(PlayerPits, PlayerHouse0), board(OpponentPits, OpponentHouse0), CurrentPlayer),
  some_pits_are_empty(PlayerPits, OpponentPits),
  move_seeds_to_house(OpponentPits, OpponentHouse0, OpponentHouse1),
  move_seeds_to_house(PlayerPits, PlayerHouse0, PlayerHouse1),
  winner(CurrentPlayer, PlayerHouse1, OpponentHouse1, Winner),
  display_winner_information(Winner), !.

% Checks if the list contains anything else then 0
pits_are_empty([]).
pits_are_empty([0|T]) :- pits_are_empty(T).

% Checks if either of the players has empty pits
some_pits_are_empty(Pits1, _) :- pits_are_empty(Pits1).
some_pits_are_empty(_, Pits2) :- pits_are_empty(Pits2).

% A silly strategy for the bot: choose the first pit which is not empty
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
