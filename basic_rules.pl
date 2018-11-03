/*
* Functions used to distribute seeds into pits.
*/

% seeds_amount(N, Board, SeedsAmount) - for the given Board (list of 6 Pits)
% it returns the amount of seeds in the Nth pit and stores it under SeedsAmount
% TODO
seeds_amount(N, Board, SeedsAmount).

% sow_seeds(N, SeedsAmount, PlayerBoard, OpponentBoard, NewPlayerBoard, NewOpponentBoard)
% Distribute the seeds from Nth pit to the consecutive pits on player and opponent
% boards (the seeds distribution direction should be counter-clockwise).
% TODO
sow_seeds(Pit, SeedsAmount, PlayerBoard, OpponentBoard, NewPlayerBoard, NewOpponentBoard, NewTurn) :-
  sow_seeds_player_side(Pit,
    SeedsAmount, PlayerBoard, OpponentBoard,
    IntermediateSeedsAmount, IntermediatePlayerBoard, IntermediateOpponentBoard,
    FinishedInEmptyPit, EmptyPit),
  sow_seeds_opponent_side(
    FinishedInEmptyPit, EmptyPit,
    IntermediateSeedsAmount, IntermediatePlayerBoard, IntermediateOpponentBoard,
    NewPlayerBoard, NewOpponentBoard).

% sow_seeds_player_side(Pit,
%                       SeedsAmount, PlayerBoard, OpponentBoard,
%                       IntermediateSeedsAmount, IntermediatePlayerBoard, IntermediateOpponentBoard),
% Distribute the seeds to consecutive pits and the house.
% If the last seed was placed in an empty pit - collect it and all seeds from
% the opposite pit and place them in player's house.
% If the last seed was placed in the house - there should be another turn for the player.
%TODO
sow_seeds_player_side(Pit,
  SeedsAmount, PlayerBoard, OpponentBoard,
  IntermediateSeedsAmount, IntermediatePlayerBoard, IntermediateOpponentBoard,
  FinishedInEmptyPit, EmptyPit).

% sow_seeds_opponent_side(IntermediateSeedsAmount, IntermediatePlayerBoard, IntermediateOpponentBoard, NewPlayerBoard, NewOpponentBoard).
% Distribute the seeds to consecutive pits, skipping the house.
% If the sow_seeds_player_side finished in empty pit - collect the seeds from
% "opposite" pit and place them in player's house.
%TODO
sow_seeds_opponent_side(
  FinishedInEmptyPit, EmptyPit,
  IntermediateSeedsAmount, IntermediatePlayerBoard, IntermediateOpponentBoard,
  NewPlayerBoard, NewOpponentBoard).
