/*
* Functions used for the pit selection, done by the bot.
*/

:- module(ai, [find_best_move/3]).
:- use_module(basic_rules).
:- use_module(utils).

depth(4).

% List of IDs of all pits on the board
find_best_move(board(PlayerPits, PlayerHouse), OpponentBoard, BestPit) :-
  boardSize(BoardSize),
  consecutive(BoardSize, InitialList),
  all_available_pits(InitialList, PlayerPits, [], CorrectPits),
  depth(Depth),
  % CorrectPits - pits that contain any seeds. Thus, they can be selected in a moves
  select_best_move_from_list(CorrectPits, board(PlayerPits, PlayerHouse), OpponentBoard,
    Depth, minimize, -1, -9999, BestPit, BestValue).

% all_available_pits([IDs of all pits], OpponentPits, SelectedPits, ResultPits)
% Pits contains a list of all available pits, that can be chosen by the bot
% (which are not empty and place on the bot's board part)
all_available_pits([], [], ResultPits, ResultPits) :- !.
all_available_pits([L|Ls], [Z|Zs], ResultPits, FinalPits) :-
  Z > 0,
  append(ResultPits, [L], ResultPits1),
  !,
  all_available_pits(Ls, Zs, ResultPits1, FinalPits).
all_available_pits([L|Ls], [Z|Zs], ResultPits, FinalPits) :-
  all_available_pits(Ls, Zs, ResultPits, FinalPits).

% Function that analyzes all possible moves made by selecting the pits from the lists
select_best_move_from_list([], PlayerBoard, OpponentBoard, Depth, Type, Pit, Value, Pit, Value).

select_best_move_from_list([Pit|Pits], PlayerBoard, OpponentBoard, Depth, Type,
  CurrentPit, CurrentValue, BestPit, BestValue) :-
    % Simulate the move
    seeds_number(Pit, PlayerBoard, SeedsNumber),
    % Check if the move would end in players house - if so, apply another move (if game not over)
    more_turns(Pit, SeedsNumber),
    sow_seeds(Pit, SeedsNumber, PlayerBoard, OpponentBoard, NewPlayerBoard, NewOpponentBoard),
    !,
    minimax(NewPlayerBoard, NewOpponentBoard, Depth, Type, Type, P, Value),
    compare(Pit, Value, CurrentPit, CurrentValue, CurrentPit1, CurrentValue1),
    select_best_move_from_list(Pits, PlayerBoard, OpponentBoard, Depth, Type,
      CurrentPit1, CurrentValue1, BestPit, BestValue).

select_best_move_from_list([Pit|Pits], PlayerBoard, OpponentBoard, Depth, Type,
  CurrentPit, CurrentValue, BestPit, BestValue) :-
    % Simulate the move
    seeds_number(Pit, PlayerBoard, SeedsNumber),

    % Check if the move would end in players house - if so, apply another move (if game not over)
    sow_seeds(Pit, SeedsNumber, PlayerBoard, OpponentBoard, NewPlayerBoard, NewOpponentBoard),
    !,
    switch_type(Type, NextType),
    minimax(NewOpponentBoard, NewPlayerBoard, Depth, Type, NextType, P, Value),
    compare(Pit, Value, CurrentPit, CurrentValue, CurrentPit1, CurrentValue1),
    select_best_move_from_list(Pits, PlayerBoard, OpponentBoard, Depth, Type,
      CurrentPit1, CurrentValue1, BestPit, BestValue).


switch_type(minimize, maximize).
switch_type(maximize, minimize).


minimax(board(PlayerPits, PlayerHouse), OpponentBoard, Depth, Type, NextType, Pit, Value) :-
  Depth > 0,
  % Get the list of currently available pits
  boardSize(BoardSize),
  consecutive(BoardSize, InitialList),
  all_available_pits(InitialList, PlayerPits, [], CorrectPits),
  % Prove that list is not empty
  nth0(0, CorrectPits, _),
  Depth1 is Depth - 1,
  select_best_move_from_list(CorrectPits, board(PlayerPits, PlayerHouse), OpponentBoard,
  Depth1, NextType, -1, -9999, Pit, Value).

  % In case of Depth == 0 or the CorrectPits list is empty
minimax(PlayerBoard, OpponentBoard, Depth, maximize, NextType, Pit, Value) :-
  evaluate(PlayerBoard, OpponentBoard, Val),
  Value is Val.

minimax(PlayerBoard, OpponentBoard, Depth, minimize, NextType, Pit, Value) :-
  evaluate(PlayerBoard, OpponentBoard, Val),
    Value is (-1) * Val.

evaluate(board(Pits, House), board(Pits2, House2), Value) :-
  sum_list(Pits, PitsSum),
  sum_list(Pits2, PitsSum2),
  Value is 2*(House - House2) + 0*(PitsSum - PitsSum2).


% Compares the give Pit and Value with the Currently best Pit and Value
% and returns the pair with higher Value
compare(Pit, Value, CurrentPit, CurrentValue, CurrentPit, CurrentValue) :-
  Value =< CurrentValue.
compare(Pit, Value, CurrentPit, CurrentValue, Pit, Value) :-
  Value > CurrentValue.
