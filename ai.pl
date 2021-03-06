/*
* Functions used for the pit selection, done by the bot.
*/

:- module(ai, [find_best_move/4]).
:- use_module(basic_rules).
:- use_module(utils).

depth(5).

% List of IDs of all pits on the board
find_best_move(GameState, BestPit, HousesWeight, BoardsWeight) :-
  GameState = game_state(board(PlayerPits, _), _, _),
  boardSize(BoardSize),
  consecutive(BoardSize, InitialList),
  all_available_pits(InitialList, PlayerPits, [], CorrectPits),
  depth(Depth),
  % CorrectPits - pits that contain any seeds. Thus, they can be selected in a moves
  select_best_move_from_list(CorrectPits, GameState,
    Depth, minimize, -1, -9999, BestPit, _, HousesWeight, BoardsWeight).

% all_available_pits([IDs of all pits], OpponentPits, SelectedPits, ResultPits)
% Pits contains a list of all available pits, that can be chosen by the bot
% (which are not empty and place on the bot's board part)
all_available_pits([], [], ResultPits, ResultPits) :- !.
all_available_pits([L|Ls], [Z|Zs], ResultPits, FinalPits) :-
  Z > 0,
  append(ResultPits, [L], ResultPits1),
  !,
  all_available_pits(Ls, Zs, ResultPits1, FinalPits).

all_available_pits([_|Ls], [_|Zs], ResultPits, FinalPits) :-
  all_available_pits(Ls, Zs, ResultPits, FinalPits).

% Function that analyzes all possible moves made by selecting the pits from the lists
select_best_move_from_list([], _, _, _, Pit, Value, Pit, Value, _, _).

select_best_move_from_list([Pit|Pits], GameState0, Depth, Type,
  CurrentPit0, CurrentValue0, BestPit, BestValue, HousesWeight, BoardsWeight) :-
    GameState0 = game_state(PlayerBoard, _, _),
    % Simulate the move
    seeds_number(Pit, PlayerBoard, SeedsNumber),
    % Check if the move would end in players house - if so, apply another move (if game not over)
    more_turns(Pit, SeedsNumber),
    sow_seeds(Pit, SeedsNumber, GameState0, GameState1),
    !,
    minimax(GameState1, Depth, Type, Type, _, Value, HousesWeight, BoardsWeight),
    compare(Pit, Value, CurrentPit0, CurrentValue0, CurrentPit1, CurrentValue1),
    select_best_move_from_list(Pits, GameState0, Depth, Type,
      CurrentPit1, CurrentValue1, BestPit, BestValue, HousesWeight, BoardsWeight).

select_best_move_from_list([Pit|Pits], GameState0, Depth, Type,
  CurrentPit0, CurrentValue0, BestPit, BestValue, HousesWeight, BoardsWeight) :-
    GameState0 = game_state(PlayerBoard0, _, _),
    GameState1 = game_state(PlayerBoard1, OpponentBoard1, CurrentPlayer1),
    GameState2 = game_state(OpponentBoard1, PlayerBoard1, CurrentPlayer1),
    % Simulate the move
    seeds_number(Pit, PlayerBoard0, SeedsNumber),
    sow_seeds(Pit, SeedsNumber, GameState0, GameState1),
    !,
    switch_type(Type, NextType),
    minimax(GameState2, Depth, Type, NextType, _, Value, HousesWeight, BoardsWeight),
    compare(Pit, Value, CurrentPit0, CurrentValue0, CurrentPit1, CurrentValue1),
    select_best_move_from_list(Pits, GameState0, Depth, Type,
      CurrentPit1, CurrentValue1, BestPit, BestValue, HousesWeight, BoardsWeight).


switch_type(minimize, maximize).
switch_type(maximize, minimize).


minimax(GameState, Depth, _, NextType, Pit, Value, HousesWeight, BoardsWeight) :-
  GameState = game_state(board(PlayerPits, _), _, _),
  Depth > 0,
  % Get the list of currently available pits
  boardSize(BoardSize),
  consecutive(BoardSize, InitialList),
  all_available_pits(InitialList, PlayerPits, [], CorrectPits),
  % Prove that list is not empty
  nth0(0, CorrectPits, _),
  Depth1 is Depth - 1,
  select_best_move_from_list(CorrectPits, GameState,
  Depth1, NextType, -1, -9999, Pit, Value, HousesWeight, BoardsWeight).

  % In case of Depth == 0 or the CorrectPits list is empty
minimax(GameState, _, maximize, _, _, Value, HousesWeight, BoardsWeight) :-
  evaluate(GameState, Val, HousesWeight, BoardsWeight),
  Value is Val.

minimax(GameState, _, minimize, _, _, Value, HousesWeight, BoardsWeight) :-
  evaluate(GameState, Val, HousesWeight, BoardsWeight),
    Value is (-1) * Val.

evaluate(GameState, Value, _, _) :-
  GameState = game_state(board(PlayerPits, PlayerHouse), board(OpponentPits, OpponentHouse), _),
  sum_list(PlayerPits, PlayerPitsSum),
  sum_list(OpponentPits, OpponentPitsSum),
  Value is 1*(PlayerHouse - OpponentHouse) + 0*(PlayerPitsSum - OpponentPitsSum).


% Compares the give Pit and Value with the Currently best Pit and Value
% and returns the pair with higher Value
compare(_, Value, CurrentPit, CurrentValue, CurrentPit, CurrentValue) :-
  Value =< CurrentValue.
compare(Pit, Value, _, CurrentValue, Pit, Value) :-
  Value > CurrentValue.
