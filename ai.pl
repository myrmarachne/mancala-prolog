/*
* Functions used for the pit selection, done by the bot.
*/

:- module(ai, [find_best_move/2]).
:- use_module(basic_rules).
:- use_module(utils).

depth(4).

% List of IDs of all pits on the board
find_best_move(GameState, BestPit) :-
  GameState = game_state(_, board(OpponentPits, _), _),
  boardSize(BoardSize),
  consecutive(BoardSize, InitialList),
  all_available_pits(InitialList, OpponentPits, [], CorrectPits),
  depth(Depth),
  % CorrectPits - pits that contain any seeds. Thus, they can be selected in a moves
  select_best_move_from_list(CorrectPits, GameState, Depth, minimize, -1, -9999, BestPit, _).

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
select_best_move_from_list([], _, _, _, _, Pit, Value, Pit, Value).

select_best_move_from_list([Pit|Pits], GameState0, Depth, Type,
  CurrentPit0, CurrentValue0, BestPit, BestValue) :-
    GameState0 = game_state(PlayerBoard, _, _),
    % Simulate the move
    writeln(1),
    seeds_number(Pit, PlayerBoard, SeedsNumber),
    writeln(2),
    % Check if the move would end in players house - if so, apply another move (if game not over)
    more_turns(Pit, SeedsNumber),
    writeln(3),
    sow_seeds(Pit, SeedsNumber, GameState0, GameState1),
    writeln(4),
    !,
    minimax(GameState1, Depth, Type, Type, _, Value),
    writeln(5),
    compare(Pit, Value, CurrentPit0, CurrentValue0, CurrentPit1, CurrentValue1),
    writeln(6),
    select_best_move_from_list(Pits, GameState0, Depth, Type,
      CurrentPit1, CurrentValue1, BestPit, BestValue).

select_best_move_from_list([Pit|Pits], GameState0, Depth, Type,
  CurrentPit0, CurrentValue0, BestPit, BestValue) :-
    GameState0 = game_state(PlayerBoard0, _, _),
    GameState1 = game_state(PlayerBoard1, OpponentBoard1, CurrentPlayer1),
    GameState2 = game_state(OpponentBoard1, PlayerBoard1, CurrentPlayer1),
    % Simulate the move
    seeds_number(Pit, PlayerBoard0, SeedsNumber),
    writeln(11),
    sow_seeds(Pit, SeedsNumber, GameState0, GameState1),
    writeln(12),
    !,
    switch_type(Type, NextType),
    writeln(13),
    minimax(GameState2, Depth, Type, NextType, _, Value),
    writeln(14),
    compare(Pit, Value, CurrentPit0, CurrentValue0, CurrentPit1, CurrentValue1),
    writeln(15),
    select_best_move_from_list(Pits, GameState0, Depth, Type,
      CurrentPit1, CurrentValue1, BestPit, BestValue).


switch_type(minimize, maximize).
switch_type(maximize, minimize).

% In case of Depth == 0
minimax(GameState, 0, maximize, _, _, Value) :-
  evaluate(GameState, Val),
  Value is Val.
minimax(GameState, 0, minimize, _, _, Value) :-
  evaluate(GameState, Val),
  Value is (-1) * Val.

minimax(GameState, Depth0, _, NextType, Pit, Value) :-
  GameState = game_state(_, board(OpponentPits, _), _),
  Depth0 > 0,
  % Get the list of currently available pits
  boardSize(BoardSize),
  consecutive(BoardSize, InitialList),
  all_available_pits(InitialList, OpponentPits, [], CorrectPits),
  Depth1 is Depth0 - 1,
  select_best_move_from_list(CorrectPits, GameState, Depth1, NextType, -1, -9999, Pit, Value).

evaluate(GameState, Value) :-
  GameState = game_state(board(PlayerPits, PlayerHouse), board(OpponentPits, OpponentHouse), _),
  sum_list(PlayerPits, PlayerPitsSum),
  sum_list(OpponentPits, OpponentPitsSum),
  Value is 2*(PlayerHouse - OpponentHouse) + 0*(PlayerPitsSum - OpponentPitsSum).


% Compares the give Pit and Value with the Currently best Pit and Value
% and returns the pair with higher Value
compare(_, Value, CurrentPit, CurrentValue, CurrentPit, CurrentValue) :-
  Value =< CurrentValue.
compare(Pit, Value, _, CurrentValue, Pit, Value) :-
  Value > CurrentValue.
