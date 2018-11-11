:- use_module(basic_rules).
:- use_module(io_functions).
:- use_module(ai).

play :- initialize(GameState),
        play(GameState).

% Initialize an empty mancala board with 6 pits (with 4 seeds in each of them) per player
% side and with 2 empty houses.
initialize(game_state(board([4,4,4,4,4,4], 0), board([4,4,4,4,4,4], 0), player)).

% Check if any of the players won. If not then:
% 1. Select a start pit to take seeds to sow from.
% 2. Make the move (sow seeds to consecutive pits). (? Display the board after the move)
% 3. Switch player and play recursively.
play(GameState0) :-
  select_and_make_move(GameState0, GameState1),
  switch_player(GameState1, GameState2),
  !,
  play(GameState2).

select_and_make_move(GameState0, GameState1) :-
  display_board(GameState0),
  not(game_over(GameState0)),
  select_pit(GameState0, Pit),
  make_move(Pit, GameState0, GameState1).

% Selects the pit to move seeds from.
% Depeding on the CurrentPlayer (whether it is players or computers turn) it waits for
% the user input (and checks its correctness) or selects the best pit for the move.
select_pit(GameState, Pit) :-
  GameState = game_state(PlayerBoard, _, player),
  nl, writeln(['Select pit']), read_pit(Pit, seeds_number, PlayerBoard), !.

% A silly strategy: choose the first non-empty pit
select_pit(GameState, Pit) :-
  GameState = game_state(_, _, bot),
  nl, writeln(['Opponent\'s turn']),
  find_best_move(GameState, Pit),
  write('Opponent selected: '),
  writeln(Pit).

% make_move(Pit, GameState0, GameState2)
% Pit - the ID of the selected start pit (Pits are numbered from 0 to 5).
% PlayerBoard, OpponentBoard - initial state of the game board
% NewPlayerBoard, NewOpponentBoard - final state of game board, after distributing
% the seeds
% Check if finished in house and whether there should be another move for this player
make_move(Pit, GameState0, GameState2) :-
  GameState0 = game_state(PlayerBoard0, _, _),
  seeds_number(Pit, PlayerBoard0, SeedsNumber),
  % Check if the move would end in players house - if so, apply another move (if game not over)
  more_turns(Pit, SeedsNumber),
  sow_seeds(Pit, SeedsNumber, GameState0, GameState1),
  !,
  select_and_make_move(GameState1, GameState2).

make_move(Pit, GameState0, GameState1) :-
  GameState0 = game_state(PlayerBoard0, _, _),
  seeds_number(Pit, PlayerBoard0, SeedsNumber),
  sow_seeds(Pit, SeedsNumber, GameState0, GameState1).
