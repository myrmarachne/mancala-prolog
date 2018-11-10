:- use_module(basic_rules).
:- use_module(io_functions).
:- use_module(ai).


% TODO: moduły, stała 6

play :- initialize(PlayerBoard, OpponentBoard, CurrentPlayer),
        play(PlayerBoard, OpponentBoard, CurrentPlayer).

% Initialize an empty mancala board with 6 pits (with 4 seeds in each of them) per player
% side and with 2 empty houses.
initialize(board([4,4,4,4,4,4], 0), board([4,4,4,4,4,4], 0), player).

% Check if any of the players won. If not then:
% 1. Select a start pit to take seeds to sow from.
% 2. Make the move (sow seeds to consecutive pits). (? Display the board after the move)
% 3. Switch player and play recursively.
play(PlayerBoard, OpponentBoard, CurrentPlayer) :-
  select_and_make_move(PlayerBoard, OpponentBoard, CurrentPlayer, NewPlayerBoard, NewOpponentBoard),
  switch_player(CurrentPlayer, NextPlayer),
  !,
  % Change the order of Board (currently the player board becomes the opponent board etc)
  play(NewOpponentBoard, NewPlayerBoard, NextPlayer).

select_and_make_move(PlayerBoard, OpponentBoard, CurrentPlayer, NewPlayerBoard, NewOpponentBoard) :-
  display_board(CurrentPlayer, PlayerBoard, OpponentBoard),
  not(game_over(CurrentPlayer, PlayerBoard, OpponentBoard)),
  select_pit(PlayerBoard, OpponentBoard, CurrentPlayer, Pit),
  make_move(Pit, CurrentPlayer, PlayerBoard, OpponentBoard, NewPlayerBoard, NewOpponentBoard).

% Selects the pit to move seeds from.
% Depeding on the CurrentPlayer (whether it is players or computers turn) it waits for
% the user input (and checks its correctness) or selects the best pit for the move.
select_pit(PlayerBoard, _, player, Pit) :- nl, writeln(['Select pit']), read_pit(Pit, seeds_number, PlayerBoard).

% a silly strategy: choose the first non-empty pit
select_pit(PlayerBoard, OpponentBoard, bot, Pit) :-
  nl, writeln(['Opponent\'s turn']),
  find_best_move(PlayerBoard, OpponentBoard, Pit),
  write('Opponent selected: '),
  writeln(Pit).

% make_move(Pit, PlayerBoard, OpponentBoard, NewPlayerBoard, NewOpponentBoard)
% Pit - the ID of the selected start pit (Pits are numbered from 0 to 5).
% PlayerBoard, OpponentBoard - initial state of the game board
% NewPlayerBoard, NewOpponentBoard - final state of game board, after distributing
% the seeds
% Check if finished in house and whether there should be another move for this player
make_move(Pit, CurrentPlayer, PlayerBoard, OpponentBoard, FinalPlayerBoard, FinalOpponentBoard) :-
  seeds_number(Pit, PlayerBoard, SeedsNumber),
  % Check if the move would end in players house - if so, apply another move (if game not over)
  more_turns(Pit, SeedsNumber),
  sow_seeds(Pit, SeedsNumber, PlayerBoard, OpponentBoard, NewPlayerBoard, NewOpponentBoard),
  !,
  select_and_make_move(NewPlayerBoard, NewOpponentBoard, CurrentPlayer, FinalPlayerBoard, FinalOpponentBoard).

make_move(Pit, CurrentPlayer, PlayerBoard, OpponentBoard, FinalPlayerBoard, FinalOpponentBoard) :-
  seeds_number(Pit, PlayerBoard, SeedsNumber),
  sow_seeds(Pit, SeedsNumber, PlayerBoard, OpponentBoard, FinalPlayerBoard, FinalOpponentBoard).
