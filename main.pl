:- [io_functions].
:- [basic_rules].
:- [ai].

play :- initialize(PlayerBoard, OpponentBoard, CurrentPlayer),
        display_board(PlayerBoard, OpponentBoard),
        play(PlayerBoard, OpponentBoard, CurrentPlayer).

% Initialize an empty mancala board with 6 pits (with 4 seeds in each of them) per player
% side and with 2 empty houses.
initialize(board_player([4, 4, 4, 4, 4, 4], 0), board_opponent([4, 4, 4, 4, 4, 4], 0), player).

% Check if any of the players won. If not then:
% 1. Select a start pit to take seeds to sow from.
% 2. Make the move (sow seeds to consecutive pits). (? Display the board after the move)
% 3. Switch player and play recursively.
% TODO
play(PlayerBoard, OpponentBoard, CurrentPlayer) :-
  select_pit(PlayerBoard, OpponentBoard, CurrentPlayer, Pit),
  make_move(Pit, CurrentPlayer, PlayerBoard, OpponentBoard, NewPlayerBoard, NewOpponentBoard),
  switch_player(CurrentPlayer, NextPlayer),
  !,
  play(NewPlayerBoard, NewOpponentBoard, NextPlayer).

% switch_player(CurrentPlayer, NextPlayer)
switch_player(bot, player).
switch_player(player, bot).

% Selects the pit to move seeds from.
% Depeding on the CurrentPlayer (whether it is players or computers turn) it waits for
% the user input (and checks its correctness) or selects the best pit for the move.
% TODO
select_pit(PlayerBoard, OpponentBoard, CurrentPlayer, Pit).

% make_move(Pit, PlayerBoard, OpponentBoard, NewPlayerBoard, NewOpponentBoard)
% Pit - the ID of the selected start pit.
% PlayerBoard, OpponentBoard - initial state of the game board
% NewPlayerBoard, NewOpponentBoard - final state of game board, after distributing
% the seeds
% Check if finished in house and whether there should be another move for this player
% TODO
make_move(Pit, CurrentPlayer, PlayerBoard, OpponentBoard, NewPlayerBoard, NewOpponentBoard) :-
  seeds_amount(Pit, PlayerBoard, SeedsAmount),
  sow_seeds(Pit, SeedsAmount, PlayerBoard, OpponentBoard, NewPlayerBoard, NewOpponentBoard, NewTurn),
  next_move(NewTurn),
  !,
  select_pit(NewPlayerBoard, NewOpponentBoard, CurrentPlayer, NewPit),
  make_move(NewPit, CurrentPlayer, NewOpponentBoard, NewPlayerBoard, FinalPlayerBoard, FinalOpponentBoard).
