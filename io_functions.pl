/*
* Functions used for displaying purposes.
* Used to display the board and the seeds in every of the pits.
*/
:- module(io_functions, [
  display_board/1,
  read_pit/3,
  display_winner_information/1
]).

:- use_module(utils).

display_board(game_state(PlayerBoard, OpponentBoard, bot)) :-
  display_board(game_state(OpponentBoard, PlayerBoard, player)).

display_board(GameState) :-
  GameState = game_state(board(PlayerPits, PlayerHouse), board(OpponentPits, OpponentHouse), player),
  reverse(OpponentPits, OpponentPitsReversed), % write the opponents board part in the reverse order
  display_pits(OpponentPitsReversed),
  display_houses(PlayerHouse, OpponentHouse),
  display_pits(PlayerPits).

% Display number of seeds in every pit
display_pits([P|Ps]) :- pit_tabs(P, Tabs), tab(Tabs), write(P), display_pits(Ps).
display_pits([]) :- nl.

pit_tabs(P, Tabs) :- P=<9, Tabs is 2.
pit_tabs(P, Tabs) :- P>9, Tabs is 1.

% Display the number of seeds in both houses
display_houses(PlayerHouse, OpponentHouse) :- write(OpponentHouse), tab(18), write(PlayerHouse), nl.

% Display information about the winner.
display_winner_information(tie) :- write('Tie').
display_winner_information(Winner) :- write('And the winner is '), write(Winner), nl.

% Functions used to handle the user input.
read_pit(Pit, CheckForZero, PlayerBoard) :-
    repeat,
        read(Pit),
        (   integer(Pit), 0 =< Pit, Pit =< 5,
            call(CheckForZero, Pit, PlayerBoard, SeedsNumber), SeedsNumber > 0
        ->  true, !
        ;   writeln('Please provide a valid number of non-empty field '),
            fail
        ).
