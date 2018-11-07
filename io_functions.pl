/*
* Functions used for displaying purposes.
* Used to display the board and the seeds in every of the pits.
*/
:- module(io_functions, [
  display_board/3,
  read_pit/3,
  display_winner_information/1
]).

:- use_module(utils).

display_board(bot, board(PlayerPits, PlayerHouse), board(OpponentPits, OpponentHouse)) :-
  display_board(player, board(OpponentPits, OpponentHouse), board(PlayerPits, PlayerHouse)).

display_board(player, board(PlayerPits, PlayerHouse), board(OpponentPits, OpponentHouse)) :-
  reverse(OpponentPits, OpponentPitsReversed), % write the opponents board part in the reverse order
  display_pits(OpponentPitsReversed),
  display_houses(PlayerHouse, OpponentHouse),
  display_pits(PlayerPits).

% Display number of seeds in every pit
display_pits([P|Ps]) :- tab(2), write(P), display_pits(Ps).
display_pits([]) :- nl.

% Display the number of seeds in both houses
display_houses(PlayerHouse, OpponentHouse) :- write(OpponentHouse), tab(18), write(PlayerHouse), nl.

/*
* Functions to display information about the winner.
* TODO
*/
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
