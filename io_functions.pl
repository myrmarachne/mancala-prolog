/*
* Functions used for displaying purposes.
* Used to display the board and the seeds in every of the pits.
*/
:- [utils].

display_board(game_state(PlayerBoard, OpponentBoard, bot)) :-
  display_board(game_state(OpponentBoard, PlayerBoard, player)).

display_board(GameState) :-
  GameState = game_state(board(PlayerPits, PlayerHouse), board(OpponentPits, OpponentHouse), _),
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
