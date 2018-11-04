/*
* Functions used for displaying purposes.
* Used to display the board and the seeds in every of the pits.
*/

display_board(board(PlayerPits, PlayerHouse), board(OpponentPits, OpponentHouse)) :-
  reverse(OpponentPits, OpponentPitsReversed), % write the opponents board part in the reverse order
  display_pits(OpponentPitsReversed),
  display_houses(PlayerHouse, OpponentHouse),
  display_pits(PlayerPits).

% Display number of seeds in every pit
display_pits([P|Ps]) :- tab(2), write(P), display_pits(Ps).
display_pits([]) :- nl.

% Display the number of seeds in both houses
display_houses(PlayerHouse, OpponentHouse) :- write(OpponentHouse), tab(18), write(PlayerHouse), nl.

% Reverse the given list (example: [1,2,3] -> [3,2,1])
reverse([], []).
reverse([H|T], ReversedList) :- reverse(T, ReversedT), append(ReversedT, [H], ReversedList).

/*
* Functions used to handle the user input.
* TODO
*/

/*
* Functions to display information about the winner.
* TODO
*/
display_winner_information(remis) :- write('Remis').
display_winner_information(Winner) :- write('And the winner is '), write(Winner), nl.
