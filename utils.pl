/*
* General-use unctions
*/
:- module(utils, [
  reverse/2,
  consecutive/2
]).

% Reverse the given list (example: [1,2,3] -> [3,2,1])
reverse([], []).
reverse([H|T], ReversedList) :- reverse(T, ReversedT), append(ReversedT, [H], ReversedList).

% Get list of N consecutive numbers (from 0 to N-1)
consecutive(N, List) :- consecutive(0, N-1, List).
consecutive(N, Max, List) :- (
  N > Max ->
  List = []
  ; N1 is N+1,
  List = [N|List1],
  consecutive(N1, Max, List1)
).
