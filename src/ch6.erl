-module(ch6).
-compile(export_all).
-include("assert.hrl").


% 6.1
minimum([X | Xs]) -> minimum(Xs, X).

minimum([]      , Min) -> Min;
minimum([X | Xs], Min) ->
  case X < Min of
    true  -> minimum(Xs, X);
    false -> minimum(Xs, Min)
  end.


% 6.2
maximum([X | Xs]) -> maximum(Xs, X).

maximum([]      , Max) -> Max;
maximum([X | Xs], Max) ->
  case X > Max of
    true  -> maximum(Xs, X);
    false -> maximum(Xs, Max)
  end.

range(Xs) ->
  Min = minimum(Xs),
  Max = maximum(Xs),
  {Min, Max}.


% 6.3
% input: "yyyy-mm-dd"
date_parts(X) ->
  Xs = re:split(X, "-", [{return, list}]),
  [ element(1, string:to_integer(X)) || X <- Xs].

% input: "yyyy-mm-dd"
julian(X) ->
  DaysPerMonth = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31],
  [Y, M, D]    = date_parts(X),
  case leap_year(Y) andalso (M >=3) of
    true  -> julian(M, DaysPerMonth, D + 1);
    false -> julian(M, DaysPerMonth, D)
  end.

leap_year(Year) ->
  (Year rem 4 == 0 andalso Year rem 100 /= 0)
  orelse (Year rem 400 == 0).

julian(1, _       , Acc) -> Acc;
julian(M, [X | Xs], Acc) -> julian(M - 1, Xs, Acc + X).


% 6.4
alert(Xxs) -> alert(Xxs, 1, []).

alert([]        , _, Acc) -> lists:reverse(Acc);
alert([Xs | Xss], I, Acc) ->
  case threshold(Xs) of
    true  -> alert(Xss, I + 1, [I | Acc]);
    false -> alert(Xss, I + 1, Acc)
   end.

% returns true if any value in Xs >= 4, and false otherwise
threshold(Xs) ->
  F = fun(X) -> X >= 4 end,
  length(lists:filter(F, Xs)) >= 1.


% 6.5
generate_teeth(Xs, P) -> generate_teeth(Xs, P, []).
% Xs: list of teeth as a string consisting of "T" and "F" characters
% P: probability that a tooth is good

generate_teeth([]      , _, Acc) -> lists:reverse(Acc);
generate_teeth([X | Xs], P, Acc) ->
  case X of
    $T -> Tooth = generate_tooth(P),
           generate_teeth(Xs, P, [Tooth | Acc]);
    $F -> generate_teeth(Xs, P, [[0] | Acc])
  end.

generate_tooth(P) ->
  Q = rand:uniform(1),
  Base =
    case Q < P of
      true  -> 2;
      false -> 3
    end,
  generate_tooth(Base, 6, []).

generate_tooth(_   , 0   , Acc) -> Acc;
generate_tooth(Base, Left, Acc) ->
  Val = rand:uniform(3), % generates 1, 2, or 3
  generate_tooth(Base, Left - 1, [ Base + (Val - 2) | Acc]).


% Tests:
tests() ->
  Xs = [4, 1, 7, -17, 8, 2, 5],
  ?assertEqual(     -17, minimum(Xs)),
  ?assertEqual(       8, maximum(Xs)),
  ?assertEqual({-17, 8}, range(Xs)),
  ?assertEqual(     366, julian("2012-12-31")),
  ?assertEqual(     365, julian("2013-12-31")),
  ?assertEqual(      36, julian("2012-02-05")),
  ?assertEqual(      36, julian("2013-02-05")),
  ?assertEqual(      60, julian("1900-03-01")),
  ?assertEqual(      61, julian("2000-03-01")),
  ?assertEqual(       1, julian("2013-01-01")),
  PocketDepths =
   [[0]          , [2,2,1,2,2,1], [3,1,2,3,2,3],
    [3,1,3,2,1,2], [3,2,3,2,2,1], [2,3,1,2,1,1],
    [3,1,3,2,3,2], [3,3,2,1,3,1], [4,3,3,2,3,3],
    [3,1,1,3,2,2], [4,3,4,3,2,3], [2,3,1,3,2,2],
    [1,2,1,1,3,2], [1,2,2,3,2,3], [1,3,2,1,3,3],
    [0]          , [3,2,3,1,1,2], [2,2,1,1,3,2],
    [2,1,1,1,1,2], [3,3,2,1,1,3], [3,1,3,2,3,2],
    [3,3,1,2,3,3], [1,2,2,3,3,3], [2,2,3,2,3,3],
    [2,2,2,4,3,4], [3,4,3,3,3,4], [1,1,2,3,1,2],
    [2,2,3,2,1,3], [3,4,2,4,4,3], [3,3,2,1,2,3],
    [2,2,2,2,3,3], [3,2,3,2,3,2]],
  ?assertEqual([9,11,25,26,29], alert(PocketDepths)).