-module(ch7).
-compile(export_all).
-include("assert.hrl").


% 7.1
derivative(F, X) ->
  Delta = 1.0e-10,
  (F(X + Delta) - F(X)) / Delta.


% 7.2
extract(Xs) ->
  [ {Name, Sex, Age} || {Name, Sex, Age} <- Xs, Sex == $M, Age > 40].

extract_b(Xs) ->
  [ {Name, Sex, Age} || {Name, Sex, Age} <- Xs, (Sex == $M) or (Age > 40)].


% 7.3
sum(Xs) -> lists:foldl(fun(X, Y) -> X + Y end, 0, Xs). % lists:sum/1

mean(Xs) -> sum(Xs) / length(Xs).

stdv(Xs) ->
  N          = length(Xs),
  Sum        = sum(Xs),
  SumSquares = sum(lists:map(fun(X) -> X * X  end, Xs)),
  math:sqrt(((N * SumSquares) - (Sum * Sum)) / (N * (N - 1))).


% 7.4
% input: "yyyy-mm-dd"
date_parts(X) ->
  Xs = re:split(X, "-", [{return, list}]),
  [ element(1, string:to_integer(X)) || X <- Xs].

% input: "yyyy-mm-dd"
julian(X) ->
  DaysPerMonth = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31],
  [Y, M, D]    = date_parts(X),
  { Months, _ } = lists:split(M - 1, DaysPerMonth),
  sum(Months) + D +
    case leap_year(Y) andalso (M >= 3) of
      true  -> 1 ;
      false -> 0
    end.

leap_year(Year) ->
  (Year rem 4 == 0 andalso Year rem 100 /= 0)
  orelse (Year rem 400 == 0).


% 7.5
make_deck() ->
  Suits  = ["Club", "Diamonds", "Hearts", "Spades"],
  Values = ["A", 2, 3, 4, 5, 6, 7, 8, 9, 10, "J", "Q", "K"],
  [ {X, Y} || X <- Values, Y <- Suits ].


% Tests:
tests() ->
  ?assertEqual(6.000000496442226
               , derivative(fun(X) -> X * X end, 3)),
  ?assertEqual(32.00000264769187
               , derivative(fun(X) -> 3 * X * X + 2 * X + 1 end, 5)),
  ?assertEqual(1.0
               , derivative(fun math:sin/1, 0)),
  People = [{"Federico", $M, 22}, {"Kim", $F, 45}, {"Hansa", $F, 30},
            {"Tran", $M, 47}, {"Cathy", $F, 32}, {"Elias", $M, 50}],
  ?assertEqual([{"Tran", $M, 47}, {"Elias", $M, 50}], extract(People)),
  ?assertEqual([{"Federico", $M, 22}, {"Kim", $F, 45}, {"Tran", $M, 47},
                {"Elias", $M, 50}]
               , extract_b(People)),
  ?assertEqual(6.0
              , mean([7, 2, 9])),
  ?assertEqual(3.605551275463989
              , stdv([7, 2, 9])),
  ?assertEqual(     366, julian("2012-12-31")),
  ?assertEqual(     365, julian("2013-12-31")),
  ?assertEqual(      36, julian("2012-02-05")),
  ?assertEqual(      36, julian("2013-02-05")),
  ?assertEqual(      60, julian("1900-03-01")),
  ?assertEqual(      61, julian("2000-03-01")),
  ?assertEqual(       1, julian("2013-01-01")).