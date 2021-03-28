-module(ch4).
-compile(export_all).
-include("assert.hrl").


% 4.1
area(Shape, A, B) when A > 0, B > 0 ->
  case Shape of
    rectangle -> A * B;
    triangle  -> A * B / 2.0;
    ellipse   -> math:pi() * A * B
  end.


% 4.2
gcd(M, N) when M == N -> M;
gcd(M, N) when M > N  -> gcd(M - N, N);
gcd(M, N)             -> gcd(M    , N - M).


% 4.3
%raise(_, 0)            -> 1;
%raise(X, 1)            -> X;
%raise(X, N) when N > 0 ->  X * raise(X, N - 1);
%raise(X, N) when N < 0 -> 1.0 / raise(X, -N).


% 4.4
% new definition of 'raise'
raise(X, N) when N > 0 -> raise(X, N, 1);
raise(_, 0) -> 1;
raise(X, N) when N < 0 -> 1.0 / raise(X, -N).

raise(_, 0, Acc) -> Acc;
raise(X, N, Acc) -> raise(X, N - 1, X * Acc).


% 4.5
nth_root(X, N) -> nth_root(X, N, X / 2.0).

nth_root(X, N, A) ->
  io:format("Current guess is ~p~n", [A]),
  F      = raise(A, N) - X,
  Fprime = N * raise(A, N - 1),
  Next   = A - F / Fprime,
  Change = abs(Next - A),
  case Change < 1.0e-8 of
    true  -> Next;
    false -> nth_root(X, N, Next)
  end.


% Tests:
test() ->
  ?assertEqual(12                , area(rectangle, 3, 4)),
  ?assertEqual(7.5               , area(triangle , 3, 5)),
  ?assertEqual(25.132741228718345, area(ellipse  , 2, 4)),
  ?assertEqual(4 , gcd(12 , 8)),
  ?assertEqual(7 , gcd(14 , 21)),
  ?assertEqual(1 , gcd(125, 46)),
  ?assertEqual(12, gcd(120, 36)),
  ?assertEqual(5    , raise(5  , 1)),
  ?assertEqual(8    , raise(2  , 3)),
  ?assertEqual(1.728, raise(1.2, 3)),
  ?assertEqual(1    , raise(2, 0)),
  ?assertEqual(0.125, raise(2, -3)),
  ?assertEqual(3.0, nth_root(27, 3)).
