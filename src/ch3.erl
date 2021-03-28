-module(ch3).
-compile(export_all).
-include("assert.hrl").


% 3.1
area(Shape, A, B) ->
  case Shape of
    rectangle -> A * B;
    triangle  -> A * B / 2.0;
    ellipse   -> math:pi() * A * B
  end.


% 3.2
area_b(rectangle, A, B) when A > 0, B > 0 -> A * B;
area_b(triangle , A, B) when A > 0, B > 0 -> A * B / 2.0;
area_b(ellipse  , A, B) when A > 0, B > 0 -> math:pi() * A * B.


% 3.3
area_c(rectangle, A, B) when A > 0, B > 0 -> A * B;
area_c(triangle , A, B) when A > 0, B > 0 -> A * B / 2.0;
area_c(ellipse  , A, B) when A > 0, B > 0 -> math:pi() * A * B;
area_c(_        , _, _)                   -> 0.


% 3.4
area({Shape, A, B}) -> area_c(Shape, A, B).


% Tests:
test() ->
  ?assertEqual(12                , area(rectangle, 3, 4)),
  ?assertEqual(7.5               , area(triangle , 3, 5)),
  ?assertEqual(25.132741228718345, area(ellipse  , 2, 4)),
  ?assertException(error,
                   function_clause,
                   area_b(square, -1, 3)),
  ?assertEqual(21               , area({rectangle, 7, 3})),
  ?assertEqual(10.5             , area({triangle , 7, 3})),
  ?assertEqual(65.97344572538566, area({ellipse  , 7, 3})).