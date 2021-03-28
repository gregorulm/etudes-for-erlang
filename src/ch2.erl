-module(ch2).
-compile(export_all).
-include("assert.hrl").


% 2.1
% Computes area of a rectangle
area(L, W) ->
  L * W.


test() ->
  ?assertEqual(12, area(3,4)),
  ?assertEqual(84, area(12,7)).
