-module(ch8).
-compile(export_all).

make_deck() ->
  Suits  = ["Club", "Diamonds", "Hearts", "Spades"],
  Values = ["A", 2, 3, 4, 5, 6, 7, 8, 9, 10, "J", "Q", "K"],
  [ {X, Y} || X <- Values, Y <- Suits ].


shuffle(List)      -> shuffle(List, []).
shuffle([], Acc)   -> Acc;
shuffle(List, Acc) ->
  {Leading, [H | T]} = lists:split(rand:uniform(length(List)) - 1, List),
  shuffle(Leading ++ T, [H | Acc]).


player(Deck) ->
  io:format("~p holds:~n ~p~n", [self(), Deck]),
  receive
    {play, Pid} ->
      io:format("~p received 'play' signal~n", [self()]),
      case Deck of
        []          -> io:format("~p has no cards left~n", [self()]),
                       Pid ! done;
        %[A, B | []] -> Pid ! {self(), twoCards, [A, B]},
        %               player([]);
        [H | T]     -> io:format("Player~p sends ~p~n", [self(), H]),
                       Pid ! {oneCard, H},
                       player(T)
      end;
    {add, Cards} -> io:format("~p received ~p~n", [self(), Cards]),
                    player(Deck ++ Cards);
    final        -> io:format("~p now holds:~n ~p~n", [self(), Deck]),
                    io:format("gg~n", [])
  end.


table(Cache) ->
  % p1 and p2 take turns
  p1 ! {play, self()},
  receive
    done -> io:format("The game has ended~n", []),
            p2 ! final,
            ok;

    {oneCard, Card1} ->
      io:format("~p played ~p~n", [self(), Card1]),
      p2 ! {play, self()},
      receive
        done -> io:format("The game has ended~n", []),
                p1 ! {add, [Card1]},
                p1 ! final,
                ok;
        {twoCards, Cards} -> ok; % not implemented
        {oneCard , Card2} ->
          io:format("(~p): P2 played ~p~n", [self(), Card2]),
          case compare(Card1, Card2) of
            p1_wins -> p1 ! {add, [Card1, Card2 | Cache] },
                       table([]);
            p2_wins -> p2 ! {add, [Card1, Card2 | Cache] },
                       table([]);
            equal   -> table([Card1, Card2])
          end
        end;
    {p1, twoCards, Cards} -> ok
  end.


% assumes Val is in Xs
% alternatively, pre-compute card values
pos(Val, N, [H | T]) ->
  case H == Val of
    true  -> N;
    false -> pos(Val, N + 1, T)
  end.


compare(Card1, Card2) ->
  {Val1, _Suit1} = Card1,
  {Val2, _Suit2} = Card2,
  Vals = [2, 3, 4, 5, 6, 7, 8, 9, 10, "J", "Q", "K", "A"],
  Pos1 = pos(Val1, 0, Vals),
  Pos2 = pos(Val2, 0, Vals),
  %io:format("Cards: ~p vs ~p ~n", [Card1, Card2]),
  %io:format("Card values: ~p vs ~p ~n", [Pos1, Pos2]),
  case Pos1 - Pos2 of
    X when X == 0 -> equal;
    X when X  < 0 -> p2_wins;
    X when X  > 0 -> p1_wins
  end.


main() ->
  %Cards = make_deck(),
  %Cards = [{"A","Club"}, {10,"Diamonds"}],
  %Cards = [{"A","Club"}, {"Q","Diamonds"}, {4,"Club"}, {2,"Diamonds"}],

  Cards = [{"A","Club"}, {"Q","Diamonds"}, {4,"Club"}, {2,"Diamonds"},
           {5,"Club"}, {8,"Diamonds"}],

  Shuffled = shuffle(Cards),
  Half = round(length(Shuffled) / 2),
  {CardsPlayer1, CardsPlayer2} = lists:split(Half, Shuffled),
  Pid1 = spawn(ch8, player, [CardsPlayer1]),
  Pid2 = spawn(ch8, player, [CardsPlayer2]),
  register(p1, Pid1),
  register(p2, Pid2),
  table([]).