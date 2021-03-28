-module(ch5).
-compile(export_all).

-define(ERROR, "Both numbers must be greater than or equal to zero.").


area(Shape, A, B) when A > 0, B > 0 ->
  case Shape of
    rectangle -> A * B;
    triangle  -> A * B / 2.0;
    ellipse   -> math:pi() * A * B
  end;
area(_, _, _) -> ?ERROR.


get_choice() ->
  X = string:trim(io:get_line("R)ectangle, T)riangle, or E)llipse: ")),
  Choice = string:to_upper(string:slice(X, 0, 1)),
  case Choice of
    "R" -> rectangle;
    "T" -> triangle;
    "E" -> ellipse;
    _   -> X
  end.


get_val(Label) ->
  X = string:trim(io:get_line("Enter " ++ Label ++ ":")),
  case string:to_float(X) of
    {Val, []} -> Val;
    _         ->
      case string:to_integer(X) of
        {Val, []} -> Val;
        _         -> bad
      end
  end.


result(Val, Shape) ->
  case Val of
    {bad  , _     } -> io:format("Error in first number~n");
    {_    , bad   } -> io:format("Error in second number~n");
    {A, B}          -> area(Shape, A, B)
  end.


main() ->
  Choice = get_choice(),
  case Choice of
    rectangle ->
      Val = {get_val("width"), get_val("height")},
      result(Val, Choice);
    triangle ->
      Val = {get_val("base"), get_val("height")},
      result(Val, Choice);
    ellipse ->
      Val = {get_val("major axix"), get_val("minor axis")},
      result(Val, Choice);
    _   -> io:format("Unknown shape " ++ Choice ++ "~n")
  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% input: "yyyy-mm-dd"
date_parts(X) ->
  Xs = re:split(X, "-", [{return, list}]),
  [ element(1, string:to_integer(X)) || X <- Xs].
