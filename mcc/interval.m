:- module interval.

:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is cc_multi.

:- implementation.
:- import_module list, int, string.
:- import_module parsing_utils, require.

:- type interval ---> interval(lo :: int, hi :: int).

:- func width(interval) = int.
width(interval(Lo, Hi)) = Hi - Lo.

:- func compare_interval(interval, interval) = comparison_result.
compare_interval(Int0, Int1) = Out :-
  ( Int0^hi < Int1^hi -> Out = (<)
  ; Int0^hi > Int1^hi -> Out = (>)
  ; width(Int0) < width(Int1) -> Out = (<)
  ; width(Int0) > width(Int1) -> Out = (>)
  ; Out = (=)
  ).

:- pred overlaps(interval::in, interval::in) is semidet.
overlaps(Int0, Int1) :-
  ( Int0^lo =< Int1^hi, Int0^hi >= Int1^hi
  ; Int1^lo =< Int0^hi, Int1^hi >= Int0^hi
).

:- func merge(interval, interval) = interval.
merge(interval(Lo0, Hi0), interval(Lo1, Hi1)) =
  interval(min(Lo0, Lo1), max(Hi0, Hi1)).

:- func merge_intervals(list(interval)) = list(interval).
merge_intervals(Spans) = Joined :-
  Sorted = list.sort(compare_interval, Spans),
  merge_intervals_(Sorted, Joined).

:- pred stable_recurse((func(list(interval)) = list(interval)), list(interval), list(interval)).
:- mode stable_recurse(in, in, out) is det.
stable_recurse(F, Xs, Zs) :-
  F(Xs) = Ys,
  list.length(Xs, M),
  list.length(Ys, N),
  ( if M = N then Zs = Ys else stable_recurse(F, Ys, Zs)).

:- pred merge_intervals_(list(interval), list(interval)).
:- mode merge_intervals_(in, out) is det.
merge_intervals_(Is, Merged) :-
  ( Is = [] -> Merged = []
  ; Is = [X] -> Merged = [X]
  ; Is = [X, Y | Zs] ->
    ( if
        overlaps(X, Y)
      then
        merge_intervals_([merge(X, Y) | Zs], Merged)
      else
        merge_intervals_([Y | Zs], Merged0),
        Merged = [X | Merged0]
    )
  ; error("unexpected merge_intervals_")
  ).

:- pred parse(string::in, list(interval)::out) is cc_multi.
parse(Str, Out) :-
  parsing_utils.parse(Str, parse_intervals, Res),
  ( Res = ok(Tmp) -> Out = Tmp
  ; error("cannot parse")
  ).

:- pred parse_intervals(src::in, list(interval)::out, ps::in, ps::out) is semidet.
parse_intervals(Src, Out, !PS) :-
  brackets("[", "]", comma_separated_list(parse_interval), Src, Out, !PS).

:- pred parse_interval(src::in, interval::out, ps::in, ps::out) is semidet.
parse_interval(Src, Out, !PS) :-
  brackets("[", "]", comma_separated_list(int_literal), Src, IList, !PS),
  IList = [Lo, Hi],
  Out = interval(Lo, Hi).


main(!IO) :-
  io.read_line_as_string(Res, !IO),
  ( Res = ok(Line) ->
    ( parse(Line, Intervals) ->
      stable_recurse(merge_intervals, Intervals, Merged),
      io.print(list.map((func(interval(Lo, Hi)) = [Lo, Hi]), Merged), !IO)
    ; io.format("no parse!\n", [], !IO)
    )
  ; Res = eof -> true
  ; Res = error(Error) -> io.format("Error: %s\n", [s(io.error_message(Error))], !IO)
  ; error("error")
  ).
