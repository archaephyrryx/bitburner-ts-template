:- module graphcolor.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is cc_multi.
:- implementation.
:- import_module int, list, map, require.
:- import_module bool.

:- type node ---> node(index :: int, color :: bool).

:- type graph ---> graph(size :: int, edges :: list(list(int))).

:- pred color(bool::out) is multi.
color(no).
color(yes).

:- pred ncolors(int::in, list(bool)::out) is multi.
ncolors(N, Colors) :-
  ( N = 0 -> Colors = []
  ; N > 0 -> color(Head), ncolors(N-1, Rest), Colors = [Head | Rest]
  ; error("N < 0")
  ).

:- pred color_graph(graph, list(int)).
:- mode color_graph(in, out) is nondet.
color_graph(Graph, Colors) :-
  ncolors(Graph^size, ColorsTmp),
  list.all_true((pred([I, J | _]::in) is semidet :- bool.xor(det_index0(ColorsTmp, I), det_index0(ColorsTmp, J)) = yes), Graph^edges),
  Colors = list.map((func(B) = C :- ( B = yes, C=1; B = no, C=0 )), ColorsTmp).


main(!IO) :-
  Input = graph(14,[[3,4],[5,7],[6,10],[3,10],[6,13],[1,3],[8,10],[2,9],[1,5],[2,10],[8,13],[0,4],[5,13],[2,13],[2,12],[7,11],[0,13],[5,9],[9,11],[4,5],[0,10],[10,11],[3,13],[5,10],[1,2]]),
  ( color_graph(Input, Solution) -> io.print(Solution, !IO)
  ; io.write_string("[]", !IO), io.nl(!IO)
  ).
