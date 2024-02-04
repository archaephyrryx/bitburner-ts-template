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
  Input = graph(11,[[6,9],[5,8],[5,6],[0,1],[0,7],[0,3],[5,10],[0,2],[2,6],[3,10],[7,8],[2,10],[6,7],[3,8],[7,10],[8,9],[1,10]]),
  ( color_graph(Input, Solution) -> io.print(Solution, !IO)
  ; io.write_string("[]", !IO), io.nl(!IO)
  ).
