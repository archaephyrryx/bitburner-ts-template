:- module spiralize.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.
:- implementation.
:- import_module int, array, require.
:- import_module list, string.

:- type matrix_view(T) ---> matrix_view(
    matrix :: array(array(T)),
    hrOffset :: int,
    trOffset :: int,
    hcOffset :: int,
    tcOffset :: int
  ).

:- func rows(matrix_view(_T)) = int.
rows(M) = size(M^matrix).

:- func cols(matrix_view(_T)) = int.
cols(M) = size(M^matrix^elem(0)).

:- func row(int, matrix_view(T)) = array(T).
row(I, M) = M^matrix^elem(I).

:- pred null(matrix_view(T)::in) is semidet.
null(M) :-
  ( M^hrOffset + M^trOffset >= rows(M)
  ; M^hcOffset + M^tcOffset >= cols(M)
  ).

:- func head_row(matrix_view(T)) = list(T).
head_row(M) = Row :-
  FullRow = row(M^hrOffset, M),
  Row = fetch_items(FullRow, M^hcOffset, size(FullRow) - 1 - M^tcOffset).

:- func head_col(matrix_view(T)) = list(T).
head_col(M) = Col :-
  F = (func(I) = Elt :-
    Row = row(I, M),
    Elt = Row^elem(M^hcOffset)
  ),
  FullCol = generate(rows(M), F),
  Col = fetch_items(FullCol, M^hrOffset, size(FullCol) - 1 - M^trOffset).

:- func last_row(matrix_view(T)) = list(T).
last_row(M) = Row :-
  FullRow = row(rows(M) - 1 - M^trOffset, M),
  Row = fetch_items(FullRow, M^hcOffset, size(FullRow) - 1 - M^tcOffset).

:- func last_col(matrix_view(T)) = list(T).
last_col(M) = Col :-
  F = (func(I) = Elt :-
    Row = row(I, M),
    Elt = Row^elem(cols(M) - 1 - M^tcOffset)
  ),
  FullCol = generate(rows(M), F),
  Col = fetch_items(FullCol, M^hrOffset, size(FullCol) - 1 - M^trOffset).


:- pred go_view(list(int)::out, matrix_view(int)::in) is det.
go_view(Items, M) :-
  (
    null(M) -> Items = []
  ;
    M0 = M,
    TopRow = head_row(M0),
    M1 = M0^hrOffset := M0^hrOffset + 1,
    (
      null(M1) -> Rem0 = []
    ;
        last_col(M1) = RightCol,
        M2 = M1^tcOffset := M1^tcOffset + 1,
          (
            null(M2) -> Rem1 = []
          ;
            RevBotRow = last_row(M2),
            BotRow = list.reverse(RevBotRow),
            M3 = M2^trOffset := M2^trOffset + 1,
              (
                null(M3) -> Rem2 = []
              ;
                RevLeftCol = head_col(M3),
                LeftCol = list.reverse(RevLeftCol),
                M4 = M3^hcOffset := M3^hcOffset + 1,
                go_view(Rest, M4),
                list.append(LeftCol, Rest, Rem2)
              ; unexpected("spiralize", "M3->M4 disjunction")
              ),
            list.append(BotRow, Rem2, Rem1)

              ; unexpected("spiralize", "M2->M3 disjunction")
          ),
        list.append(RightCol, Rem1, Rem0)
    ; unexpected("spiralize", "M1->M2 disjunction")
    ),
  list.append(TopRow, Rem0, Items)).

:- func spiralize(list(list(int))) = list(int).
spiralize(X) = Spiral :-
  Mat = array.from_list(list.map(array.from_list, X)),
  MatView = matrix_view(Mat, 0, 0, 0, 0),
  go_view(Spiral, MatView).


:- pred pprint_list(list(T)::in, io::di, io::uo) is det.
pprint_list([], !IO).
pprint_list([X | Xs], !IO) :-
  io.print(X, !IO),
  ( Xs = [_ | _] -> io.write_string(", ", !IO)
  ; true
  ),
  pprint_list(Xs, !IO).

:- pred print_list(list(T)::in, io::di, io::uo) is det.
print_list(Xs, !IO) :-
  io.write_char('[', !IO),
  pprint_list(Xs, !IO),
  io.write_char(']', !IO).

:- pred diffs(list(int)::in, list(int)::in, io::di, io::uo) is det.
diffs(Xs, Ys, !IO) :-
  ( Xs = [], Ys = [] -> io.format("vvvvv\n", [], !IO)
  ; Xs = [], Ys = [Y | Yt] ->
    io.format("> %d\n", [i(Y)], !IO),
    diffs([], Yt, !IO)
  ; Xs = [X | Xt], Ys = [] ->
    io.format("%d <\n", [i(X)], !IO),
    diffs(Xt, [], !IO)
  ; Xs = [X | Xt], Ys = [Y | Yt] ->
    ( X = Y -> io.format("= %d =\n", [i(X)], !IO)
    ; io.format("%d <> %d\n", [i(X), i(Y)], !IO)
    ),
    diffs(Xt, Yt, !IO)
  ; unexpected("spiralize", "one must succeed")
  ).

main(!IO) :-
  Input =
     [
         [27,18, 6,28, 9, 9,34,14, 5,17,33,24,48],
         [33,46,20,44,38,43,16,11,25,41,35,17, 9],
         [44,47,38,41,13,31,18,41,36,33,34, 9, 9],
         [23,12,13,11,22,10,41,35, 1,22,22,28,19],
         [47,47,45,10,13,36,46,38, 5,18,11,38,36],
         [ 3,37,33,33,47, 2,33,44,25,32,50,44,41],
         [45,31,48, 4,13,28,41,17,17,41,23,22,13]
     ],
  Spiral = spiralize(Input),
  SpiralLen = length(Spiral),
  MN = (length(det_head(Input)) * length(Input)),
  ( SpiralLen \= MN -> io.format("List length differs: Spiral %d, Matrix %d!\n", [i(SpiralLen), i(MN)], !IO)
  ; print_list(Spiral, !IO),
    io.nl(!IO)
  ).
