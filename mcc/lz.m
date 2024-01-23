:- module lz.,
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.
:- implementation.
:- import_module list, string, char, int.

:- type cursor ---> cursor(buffer :: string, position :: int).

:- pred expand_chunk(list(char)::out, cursor::di, cursor::uo) is semidet.
expand_chunk(Out, !Cursor) :-
  !.Cursor^buffer = Buf,
  !.Cursor^position = Pos,
  string.index(Buf, Pos, LenChar),
  Len = char.decimal_digit_to_int(LenChar),
  ( Len = 0 -> Out = [], !:Cursor = (!.Cursor^position := Pos + 1)
  ; string.index(Buf, Pos+1, HeadChar),
    ( char.is_decimal_digit(HeadChar) ->
      delta = char.decimal_digit_to_int(HeadChar),

      


:- pred unchunk(string::in, int::in, ) is semidet.
unchunk(
