:- lib(ic).
:- lib(ic_search).
:- [sudex_toledo].
:- import alldifferent/1 from ic_global.

solve(Name):-
  puzzles(AlmostBoard,Name),
  lists2rows(AlmostBoard,Board),
  print_board(Board),
  sudoku(Board),
  array_list(MBoard, Board),  
  labeling(MBoard),
  array_list(MBoard, B),
  print_board(B).

sudoku(Sudoku):-
  length(Sudoku,N2),
  N is integer(sqrt(N2)),
  ( foreach(Row,Sudoku), param(N2)  do
      permutation([1..N2],Row)
  ),
  ( foreach(Row,Sudoku), param(N2) do
    ( for(J,1,N2), foreach(E,Col) do
        selectElement(E,J,Row)
    ),
    alldifferent(Col)
  ),
  ( multifor([I,J],0,(N-1)), param(N,Sudoku) do
      ( multifor([K,L],1,N), param(N,I,J,Sudoku), foreach(B,Block) do
          R is (I*N)+K,
          C is (J*N)+L,
          selectElement(Row,R,Sudoku),
          array_list(Row,List),
          selectElement(B,C,List)
      ),
      alldifferent(Block)
  ).

permutation(Sequence,Permutation):-
  Permutation :: Sequence,
  alldifferent(Permutation).

selectElement(E,1,[E|_]):- !.
selectElement(E,I,[_|Tail]):-
  J is I-1,
  selectElement(E,J,Tail).

print_board(Board) :-
	length(Board,N),
	( foreach(Row,Board), param(N) do
            array_list(Row,T),
	    ( for(J,1,N), param(I,T) do
                    selectElement(X,J,T),
		    ( var(X) -> write("  _") ; printf(" %2d", [X]) )
	    ), nl
	), nl.


lists2rows(Lists,Rows) :-
  ( foreach(List,Lists), foreach(Row,Rows) do
      array_list(Row, List)
  ).
