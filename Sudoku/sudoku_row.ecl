:- lib(ic).
:- lib(ic_search).
:- [sudex_toledo].
:- import alldifferent/1 from ic_global.

solve(Name):-
  puzzles(AlmostBoard,Name),
  lists2rows(AlmostBoard,Board),
  print_board(Board),
  sudoku(Board),
  labeling(Board),
  print_board(Board).

sudoku(Sudoku):-
  length(Sudoku,N2),
  N is integer(sqrt(N2)),
  ( for(I,1,N2), param(N2,Sudoku)  do
      Row is Sudoku[I],
      permutation([1..N2],Row)
  ),
  ( for(I,1,N2), param(N2,Sudoku) do
    ( for(J,1,N2), param(N2,I,Sudoku), foreach(E,Col) do
        Row is Sudoku[J],
        selectElement(E,I,Row)
    ),
    alldifferent(Col)
  ),
  ( multifor([I,J],0,(N-1)), param(N,Sudoku) do
      ( multifor([K,L],1,N), param(N,I,J,Sudoku), foreach(B,Block) do
          R is (I*N)+K,
          C is (J*N)+L,
          Row is Sudoku[R],
          selectElement(B,C,Row)
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
	( for(I,1,N), param(Board,N) do
	    ( for(J,1,N), param(Board,I) do
		    Row is Board[I],
                    array_list(T,Row),
                    selectElement(X,J,T),
		    ( var(X) -> write("  _") ; printf(" %2d", [X]) )
	    ), nl
	), nl.

%array_list(A, L) :- A =.. [[]|L].

lists2rows(Lists,Rows) :-
  ( foreach(List,Lists), foreach(Row,Rows) do
      array_list(Row, List)
  ).

lists2matrix(Lists, Matrix) :-
    ( foreach(List,Lists), foreach(Row,Temp) do
        array_list(Row, List)
    ),
    % list of arrays to array of arrays
    array_list(Matrix, Temp).
