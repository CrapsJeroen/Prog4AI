:- lib(ic).
:- lib(ic_search).
:- import alldifferent/1 from ic_global.

solve(Name):-
  puzzles(AlmostBoard,65Name),
  lists2matrix(AlmostBoard,Board),
  print_board(Board),
  sudoku(Board),
  labeling(Board),
  print_board(Board).

sudoku(Sudoku):-
  dim(Sudoku,[N2,N2]),
  N is integer(sqrt(N2)),
  ( for(I,1,N2), param(N2,Sudoku)  do
      Col is Sudoku[1..N2,I],
      permutation([1..N2],Col)
  ),
  ( for(I,1,N2), param(N2,Sudoku) do
    ( for(J,1,N2), param(N2,I,Sudoku), foreach(E,Row) do
        Col is Sudoku[1..N2,J],
        selectElement(E,I,Col)
    ),
    alldifferent(Row)
  ),
  ( multifor([I,J],0,(N-1)), param(N,Sudoku) do
      ( multifor([K,L],1,N), param(N,I,J,Sudoku), foreach(B,Block) do
          R is (I*N)+K,
          C is (J*N)+L,
          B is Sudoku[R,C]
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
	dim(Board, [N,N]),
	( for(I,1,N), param(Board,N) do
	    ( for(J,1,N), param(Board,I) do
		X is Board[I,J],
		( var(X) -> write("  _") ; printf(" %2d", [X]) )
	    ), nl
	), nl.

array_list(A, L) :- A =.. [[]|L].

lists2matrix(Lists, Matrix) :-
    ( foreach(List,Lists), foreach(Row,Temp) do
        array_list(Row, List)
    ),
    % list of arrays to array of arrays
    array_list(Matrix, Temp).
