:- lib(ic).
:- lib(ic_search).
:- lib(listut).
:- [sudex_toledo].
:- import alldifferent/1 from ic_global.

solve(Name):-
  puzzles(Board,Name),
  print_board(Board),
  sudoku(Board),
  flatten(Board,FlatList), 
  search(FlatList,0,input_order,indomain,complete,[backtrack(Back)]),
  writeln(Back),
  print_board(Board),
  !.

solveAll:-
        puzzles(_,Name),
        write(Name), nl,
        solve(Name).

solveAllAuto:-
	(solveAll,fail; true).
	
sudoku(Sudoku):-
  length(Sudoku,N2),
  N is integer(sqrt(N2)),
  ( foreach(Row,Sudoku), param(N2)  do
      permutation([1..N2],Row)
  ),
  ( for(J,1,N2), param(Sudoku) do
    ( foreach(Row,Sudoku), param(J), foreach(E,Col) do
        nth1(J,Row,E)
    ),
    alldifferent(Col)
  ),
  ( multifor([I,J],0,(N-1)), param(N,Sudoku) do
      ( multifor([K,L],1,N), param(N,I,J,Sudoku), foreach(B,Block) do
          R is (I*N)+K,
          C is (J*N)+L,
          nth1(R,Sudoku,Row),
          nth1(C,Row,B)
      ),
      alldifferent(Block)
  ).

permutation(Sequence,Permutation):-
  Permutation :: Sequence,
  alldifferent(Permutation).

print_board(Board) :-
	length(Board,N),
	( foreach(Row,Board), param(N) do
	    ( for(J,1,N), param(Row) do
                    nth1(J,Row,X),
		    ( var(X) -> write("  _") ; printf(" %2d", [X]) )
	    ), nl
	), nl.


lists2rows(Lists,Rows) :-
  ( foreach(List,Lists), foreach(Row,Rows) do
      array_list(Row, List)
  ).
