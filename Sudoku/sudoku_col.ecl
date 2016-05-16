:- lib(ic).
:- lib(ic_search).
:- import alldifferent/1 from ic_global.

solve(Sudoku):-
  initialise(Sudoku),
  constraints(Sudoku),
  search(Sudoku).

initialise(Sudoku):-
  ( foreach(Col,Sudoku)  do
      permutation([1..N2],Col)
  ).

constraints(Sudoku):-
  rowConstraints(Sudoku),
  blockConstraints(Sudoku).

rowConstraints(Sudoku):-
  ( for(I,1,N2), param(Sudoku) do
    ( foreach(Col,Sudoku), param(I), foreach(E,Row) do
        selectElement(E,I,Col)
    )
    alldifferent(Row)
  ).

blockConstraints(Sudoku):-
  ( multifor([I,J],1,N2,N), param(Sudoku) do
      ( multifor([K,L],0,(N-1)), param(I,J,Sudoku), foreach(B,Block) do
          R is (I*N)+K,
          C is (J*N)+L,
          selectElement(Col,C,Sudoku),
          selectElement(B,R,Col)
      )
      alldifferent(Block)
  ).

permutation(Sequence,Permutation):-
  Permutation :: Sequence,
  alldifferent(Permutation).

selectElement(E,1,[E|_]):- !.
selectElement(E,I,[Head|Tail]):-
  J is I-1,
  selectElement(E,J,Tail).
