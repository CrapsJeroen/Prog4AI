:- lib(ic).
:- lib(ic_search).
:- import alldifferent/1 from ic_global.

solve(Sudoku):-
    dim(Sudoku,[N2,N2]),
    N is sqrt(N2),
    Sudoku[N2,N2] :: 1..9,
    ( for(I,1,N2), param(Sudoku,N2) do
        Row is Sudoku[I,1..N2],
        alldifferent(Row),
        Col is Sudoku[1..N2,I],
        alldifferent(Col)
    ),
    ( multifor([I,J],0,(N-1)), param(Sudoku, N, N2) do
        ( multifor([K,L],1,N), param(Sudoku, N, N2), foreach(B,Block) do
            R is (I*N)+K,
            C is (J*N)+L,
            B is Sudoku[R,C]
        ),
        alldifferent(Block)
    ).
