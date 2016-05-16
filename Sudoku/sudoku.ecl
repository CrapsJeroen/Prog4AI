:- lib(ic).
:- import alldifferent/1 from ic_global.

solve(Name) :-
        puzzles(AlmostBoard,Name),
        lists2matrix(AlmostBoard,Board),
	print_board(Board),
	sudoku(Board),
	labeling(Board),
	print_board(Board).

sudoku(Sudoku):-
    dim(Sudoku,[N2,N2]),
    N is integer(sqrt(N2)),
    Sudoku[1..N2,1..N2] :: 1..N2,
    ( for(I,1,N2), param(Sudoku,N2) do
        Row is Sudoku[I,1..N2],
        alldifferent(Row),
        Col is Sudoku[1..N2,I],
        alldifferent(Col)
    ),
    ( multifor([I,J],0,(N-1)), param(Sudoku, N) do
        ( multifor([K,L],1,N), param(Sudoku, I, J, N), foreach(B,Block) do
            R is (I*N)+K,
            C is (J*N)+L,
            B is Sudoku[R,C]
        ),
        alldifferent(Block)
    ).


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
