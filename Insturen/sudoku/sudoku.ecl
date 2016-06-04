:- lib(ic).
:- lib(ic_search).
:- [sudex_toledo].
:- import alldifferent/1 from ic_global.

%%% MAIN %%%

/*
*	Solve the puzzle with the given Name.
*/
solve(Name) :-
        puzzles(AlmostBoard,Name),
        lists2matrix(AlmostBoard,Board),
	print_board(Board),
	sudoku(Board),
	search(Board,0,input_order,indomain,complete,[backtrack(Back)]),
    writeln(Back),
	print_board(Board),
    !.
		
/*
*	Solve all puzzles and pause inbetween.
*/
solve_all:-
        puzzles(_,Name),
        write(Name), nl,
        solve(Name).

/*
*	Solve all puzzles without pausing.
*/		
solve_all_auto:-
	(solve_all,fail; true).
 
 
/*
* Actually solve the given Sudoku board
*/ 
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

/*
* Print the Sudoku board
*/
print_board(Board) :-
	dim(Board, [N,N]),
	( for(I,1,N), param(Board,N) do
	    ( for(J,1,N), param(Board,I) do
		X is Board[I,J],
		( var(X) -> write("  _") ; printf(" %2d", [X]) )
	    ), nl
	), nl.
/*
* Convert the given list of lists to a matrix
*/
lists2matrix(Lists, Matrix) :-
    ( foreach(List,Lists), foreach(Row,Temp) do
        array_list(Row, List)
    ),
    % list of arrays to array of arrays
    array_list(Matrix, Temp).
