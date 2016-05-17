:- lib(gfd).
:- lib(util),lib(lists),lib(listut).
:- include(puzzles).
%%% problem(tiny, 3, 3, [(1,1,4), (3,1,2), (2,3,3)]).
%%% solve(Rects, [(1,1,4), (3,1,2), (2,3,3)]).
%%% ID=tiny, problem(ID, W, B, Hints), time(solve(Rects, W, B, Hints)), show(W, B, Hints, Rects, ascii).
is_of_size(Size, s(A,B)):-
	A :: 1..Size,
	B :: 1..Size,
	Size #= A * B.

solve(Rects, Name):-
        problem(Name,Width,Height,Points),
	fill_known(Board,Width,Height,Points),
	(fromto(Points,[(A,B,Val)|Tail],Tail,[]), param(Board), foreach(R,Rects) do
                Point = (A,B,Val),
		fit_rect(Board, Point, rect(c(A,B),TopLeft,Size)),
                outsides(TopLeft,Size,Tail),
                R = rect(c(A,B),TopLeft,Size)
	),
        rect_to_struct(Rects,Structs),
	disjoint2(Structs),
        (foreach(rect(X,Y,W,H,_),Structs), foreach([X,Y,W,H],List) do
                true
        ),
        flatten(List,FlatList),
        labeling(FlatList).

rect_to_struct([],[]).
rect_to_struct([rect(c(X,Y),c(I,J),s(W,H))|Rects],[rect{x:X,y:Y,w:W,h:H}|Structs]):-
        rect_to_struct(Rects,Structs).

no_intersect_list([_]).
no_intersect_list([Rect|Rects]) :-
	(foreach(R, Rects), param(Rect) do
		no_intersect_rect(Rect,R)	
	),
	no_intersect_list(Rects).

iterate_rect(Board, [Point|Points], CheckRects, [rec(_,C,S)|OutRects])	:-
	fit_rect(Board, Point, Rect),
	(foreach(R, CheckRects), param(rec(_,C,S)) do
		no_intersect_rect(rec(_,C,S),R)	
	),
        %%% no_contain_points(Points,C,S),
	iterate_rect(Board, Points,[Rect|CheckRects], OutRects).
	
iterate_rect(_,[],_,_).
	
no_intersect_rect(rect(_,c(XA1,YA1),s(W1,H1)), rect(_,c(XB1,YB1),s(W2,H2))) :-
	XA2 #= XA1+W1-1,
	YA2 #= YA1+H1-1,
	XB2 #= XB1+W2-1,
	YB2 #= YB1+H2-1,
	(XA1 #> XB2;
	XA2 #< XB1;
	YA1 #> YB2;
	YA2 #< YB1
	).

outsides(c(X,Y),s(W,H),Points) :-
        (foreach((I,J,_),Points), param(X,Y,W,H) do
                outside(c(X,Y),s(W,H),c(I,J))
        ).

outside(c(X,Y),s(W,H),c(I,J)):-
        I #< X ; J #< Y ; I #>=X+W ; J #>= Y+H.

fill_known(Board,XDim, YDim, Points) :-
	dim(Board, [YDim,XDim]),
	N is length(Points),
	Max is XDim * YDim,
	Board[1..XDim,1..YDim] :: [1..Max],
	(foreach((X,Y,_),Points), for(I,1,N), param(Board) do
		I is Board[Y,X]
	).

fit_rect(Board, (XVal,YVal,Val), rect(c(XVal, YVal), TopLeft, Size)) :-
	is_of_size(Val, Size),
	is_in_rect(c(XVal,YVal), TopLeft, Size),
	rect_fits_on_board(Board, TopLeft, Size).

	
create_list(Points, List):-
	length(Points,N),
	(foreach((_,_,Val),Points), for(I,1,N), foreach(B,List) do
		B = (I,Val)
	).

search(Elem,[(Elem,Val)|_],Val).
search(Elem,[_|Tail],Val):-
	search(Elem,Tail,Val).

rect_fits_on_board(Board, c(X,Y), s(W,H)) :-
	is_in_board(Board, c(X,Y)),
	X2 #= X+W-1,
	Y2 #= Y+H-1,
	BotRight = c(X2,Y2),
	is_in_board(Board, BotRight).

is_in_board(Board, c(X,Y)) :-
	dim(Board, [YDim,XDim]),
	X :: 1..XDim,
	Y :: 1..YDim.

is_in_rect(c(X,Y), c(XR,YR), s(W,H)) :-
	XR #> 0,
	YR #> 0,
	XMax #= XR+W-1,
	YMax #= YR+H-1,
	X #=< XMax,
	X #>= XR,
	Y #=< YMax,
	Y #>= YR.

print_board(Board) :-
	dim(Board, [N,N]),
	( for(I,1,N), param(Board,N) do
	    ( for(J,1,N), param(Board,I) do
		X is Board[I,J],
		( var(X) -> write("  _") ; printf(" %2d", [X]) )
	    ), nl
	), nl.
