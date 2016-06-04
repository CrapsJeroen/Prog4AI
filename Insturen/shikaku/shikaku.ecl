:- lib(gfd),lib(util),lib(lists),lib(listut).
:- include(puzzles).
:- include(print).

%%% MAIN %%%

/*
*	Solve the puzzle with the given Name.
*/
solve(Name):-
	problem(Name, W, H, Points),
	writeln(Name),
	solve(W, H, Points, Rects),
	show(W, H, Points, Rects),
	!.

/*
*	Solve all puzzles and pause inbetween.
*/
solve_all:-
	problem(Name, _, _, _),
	solve(Name).	
	
/*
*	Solve all puzzles without pausing.
*/
solve_all_auto:-
	(solve_all,fail; true).

/*
* Actually solve the puzzle with the imported data.
*/
solve(W,H,Points,Rects):-
	create_rectangles(W,H,Points,Rects),
	rect_to_struct(Rects,Structs),
	disjoint2(Structs),
  (foreach(rect(X,Y,W,H,_),Structs), foreach([X,Y,W,H],List) do
  	true
  ),
  flatten(List,FlatList),
  search(FlatList,0,anti_first_fail,indomain,complete,[backtrack(Back)]),
        writeln(Back).

		
%%% QUICKSORT ( for own search strategy) %%%
% Needs some modifications to run

quick_sort(List,Sorted):-quick_sort(List,[],Sorted).
quick_sort([],Acc,Acc).
quick_sort([Split|T],Acc,Sorted):-
	pivot(Split,T,L1,L2),
	quick_sort(L1,Acc,Sorted1),quick_sort(L2,[Split|Sorted1],Sorted).
pivot(_,[],[],[]).
pivot(rect(P1,Q1,D1,S1),[rect(P2,Q2,D2,S2)|T],[rect(P2,Q2,D2,S2)|L],G):-
	S2=<S1,pivot(rect(P1,Q1,D1,S1),T,L,G).
pivot(rect(P1,Q1,D1,S1),[rect(P2,Q2,D2,S2)|T],L,[rect(P2,Q2,D2,S2)|G]):-
	S2>S1,pivot(rect(P1,Q1,D1,S1),T,L,G).
		
		
%%% RECTANGLE GENERATION %%%


/*
* Import the points from the puzzle and generate the rectangles with 
* their appropriate domains.
*/
create_rectangles(_,_,[],_).
create_rectangles(Width,Height,[(X,Y,Val)|Tail],[R|Rects]) :-
        rectangle((X,Y,Val),Tail,Width,Height,R),
        create_rectangles(Width,Height,Tail,Rects).

/*
* Generate a single rectangle for the given point and apply all the constraints.
*/
rectangle((I,J,N),Others,Width,Height,rect(c(I,J),c(X,Y),s(W,H))):-
        X :: 1..Width,
        Y :: 1..Height,
        W :: 1..N,
        H :: 1..N,
        W*H #= N,
        X+W-1 #=< Width,
        Y+H-1 #=< Height,
        inside(c(X,Y),s(W,H),c(I,J)),
		outsides(c(X,Y),s(W,H),Others).
		
/*
* Transform the rect predicate to a rect structure for use with disjoint2.
*/		
rect_to_struct([],[]).
rect_to_struct([rect(c(_,_),c(X,Y),s(W,H))|Rects],[rect{x:X,y:Y,w:W,h:H}|Structs]):-
        rect_to_struct(Rects,Structs).


%%% CONSTRAINTS %%%		


/*
* Ensure that the given point c(I,J) is inside the rectangle described by
* C(X,Y) and s(W,H)
*/
inside(c(X,Y),s(W,H),c(I,J)) :-
        I #>= X,
        J #>= Y,
        I #< X+W,
        J #< Y+H.

/*
* Ensure that none of the points in Points are inside the rectangle described by
* C(X,Y) and s(W,H)
*/
outsides(c(X,Y),s(W,H),Points) :-
        (foreach((I,J,_),Points), param(X,Y,W,H) do
                outside(c(X,Y),s(W,H),c(I,J))
        ).

/*
* Ensure that the given point c(I,J) is not inside the rectangle described by
* C(X,Y) and s(W,H)
*/		
outside(c(X,Y),s(W,H),c(I,J)):-
        I #< X or J #< Y or I #>=X+W or J #>= Y+H.


