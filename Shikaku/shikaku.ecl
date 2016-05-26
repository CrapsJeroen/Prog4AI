%:-  encoding(utf8).
:- lib(gfd),lib(util),lib(lists),lib(listut).
:- include(puzzles).
:- include(print).

solveAllAuto:-
	(solveAll,fail; true).

solveAll:-
	problem(Name, _, _, _),
	solve(Name).

solve(Name):-
	problem(Name, W, H, Points),
	writeln(Name),
	time(solve(W, H, Points, Rects)),
	show(W, H, Points, Rects),
	!.
	
quick_sort(List,Sorted):-quick_sort(List,[],Sorted).
quick_sort([],Acc,Acc).
quick_sort([Split|T],Acc,Sorted):-
	pivot(Split,T,L1,L2),
	quick_sort(L1,Acc,Sorted1),quick_sort(L2,[Split|Sorted1],Sorted).
	
pivot(Split,[],[],[]).
pivot(rect(P1,Q1,D1,S1),[rect(P2,Q2,D2,S2)|T],[rect(P2,Q2,D2,S2)|L],G):-S2=<S1,pivot(rect(P1,Q1,D1,S1),T,L,G).
pivot(rect(P1,Q1,D1,S1),[rect(P2,Q2,D2,S2)|T],L,[rect(P2,Q2,D2,S2)|G]):-S2>S1,pivot(rect(P1,Q1,D1,S1),T,L,G).


solve(W,H,Points,Rects):-
	create_rectangles(W,H,Points,UnsortedRects),
	quick_sort(UnsortedRects,Rects),
	rect_to_struct(Rects,Structs),
	disjoint2(Structs),
  (foreach(rect(X,Y,W,H,_),Structs), foreach([X,Y,W,H],List) do
  	true
  ),
  flatten(List,FlatList),
  search(FlatList,0,input_order,indomain,complete,[]).

rect_to_struct([],[]).
rect_to_struct([rect(c(_,_),c(X,Y),s(W,H),_)|Rects],[rect{x:X,y:Y,w:W,h:H}|Structs]):-
        rect_to_struct(Rects,Structs).

inside(c(X,Y),s(W,H),c(I,J)) :-
        I #>= X,
        J #>= Y,
        I #< X+W,
        J #< Y+H.

outsides(c(X,Y),s(W,H),Points) :-
        (foreach((I,J,_),Points), param(X,Y,W,H) do
                outside(c(X,Y),s(W,H),c(I,J))
        ).

outside(c(X,Y),s(W,H),c(I,J)):-
        I #< X or J #< Y or I #>=X+W or J #>= Y+H.

create_rectangles(_,_,[],_).
create_rectangles(Width,Height,[(X,Y,Val)|Tail],[R|Rects]) :-
        rectangle((X,Y,Val),Tail,Width,Height,R),
        create_rectangles(Width,Height,Tail,Rects).

rectangle((I,J,N),Others,Width,Height,rect(c(I,J),c(X,Y),s(W,H),N)):-
        X :: 1..Width,
        Y :: 1..Height,
        W :: 1..N,
        H :: 1..N,
        W*H #= N,
        X+W-1 #=< Width,
        Y+H-1 #=< Height,
        inside(c(X,Y),s(W,H),c(I,J)).
		%outsides(c(X,Y),s(W,H),Others).
