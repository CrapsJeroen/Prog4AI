:- lib(gfd),lib(util),lib(lists),lib(listut).
:- include(puzzles).
:- include(print).

solveAll:-
	problem(Name, _, _, _),
	solve(Name).

solve(Name):-
	problem(Name, W, H, Points),
	writeln(Name),
	time(solve(W, H, Points, Rects)),
	%show(W, H, Points, Rects),
	!.

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

rect_to_struct([],[]).
rect_to_struct([rect(c(_,_),c(X,Y),s(W,H))|Rects],[rect{x:X,y:Y,w:W,h:H}|Structs]):-
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

rectangle((I,J,N),Others,Width,Height,rect(c(I,J),c(X,Y),s(W,H))):-
        X :: 1..Width,
        Y :: 1..Height,
        W :: 1..N,
        H :: 1..N,
        W*H #= N,
        X+W-1 #=< Width,
        Y+H-1 #=< Height,
        inside(c(X,Y),s(W,H),c(I,J)).
        %,outsides(c(X,Y),s(W,H),Others).
