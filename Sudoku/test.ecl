
denominator(Number,S,List) :-
        [A,B] :: [1..Number],
        findall(c(A,B), Number = A*B, List).

