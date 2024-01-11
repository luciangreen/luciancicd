% Base case: diff of empty lists is empty
diff([], [], [], [], _, L, L).

% Case 1: Head elements are the same
diff([X|Xs], [X|Ys], Ins, Del, PI, List1, List2) :-
    (member(X,PI)->append(List1,[[p,X]],List3);
    append(List1,[X],List3)),
    diff(Xs, Ys, Ins, Del, PI, List3, List2),
    !.

% Case 2: Head elements are different - X is in deletions
diff([X|Xs], Ys, [X1|Ins], Del, PI, List1, List2) :-
    (member(X,PI)->(append(List1,[[p,X]],List3),
    X1=[]);
    (append(List1,[[i,X]],List3),
    X1=X)),
    diff(Xs, Ys, Ins, Del, PI, List3, List2),
    !.

% Case 3: Head elements are different - Y is in insertions
diff(Xs, [Y|Ys], Ins, [Y1|Del], PI, List1, List2) :-
    (member(Y,PI)->(append(List1,[[p,Y]],List3),
    Y1=[]);
    (append(List1,[[d,Y]],List3),
    Y1=Y)),
    diff(Xs, Ys, Ins, Del, PI, List3, List2),
    !.
