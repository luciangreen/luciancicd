
diff_new([], [], [], [], _, L, L).

% Case 1: Head elements are the same
diff_new([X|Xs], [X|Ys], Ins, Del, PI, List1, List2) :-
    (member(X,PI)->append(List1,[[p,X]],List3);
    append(List1,[X],List3)),
    diff_new(Xs, Ys, Ins, Del, PI, List3, List2),
    !.

% Case 2: Head elements are diff_newerent - X is in insertions
diff_new(Xs, [Y|Ys], [Y1|Ins], Del, PI, List1, List2) :-
    (member(Y,PI)->(append(List1,[[p,Y]],List3),
    Y1=[]);
    (append(List1,[[i,Y]],List3),
    Y1=Y)),
    diff_new(Xs, Ys, Ins, Del, PI, List3, List2),
    %append(Ins,[Y1],Ins1),
    !.

% Case 3: Head elements are diff_newerent - Y is in deletions
diff_new([X|Xs], Ys, Ins, [X1|Del], PI, List1, List2) :-
    (member(X,PI)->(append(List1,[[p,X]],List3),
    X1=[]);
    (append(List1,[[d,X]],List3),
    X1=X)),
    diff_new(Xs, Ys, Ins, Del, PI, List3, List2),
    %append(Del,[X1],Del1),
    !.
