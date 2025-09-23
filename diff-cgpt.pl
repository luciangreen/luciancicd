% Base case: diff of empty lists is empty
diff([], [], [], [], _, L, L).

% Case 1: Head elements are the same
diff([X|Xs], [X|Ys], Ins, Del, PI, List1, List2) :-
    (member(X,PI)->append(List1,[[p,X]],List3);
    append(List1,[X],List3)),
    diff(Xs, Ys, Ins, Del, PI, List3, List2),
    !.

% Case 2: Head elements are different - X is in insertions
diff(Xs, [Y|Ys], [Y1|Ins], Del, PI, List1, List2) :-
    (member(Y,PI)->(append(List1,[[p,Y]],List3),
    Y1=[]);
    (append(List1,[[i,Y]],List3),
    Y1=Y)),
    diff(Xs, Ys, Ins, Del, PI, List3, List2),
    %append(Ins,[Y1],Ins1),
    !.

% Case 3: Head elements are different - Y is in deletions
diff([X|Xs], Ys, Ins, [X1|Del], PI, List1, List2) :-
    (member(X,PI)->(append(List1,[[p,X]],List3),
    X1=[]);
    (append(List1,[[d,X]],List3),
    X1=X)),
    diff(Xs, Ys, Ins, Del, PI, List3, List2),
    %append(Del,[X1],Del1),
    !.


%%%%%%%%%

% Use base number unless a member of I1,D1
already_member1(X1,X2,Insertions1,Deletions1,
	X) :-
	get_base_token_number(X1,%X%
	X11
	),
	get_base_token_number(X2,%X%
	X21
	),
	X11=X21,
	((member(X1,Insertions1)->true;member(X1,Deletions1))->X=X1;
	(X=X11)),
	!.
already_member2(X1,Insertions1,X) :-
	get_base_token_number(X1,X11),
	((member(X1,Insertions1))->X=X1;
	(X=X11)),!.

diff(A, B, C, D, E, F, G, H, J) :-
 diff_a(A, B, C, D, E1, F1, G, H, J),
 delete(E1,[],E),
 delete(F1,[],F).

% Base case: diff_a of empty lists is empty
diff_a([], [], _,_,[], [], _, L, L).

% Case 1: Head elements are the same
diff_a([X1|Xs], [X2|Ys], Insertions1,Deletions1,Ins, Del, PI, List1, List2) :-
%trace,
	already_member1(X1,X2,Insertions1,Deletions1,
	X),
    ((member(X3,PI),(catch(get_base_token_number(X3,X),_,false)->true;X3=X))->append(List1,[[p,X3]],List3);
    append(List1,[X],List3)),
    diff_a(Xs, Ys, Insertions1,Deletions1,Ins, Del, PI, List3, List2),
    !.

% Case 2: Head elements are diff_aerent - X is in insertions
diff_a(Xs, [Y2|Ys], Insertions1,Deletions1,[Y1|Ins], Del, PI, List1, List2) :-
	already_member2(Y2,Insertions1,Y),
    ((member(Y3,PI),(catch(get_base_token_number(Y3,Y),_,false)->true;Y3=Y))->(append(List1,[[p,Y3]],List3),
    Y1=[]);
    (append(List1,[[i,Y]],List3),
    Y1=Y)),
    diff_a(Xs, Ys, Insertions1,Deletions1,Ins, Del, PI, List3, List2),
    %append(Ins,[Y1],Ins1),
    !.

% Case 3: Head elements are diff_aerent - Y is in deletions
diff_a([X2|Xs], Ys, Insertions1,Deletions1,Ins, [X1|Del], PI, List1, List2) :-
	already_member2(X2,Deletions1,X),
    ((member(X3,PI),(catch(get_base_token_number(X3,X),_,false)->true;X3=X))->(append(List1,[[p,X3]],List3),
    X1=[]);
    (append(List1,[[d,X]],List3),
    X1=X)),
    diff_a(Xs, Ys, Insertions1,Deletions1,Ins, Del, PI, List3, List2),
    %append(Del,[X1],Del1),
    !.


