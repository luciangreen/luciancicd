/*
continuous integration (with diff x)

p2lp
comments even where spaces are x ignore comments xx check comments checked for in all spaces
- move to after command xxx
detect changed preds
[1,2,3],[1,2,4,3] - notice insertions, test
[1,2,3],[1,3] - notice deletions, test
[1,2,3],[1,4,3] - x, as above
- try combos of preds, starting with no changes
convert to pl, test
copy to build folder if passes


diff_combos([1,2,3],[1,2,4,3],C).
C = [[1, 2, 4, 3], [1, 2, 3]].

diff_combos([1,2,3],[1,3],C).
C = [[1, 2, 3], [1, 3]].

diff_combos([1,2,3],[1,5,2,4,3],C).
C = [[1, 5, 2, 4, 3], [1, 5, 2, 3], [1, 2, 4, 3], [1, 2, 3]].

diff_combos([1,2,3,4,5],[1,3,5],C).
C = [[1, 2, 3, 4, 5], [1, 2, 3, 5], [1, 3, 4, 5], [1, 3, 5]].

diff_combos([1,3,4,5],[1,2,3,5],C).
C = [[1, 2, 3, 4, 5], [1, 2, 3, 5], [1, 3, 4, 5], [1, 3, 5]].

*/

diff_combos(Before,After,Combos4) :-
 find_insertions_and_deletions(Before,After,Insertions,Deletions),
 replace11(After,Insertions,[],After2),
 replace12(Before,After2,Deletions,[],After3),
 findall(Combos,find_combos1(Insertions,Deletions,Combos),Combos2),
 findall(Combos1,(member(Combos3,Combos2),
 find_combos3(After3,Combos3,[],Combos1)),Combos41),
 sort(Combos41,Combos4),!.

replace11([],_Insertions,After,After) :- !.
replace11(After,Insertions,After2,After3) :-
 After=[After4|After5],
 (member(After4,Insertions)->
 After7=[i,After4];
 After7=After4),
 append(After2,[After7],After6),
 replace11(After5,Insertions,After6,After3),!.

replace12(Before,After,Deletions,After2,After3) :-
 %Before=[B|Bs],
 After=[[i,A]|As],
 append(After2,[[i,A]],After4),
 replace12(Before,As,Deletions,After4,After3),!.

replace12(_,After,[],_After1,After) :-
 %append(After1,[A],After2),
 !.
replace12([],[],_Deletions,After,After) :-
 %append(After1,[A],After2),
 !.
%replace12([A],[A],_Deletions,After1,After2) :-
% append(After1,[A],After2),!.
replace12(Before,After,Deletions,After2,After3) :-
 append(After4,After5,After),
 not(After4=[]),
 append(After51,After52,After5),
 
 append(After4,Before5,Before),
 append(Before53,Before54,Before5),
 append(After51,Before52,Before54),
 %After=[After4|After5],
 %not(Before53=[]),
 (length(Before53,1)->Before53=[Before55];Before53=Before55),
 (true%member(Before53,Deletions)
 ->
 (Before53=[]->After7=[];After7=[[d,Before55]]);
 After7=Before55),
 (After7=[]->
 foldr(append,[After2,After4],After6);
 foldr(append,[After2,After4,After7],After6)),
 replace12(Before52,After52,Deletions,After6,After3).

% find_insertions_and_deletions([1,2,3],[1,2,4,3],In,D).
% In = [4],
% D = [].
 
% find_insertions_and_deletions([1,2,3],[1,3],In,D).
% In = [],
% D = [2].

find_insertions_and_deletions(Before,After,Insertions,Deletions) :-
 subtract(After,Before,Insertions),
 subtract(Before,After,Deletions).

find_combos1(Insertions,Deletions,Combos) :-
 findall([i,In],member(In,Insertions),In1),
 findall([d,De],member(De,Deletions),De1),
 append(In1,De1,Ops),
 find_combos2(Ops,[],Combos).

find_combos2([],Combos,Combos).
find_combos2(Ops,Combos1,Combos2) :-
 Ops=[Op|Ops1],
 member(Switch,[on,off]),
 append(Combos1,[[Op,Switch]],Combos3),
 find_combos2(Ops1,Combos3,Combos2).

%findall([Op,Switch],(member(Op,Ops),member(Switch,[on,off])),Switches).
% ((member(In,Insertions)->true;In=[]),
% (member(De,Deletions)->true;De=[]),
 
find_combos3([],_Combos,Combos,Combos) :- !.
find_combos3(After,Combos,Combos1,Combos2) :-
 After=[Item1|After2],
 ((Item1=[Type,N],
 member([[Type,N],Switch],Combos),
 ((%Type=i,
 Switch=on)->Item=[N];Item=[]))->true;Item=[Item1]),
 append(Combos1,Item,Combos3),
 find_combos3(After2,Combos,Combos3,Combos2),!.
