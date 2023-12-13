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


diff_combos_vintage([1,2,3],[1,2,4,3],C).
C = [[1, 2, 4, 3], [1, 2, 3]].

diff_combos_vintage([1,2,3],[1,3],C).
C = [[1, 2, 3], [1, 3]].

diff_combos_vintage([1,2,3],[1,5,2,4,3],C).
C = [[1, 5, 2, 4, 3], [1, 5, 2, 3], [1, 2, 4, 3], [1, 2, 3]].

diff_combos_vintage([1,2,3,4,5],[1,3,5],C).
C = [[1, 2, 3, 4, 5], [1, 2, 3, 5], [1, 3, 4, 5], [1, 3, 5]].

diff_combos_vintage([1,3,4,5],[1,2,3,5],C).
C = [[1, 2, 3, 4, 5], [1, 2, 3, 5], [1, 3, 4, 5], [1, 3, 5]].

diff_combos_vintage([1,4,6,5],[1,5],C).
C = [[1, 4, 5], [1, 4, 6, 5], [1, 5], [1, 6, 5]].

diff_combos_vintage([4,6,5],[5],C).
C = [[4, 5], [4, 6, 5], [5], [6, 5]].

diff_combos_vintage([5],[4,5],C).
C = [[4, 5], [5]].

*/
diff_combos_vintage(A,A,[A]) :- !.

diff_combos_vintage(Before,After,Combos4) :-
 find_insertions_and_deletions_vintage(Before,After,Insertions,Deletions),
 replace11_vintage(After,Insertions,[],After2),
 replace12_vintage(Before,After2,Deletions,[],After3),
 findall(Combos,find_combos1_vintage(Insertions,Deletions,Combos),Combos2),
 findall(Combos1,(member(Combos3,Combos2),
 find_combos3_vintage(After3,Combos3,[],Combos1)),Combos41),
 sort(Combos41,Combos4),!.

replace11_vintage([],_Insertions,After,After) :- !.
replace11_vintage(After,Insertions,After2,After3) :-
 After=[After4|After5],
 (member(After4,Insertions)->
 After7=[i,After4];
 After7=After4),
 append(After2,[After7],After6),
 replace11_vintage(After5,Insertions,After6,After3),!.

replace12_vintage(_,After,[],_After1,After) :-
 %append(After1,[A],After2),
 !.

replace12_vintage(Before,After,Deletions,After2,After3) :-
 %Before=[B|Bs],
 After=[[i,A]|As],
 append(After2,[[i,A]],After4),
 replace12_vintage(Before,As,Deletions,After4,After3),!.

replace12_vintage([],[],_Deletions,After,After) :-
 %append(After1,[A],After2),
 !.
%replace12([A],[A],_Deletions,After1,After2) :-
% append(After1,[A],After2),!.
replace12_vintage(Before,After,Deletions,After2,After3) :-
 append(After4,After5,After),
 not(After4=[]),
 append(After51,After52,After5),
 
 append(After4,Before5,Before),
 append(Before53,Before54,Before5),
 append(After51,Before52,Before54),
 %After=[After4|After5],
 %not(Before53=[]),
 (length(Before53,1)->Before53=[Before55];
 Before53=Before55),
 (true%member(Before53,Deletions)
 ->
 (Before53=[]->After7=[];
 
 (is_list(Before55)->
 findall([d,B],member(B,Before55),After7);
 After7=[[d,Before55]])
 );
 
 After7=Before55),
 (After7=[]->
 foldr(append,[After2,After4],After6);
 foldr(append,[After2,After4,After7],After6)),
 replace12_vintage(Before52,After52,Deletions,After6,After3).
replace12_vintage(Before,After,Deletions,After2,After3) :-
 %trace,%append(After4,After5,After),
 %(After4=[]),
 append(After51,After52,After),
 
 %append(After4,Before5,Before),
 append(Before53,Before54,Before),
 append(After51,Before52,Before54),
 %After=[After4|After5],
 %not(Before53=[]),
 %not(length(Before53,1)),%->
 not(Before53=[]),%->
 Before53=[Before55],%;
 %Before53=Before55),
 (true%member(Before53,Deletions)
 ->
 (Before53=[]->After7=[];
 
 (is_list(Before55)->
 findall([d,B],member(B,Before55),After7);
 After7=[[d,Before55]])
 );
 
 After7=Before55),
 (After7=[]->
 foldr(append,[After2%,After4
 ],After6);
 foldr(append,[After2,%After4,
 After7],After6)),
 replace12_vintage(Before52,After52,Deletions,After6,After3).

% find_insertions_and_deletions_vintage([1,2,3],[1,2,4,3],In,D).
% In = [4],
% D = [].
 
% find_insertions_and_deletions_vintage([1,2,3],[1,3],In,D).
% In = [],
% D = [2].

find_insertions_and_deletions_vintage(Before,After,Insertions,Deletions) :-
%trace,
 /*
 correspondences(Corr),
 keep(Kept),
 findall(A1,(member(A1,Before),get_base_token_number(A1,A10),member([Comm,A10],Corr),
 (string_concat(Comm1,",",Comm)->true;Comm1=Comm),catch(term_to_atom(A2,Comm1),_,fail),A2=[[_,Name],Args],length(Args,Arity),member([Name,Arity],Kept)),Before1),
 %append(After,Before1,After2),
 subtract(Before,Before1,Before2),
 */
 subtract(After,Before,Insertions),
 subtract(Before,After,Deletions).

find_combos1_vintage(Insertions,Deletions,Combos) :-
 findall([i,In],member(In,Insertions),In1),
 findall([d,De],member(De,Deletions),De1),
 append(In1,De1,Ops),
 find_combos2_vintage(Ops,[],Combos).

find_combos2_vintage([],Combos,Combos).
find_combos2_vintage(Ops,Combos1,Combos2) :-
 Ops=[Op|Ops1],
 member(Switch,[on,off]),
 append(Combos1,[[Op,Switch]],Combos3),
 find_combos2_vintage(Ops1,Combos3,Combos2).

%findall([Op,Switch],(member(Op,Ops),member(Switch,[on,off])),Switches).
% ((member(In,Insertions)->true;In=[]),
% (member(De,Deletions)->true;De=[]),
 
find_combos3_vintage([],_Combos,Combos,Combos) :- !.
find_combos3_vintage(After,Combos,Combos1,Combos2) :-
 After=[Item1|After2],
 ((Item1=[Type,N],
 member([[Type,N],Switch],Combos),
 ((%Type=i,
 Switch=on)->Item=[N];Item=[]))->true;Item=[Item1]),
 append(Combos1,Item,Combos3),
 find_combos3_vintage(After2,Combos,Combos3,Combos2),!.
