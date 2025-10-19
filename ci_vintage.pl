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
 %trace,
 %find_insertions_and_deletions_vintage(Before,After,Insertions1,Deletions1,Permanent_insertions),
 


 %Permanent_insertions=[],
  diff_lists(Before,After,%Insertions,Deletions,Permanent_insertions, [], 
 After31),
 
 
keep1(Kept),
 %trace,
 
 findall(A1,(member(A0,%[_,A1],
 After31),%get_base_token_number(A1,A10),member([Comm,A10],Corr),
%((
(A0=[_,A00],((catch(((string_concat(Comm2,",",% was ",",
A00)->true;Comm2=A00),(string_concat(",",Comm1,% was ",",
Comm2)->true;Comm2=Comm1),term_to_atom(A2,Comm1)),_,fail),A2=[[_,Name],Args],length(Args,Arity),member([Name,Arity],Kept),A1=A00%A1=[p,A00]
)->true;
(A0=[i,_]->A1=A0;
(A0=[d,_]->A1=A0)))->true;
A0=A1
)
%)->true;Comm=",")
),After3),
%sort(Permanent_insertions1,Permanent_insertions),

 
 findall(Insertions2,member([i,Insertions2],After3),Insertions),
 findall(Deletions2,member([d,Deletions2],After3),Deletions),
 findall(Permanent_insertions2,member([p,Permanent_insertions2],After3),Permanent_insertions),
 
 %delete(Insertions1,Permanent_insertions,Insertions),
 %delete(Deletions1,Permanent_insertions,Deletions),
 

%id_to_e(After31,After3),
 %trace,
%diff(Before,After,Insertions1,Deletions1,Insertions,Deletions,Permanent_insertions,[],After3),

 %replace11_vintage(After,Insertions,Permanent_insertions,[],After2),
 %trace,
 %append(Before,["*"],Before1),
 %append(After2,["*"],After21),
 %replace12_vintage(Before,After2,Deletions,[],After3),
 %delete(After31,"*",After3),
 %save_diff_html(After3),
 fail_if_greater_than_n_changes(After3),
  %trace,
  findall(Combos,find_combos1_vintage(Insertions,Deletions,Permanent_insertions,Combos),Combos2),
 findall(Combos1,(member(Combos3,Combos2),
 find_combos3_vintage(After3,Combos3,[],Combos1)),Combos41),
 sort(Combos41,Combos4),!.
 
diff_combos_vintage(_Before,After,[After]) :- !.

/*
replace11_vintage([],_Insertions,_Permanent_insertions,After,After) :- !.
replace11_vintage(After,Insertions,Permanent_insertions,After2,After3) :-
 After=[After4|After5],
 (member(After4,Insertions)->
 After7=[i,After4];
 (member(After4,Permanent_insertions)->
 After7=[p,After4];
 After7=After4)),
 append(After2,[After7],After6),
 replace11_vintage(After5,Insertions,Permanent_insertions,After6,After3),!.

replace12_vintage(_,After,[],_After1,After) :-
 %append(After1,[A],After2),
 !.

replace12_vintage(Before,After,Deletions,After2,After3) :-
 %Before=[B|Bs],
 %trace,

 After=[[I,A]|As],
 %writeln([*,A]),
 %(A="9.0x"->trace;true),
 (I=i->true;I=p),
 append(After2,[[I,A]],After4),
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
*/

% find_insertions_and_deletions_vintage([1,2,3],[1,2,4,3],In,D).
% In = [4],
% D = [].
 
% find_insertions_and_deletions_vintage([1,2,3],[1,3],In,D).
% In = [],
% D = [2].

find_insertions_and_deletions_vintage(Before,After,Insertions1,Deletions1,Permanent_insertions2) :-
%trace,
 %/*
 correspondences(Corr),
 keep1(Kept),
 findall(A1,(member(A1,After),get_base_token_number(A1,A10),member([Comm,A10],Corr),
%((
(string_concat(Comm1,",",% was ",",
Comm)->true;Comm1=Comm),catch(term_to_atom(A2,Comm1),_,fail),A2=[[_,Name],Args],length(Args,Arity),member([Name,Arity],Kept)
%)->true;Comm=",")
),Permanent_insertions1),
sort(Permanent_insertions1,Permanent_insertions),
 %Permanent_insertions=[],
 %()subtract(After,After1,After2),
 %*/
 
 findall([M21,M11],(member(M21,After),get_base_token_number(M21,M11)),After2),
 findall([M22,M12],(member(M22,Before),get_base_token_number(M22,M12)),Before2),
 
 %findall(P11,(member([P11,P12],M31),member([P13,P14],M32),not(P12=P14)),After11),
 %sort(After11,After1),
 %findall(P11,(member([P11,P12],M32),member([P13,P14],M31),not(P12=P14)),Before11),
% sort(Before11,Before1),
%trace,
 %subtract(M31,M32,After1),
 %subtract(M32,M31,Before1),
 subtract_civ(After2,Before2,[],After1),
 subtract_civ(Before2,After2,[],Before1),
 
 %Before1=Insertions,
 %After1=Deletions.
 
 append(Before,After,Both),
 %trace,
 findall(A1,(member(A1,Both),%A1=A10,
 get_base_token_number(A1,A10),
 member([",",A10],Corr)),%)->A11=[A1];
 A11%=[]
 ),
 
 subtract(After1,Permanent_insertions,Insertions),
 subtract(Before1,Permanent_insertions,Deletions),

 subtract(Permanent_insertions,A11,Permanent_insertions2),
 union(A11,Insertions,Insertions1),
 subtract(Deletions,A11,Deletions1).
/*
find_insertions_and_deletions_vintage_old(Before,After,Insertions,Deletions) :-
 subtract(After,Before,Insertions),
 subtract(Before,After,Deletions),!.
*/
subtract_civ([],_M32,A,A) :- !.
subtract_civ(M31,M32,A1,A2) :-
 M31=[[C,D]|E],
 (member([_,D],M32)->
 A1=A3;
 append(A1,[C]%[[C,D]]
 ,A3)),
 subtract_civ(E,M32,A3,A2).


find_combos1_vintage(Insertions,Deletions,Permanent_insertions,Combos) :-
 findall([i,In],member(In,Insertions),In1),
 findall([d,De],member(De,Deletions),De1),
 findall([p,Pe],member(Pe,Permanent_insertions),Pe1),
 foldr(append,[In1,De1,Pe1],Ops),
 find_combos2_vintage(Ops,[],Combos).

find_combos2_vintage([],Combos,Combos).
find_combos2_vintage(Ops,Combos1,Combos2) :-
 Ops=[Op|Ops1],
 (Op=[p,_]->Switch=on;
 member(Switch,[on,off])),
 append(Combos1,[[Op,Switch]],Combos3),
 find_combos2_vintage(Ops1,Combos3,Combos2).

%findall([Op,Switch],(member(Op,Ops),member(Switch,[on,off])),Switches).
% ((member(In,Insertions)->true;In=[]),
% (member(De,Deletions)->true;De=[]),
 
find_combos3_vintage([],_Combos,Combos,Combos) :- !.
find_combos3_vintage(After,Combos,Combos1,Combos2) :-
 After=[Item1|After2],
 ((Item1=[Type,N],
 %(%catch(get_base_token_number(N,N1),_,false)->true;true),
 %trace,
 %[]=N1),
 %trace,
 (%member([[Type,N1],Switch],Combos)->N2=N1;
 (member([[Type,N],Switch],Combos),N2=N)),
 ((%Type=i,
 %true%
 Switch=on
 )->Item=[N2];Item=[]))->true;Item=[Item1]),
 append(Combos1,Item,Combos3),
 find_combos3_vintage(After2,Combos,Combos3,Combos2),!.
