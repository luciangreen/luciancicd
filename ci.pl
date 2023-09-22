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

*/

:-include('../Prolog-to-List-Prolog/p2lpconverter.pl').
%:-include('../List-Prolog-to-Prolog-Converter/lp2pconverter.pl').
%:-include('luciancicd.pl').
:-dynamic term_to_numbers1/1.
%:-dynamic term_to_numbers2/1.
:-dynamic changes/1.
:-dynamic correspondences/1.

/*cicd(Path) :-
 merge(Path),
 build_and_test.
*/

get_file(Type,File1,S1) :-
 (Type=file->(exists_file_s(File1)->
 (fastp2lp(File1,S1)
 %p2lpconverter([Type,File1],S1)
 ->true;
 open_string_file_s(File1,S10),
 lines_to_comments(S10,S1)));
 fastp2lp(File1,S1)
 %p2lpconverter([Type,File1],S1)
 ),!.

/*
set_up_merge(Type,File1) :-
 get_file(Type,File1,S1),
 %p2lpconverter([Type,File1],S1),
 pp0(S1,S2),S2=S3,%term_to_atom(S2,S3),
 open_s("test.lp",write,S21),
 write(S21,S3),close(S21).
*/

% build saves over gh (which is moved to lc) with a command

merge(K11,File1,Path1,Tests) :-
%writeln(merge(K11,File1,Path1,Tests)),``
%trace,%trace,
%%%%%%%%%*****Change gh2 to gh
 %foldr(string_concat,["../../Github2/",K11,"/",File1],Path11),
 get_file(file,Path1,S1),
 %p2lpconverter([Type,File1],S1),
 %pp0(S1,S2),S2=S3,%term_to_atom(S2,S3)
 %trace,
 split_string(Path1,"/","/",P2),
 append(_,[P3],P2),

 working_directory1(A2,A2),

 home_dir(A1),

 working_directory1(_,A1),


 %working_directory1(_,"../../Github_lc/"),
foldr(string_concat,["../../Github_lc/tests_",K11,".txt"%"/",File1
 ],File2),
 
 foldr(string_concat,["[[n,comment],[[""File delimiter"",""../../Github_lc/",K11,""",""",P3,"""]]]"],String0),
 catch(open_file_s(File2,[_,Old_S11]),_,Old_S11=["[",String0,"]"]),
 
 foldr(string_concat,Old_S11,Old_S112),
 
 term_to_atom(Old_S1121,Old_S112),
 %trace,
 split_into_lp_files(Old_S1121,Old_S113),
 
 foldr(string_concat,["../../Github_lc/",K11],PZ),
 %trace,
 foldr(string_concat,[P3],FZ),
 
 (once(member([[[n, comment], [["File delimiter", PZ, FZ]]]|Old_S1],Old_S113))->true;Old_S1=[]),
 
 %term_to_atom(Old_S114,Old_S1141),
 %findall([AT1,",\n"],(member(AT1,Old_S11)),AT12),flatten(AT12,AT1x),%)),AT12),
 %append(AT14,[_],AT1x),
 %foldr(string_concat,Old_S1141,AT135),
 %foldr(string_concat,["[",AT135,"]"],AT132),
 %term_to_atom(AT134,AT135),
 %foldr(append,AT131,AT133),
 %trace,
 %pp0(AT133,AT134),
 %Old_S114=[_|Old_S1],
 %split_string(AT134,"\n","\n",AT13)
 %,trace 
  working_directory1(_,A2),

 %trace,
 %open_file_s(File2,Old_S1),
 (S1=Old_S1->
 (%trace,
 writeln(["Files",K11,"/",File1,"and in",File2,"are identical"]),
 ci_fail(Ci_fail),
 append(Ci_fail,[1],Ci_fail1),
 retractall(ci_fail(_)),
 assertz(ci_fail(Ci_fail1))
 %Tests=[]%fail%abort
 %Tests=[[K11,File1,Old_S1,S1]]%fail%abort
 );(
 ci_fail(Ci_fail),
 append(Ci_fail,[0],Ci_fail1),
 retractall(ci_fail(_)),
 assertz(ci_fail(Ci_fail1))
 )),%trace,
 foldr(string_concat,["../../Github_lc/",K11],K12),
 %trace,
 %term_to_atom(String01,String0),
 %(append(String01,_,Old_S1)->Old_S1=Old_S10;
 %append([String01],Old_S1,Old_S10)),
 Old_S1=Old_S10,
 %(append(String01,_,S1)->S1=S10;
 %append([String01],S1,S10)),
 S1=S10,
 %trace,
 Tests=[[K12,File1,Old_S10,S10]],!.
 
merge2(Old_S1,S1,T3) :-
 %open_s("test.lp",write,S21),
 %write(S21,S3),close(S21),
 retractall(term_to_numbers1(_)),
 assertz(term_to_numbers1(1)),
 term_to_numbers(term_to_numbers1,Old_S1,[],Corr,[],N1),
 term_to_numbers(term_to_numbers1,S1,Corr,Corr2,[],N2),
 retractall(correspondences(_)),
 assertz(correspondences(Corr2)),
 diff_group_combos(N1,N2,C),
 findall(T,(member(C1,C),numbers_to_term(C1,Corr2,[],T0)%,lp2p1(T0,T)
 ,T=T0
 ),T1),
 delete(T1,[],T31),
 
 %subtract(Combos411,[[]],Combos412),
 /*
 findall(Combos413,(member(Combos413,Combos412),
 sort(Combos413,Combos414),
 sort(Before,Before1),
 not(Combos414=Before1)),Combos41),
 */
 sort(T31,Combos41),
 %subtract(Combos413,[Old_S1],Combos41),
 sort_by_length(Combos41,T3),
 
 !.
 
 
merge21(Old_S11,S11,T3) :-
 retractall(term_to_numbers1(_)),
 assertz(term_to_numbers1(1)),

 findall(A,(member([_,A1],Old_S11),term_to_atom(A1,A)),Old_S1),
 findall(A,(member([_,A1],S11),term_to_atom(A1,A)),S1),
 term_to_numbers(term_to_numbers1,Old_S1,[],Corr,[],N1),
 %trace,
 term_to_numbers(term_to_numbers1,S1,Corr,Corr2,[],N2),
 
 
 length(Old_S11,Old_S11L),
 numbers(Old_S11L,1,[],Old_S11N),
 findall([A,C],(member(Old_S11N1,Old_S11N),get_item_n(Old_S11,Old_S11N1,[A1,_]),
 (A1=[n, comment]->A=_;A=A1),
 get_item_n(N1,Old_S11N1,C)),N11),

 length(S11,S11L),
 numbers(S11L,1,[],S11N),
 findall([A,C],(member(S11N1,S11N),get_item_n(S11,S11N1,[A1,_]),
  (A1=[n, comment]->A=_;A=A1),get_item_n(N2,S11N1,C)),N21),
 append(N11,N21,N31),
 

 retractall(correspondences(_)),
 assertz(correspondences(Corr2)),
 diff_group_combos1(N1,N2,C),
 %trace,
 findall([N32,T],(member(C1,C),numbers_to_term([C1],Corr2,[],T0),T0=[T02],
 term_to_atom(T01,T02),
 %trace,
 member([N32,C1],N31),%,lp2p1(T0,T)
 T=T01,
 not(T=[])
 ),T31),
 sort1(T31,T3),
 %delete(T1,[],T31),
 
 %sort(T31,Combos41),
 %subtract(Combos413,[Old_S1],Combos41),
 %sort_by_length(Combos41,T3),
 %findall([A3,A2],(member(A1,T32),term_to_atom(A2,A1),member([A3,A2],N31)),T3),
 !. 
 %T3=[T4|_],
 %open_s("test.pl",write,S22),
 %write(S22,T4),close(S22).

/*
merge2a(Old_S1,S1,T3) :-
 %open_s("test.lp",write,S21),
 %write(S21,S3),close(S21),
 retractall(term_to_numbers1(_)),
 assertz(term_to_numbers1(1)),
 term_to_numbers(Old_S1,[],Corr,[],N1),
 term_to_numbers(S1,Corr,Corr2,[],N2),
 diff_group_combos(N1,N2,C),
 findall(T,(member(C1,C),numbers_to_term(C1,Corr2,[],T)%,
 %lp2p1(T0,T)
 ),T1),
 delete(T1,[],T3),!.
*/

get_token_number(N1,S1,N,N2) :-
%writeln(get_token_number(N1,S1,N,N2)),
 %trace,
 %findall(*,(member(AAA,N1),))
 findall(S1xx,(member(S1x,N1),(number(S1x)->number_string(S1x,S1xx);%S1x=S1xx),
 get_base_token_number(S1x,S1xx))),S2),
 (number(S1)->number_string(S1,S1xxx);%S1x=S1xx),
 get_base_token_number(S1,S1xxx)),
 findall(S2,member(S1xxx,S2),S3),
 length(S3,L),
 foldr(string_concat,[N,".",L,"x"],N2),!.

get_base_token_number(S1x,S1) :-
 split_string(S1x,".x",".x",[S1|_]),!. 

term_to_numbers(_,[],C,C,N,N) :- !.
term_to_numbers(term_to_numbers1,S,C1,C2,N1,N2) :-
 S=[S1|S2],
 (member([S1,N],C1)->
 (C1=C3,
 get_token_number(N1,S1,N,N2A));
 (term_to_numbers1(N2A1),
 retractall(term_to_numbers1(_)),
 N4 is N2A1+1,
 assertz(term_to_numbers1(N4)),
 number_string(N2A1,N2AA),
 append(C1,[[S1,N2AA]],C3),
 foldr(string_concat,[N2AA,".",0,"x"],N2A)
 )),
 %trace,
 %(member(N2A,N1)->(%trace,
 %get_token_number(N1,N2A,N2A,N2A2));N2A2=N2A),
 append(N1,[N2A],N3),
 term_to_numbers(term_to_numbers1,S2,C3,C2,N3,N2),!.


 /*
term_to_numbers(term_to_numbers2,S,C1,C2,N1,N2) :-
 S=[S1|S2],
 (member([S1,N],C1)->
 C1=C3;
 (term_to_numbers2(N),
 retractall(term_to_numbers2(_)),
 N4 is N+1,
 assertz(term_to_numbers2(N4)),
 append(C1,[[S1,N]],C3))),
 append(N1,[N],N3),
 term_to_numbers(S2,C3,C2,N3,N2),!.
*/
numbers_to_term([],_,T,T) :- !.
numbers_to_term(SN,C1,T1,T2) :-
%trace,
 SN=[SN1|SN2],
 get_base_token_number(SN1,SN3),
 member([S1,SN3],C1),
 append(T1,[S1],T3),
 numbers_to_term(SN2,C1,T3,T2),!.

 
/*

X:

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

diff_combos([1,4,6,5],[1,5],C).
C = [[1, 4, 5], [1, 4, 6, 5], [1, 5], [1, 6, 5]].

diff_combos([4,6,5],[5],C).
C = [[4, 5], [4, 6, 5], [5], [6, 5]].

diff_combos([5],[4,5],C).
C = [[4, 5], [5]].

---

do all in one go without splitting on \n xx
don't worry about 121 131 repeating units, just group

[1, 2, [d, 5], 1, [i, 4]]

- join same types between non i,d
- change i,d in same space to c (change)

*/

%/*

% use in find_insertions_and_deletions,
% but need to differentiate same values [1,1] as 1_1 and 1_2

/*
subtract1(A,B,C) :-
 subtract1(A,B,[],C),!.
  
subtract1([],_,B,B) :- !.
subtract1(A,B,C,G) :-
 A=[D|E],
 (member(D,B)->
 (delete(B,D,F),
 append(C,[D],H));
 (F=B,C=H)),
 subtract1(E,F,H,G),!.

subtract2(A,B,C) :-
 subtract2(A,B,[],C),!.

subtract2([],_,B,B) :- !.
subtract2(A,B,C,G) :-
 A=[[D,D1]|E],
 (member([D,_],B)->
 (delete(B,[D,_],F),
 append(C,[[D,D1]],H));
 (F=B,C=H)),
 subtract2(E,F,H,G),!.
*

differentiate(A,B) :-
 differentiate(A,[],_,[],B),!.
differentiate([],Corrs,Corrs,Diffs,Diffs) :- !.
differentiate(List,Corrs1,Corrs2,Diffs1,Diffs2) :-
 List=[A|B],
 findall([N,A],member([N,A],Corrs1),C),
 (C=[]->(N1=1,append(Corrs1,[[N1,A]],Corrs3));
 (sort(C,C1),append(_,[[N2,_]],C1),
 N1 is N2+1,append(Corrs1,[[N1,A]],Corrs3))),
 append(Diffs1,[[A,N1]],Diffs3),
 differentiate(B,Corrs3,Corrs2,Diffs3,Diffs2).

%subtract2(A,B,C) :-
break_into_tokens(A,B) :-
 string_codes(A,A1),
 split_on_substring117(A1, `#@~%$?-+*^,()|.:;=_/[]<>{}\n\r\s\t\\"!\``,[],B),!. 
*/

fail_if_greater_than_n_changes(N,After3) :-
%trace,
 findall(A,(member(A,After3),not(string(A))),B),
 length(B,L),L=<N.

 

diff_group_combos(A,A,[A]) :- !.
diff_group_combos(Before,After,Combos4) :-
 %length(Before,BL),length(After,AL),
 %((A>10->true;B>10)->fail;true),
/*
 sort(After0,After01),sub*
 retractall(term_to_numbers2(_)),
 assertz(term_to_numbers2(-1000000)),
 term_to_numbers(term_to_numbers2,Old_S1,[],Corr,[],N1),
 term_to_numbers(term_to_numbers1,S1,Corr,Corr2,[],N2),
 diff_group_combos(N1,N2,C),
 findall(T,(member(C1,C),numbers_to_term(C1,Corr2,[],T0),
*/

 retractall(changes(_)),
 assertz(changes(1)),
 %differentiate(Before,Before0),
 %differentiate(After,After0),
 find_insertions_and_deletions(Before,After,Insertions,Deletions),
 

 
 replace11(After,Insertions,[],After2),
 replace12(Before,After2,Deletions,[],After31),
 join_and_change(After31,[],After3),
 %trace,
 save_diff_html(After3),
 fail_if_greater_than_n_changes(7,After3),
 %length(After3,L)
 findall([[i,_],I],(member([[i,_NA],I],After3)%,not(number(NA))
 ),Insertions1),
 findall([[d,_],D],(member([[d,_NB],D],After3)%,not(number(NB))
 ),Deletions1),
 %findall([c,C],(member([[c,_],C],After3)),Changes1),
 findall(Combos,find_combos1(Insertions1,Deletions1,%Changes1,
 Combos),Combos2),
 findall(Combos10,(member(Combos3,Combos2),
 find_combos3(After3,Combos3,[],Combos1),
 flatten(Combos1,Combos10)),Combos4),
 %subtract(Combos411,[[]],Combos412),
 /*
 findall(Combos413,(member(Combos413,Combos412),
 sort(Combos413,Combos414),
 sort(Before,Before1),
 not(Combos414=Before1)),Combos41),
 */
 %sort(Combos412,Combos413),
 %subtract(Combos413,[Before],Combos41),
 %sort_by_length(Combos41,Combos4),
 (Combos4=[]->fail;true),
 %length(Combos4,L),
 %trace,
 %(L>100->fail;true),
 !.
%diff_group_combos(_Before,_After,[]).
diff_group_combos(_Before,After,[After]).



diff_group_combos1(A,A,[A]) :- !.
diff_group_combos1(Before,After,Combos4) :-

 retractall(changes(_)),
 assertz(changes(1)),
 %differentiate(Before,Before0),
 %differentiate(After,After0),
 find_insertions_and_deletions(Before,After,Insertions,Deletions),
 

 
 replace11(After,Insertions,[],After2),
 replace12(Before,After2,Deletions,[],After31),
 %trace,
 join_and_change(After31,[],After3),
 %trace,

 findall(A1,(member(A,After3),
 (string(A)->A1=[A];
 (A=[[c,_],O,N]->
 A1=[O,N];
 A=[[_, _], E]->
 A1=[E]))),A2),
 flatten(A2,Combos4),

 %findall(A,member([[_,_NA],A],After3),Combos4),
 !.
%diff_group_combos(_Before,_After,[]).
diff_group_combos1(_Before,After,[After]).

sort_by_length(A,F) :-
 findall([L,B],(member(B,A),length(B,L)),C),sort(C,D),findall(E,member([_,E],D),F).
 
%join_and_change(After31,After3) :-
 
i_or_d([i,_]).
i_or_d([d,_]).
 
%group(-,After4,After4) :- !.
group(L,After4,After41) :-
 findall(A,(member([L,A],After4)),After42),
 (not(After42=[])->
 After41=[[[L,-]
 ,After42]];
 After41=[]),!.

find_change(%After40,
After41,After42,After43) :-
 ((not(After41=[]),not(After42=[]))->
 (After41=[[_,A1]],After42=[[_,A2]],
 changes(N),
 N1 is N+1,
 retractall(changes(_)),
 assertz(changes(N1)),
 %append(A1,A2,A3),After43=[[[c,N],A3]]
 After43=[[[c,N],A1,A2]%,[[d,N],A2]
 ]);
 append(After41,After42,After43)),!.
 
join_and_change([],%_Insertions,
After,After) :- !.
join_and_change(After,%Insertions,
After2,After3) :-
 After=[After4|After5],
 not(i_or_d(After4)),
 append(After2,[After4],After6),
 join_and_change(After5,%Insertions,
After6,After3),!.

/*
 append(After4,After5,After),
 not(After4=[]),
 %not(i_or_d(After4)),
 %forall(member(After45,After4),not(i_or_d(After45)))->

 append(After53,_After54,After5),
 not(After53=[]),
 (i_or_d(After53)),
 append(After4,After53,After500),
 (After500=[]->After2=After3;
 append(After2,After4,After6),
 join_and_change(After5,After6,After3)),!.
*/
join_and_change(After,%Insertions,
After2,After3) :-
 %After=[After4|After5],

 append(After4,After5,After),
 not(After4=[]),
 %forall(member(After45,After4),not(i_or_d(After45)))->

 append(After53,_After54,After5),
 (After5=[]->true;(not(After53=[]),
 %append(After51,_After52,After54),
 %not(After51=[]),
 [After531]=After53,
 not(i_or_d(After531)))),
 
 %group(-,After53,After40),
 group(i,After4,After41),
 group(d,After4,After42),
 find_change(%After40,
 After41,After42,After43),
 
 %After53=[],
 %After43=After4)),

 foldr(append,[After2,%After4,
 %After50,
 After43],After6),
 %After50=After5,
 %;(append(After2,After4,After6),
 %*x append(After53,After54,After50))),
 %After50=After5)),
 join_and_change(After5,After6,After3),!.

join_and_change(A,%_Insertions,
After1,After2) :- append(After1,A,After2),!.

/*
diff_combos(A,A,[]) :- !.
diff_combos(Before,After,Combos4) :-
 find_insertions_and_deletions(Before,After,Insertions,Deletions),
 replace11(After,Insertions,[],After2),
 replace12(Before,After2,Deletions,[],After3),
 findall(Combos,find_combos1(Insertions,Deletions,Combos),Combos2),
 findall(Combos1,(member(Combos3,Combos2),
 find_combos3(After3,Combos3,[],Combos1)),Combos41),
 sort(Combos41,Combos4),!.
*/

replace11([],_Insertions,After,After) :- !.
replace11(After,Insertions,After2,After3) :-
 After=[After4|After5],
 (member(After4,Insertions)->
 After7=[i,After4];
 After7=After4),
 append(After2,[After7],After6),
 replace11(After5,Insertions,After6,After3),!.

replace12(_,After,[],_After1,After) :-
 %append(After1,[A],After2),
 !.

replace12(Before,After,Deletions,After2,After3) :-
 %Before=[B|Bs],
 After=[[i,A]|As],
 append(After2,[[i,A]],After4),
 replace12(Before,As,Deletions,After4,After3),!.

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
 replace12(Before52,After52,Deletions,After6,After3).
replace12(Before,After,Deletions,After2,After3) :-
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

find_combos1(Insertions,Deletions,%Changes,
Combos) :-
 %findall([i,In],member(In,Insertions),In1),
 %findall([d,De],member(De,Deletions),De1),
 %findall([c,Ch],member(Ch,Changes),Ch1),
 foldr(append,[Insertions,Deletions%,Changes
 ],Ops),
 find_combos2(Ops,[],Combos).

find_combos2([],Combos,Combos).
find_combos2(Ops,Combos1,Combos2) :-
 Ops=[Op|Ops1],
 %trace,
 %member(Switch,[on,off]),
 (Op=[[i,_],_]->(S=[on,off
 ]%,S2=n
 );(Op=[[d,_],_],S=[on,off
 ]%,S2=_)
 )),
 member(Switch,S),
 append(Combos1,[[Op,Switch]],Combos3),
 find_combos2(Ops1,Combos3,Combos2).

%findall([Op,Switch],(member(Op,Ops),member(Switch,[on,off])),Switches).
% ((member(In,Insertions)->true;In=[]),
% (member(De,Deletions)->true;De=[]),

find_combos3([],_Combos,Combos,Combos) :- !.

find_combos3(After,Combos,Combos1,Combos2) :-
 After=[Item1|After2],
 %trace,
 (Item1=[_Type,N1,N2],member(Item,[N1,N2])),
 append(Combos1,Item,Combos3),
 find_combos3(After2,Combos,Combos3,Combos2).


find_combos3(After,Combos,Combos1,Combos2) :-
 After=[Item1|After2],
 not(Item1=[_,_,_]),
 %trace,
 %((Item1=[Type,N1,N2],member(Item,[N1,N2]))->true;
 ((Item1=[Type,N],
 member([[Type,N],Switch],Combos),
 ((%Type=i,
 Switch=on)->Item=[N];Item=[]))->true;(%number
 string(Item1)->Item=[Item1];Item=[])
 ),
 append(Combos1,Item,Combos3),
 find_combos3(After2,Combos,Combos3,Combos2).
/*
find_combos2([],Combos,Combos).
find_combos2(Ops,Combos1,Combos2) :-
%trace,
 Ops=[[[Op,N1],A]|Ops1], 
 % for n, i or d
 %member([_,A],Ops),
 %changes(N),
 %member(Op,N),
 (number(N1)->
 (S3=[i,d],
 member(Switch,S3),
 S2=_);
 
 ((Op=i->(S=[on,off
 ],S2=n);(S=[on%,off
 ],S2=_)),
 member(Switch,S%[on,off
 %]
 ))),
 append(Combos1,[[[[Op,S2],A],Switch]],Combos3),
 find_combos2(Ops1,Combos3,Combos2).

%findall([Op,Switch],(member(Op,Ops),member(Switch,[on,off])),Switches).
% ((member(In,Insertions)->true;In=[]),
% (member(De,Deletions)->true;De=[]),
 
find_combos3([],_Combos,Combos,Combos) :- !.
find_combos3(After,Combos,Combos1,Combos2) :-
%trace,
 After=[Item1|After2],
 (Item1=[Type1,N1], %T=N, i or d
 member([[Type,N],Switch],Combos),%trace,
 Type=[T,_],Type1=[T,_],(Type1=[_,ID],(%)->
 number(N)->(ID=i->Switch1=on;(ID=d->Switch1=off));Switch=Switch1),
 ((%Type=i,
 Switch1=on)->Item=[N];Item=[]))->true;Item=[Item1]),
 append(Combos1,Item,Combos3),
 find_combos3(After2,Combos,Combos3,Combos2),!.

*
find_combos3([],_Combos,Combos,Combos) :- !.
find_combos3(After,Combos,Combos1,Combos2) :-
%trace,
 After=[Item1|After2],
 ((Item1=[Type1,N],
 member([[Type,N],Switch],Combos),%trace,
 Type=[T,_],Type1=[T,_],(Type1=[_,n]->number(N1);true),
 ((%Type=i,
 Switch=on)->Item=[N];Item=[]))->true;Item=[Item1]),
 append(Combos1,Item,Combos3),
 find_combos3(After2,Combos,Combos3,Combos2),!.
*/