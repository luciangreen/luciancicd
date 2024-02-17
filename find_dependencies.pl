% find_dependencies - finds dependencies of predicates, orders bottom up

% Deps - after ordering sm, finds deps of sm pred nums

% modes not types

% modes will help add commands to sm, for conversion to c

:-include('../Philosophy/sub_term_with_address.pl').
%:-include('../SSI/ssi.pl'). %XXX
%:-dynamic resort_n/1.
find_dependencies(Dep99_name,Dep99_arity,F,F2
,Functions2b,Pred_numbers2) :-
%trace,
%writeln1(find_dependencies(Dep99_name,Dep99_arity,F,F2,Functions2b,Pred_numbers2)),
%trace,
/*
A=[15
],
findall(%[B,
Functions2b%]
,(_Debug=off,member(B,A),
*/
%trace,
%test(248%
%15
%,Q,F,_R),
numbers(Dep99_arity,1,[],V2),

%trace,
length(Q4,Dep99_arity),
Q5=[[n,Dep99_name],Q4|_],

findall([v,V3],(member(V1,V2),atom_concat("a",V1,V3)),V),
Q=[[n,Dep99_name],V],
query_box(Q,_Query1,F,Functions1),
%trace,
%get_n_item(Functions1,Q,N2), x
%N2=1,%XXX
findall(N2,get_n_item(Functions1,Q5,N2),N2s),
findall(N22,(member(N21,N2s),N22 is N21-1),N22s),%*N1 is N2-1,

convert_to_grammar_part1(Functions1,[],Functions2,_),add_line_numbers_to_algorithm1(Functions2,Functions2a),find_pred_numbers_to_cut(Functions2a,[],Pred_numbers),find_state_machine1(Functions2a,Functions3,Pred_numbers),
%trace,
a_to_m2(N22s,Functions3%2a%3
,Pred_numbers,
Functions2b),

%trace,
%/*
findall([NF,Arity,PN2],(member(PN1,Functions2b),member([NF,Arity,PN2],Pred_numbers),%member(PN1%
(PN1=[loop1,PN11]->member(PN12,PN11);PN12=PN1),member(PN12
,PN2)),Pred_numbers21),%Pred_numbers21=Pred_numbers2,%
sort(Pred_numbers21,Pred_numbers2),
%Pred_numbers21=Pred_numbers2,
%*/
%Pred_numbers2=Pred_numbers,

findall([NF,Args|Rest],(member([NF,Arity,_],Pred_numbers2),
member([NF,Args|Rest],F),length(Args,Arity)),F21),%F21=F2,%
sort(F21,F2),
!.
%a_and_m_to_clp(Functions3,Functions2b,Functions2c),
%lp_to_c(Functions2c,Functions2d)

%),Functions2d).%find_pred_numbers_dependencies2(Functions3,Functions2b),lucianpl(Debug,Q,F,R1),Functions2b=[[_|_]|Functions2c],lucianpl(off,Q,Functions2c,R2),(R1=R2->Result=success;Result=fail),writeln([B,Result])),C),sort(C,C1),writeln(C1).

% could run in lp or compile and run in c (taking alg file as argument)

find_pred_numbers_to_cut(Functions2a,Functions2ab,Pred_numbers) :-
 find_pred_numbers(Functions2a,Functions2ab,Pred_numbers),!.
 
a_to_m(N1,Functions1,Pred_numbers,
Ordered_pred_nums1) :-
%trace,
%find pred nums in sm (done)

% find occurrence of pred calls in preds regardless of clause, for finding modes in bottom-up order
% find order in terms of pred name, arity
% x modify find_pred_numbers

find_pred_numbers_dependencies(Functions1,[],Functions2a,Pred_numbers),
N=0,
%member([N,P],Functions2a),
%delete(Functions2a,[N,P],F),

% order_preds_bottom_up1_post_order_dfs(_L1,[N],Functions2a,[N],Ordered_pred_nums0,[N]),
% Ordered_pred_nums0=[0, [1, 2, [loop, 1]]]
% In test 7, query box (predicate 0) and predicate 1 are called, where predicate 1 calls predicate 2 and itself.


%order_preds_bottom_up1_post_order_dfs(1,Functions2a,[],Ordered_pred_nums0),
%trace,
order_preds_bottom_up1_post_order_dfs(_L1,[N],Functions2a,[],Ordered_pred_nums01,[N]),

(Ordered_pred_nums01=[Ordered_pred_nums02]->true;
Ordered_pred_nums01=Ordered_pred_nums02),

%flatten_except_loops1(Ordered_pred_nums02,Ordered_pred_nums0),
Ordered_pred_nums02=Ordered_pred_nums0,
%foldr(append,Ordered_pred_nums01,Ordered_pred_nums0),


/*
% find min, max

findall(L,member([L,_]),L1),
sort(L1,L2),
append([Min_L],Rest,L2),
append(_,[Max_L],Rest),
*/

% find_groups([0, [1, 2, [loop, 1]]], [0], A).

% A = [[loop1, 1], 2, 0]

% In post order depth-first search, the order of the predicates to test are predicate 1, which calls itself, predicates 2 and 0.
% loop1 denotes a group of predicates that are in a loop, so have to be tested separately, with their own combinations of changes
trace,
find_groups(Ordered_pred_nums0,[N1],Ordered_pred_nums11,true),
%reverse(Ordered_pred_nums11,Ordered_pred_nums13),
list_to_set(Ordered_pred_nums11,Ordered_pred_nums14),
%reverse(Ordered_pred_nums12,Ordered_pred_nums14),
remove_dups_from_loops(Ordered_pred_nums14,Ordered_pred_nums15),

%trace,
findall(Ordered_pred_nums19,(member(Ordered_pred_nums16,Ordered_pred_nums15),
(Ordered_pred_nums16=[loop1,Ordered_pred_nums17]->(list_to_set(Ordered_pred_nums17,Ordered_pred_nums18),Ordered_pred_nums19=[loop1,Ordered_pred_nums18]);Ordered_pred_nums19=Ordered_pred_nums16
)),Ordered_pred_nums20),

delete(Ordered_pred_nums20,loop,Ordered_pred_nums21),

 findall(E,(member(F,Ordered_pred_nums21),
 (F=[loop1,[A1]]->E=A1;E=F)),Ordered_pred_nums1),
%flatten(Ordered_pred_nums0,Ordered_pred_nums1),
/* bfs:
foldr(append,Ordered_pred_nums0,Ordered_pred_nums),
sort(Ordered_pred_nums,Ordered_pred_nums2),
reverse(Ordered_pred_nums2,Ordered_pred_nums3),
findall(B,member([_,B],Ordered_pred_nums3),Ordered_pred_nums4),
flatten(Ordered_pred_nums4,Ordered_pred_nums1),
*/
!.

%alg_to_modes(Ordered_pred_nums1,Functions1,[],_Var_modes,[],_Functions_with_modes)


%find_pred_numbers_dependencies(Functions1,_Reserved_words,Pred_numbers,Functions2a) :-

% pred group y is called by pred group z
% don't need name, arity

%find_pred_numbers_dependencies2(Functions1,Functions2a,Pred_numbers).

% *** do in sm form instead to find deps
% - attribute calls in preds to groups of preds


%find_pred_numbers_dependencies(Algorithm1,Algorithm2,Pred_numbers) :-

%findall(Pred_nums,member([_Name,_Arity,Pred_nums],Pred_numbers),Pred_numbers1),
	%find_pred_numbers_dependencies(_Algorithm1,[],_Deps,_Pred_numbers),!.

% * need p, clause number in deps

find_pred_numbers_dependencies([],Deps,Deps,_) :- !.
find_pred_numbers_dependencies(Algorithm1,Deps1,Deps2,Pred_numbers) :-
	Algorithm1=[Function1|Functions],
	(Function1=[Number,_Name,_Arguments1,_Symbol1,Body1]
	->%symbol(Symbol1,Symbol2),
	(%trace,
	find_deps3(Body1,%Body2,
	%[],Body2,
	Pred_numbers,
	Deps3),
	foldr(append,Deps3,Deps31),
	append(Deps1,[[Number,
	%Name,Arguments1,Symbol1,Body2
	Deps31]],Deps4)),
	find_pred_numbers_dependencies(Functions,Deps4,Deps2,Pred_numbers)),!.
	
	
find_deps3(Body1,%Body2,Body3,
Pred_numbers,
Deps) :-

 findall(Pred_nums,(member([_Number,[_Dbw_on_true,_Statements1_number],[_Dbw_go_after,_Statements2_number],[_Dbw_on_false,_Return_line_false],[_Dbw_go_to_predicates,_Predicates],[_Dbw_n_or_v1,F]|Arguments1],Body1),
 foldr(append,Arguments1,Arguments),
 length(Arguments,Arity),
 get_lang_word("n",Dbw_n1),Dbw_n1=Dbw_n,
 member([[Dbw_n,F],Arity,Pred_nums],Pred_numbers)
 ),Deps1),
 sort(Deps1,Deps),!.

/*
order_preds_bottom_up_post_order_dfs(_L,Functions,Ordered_pred_nums1,Ordered_pred_nums2) :-
%trace,
Functions=[[N,P]|F],

append(Ordered_pred_nums1,[%[L,
N%]
],Ordered_pred_nums3),
 findall(Ordered_pred_nums21,order_preds_bottom_up1_post_order_dfs(_L1,P,F,Ordered_pred_nums3,Ordered_pred_nums21),Ordered_pred_nums2),!.
 */

% program may have unconnected preds, causing a bug

%order_preds_bottom_up1_post_order_dfs(_,[],_%[[N, []]]
%,Ordered_pred_nums,Ordered_pred_nums) :- 
 %append(Ordered_pred_nums1,[N],Ordered_pred_nums2),
% fail,!.
 
order_preds_bottom_up1_post_order_dfs(_,_,[],Ordered_pred_nums,Ordered_pred_nums,_) :- !.
order_preds_bottom_up1_post_order_dfs(_L,N,Functions,Ordered_pred_nums1,Ordered_pred_nums2,Pre_order1) :-
%L3 is L+1,
% assign a level, record min, max levels

%findall(Ordered_pred_nums8,(
%member(P4,P3),%),Ordered_pred_nums1a),

%foldr(append,[Ordered_pred_nums1,Ordered_pred_nums1a],Ordered_pred_nums1b),

%findall(Ordered_pred_nums31,
%(member([N2,P4],Functions),%L2 is L-1,
(member(P2,N),member([P2,P3],Functions),

order_preds_bottom_up1_post_order_dfs2(P3,P2,Functions,Ordered_pred_nums1,Ordered_pred_nums3,Pre_order1)
%append([P3],Ordered_pred_nums3,Ordered_pred_nums11)

),

%(P3=[]->Ordered_pred_nums3=P2;
%delete(Functions,[P2,P3],Functions2),%member(P1,P),
%order_preds_bottom_up1_post_order_dfs(_L3,P3,Functions,[],Ordered_pred_nums3)))
%append(Ordered_pred_nums3,[N]%[[L,N]]
%,Ordered_pred_nums11),

foldr(append,[Ordered_pred_nums1,Ordered_pred_nums3],Ordered_pred_nums2).

%foldr(append,Ordered_pred_nums3,Ordered_pred_nums5),
%(N=[]->Ordered_pred_nums8=Ordered_pred_nums2;append(Ordered_pred_nums8,[N,P3]%[[L3,P3]]
%,Ordered_pred_nums2))

%),Ordered_pred_nums2)

order_preds_bottom_up1_post_order_dfs2(P3,_P2,_Functions,_Ordered_pred_nums1,Ordered_pred_nums3,_) :-
P3=[],Ordered_pred_nums3=[],!.

order_preds_bottom_up1_post_order_dfs2(P3,_P2,Functions,_Ordered_pred_nums1,Ordered_pred_nums3,Pre_order1) :-
member(P4,P3),flatten(Pre_order1,Ordered_pred_nums2),
member(P4,Ordered_pred_nums2),
append(P5,P6,P3),
append([P4],P7,P6),
order_preds_bottom_up1_post_order_dfs2(P7,_P21,Functions,[],Ordered_pred_nums31,Pre_order1),
foldr(append,[P5,[[loop,P4]],Ordered_pred_nums31],Ordered_pred_nums3),!.
%delete(Functions,[P2,P3],Functions2),%member(P1,P),

order_preds_bottom_up1_post_order_dfs2(P3,_P2,Functions,_Ordered_pred_nums1,Ordered_pred_nums32,Pre_order1) :-
findall(Ordered_pred_nums31,
(member(P4,P3),append(Pre_order1,[P4],Pre_order2),order_preds_bottom_up1_post_order_dfs(_L3,[P4],Functions,[],Ordered_pred_nums3,Pre_order2),append([P4],Ordered_pred_nums3,Ordered_pred_nums31)),Ordered_pred_nums32).


find_groups([],Ordered_pred_nums,Ordered_pred_nums,_) :- !.


find_groups(A,Ordered_pred_nums1,Ordered_pred_nums2,First) :- 
 (number(A)->A=B;(A=[B],number(B))),
 (First=true->Ordered_pred_nums1=Ordered_pred_nums2;
 (member(B,Ordered_pred_nums1)->Ordered_pred_nums1=Ordered_pred_nums2;
 append([B],Ordered_pred_nums1,Ordered_pred_nums2))),
 !.

find_groups(Ordered_pred_nums0,Ordered_pred_nums1,Ordered_pred_nums22,_) :-
 Ordered_pred_nums0=[Ordered_pred_nums3|Ordered_pred_nums4],
 %findall(Ordered_pred_nums2,(
 %member(Ordered_pred_nums41,Ordered_pred_nums4),
%append([Ordered_pred_nums3],Ordered_pred_nums1,Ordered_pred_nums5),
find_groups2(Ordered_pred_nums3,Ordered_pred_nums4,Ordered_pred_nums1,Ordered_pred_nums22).

find_groups2(_,[],Ordered_pred_nums,Ordered_pred_nums) :- !.
%find_groups2(_,[A],Ordered_pred_nums1,Ordered_pred_nums2) :- 
% append([A],Ordered_pred_nums1,Ordered_pred_nums2),!.
find_groups2(_,A,Ordered_pred_nums1,Ordered_pred_nums2) :- 
 (number(A)->A=B;(A=[B],number(B))),
 append([B],Ordered_pred_nums1,Ordered_pred_nums2),!.
find_groups2(Ordered_pred_nums3,Ordered_pred_nums4,Ordered_pred_nums1,Ordered_pred_nums22) :-

%writeln(find_groups2(Ordered_pred_nums3,Ordered_pred_nums4,Ordered_pred_nums1,Ordered_pred_nums22)),

 Ordered_pred_nums4=[Ordered_pred_nums41|Ordered_pred_nums42],
 (contains_loop(Ordered_pred_nums3,Ordered_pred_nums41,[],_)->
 %append(P5,P6,Ordered_pred_nums4),
%append([Ordered_pred_nums41],P7,P6),
%P71=[%Ordered_pred_nums3|
%P7],
%trace,
 ((Ordered_pred_nums4=[Ordered_pred_nums43]->true;
 Ordered_pred_nums4=Ordered_pred_nums43),
 
 ((Ordered_pred_nums43=[[AN|AN2]|_],number(AN))->
 (find_groups([Ordered_pred_nums3,[AN|AN2]],[]%Ordered_pred_nums1
 ,%[],%Ordered_pred_nums24,
 Ordered_pred_nums25,true),
 subtract(Ordered_pred_nums25,Ordered_pred_nums1,Ordered_pred_nums225),

foldr(append,[Ordered_pred_nums225,
Ordered_pred_nums1],Ordered_pred_nums24))
 
 ;

 (%trace,
 %trace,
 %find_groups_replace_loops(%Ordered_pred_nums3,
 %Ordered_pred_nums1,Ordered_pred_nums1a),%P71,%Ordered_pred_nums1,
 in_or_exiting_loop(Ordered_pred_nums3,Ordered_pred_nums43,[],In_loop,[],%Ordered_pred_nums1,
 Exiting_loop%,[],Rest_of_preds
 ),
 %notrace,
find_groups_replace_loops(%Ordered_pred_nums3,
 In_loop,%P71,%Ordered_pred_nums1,
 Ordered_pred_nums311),
find_groups_replace_loops(%Ordered_pred_nums3,
 Exiting_loop,%P71,%Ordered_pred_nums1,
 Ordered_pred_nums312),
 flatten(Ordered_pred_nums311,Ordered_pred_nums321),
 flatten(Ordered_pred_nums312,Ordered_pred_nums322),
 append(Ordered_pred_nums321,[Ordered_pred_nums3],Ordered_pred_nums323),

%trace,
subtract(Ordered_pred_nums322,Ordered_pred_nums1,Ordered_pred_nums324),
%subtract(Ordered_pred_nums1,Ordered_pred_nums322,Ordered_pred_nums111),
%trace,
insert_loop1([loop1,Ordered_pred_nums323],Ordered_pred_nums1,Ordered_pred_nums1b),
list_to_set(Ordered_pred_nums1b,Ordered_pred_nums1c),
foldr(append,[%Rest_of_preds,
Ordered_pred_nums324,%[[loop1,Ordered_pred_nums323]],
Ordered_pred_nums1c%P5,
],Ordered_pred_nums251),
%,notrace
%reverse(Ordered_pred_nums251,Ordered_pred_nums261),
%list_to_set(Ordered_pred_nums261,Ordered_pred_nums271),
list_to_set(Ordered_pred_nums251,Ordered_pred_nums24)
%reverse(Ordered_pred_nums271,Ordered_pred_nums24)
%,notrace
)),

find_groups([Ordered_pred_nums3|Ordered_pred_nums42],Ordered_pred_nums24,Ordered_pred_nums22,true)

);
(%(Ordered_pred_nums3=3->trace;true),%
(member(Ordered_pred_nums3,Ordered_pred_nums1)->Ordered_pred_nums1=Ordered_pred_nums5;
(number(Ordered_pred_nums3)->append([Ordered_pred_nums3],Ordered_pred_nums1,Ordered_pred_nums5);
(Ordered_pred_nums3=[Ordered_pred_nums31|_]->
(number(Ordered_pred_nums31)->append([Ordered_pred_nums31],Ordered_pred_nums1,Ordered_pred_nums5);
Ordered_pred_nums1=Ordered_pred_nums5));
Ordered_pred_nums1=Ordered_pred_nums5)),
%notrace,
find_groups(Ordered_pred_nums41,Ordered_pred_nums5,Ordered_pred_nums23,false),
find_groups2(Ordered_pred_nums3,Ordered_pred_nums42,Ordered_pred_nums23,Ordered_pred_nums22)
)).
/*

%append(Ordered_pred_nums1,[[loop1,Ordered_pred_nums3]],Ordered_pred_nums2));
 (%trace,
 %append([Ordered_pred_nums3],Ordered_pred_nums1,Ordered_pred_nums5),
findall(Ordered_pred_nums22,(member(Ordered_pred_nums412,Ordered_pred_nums4),
find_groups(Ordered_pred_nums412,[]%Ordered_pred_nums5
,Ordered_pred_nums22)),Ordered_pred_nums21),
%trace,
 (foldr(append,
 Ordered_pred_nums21,Ordered_pred_nums23)%->true;
 %Ordered_pred_nums21=Ordered_pred_nums23
 ),

foldr(append,[Ordered_pred_nums23,[Ordered_pred_nums3],Ordered_pred_nums1],Ordered_pred_nums2)
 %flatten(

)),
 %flatten(%foldr(append,
 %Ordered_pred_nums2,Ordered_pred_nums22).
 Ordered_pred_nums2=Ordered_pred_nums22.
 */
 
contains_loop(A, [loop, A], B, C) :- append(B,[A],C),!.

contains_loop(Ordered_pred_nums1,Ordered_pred_nums2,P1,P2) :-
 (Ordered_pred_nums2=[loop,Ordered_pred_nums1]->P1=P2;
 (Ordered_pred_nums2=[Ordered_pred_nums3|Ordered_pred_nums4],
 member(Ordered_pred_nums41,Ordered_pred_nums4),
 append(P1,[Ordered_pred_nums3],P3),
 contains_loop(Ordered_pred_nums1,Ordered_pred_nums41,P3,P2))).
 
not_contains_loop(A, [loop, A], B, B) :- !.

not_contains_loop(Ordered_pred_nums1,Ordered_pred_nums2,P1,P21) :-
%trace,
 recursive_reverse(Ordered_pred_nums2,Ordered_pred_nums21),
 not_contains_loop1(Ordered_pred_nums1,Ordered_pred_nums21,P1,P21).
 %reverse(P2,P22),
 %list_to_set(P22,P23),
 %reverse(P23,P21).

not_contains_loop1(Ordered_pred_nums1,Ordered_pred_nums2,P1,P21) :-
%writeln([ordered_pred_nums2,Ordered_pred_nums2]),
 ((Ordered_pred_nums2=[_Ordered_pred_nums1x,loop]%->true;
 %(Ordered_pred_nums2=loop%->true;
 %Ordered_pred_nums2=[loop]
 )->fail;%P1=P2;
 ((Ordered_pred_nums2=A,number(A))->append(P1,[A],P21);
 (append(Ordered_pred_nums4,[Ordered_pred_nums3],Ordered_pred_nums2),
 %reverse(Ordered_pred_nums4,Ordered_pred_nums42),
 findall(P3,(member(Ordered_pred_nums41,Ordered_pred_nums4),
 %reverse(Ordered_pred_nums41,Ordered_pred_nums42),

 not_contains_loop1(Ordered_pred_nums1,Ordered_pred_nums41,[],P3)),P31),
 %retractall(resort_n(_)),
 %assertz(resort_n(1)),
 %append(P31,[P1],P33),
 resort(P31,P32),
 %reverse(P31,P32),
 %trace,
 foldr(append,[
 %P1,
 P32%[Ordered_pred_nums3
 ,[Ordered_pred_nums3]],P2),
 %notrace,
 flatten(P2,P21)
 ))).
 
resort(P31,P32) :-
 member([A|B],P31),member([A|C],P31),not(B=C),
 subtract(P31,[[A|B],[A|C]],P33),
 foldr(append,[[A],B,C],D),
 append(P33,[D],P32),!.
resort(A,B) :-
 member([C|D],A),member(E,A),append(F,[C],E),
 subtract(A,[[C|D],E],G),
 foldr(append,[F,[C],D],H),
 append(G,[H],B),!.
resort(A,A) :- !.

recursive_reverse(A,B) :-
 recursive_reverse(A,[],B).
 
recursive_reverse([],A,A) :- !.
recursive_reverse(A,B,C) :-
 A=[D|E],
 (not(is_list(D))->D=D1;
 (flatten(D,D)->reverse(D,D1);
 recursive_reverse(D,[],D1))),
 append([D1],B,D2),
 recursive_reverse(E,D2,C).

remove_dups_from_loops(A,B) :-
 remove_dups_in_loops_from_rest(A,C),
 remove_dups_in_loops(C,B).
 %trace,
 %findall(E,(member(F,D),
 %(F=[loop1,[A1]]->E=A1;E=F)),B).

remove_dups_in_loops_from_rest(A,C) :-
 findall(D,member([loop1,D],A),E),
 flatten(E,F),
 subtract(A,F,C).
 
remove_dups_in_loops(C,B) :-
 sub_term_wa([loop1,_],C,A),
 remove_dups_in_loops1(A,[],D),
 foldr(put_sub_term_wa_ae,D,C,B).

remove_dups_in_loops1([],A,A) :- !.
remove_dups_in_loops1(A,B,C) :-
 A=[[Add,[loop1,List]]|D],
 remove_dups_in_loops1(List,D,[],E),
 append(B,[[Add,[loop1,List]]],F),
 remove_dups_in_loops1(E,F,C).

remove_dups_in_loops1(_,[],F,F) :- !.
remove_dups_in_loops1(List1,D,F,E) :-
 D=[[Add,[loop1,List2]]|G],
 subtract(List2,List1,List3),
 append(F,[[Add,[loop1,List3]]],H),
 remove_dups_in_loops1(List1,G,H,E).
 
/*
resort(A,B) :-
 %length(A,C),numbers(C,1,[],N),
 resort_n(NDs),
 findall(N-D,(member(D,A),
 (member(N-D,NDs)->true;get_resort_n(N))),E),
 
 
get_resort_n(A) :-
 resort_n(A),
 A1 is A+1,
 retractall(resort_n(_)),
 assertz(resort_n(A1)).
*/
 /*
not_contains_loop2a(Ordered_pred_nums1,Ordered_pred_nums2,P1,P23,First) :-
 (Ordered_pred_nums2=[loop,Ordered_pred_nums1]->P23=[];
 (Ordered_pred_nums2=[[loop,Ordered_pred_nums1]|_]->fail;
 (Ordered_pred_nums2=[]->true;%P1=P2;
 %((Ordered_pred_nums2=[A|_],number(A))->P2=A;%P1=P2;%append(P1,[A],P2);
 ((Ordered_pred_nums2=[Ordered_pred_nums21]->true;
 Ordered_pred_nums2=Ordered_pred_nums21),
 Ordered_pred_nums21=[_Ordered_pred_nums3|Ordered_pred_nums4],
 findall(P21,(member(Ordered_pred_nums41,Ordered_pred_nums4),
 (Ordered_pred_nums41=[loop,_]->fail;
((Ordered_pred_nums41=A,number(A),First=false
)->P21=A; %(number(Ordered_pred_nums41)->Ordered_pred_nums43=[Ordered_pred_nums41];Ordered_pred_nums43=[]),
 %(append(Ordered_pred_nums41
 %,[Ordered_pred_nums3]
 %,P3),
 %delete(Ordered_pred_nums4,Ordered_pred_nums41,Ordered_pred_nums412),
 ((get_n_item(Ordered_pred_nums4,Ordered_pred_nums41,1)->First=true;First=false),
 not_contains_loop(Ordered_pred_nums1,[Ordered_pred_nums41],[],
 P21,First))))),P22),
 foldr(append,[P1,P22],P2),
 flatten(P2,P23))))).
*/
find_groups_replace_loops(A,%B,
 C) :-
 %trace,
 sub_term_wa([loop,_],A,D),
 findall(E,member([E,_],D),F),
 foldl(delete_sub_term_wa,[F],A,C).
 
%in_or_exiting_loop(_,[],In_loop,In_loop,Exiting_loop,Exiting_loop) :- !.
in_or_exiting_loop(Ordered_pred_nums3,P71,In_loop1,In_loop2,Exiting_loop1,Exiting_loop2%,Rest_of_preds1,Rest_of_preds2
) :-
 %P71=[P72|P73],
 (contains_loop(Ordered_pred_nums3,P71,[],P)->
 append(In_loop1,[P],In_loop2);
 (%P=[],
 In_loop1=In_loop2)),
 %Exiting_loop1=Exiting_loop2);
 (not_contains_loop(Ordered_pred_nums3,P71,[],P1%,false
 ),
 %In_loop1=In_loop2,
 /*
 flatten(P71,P71F),
 subtract(P71F,[Ordered_pred_nums3,P,loop],P1),
 append(Exiting_loop1,[P1],Exiting_loop2).
 */
 %Exiting_loop1=Exiting_loop2).
 %trace,
 %P71=[P72|P73],
 %(
 %contains_loop(Ordered_pred_nums3,P71,[],_P2)->
 %(find_groups([Ordered_pred_nums3,[P71]]%P73]
 %,[%P72
 %],%Ordered_pred_nums24,
 %P2,true),
 %find_groups2(Ordered_pred_nums3,P71,[],%Ordered_pred_nums24,
 %P2),
%find_groups2(Ordered_pred_nums3,Ordered_pred_nums42,Ordered_pred_nums23,Ordered_pred_nums22)

 append(Exiting_loop1,[P1],Exiting_loop2)
 ).%);
 %(find_groups_replace_loops(P71,P74),
 %append(Rest_of_preds1,[P74%P72|P74
 %],Rest_of_preds2))).

 
contains_loop1(A) :-
 member([loop,_],A),!.%,flatten(A,B),member(loop,B),!.

%flatten_except_loops1([],[]) :- !.
%flatten_except_loops1([A],[A]) :- !.
flatten_except_loops1(A,B) :-
 flatten_except_loops2(A,[],B,false).
 %flatten_except_loops2([A],[],B1),
 %flatten_except_loops2(C,B1,B),!.
 
flatten_except_loops2([],B,B,_) :- !.
flatten_except_loops2(A,B,C,First) :-
 (not(is_list(A))->append([],[A],C);
 (A=[D|E],
 (D=[loop,_]->(append([],[D],F)%,Flag=nowrap
 );
 ((D=[D1],number(D1))->(append([],[D1],F)%,Flag=nowrap
 );
 (flatten_except_loops2(D,[],F,true)%,Flag=wrap
 ))),
 %(Flag=wrap->append([],[F],C2);append([],F,C2)), 
 flatten_except_loops2(E,F,C1,false),
 (First=true%Flag=wrap
 ->append(B,[C1],C);append(B,C1,C)))),!.

 /*
 findall([E,Ad],member([Ad,[loop,E]],D),F),
 foldr(put_sub_term_wa_ae,F,
   A, C1),
 (C1=A->C1=C;
 find_groups_replace_loops(C1,C)).
*/

 

put_sub_term_wa_ae([E,A],B,C) :-
 put_sub_term_wa(A,E,B,C),!.


order_preds_bottom_up_bfs(L,Functions,Ordered_pred_nums1,Ordered_pred_nums2) :-

Functions=[[N,P]|F],

append(Ordered_pred_nums1,[[L,N]],Ordered_pred_nums3),
 order_preds_bottom_up1(L,P,F,Ordered_pred_nums3,Ordered_pred_nums2),!.
 

% program may have unconnected preds, causing a bug

order_preds_bottom_up1(_,_,[],Ordered_pred_nums,Ordered_pred_nums) :- !.
order_preds_bottom_up1(L,N,Functions,Ordered_pred_nums1,Ordered_pred_nums2) :-
L3 is L+1,
append(Ordered_pred_nums1,[[L,N]],Ordered_pred_nums11),
% assign a level, record min, max levels
findall(Ordered_pred_nums8,(member(P2,N),member([P2,P3],Functions),
%member(P4,P3),%),Ordered_pred_nums1a),

%foldr(append,[Ordered_pred_nums1,Ordered_pred_nums1a],Ordered_pred_nums1b),

%findall(Ordered_pred_nums3,
%(member([N2,P4],Functions),%L2 is L-1,
delete(Functions,[P2,P3],Functions2),%member(P1,P),
order_preds_bottom_up1(L3,P3,Functions2,[],Ordered_pred_nums3),
foldr(append,Ordered_pred_nums3,Ordered_pred_nums5),
append([[L3,P3]],Ordered_pred_nums5,Ordered_pred_nums7),
append(Ordered_pred_nums11,Ordered_pred_nums7,Ordered_pred_nums8)

),Ordered_pred_nums2),!.%),_Ordered_pred_nums4),
/*
% what about sort

delete(Functions,[N,P],Functions2),
%***
%trace,
%foldr(append,Ordered_pred_nums4,Ordered_pred_nums5),


findall(Ordered_pred_nums6,
(member([N2,P1],Functions),member(N,P1),L2 is L+1,delete(Functions2,[N2,P1],Functions3),
order_preds_bottom_up1(L2,N2,Functions3,[],Ordered_pred_nums6)),Ordered_pred_nums7),

findall([N2,P1],(not(member([N2,P1],Functions2)),
member(N,P1)),Functions3),

foldr(append,[Ordered_pred_nums1b,Ordered_pred_nums7],Ordered_pred_nums2).
*/

% delete unconnected preds
%order_preds_bottom_up(L100,Functions3,[],Ordered_pred_nums2)


%find bottom up order of preds

% - delete 1 in 1-1 (cycle)

%find_dependencies(SM1,[],Deps).

%find_dependencies(SM1,Deps1,Deps2) :-



%alg_to_modes(Ordered_pred_nums1,Functions1,Functions_with_modes1,Functions_with_modes2) :-


	/*
alg_to_modes([],_,Functions_with_modes,Functions_with_modes) :- !.
alg_to_modes(Ordered_pred_nums1,Functions1,Functions_with_modes1,Functions_with_modes2) :-
	Ordered_pred_nums1=[[_,Number]%Function1
	|Functions],
	
	(member([Number,Name,Arguments1,Symbol1,Body1],Function1)
	->%symbol(Symbol1,Symbol2),
	(
	alg_to_modes3(Body1,%Body2,
	%[],Body2,
	[],%var Modes 1
	_Var_modes,%var Modes 2
	[],%Modes1
	Modes),
	
	% *** do header as well
	
	append(Functions_with_modes1,[[Number,Name,Arguments1,Symbol1,Modes]],Functions_with_modes5)),
	alg_to_modes(Functions,Functions1,Functions_with_modes4,Functions_with_modes2).
	*/
/*
alg_to_modes2(Body1,%Body2,
	%[],Body2,
	Var_modes1,%var Modes
	Var_modes2,%var Modes
	Modes1,%Modes1
	Modes2) :-
	*/

	
insert_loop1([loop1,Ordered_pred_nums323],[],Ordered_pred_nums1b) :-
 Ordered_pred_nums1b=[[loop1,Ordered_pred_nums323]],!.
insert_loop1([loop1,Ordered_pred_nums323],Ordered_pred_nums1,Ordered_pred_nums1b) :-
 append(_,[Index],Ordered_pred_nums323),
 ((append(A,B,Ordered_pred_nums1),append([[loop1,D]],C,B),
 append(_,[Index],D))->(union(D,Ordered_pred_nums323,E),%subtract(Ordered_pred_nums1,[[loop1,D]],E),
 foldr(append,[A,[[loop1,E]],C],Ordered_pred_nums1b));
 
 ((append(A,B,Ordered_pred_nums1),append([Index],C,B))->(union([Index],Ordered_pred_nums323,E),%subtract(Ordered_pred_nums1,[Index],E),
 foldr(append,[A,[[loop1,E]],C],Ordered_pred_nums1b)))),!.
 
