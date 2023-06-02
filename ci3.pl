/*

ci3.pl

- use diff group combos to find changed preds - setting to make interpred comments perm v included with space changes
- do by file
 - (use a version of dgc that returns i as well as d for changes to get out code to check)
- within changed groups, separates into different pred name, arity
- remove comments within preds (x make perm), change interpred comments to permanent (to delete manually later if nec) x delete interline comments xxx they will be included with space changes
- use diff group combos (dgc) with tokens not strings to compare old and new preds (use a version of dgc that returns i as well as d for changes x)
- bu
- run tests to find smallest set of changes needed to each pred

x delete interline comments
or convert them to lp to see if they are comm, then keep them x they are with the changes

*/
 
:- use_module(library(date)).

%:-include('luciancicd.pl').
:-include('../Prolog-to-List-Prolog/pretty_print_lp2p.pl').
%:-include('../Alg_to_Types/find_dependencies.pl').
%:-dynamic merges_files/1.
%:-dynamic merges_preds/1.

ci :-

working_directory1(A,A),

	retractall(home_dir(_)),assertz(home_dir(A)),
	retractall(ci_fail(_)),assertz(ci_fail([])),

(exists_directory('../../Github_lc')->true;make_directory('../../Github_lc')),

repositories_paths(K),

omit_paths(Omit),

%findall(Omit1,(member(Omit2,Omit),atom_string(Omit1,Omit2)),Omit3),
findall([K1,G4],(member(K1,K), directory_files(K1,F),
	delete_invisibles_etc(F,G),

%retractall(merges_files(_)),
%assertz(merges_files([])),
%retractall(merges_preds(_)),
%assertz(merges_preds([])),
% merge_files contains the file data from the repository.
% Its predicates need to be updated and saved.

% merges_preds contains the name and arity of updated preds x
%findall(H,(member(H,G),not(string_concat("dot",_,H)),

subtract(G,Omit,G1),

findall(G3,(member(G2,G1),string_concat(G2,"/",G3)),G4)
%not(member(G,Omit))

),K01),
%trace,
%foldr(append,K0,K01),

working_directory1(Old_D,Old_D),

findall(Tests1,(member([D,K31],K01),

%trace,

working_directory1(_,Old_D),

working_directory1(_,D),

%member(K2,K31),

%exists_directory(K2),
%trace,
process_directory_merge(K31,%_G,
 %Omit,%
 true,
 Tests11),
 
 % get reverse order of preds
 
 % go through them in order, ", for each combo make changes to original, test until find the simplest working version
 %trace,
 %find_pred_order(Tests11,Ordered_pred_nums),
 
 prepare_repositories(Tests11,Tests1) %xx delimit files, combine in luciancicd, find combos in multireps x reps, take apart on file delimiter comments, find shortest combos first, test

 %process_merge_preds(Tests11,Ordered_pred_nums,Tests1)
 
 
 %process_merge_preds()
 ),Tests2),
 foldr(append,Tests2,Tests),
 %trace,
 retractall(lc_tests(_)),
 assertz(lc_tests(Tests))

 ,working_directory1(_,A)

,%writeln("All tests were successful."),

!.

%/*


prepare_repositories(Tests,T3%,Ordered_pred_nums
) :- %* in a separate file.pl, process dirs rec'ly
%trace,


findall([Tests12,Tokens2,Tokens1],
%findall(Tests143,(
(member([Tests12,C],Tests),
 %Tests=[[Tests12,C]],
 term_to_atom(Tests14,C),%),Tests142),
 %foldr(append,Tests142,Tests14),
 
 %findall([[[n,comment],["File delimiter",P,F]],O],=(
 %trace,
 findall(O1,
 (
 member([P,F,O,N],Tests14),
 %member(A,[1,2]),
 %(%A= 1->
 %(
 (O=[]->O1=O;
 append([[[n,comment],["File delimiter",P,F]]],O,O1))
 %[P,F,O,_N],Tests14),N2),
 %foldr(append,O1,Functions2),
 %term_to_atom(Functions2,String2),
 %break_into_tokens(String2,Tokens2),
 %delete(O1,[],Functions21),
 ),Tokens2a),
 foldr(append,Tokens2a,O2),
 pp0_1(O2,String2),
 split_string(String2,"\n\r","\n\r",Tokens2),
 %),%;
 %(%A=2,
 %findall([[[n,comment],["File delimiter",P,F]],N],=(
 %[P,F,_O,N]=Tests14,
 %member([P,F,O,N],Tests14),
 findall(N1,
 (
 member([P,F,O,N],Tests14),
 (N=[]->N1=N;
 append([[[n,comment],["File delimiter",P,F]]],N,N1))
 %[P,F,_O,N],Tests14),N1),
 %foldr(append,N1,Functions1),
 %term_to_atom(Functions1,String1),
 %break_into_tokens(String1,Tokens1)
 %delete(N1,[],Functions11),
 ),Tokens1a),
 foldr(append,Tokens1a,N2),
 pp0_1(N2,String1),
 split_string(String1,"\n\r","\n\r",Tokens1)
 ),
T3),!.
 %merge2(Tokens2,Tokens1,T4),
 %T3=[["../../Github_lc/tests.txt"%_Tests12
 %,Tokens2,Tokens1%T4
 %]].

%trace,
/*for a repo:
- find reverse ordered preds, find changed, not inserted or deleted preds - (no combos x) - save changed preds as go, find min alg pred set (don't find preds' combos until needed by the minimum macrocombos x)
for each changed pred:
find i,d,c parts' combos
run pred tests on repo, and deps, and determine minimum version of pred, save in pre-build folder
go to build
*/
%A=[1],
%findall(B,(member(Functions1,N2),%Debug=off,member(B,A),test(B,Q,F,R),query_box(Q,Query1,F,Functions1),
/*convert_to_grammar_part1(Functions1,[],Functions2,_),add_line_numbers_to_algorithm1(Functions2,Functions2a),find_pred_numbers(Functions2a,[],Pred_numbers),find_state_machine1(Functions2a,Functions3,Pred_numbers),


find_pred_numbers_dependencies(Functions3,[],Functions2a,Pred_numbers),

order_preds_bottom_up(1,Functions2a,[],Ordered_pred_nums)
.
*/

process_directory_merge(K,%G,
 Top_level,%Tests1,
 Tests61) :-
working_directory1(A0,A0),

%G=K,
%/*
findall(K4,(member(K1,K), 
working_directory1(_,A0),
%exists_directory(K1),
directory_files(K1,F),
	delete_invisibles_etc(F,G),
%*/
findall(Tests3,(member(H,G),not(string_concat("dot",_,H)),

%not(member(H,Omit)),


foldr(string_concat,[K1,H],H1),

% if a file then find modification date
% if a folder then continue finding files in folder
(exists_directory(H1)->

(string_concat(H1,"/",H2),
process_directory_merge([H2],%[H],
 false,%[],%Omit % only omit top level dirs xx
 %Tests1,
 Tests3)
 %foldr(append,Tests31,Tests3)
 );

(true%string_concat(_,".pl",H)
->

(%trace,

find_merge(K1,H,H1,Tests3)
%p2lpconverter([file,H1],LP),

%time_file(H1,Tests4),
%trace,
%append(Tests1,[[H1,Tests4]],Tests3)))
%Tests3=[[H1,Tests]]
)
;
(Tests3=[]
)
))

),Tests5),%trace,
foldr(append,Tests5,Tests51),

%Tests5=Tests51,

(%true%
Top_level=true%not(Omit=[]) % at top level
->
( % folder/file, pl
findall([T1,',\n'],(member([T,TT,TTT,TTTT
],Tests51),term_to_atom(TTT,TTT1),term_to_atom(TTTT,TTTT1),
foldr(atom_concat,["[","\"",T,"\"",",","\"",TT,"\"",",",TTT1,",",TTTT1,
"]"],T1)%term_to_atom(T,T1)
),T2),
%trace,
flatten(T2,TT2),
foldr(atom_concat,TT2,T21),
(T2=[]->T6=[];(find_sl_2(T21,T6)%string_concat(T4,T5,T21),%trace,
%string_length(T5,2),
%foldr(string_concat,["[","\n",T4,"\n","]"],T6)
)),
%term_to_atom(Tests51,Tests52),
%trace,
string_concat(K3,"/",K1),
foldr(string_concat,["../../Github_lc/tests_",K3,".txt"],K2),
%trace,
K4=[K2,T6]
%open_s(K2,write,S),
%write(S,Tests52),close(S)

%writeln(["*",K2,
%Tests52]
%
);
(%trace,
K4=Tests51)
)



),Tests6),
%trace,
(%not(Omit=[])->
Top_level=true->
Tests6=Tests61;
(%trace,
foldr(append,Tests6,Tests61))),

!.
	
	/*
test_b(Tests) :-

working_directory1(A,A),

working_directory1(_,'../../GitHub2/'),

[K1,H,_H1]=["Philosophy/", "4 2 23.pl", "Philosophy/4 2 23.pl"],
	string_concat(K11,"/",K1),

LP=[[[n, c], ["%r(NA)."]], [[n, c], ["%NA=2."]]%, [[n, c], ["% r([a],N2)."]], [[n, c], ["% N2 = 3."]]
,[[n,r]]
],

(find_merge2(H,K11,LP,Tests)->true;working_directory1(_,A)).
*/

find_merge(K1,H,H1,Tests) :-

	string_concat(K11,"/",K1),

	merge(K11,H,H1,Tests).

%catch(call_with_time_limit(0.005,
	%p2lpconverter([file,H1],LP),%),_,false),
	%,writeln1(Result2)

	%find_merge2(H,K11,LP,Tests).

/*
find_merge2(H,K11,LP,Tests) :-

	%trace,
	findall(N1,(member([[n,N]|_],LP),
	string_strings(N,N1)),Ns),

	findall([K11,H,F2],(member([[n,comment%c
	],[Comment]],LP),
	string_strings(Comment,C),
	member(N2,Ns),
	append(_A,B,C),
	append(N2,Dx,B),
	%trace,
	append(Ex,Dx1,Dx),
	%append(_Ex1,Dx2,Dx1),
	append(["."],_N21,Dx1),
	%trace,
	flatten([N2,Ex%,"."
	],N2Ex),
	foldr(string_concat,N2Ex,F),
	
	% the answer is A= ... "." or " "
	% in this, just 1 answer
	%trace,
	reverse(Ex,D),append(E2,_E3,D),reverse(E2,E31),(append([","],E5,E31)->true;append(["("],E5,E31)),append(E6,E7,E5),append([")"],_,E7),
	%trace,
	%member(N21,Ns),

	member([[n,comment%c
	],[Comment1]],LP),
	string_strings(Comment1,C1),
	
	append(_A1,Bx,C1),
	append(E6,Dxx,Bx),
	append(E61,Dxx1,Dxx),
	%trace,
	(append(["."],_Exx,Dxx1)%->true;
	%append([],Exx,Dxx1)
	),
	%trace,
	%writeln([_A1,Bx,E6,Dxx,E61,Dxx1]),
	%flatten([])
	foldr(string_concat,E61,E612),
	sub_string(E612,_,_,_,"="),
	%trace,
	flatten([E6,E612%,Exx%,"."
	],E6Exx),
	foldr(string_concat,E6Exx,F1),
	%trace,
	%term_to_atom((F,F1),F00),
	%term_to_atom(F0,F00),
	%term_to_atom(F10,F1),
	foldr(string_concat,["(",F,",",F1,")"],F2)
	%atom_string(F0,F),
	%atom_string(F10,F1),
	%F2=%F0%
	%(F0,F10)
	),Tests),!.

	
	% is K11 just d or gh/d x d
	
%[K11,H,]	
%[["d","a.pl",(a(B),B=1)]]
*/

/*
process_merge_preds(Tests11,Ordered_pred_nums,Tests1) :-
 Tests11=[B,Tests12],term_to_atom(C,Tests12),
 findall(A,(member([P,F,O,N],C)
 
 ),_).
*/

find_sl_2(T21,T6) :-
 find_sl_21(T21,T6),!.
find_sl_21(T21,T6) :-
 string_concat(T4,T5,T21),%trace,
 string_length(T5,2),
 foldr(string_concat,["[","\n",T4,"\n","]"],T6).
