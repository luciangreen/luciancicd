/*

find tests from repos
- convert pl->lp,
- finds list of pred names
- find egs in comments

- find eg

% beginnings_middles_ends(10,5,BME).
% [[1,[1,2,3,4,5]],[2,[1,2,3,4,5]],[3,[1,2,3,4,5]],[4,[1,2,3,4,5]],[5,[1,2,3,4,5]],[6,[1,2,3,4,5]],[7,[1,2,3,4,5]],[8,[1,2,3,4,5]],[9,[1,2,3,4,5]],[10,[1,2,3,4,5]]]

or 

% phrase(sentence(A),[the,john,read,to,sera]).

- extract query and answers

- if no answers given, a warning

- if can't convert, a warning

- moves old cicd.txt to new loc

- later: find if all given preds have egs
- and whether preds called by preds have egs (if don't, give a warning)
- a warning if there is no eg at all

* which file should it load?

*/

:- use_module(library(date)).

%:-include('luciancicd.pl').
%:-include('../Prolog-to-List-Prolog/p2lpconverter.pl').

find_tests_from_repos :-

working_directory1(A,A),

(exists_directory('../private2/luciancicd-cicd-tests')->true;make_directory('../private2/luciancicd-cicd-tests')),

repositories_paths(K),

omit_paths(Omit),

%findall(Omit1,(member(Omit2,Omit),atom_string(Omit1,Omit2)),Omit3),
findall([K1,G4],(member(K1,K), directory_files(K1,F),
	delete_invisibles_etc(F,G),

%findall(H,(member(H,G),not(string_concat("dot",_,H)),

subtract(G,Omit,G1),

findall(G3,(member(G2,G1),string_concat(G2,"/",G3)),G4)
%not(member(G,Omit))

),K01),
%trace,
%foldr(append,K0,K01),

working_directory1(Old_D,Old_D),

findall(Tests1,(member([D,K31],K01),

working_directory1(_,Old_D),

working_directory1(_,D),

%member(K2,K31),

%exists_directory(K2),

process_directory_tests(K31,%_G,
 %Omit,%
 true,
 Tests1)%),Tests)
 ),Tests2),
 foldr(append,Tests2,Tests),
 
 working_directory1(_,A),
 
 (exists_directory('../private2/luciancicd-cicd-tests')->
 
 (		time1(Time),
	foldr(string_concat,["../private2/luciancicd-cicd-tests",Time,"/"],Folder1),
	%concat_list3(File1,[".txt"],File2),

mv_lc("../private2/luciancicd-cicd-tests/",Folder1)
 %foldr(string_concat,["rsync -av --exclude=\".*\"  ../private2/luciancicd-cicd-tests/ ",Folder1],Command314),
 	%catch(bash_command(Command314,_), _, (foldr(string_concat,["Warning."%%"Error: Can't clone ",User3,"/",Repository3," repository on GitHub."],Text41),writeln1(Text41),abort))
 	);
 	(
 	%exists_directory('../private2/luciancicd-cicd-tests')->true;
%make_directory('../private2/luciancicd-cicd-tests')
true)),

findall(_,(member([K21,Tests521],Tests),
open_s(K21,write,S21),
write(S21,Tests521),close(S21)
),_),
%writeln("All tests were successful."),

!.

process_directory_tests(K,%G,
 Top_level,%Tests1,
 Tests61) :-

%G=K,
%/*
findall(K4,(member(K1,K), %exists_directory(K1),
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
process_directory_tests([H2],%[H],
 false,%[],%Omit % only omit top level dirs xx
 %Tests1,
 Tests3)
 %foldr(append,Tests31,Tests3)
 );

(string_concat(_,".pl",H)->

(

find_tests(K1,H,H1,Tests3)
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
(
findall([T1,',\n'],(member([T,TT,TTT],Tests51),foldr(atom_concat,["[","\"",T,"\"",",","\"",TT,"\"",",",TTT,"]"],T1)%term_to_atom(T,T1)
),T2),
flatten(T2,TT2),
foldr(atom_concat,TT2,T21),
(T2=[]->T6=[];(string_concat(T4,T5,T21),string_length(T5,2),
foldr(string_concat,["[","\n",T4,"\n","]"],T6))),
%term_to_atom(Tests51,Tests52),
string_concat(K3,"/",K1),
foldr(string_concat,["../private2/luciancicd-cicd-tests/tests_",K3,".txt"],K2),
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

(%not(Omit=[])->
Top_level=true->
Tests6=Tests61;
(%trace,
foldr(append,Tests6,Tests61))),

!.
	
/*
test_a(Tests) :-

working_directory1(A,A),

working_directory1(_,'../../GitHub2/'),

[K1,H,_H1]=["Philosophy/", "4 2 23.pl", "Philosophy/4 2 23.pl"],
	string_concat(K11,"/",K1),

LP=[[[n, c], ["%r(NA)."]], [[n, c], ["%NA=2."]]%, [[n, c], ["% r([a],N2)."]], [[n, c], ["% N2 = 3."]]
,[[n,r]]
],

(find_tests2(H,K11,LP,Tests)->true;working_directory1(_,A)).
*/

find_tests(K1,H,H1,Tests) :-

	string_concat(K11,"/",K1),


%catch(call_with_time_limit(0.005,
%trace,	
%p2lpconverter([file,H1],LP),%),_,false),
	fastp2lp(H1,LP1),
%trace,
	find_tests2(%H1,
	H,K11,LP1,Tests).

fastp2lp(H1,LP1) :-
%trace,
(string_concat(_,".pl",H1)->
(
open_string_file_s(H1,F),
(string_concat(F1,"\n% ",F)->true;%F2 = F;
(string_concat(F,"\n% ",F2),

%atomic_list_concat(F3,'\n\n',F2),
%atomic_list_concat(F3,'',F4),
%time_file(H1,T),
save_file_s(H1,F2))
%set_time_file(H1,[],T))
));true),

foldr(string_concat,["#!/usr/bin/swipl -g main -q\n\n",":-include('../GitHub/Prolog-to-List-Prolog/p2lpconverter.pl').\n","handle_error(_Err):-\n  halt(1).\n","main :-\n    catch((p2lpconverter([file,\"",H1,"\"],LP),term_to_atom(LP,LP1), write(LP1)),Err, handle_error(Err)), nl,\n    halt.\n","main :- halt(1).\n"],String),

%trace,
	fastp2lp1(String,LP1).

fastp2lp2(H1,LP1) :-

%string_concat(H10,"\n%",H1),

foldr(string_concat,["#!/usr/bin/swipl -g main -q\n\n",":-include('../GitHub/Prolog-to-List-Prolog/p2lpconverter.pl').\n","handle_error(_Err):-\n  halt(1).\n","main :-\n    catch((p2lpconverter([string,\"",H1,"\"],LP),term_to_atom(LP,LP1), write(LP1)),Err, handle_error(Err)), nl,\n    halt.\n","main :- halt(1).\n"],String),

working_directory1(A,A),

%writeln([*,A]),

working_directory1(_,"../"),


	fastp2lp1(String,LP1),
	
	working_directory1(_,A).

fastp2lp1(String,LP1) :-

%trace,
%working_directory1(_,A),
foldr(string_concat,[%"../private2/luciancicd-testing/",Repository1b,"/",Go_path5,
"tmp.pl"],GP),
%string_concat(Go_path,"testcicd.pl",GP),
open_s(GP,write,S1),
write(S1,String),close(S1),
foldr(string_concat,["chmod +x ",GP,"\n","swipl -g main -q ./",GP],S3),%,

(catch(bash_command(S3,LP), _, (foldr(string_concat,["Warning."%%"Error: Can't clone ",User3,"/",Repository3," repository on GitHub."
	],_Text4),%writeln1(Text4),
	fail%abort
 	))->
 	(
%trace,	working_directory1(A,A),
%	writeln([*,A]),

delete_tmp,
	term_to_atom(LP1,LP)
	);(writeln("Fatal error on converting Prolog to List Prolog."),delete_tmp,fail)).
	
delete_tmp:-
foldr(string_concat,[%"scp -pr ../../Github_lc/ ",
 "rm -f tmp.pl"
 %Folder1
 ],Command315),
 	catch(bash_command(Command315,_), _, (foldr(string_concat,["Warning."%%"Error: Can't clone ",User3,"/",Repository3," repository on GitHub."
	],_Text42)%,writeln1(Text42)%,abort
 	)),

foldr(string_concat,[%"scp -pr ../../Github_lc/ ",
 "rm -f luciancicd/tmp.pl"
 %Folder1
 ],Command316),
 	catch(bash_command(Command316,_), _, (foldr(string_concat,["Warning."%%"Error: Can't clone ",User3,"/",Repository3," repository on GitHub."
	],_Text42)%,writeln1(Text42)%,abort
 	)).
	%,writeln1(Result2)
	
find_tests2(%H1,
H,K11,LP,Tests) :-

	%foldr(string_concat,[H1%,"/",K11
	%],K12),
	%trace,
	findall(N1,(member([[n,N]|_],LP),
	string_strings(N,N1)),Ns),

	findall([K11,H,F2],(append(_,LP1,LP),
	append([[[n,comment%c
 	],[Comment]]],LP2,LP1),
	
	%findall([K11,H,F2],(member([[n,comment%c
	%],[Comment]],LP),

	
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

	append([[[n,comment%c
	],[Comment1]]],_,LP2),

	%member([[n,comment%c
	%],[Comment1]],LP),
	
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
	),Tests1),sort(Tests1,Tests),!.

	
	% is K11 just d or gh/d x d
	
%[K11,H,]	
%[["d","a.pl",(a(B),B=1)]]
/*
find_tests3(H,K11,LP,Tests) :-

findall(B,(member([[n,comment],A]),
term_to_atom(A1,A),
((functor(A1,(=),2),arg(1,A1,N),arg(2,A1,Ans),B=[ans,N,=,Ans])->true;
(functor(A1,N,Ar),numbers(Ar,1,[],ArN),findall(ArN2,(member(ArN1,ArN),arg(ArN1,A1,ArN2),var(ArN2)),ArN3),
B=[A1,ArN3]))),C),
findall([K11,H,F2],(member([ans,N,=,Ans],C),
member([A1,Ans],C),
F=A1,
F1=(N=Ans),
foldr(string_concat,["(",F,",",F1,")"],F2)),Tests),!.


*/