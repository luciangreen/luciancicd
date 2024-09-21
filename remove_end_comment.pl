remove_end_comment :-

working_directory1(A,A),

%(exists_directory('../private2/luciancicd-data')->true;make_directory('../private2/luciancicd-data')),

repositories_paths(K),

omit_paths(Omit),

%findall1(Omit1,(member(Omit2,Omit),atom_string(Omit1,Omit2)),Omit3),
findall1([K1,G4],(member(K1,K), directory_files(K1,F),
	delete_invisibles_etc(F,G),

%findall1(H,(member(H,G),not(string_concat("dot",_,H)),

subtract(G,Omit,G1),

findall1(G3,(member(G2,G1),string_concat(G2,"/",G3)),G4)
%not(member(G,Omit))

),K01),
%trace,
%foldr(append,K0,K01),

working_directory1(Old_D,Old_D),

findall1(Mod_time1,(member([D,K31],K01),

working_directory1(_,Old_D),

working_directory1(_,D),

%member(K2,K31),

%exists_directory(K2),

process_directory_remove_end_comment(K31,%_G,
 %Omit,%
 true,
 Mod_time1)%),Mod_time)
 ),Mod_time2),
 foldr(append,Mod_time2,Mod_time),
 
 %/*
 %trace,
findall1(_,(member([_,Tests521],Mod_time),
term_to_atom(Tests523,Tests521),
member([K21,Tests522,T],Tests523),
open_s(K21,write,S21),
write(S21,Tests522),close(S21)
,set_time_file(K21,[],[modified(T)])
),_),
sleep1(2),

%*/

/*

[["c/main_file.txt","[[""c.pl"",[[c,3]]]]
"],["c/c.pl","%c(1,1,A).
%A=2.
%c(1,2,A).
%A=3.
c(A,B,B4):-B4 is A+B."]]

findall1([F1,Mod_times12],
(member(F2,G),string_concat('../private2/luciancicd-data/',F2,F1),
open_file_s(F1,Mod_times1),
term_to_atom(Mod_times1,Mod_times12)),Mod_times11),
*/

 working_directory1(_,A)


 ,!.

process_directory_remove_end_comment(K,%G,
 Top_level,%Mod_time1,
 Mod_time61) :-

%G=K,
%/*
findall1(K4,(member(K1,K), directory_files(K1,F),
	delete_invisibles_etc(F,G),
%*/
findall1(Mod_time3,(member(H,G),%not(string_concat("dot",_,H)),

%not(member(H,Omit)),


foldr(string_concat,[K1,H],H1),

% if a file then find modification date
% if a folder then continue finding files in folder
(exists_directory(H1)->

(string_concat(H1,"/",H2),
process_directory_remove_end_comment([H2],%[H],
 false,%[],%Omit % only omit top level dirs xx
 %Mod_time1,
 Mod_time3)
 %foldr(append,Mod_time31,Mod_time3)
 );

(time_file(H1,T),
(string_concat(_,".pl",H1)->
remove_end_comments1(H1,Mod_time4);
open_string_file_s(H1,Mod_time4)),
%trace,
%append(Mod_time1,[[H1,Mod_time4]],Mod_time3)))
Mod_time3=[[H1,Mod_time4,T]]))

),Mod_time5),%trace,
foldr(append,Mod_time5,Mod_time51),

%Mod_time5=Mod_time51,

(Top_level=true%not(Omit=[]) % at top level
->
(
term_to_atom(Mod_time51,Mod_time52),
string_concat(K3,"/",K1),
foldr(string_concat,[K3,".txt"],K2),
K4=[K2,Mod_time52]
%open_s(K2,write,S),
%write(S,Mod_time52),close(S)

%writeln(["*",K2,
%Mod_time52]
%)
);
K4=Mod_time51
)



),Mod_time6),

(%not(Omit=[])->
Top_level=true->
Mod_time6=Mod_time61;
foldr(append,Mod_time6,Mod_time61)),

!.

remove_end_comments1(H1,Mod_time4) :-
 open_string_file_s(H1,Mod_time5),
 remove_end_comments2(Mod_time5,Mod_time4).

remove_end_comments2(Mod_time5,Mod_time4) :-
 string_concat(Mod_time6,"\n% ",Mod_time5),
 remove_end_comments2(Mod_time6,Mod_time4).
remove_end_comments2(Mod_time5,Mod_time4) :-
 string_concat(Mod_time6,"\n",Mod_time5),
 remove_end_comments2(Mod_time6,Mod_time4).
remove_end_comments2(Mod_time4,Mod_time4) :- 
 not(string_concat(_,"\n% ",Mod_time4)),
 not(string_concat(_,"\n",Mod_time4)),!.

