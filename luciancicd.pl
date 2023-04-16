% luciancicd.pl

/*

CI/CD for LPPM repositories

- records modification date for each file
- command that resets modification dates to that of files x
- installs repositories in which it or a dependency repository has been modified
- tests each repository above at a certain time each day
- can turn off or manually run install/tests for certain repositories (have tests if needed)
- emails results of failures
- only allows commits if tests have been run

NB:
- ignore invisible files
- stores data in private2/luciancicd-data folder - needs to be created
- stores modification dates in separate files for each repository
- uses LPPM dependencies

Later:
- converts pl to lp and checks if code has changed or if it is the same or if just comments have changed
- requires saving the last version of the lp code
- a pl pretty printer based on lp

*/

% - records modification date for each file

:-include('../listprologinterpreter/listprolog.pl').
:-include('../listprologinterpreter/la_files.pl').
:-include('../List-Prolog-Package-Manager/lppm.pl').
:-include('lppm_install_luciancicd.pl').

% [debug]  ?- modification_dates(MT),writeln1(MT).
% [["a/a1.txt",1681386225.0],["a/c/untitled text 127.txt",1681386249.0],["a/c/d/untitled text 128.txt",1681387161.0]]

set_up_luciancicd :-

modification_dates(Mod_times),
findall(_,(member([K2,Mod_time52],Mod_times),
open_s(K2,write,S),
write(S,Mod_time52),close(S)
),_),!.

luciancicd :-

	lppm_get_registry_luciancicd(LPPM_registry_term1),

(exists_directory('../private2')->true;make_directory('../private2')),

(exists_directory('../private2/luciancicd-data')->true;make_directory('../private2/luciancicd-data')),

directory_files('../private2/luciancicd-data/',F),
	delete_invisibles_etc(F,G),

findall([F1,Mod_times12],
(member(F2,G),string_concat('../private2/luciancicd-data/',F2,F1),
open_file_s(F1,Mod_times1),
term_to_atom(Mod_times1,Mod_times12)),Mod_times11),

modification_dates(Mod_times2),


    %msort(Mod_times11, Sorted1),
    %msort(Mod_times2, Sorted2),
    subtract(Mod_times2,Mod_times11,New),

(    %Sorted1=Sorted2
	New=[]
->writeln("There are no new modifications to repositories to test.");

% if 

(

	concat_list(["rm -rf ../private2/luciancicd-data/"],Command31),
 	catch(bash_command(Command31,_), _, (concat_list(["Warning."%%"Error: Can't clone ",User3,"/",Repository3," repository on GitHub."
	],Text41),writeln1(Text41),abort
 	)),

(exists_directory('../private2/luciancicd-data')->true;make_directory('../private2/luciancicd-data')),

findall(_,(member([K21,Mod_time521],Mod_times2),
open_s(K21,write,S21),
write(S21,Mod_time521),close(S21)
),_),

findall(Dependencies3,(member([Path,_],New),
string_concat(Path1,".txt",Path),
string_concat("../private2/luciancicd-data/mod_times_",Repository1,Path1),
%trace,
find_all_depended_luciancicd(LPPM_registry_term1,Repository1,[],Dependencies3)),Dependencies4),
flatten(Dependencies4,Dependencies41),
sort(Dependencies41,Dependencies5),


(findall(_,(member(Repository1,Dependencies5),


(exists_directory('../private2/luciancicd-testing')->true;make_directory('../private2/luciancicd-testing')),

	concat_list(["rm -rf ../private2/luciancicd-testing/"],Command3),
 	catch(bash_command(Command3,_), _, (concat_list(["Warning."%%"Error: Can't clone ",User3,"/",Repository3," repository on GitHub."
	],Text4),writeln1(Text4),abort
 	)),

(exists_directory('../private2/luciancicd-testing')->true;make_directory('../private2/luciancicd-testing')),


lppm_install_luciancicd(LPPM_registry_term1,"luciangreen",Repository1),

% test non-interactive algorithms
%trace,
foldr(string_concat,["../private2/luciancicd-testing/",Repository1,"/cicd.txt"],Test_script_path),
(catch(open_file_s(Test_script_path,Tests),_,
(writeln(["Cannot find",Test_script_path]),abort))->

(working_directory(A,A),
findall(Result,(member([Go_path,File,Command],Tests),
((working_directory(_,A),
working_directory(_,Go_path),

% *** Change path to swipl if necessary

term_to_atom(Command,Command1),
foldr(string_concat,["#!/usr/bin/swipl -f -q\n\n:- initialization main.\n:-include('",File,"').\n","main :-\n    ",Command1,", nl,\n    halt.\n","main :- halt(1).\n"],String),
%trace,
foldr(string_concat,["../private2/luciancicd-testing/",Repository1,"/testcicd.pl"],GP),
%string_concat(Go_path,"testcicd.pl",GP),
open_s(GP,write,S1),
write(S1,String),close(S1),
foldr(string_concat,["chmod +x ",GP,"\n","swipl -f -q ./",GP],S3)%,
,catch(bash_command(S3,_), _, (concat_list(["Warning."%%"Error: Can't clone ",User3,"/",Repository3," repository on GitHub."
	],_Text4),%writeln1(Text4),
	fail%abort
 	))
%Command
)->Result=success;Result=fail),
writeln([Go_path,File,Command,Result])),_Results))
;
true)),_)))),!.



repositories_paths([
%"../../GitHub/"
"reps/"
%"e/"
]).

omit_paths([
%"private2"
"b" % omits a/b/
]).


modification_dates(Mod_time) :-

(exists_directory('../private2/luciancicd-data')->true;make_directory('../private2/luciancicd-data')),

repositories_paths(K),

omit_paths(Omit),

%findall(Omit1,(member(Omit2,Omit),atom_string(Omit1,Omit2)),Omit3),


process_directory(K,%_G,
 Omit,%[],
 Mod_time)%),Mod_time)
 ,!.

process_directory(K,%G,
 Omit,%Mod_time1,
 Mod_time61) :-

findall(K4,(member(K1,K), directory_files(K1,F),
	delete_invisibles_etc(F,G),

findall(Mod_time3,(member(H,G),not(string_concat("dot",_,H)),

not(member(H,Omit)),

string_concat(K1,H,H1),

% if a file then find modification date
% if a folder then continue finding files in folder
(exists_directory(H1)->

(string_concat(H1,"/",H2),
process_directory([H2],%[H],
 [],%Omit % only omit top level dirs xx
 %Mod_time1,
 Mod_time3));

(time_file(H1,Mod_time4),
%append(Mod_time1,[[H1,Mod_time4]],Mod_time3)))
Mod_time3=[[H1,Mod_time4]]))

),Mod_time5),%trace,
foldr(append,Mod_time5,Mod_time51)


,
(not(Omit=[]) % at top level
->
(
term_to_atom(Mod_time51,Mod_time52),
string_concat(K3,"/",K1),
foldr(string_concat,["../private2/luciancicd-data/mod_times_",K3,".txt"],K2),
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

(not(Omit=[])->
Mod_time6=Mod_time61;
foldr(append,Mod_time6,Mod_time61)),

!.


	%find_all_depended_luciancicd(LPPM_registry_term1,Repository1,Dependencies,Dependencies) :- !.
	find_all_depended_luciancicd(LPPM_registry_term1,Repository1,Dependencies7,Dependencies72) :-
(member([User1,Repository1,_Description,_Dependencies1],LPPM_registry_term1)->
(findall(Dependencies5,(member([User1,Repository2,_Description,Dependencies2],LPPM_registry_term1),
member([User1,Repository1],Dependencies2),
find_all_depended_luciancicd(LPPM_registry_term1,Repository2,[],Dependencies4),
foldr(append,[Dependencies4],Dependencies5)

),Dependencies3),
append([Repository1],Dependencies3,Dependencies6),
flatten(Dependencies6,Dependencies72));
true),
%flatten(Dependencies71,Dependencies72),
!.

%****  change later
lppm_get_registry_luciancicd(LPPM_registry_term1) :-
	catch(phrase_from_file_s(string(LPPM_registry_string), "lppm_registry.txt"),_,(writeln1("Error: Cannot find lppm_registry.txt"),abort)),

term_to_atom(LPPM_registry_term1,LPPM_registry_string).
