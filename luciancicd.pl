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

:-include('../SSI/ssi.pl').
:-include('../listprologinterpreter/la_files.pl').
:-include('../List-Prolog-Package-Manager/lppm.pl').
%:-include('lppm_install_luciancicd.pl').
:-include('find_tests_from_repos.pl').
:-include('ci.pl').
:-include('ci3.pl').
:-include('save_diff_html.pl').
:-include('move_to_repository_or_back.pl').
:-include('luciancicd_ws.pl').

:-dynamic lc_tests/1.
:-dynamic home_dir/1.
:-dynamic ci_fail/1.
:-dynamic time1/1.
:-dynamic log/1.

%:-dynamic lc_mode/1.


set_up_luciancicd :-

get_time1,

check_repositories_paths,

working_directory1(A1,A1),

	
modification_dates(Mod_times),

clear_mod_dates,

findall(_,(member([K2,Mod_time52],Mod_times),
open_s(K2,write,S),
write(S,Mod_time52),close(S)
),_),!,



%A1="../../Github_lc/", %working_directory1(_,"../../Github_lc/"),

	%working_directory1(_,A1),


	%retractall(home_dir(_)),assertz(home_dir(A1)),
%retractall(home_dir(_)),assertz(home_dir(_))
	%retractall(ci_fail(_)),assertz(ci_fail([])),

ci,
ci_end,
working_directory1(_,A1)
.

% Mode = "token", "line" or "predicate"

%luciancicd(Mode) :-

%	retractall(lc_mode(_)),assertz(lc_mode(Mode)),
%	luciancicd.


luciancicd :-
	
	get_time1,
	
	check_repositories_paths,
	%(lc_mode(_)->true;
	%(retractall(lc_mode(_)),assertz(lc_mode("line")))),
	
	working_directory1(A1z,A1z),

	find_tests_from_repos,
	
	working_directory1(_,A1z),

	retractall(log(_)),assertz(log("")),

	retractall(success(_)),assertz(success(0)),
	retractall(ci_fail(_)),assertz(ci_fail([])),
	
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

%trace,
    %msort(Mod_times11, Sorted1),
    %msort(Mod_times2, Sorted2),
    subtract(Mod_times2,Mod_times11,New),
    
    working_directory1(A1,A1),

	retractall(home_dir(_)),assertz(home_dir(A1)),

ci,
working_directory1(_,A1),

(    %Sorted1=Sorted2
	(%trace,
	(New=[]->true;(ci_fail(Ci_fail),forall(member(Ci_fail1,Ci_fail),Ci_fail1=1))))
->writeln("There are no modifications to repositories to test.");

% if 

(




%trace,


findall(Repository1,(member([Path,_],New),
string_concat(Path1,".txt",Path),
string_concat("../private2/luciancicd-data/mod_times_",Repository1,Path1)),Repositories),
%trace,
findall([Repository1,Dependencies5],(member(Repository1,Repositories),
%trace,
find_all_depending_luciancicd(LPPM_registry_term1,Repository1,[],Dependencies5)
%flatten(Dependencies42,Dependencies41),
%sort(Dependencies41,Dependencies5)
),Dependencies6),

findall(Dependencies5,(member([Repository1,Dependencies5],Dependencies6)),Dependencies8),
flatten(Dependencies8,Dependencies83),


  sort(Dependencies83,Dependencies9),


%trace,
 
%(findall(Results%[Repository1,T4]

 %BD='../../Github_lc/build',
%(exists_directory(BD)->true;make_directory(BD)),

%working_directory1(BD,BD),

LCTD="../private2/luciancicd-testing",


%trace,
findall(Dependencies990%Results
,(member(Repository1,Dependencies9),


working_directory1(_,A1),

 (success(1)->fail;true),

	foldr(string_concat,["rm -rf ../private2/luciancicd-testing/"],Command312),
 	catch(bash_command(Command312,_), _, (foldr(string_concat,["Warning."%%"Error: Can't clone ",User3,"/",Repository3," repository on GitHub."
	],Text412),writeln1(Text412),abort
 	)),

(exists_directory_s(LCTD)->true;make_directory_s(LCTD)),



User1="luciangreen",

find_all_dependencies(LPPM_registry_term1,%[[User1,Repository1]],%%,Description,Dependencies1
	[[User1,Repository1]%|Dependencies1
	],[],Dependencies1a)
		,
		%)),
 %         time_limit_exceeded,
  %        (concat_list(["Error: Cycle in lppm_registry.txt: ",Dependencies1],Note_a),writeln(Note_a),abort)),
  
	append([[User1,Repository1%%,Description,Dependencies1
	]],Dependencies1a,Dependencies2),

findall(D21,member([_,D21],Dependencies2),D22),
append(Dependencies9,D22,D23),
  sort(D23,Dependencies990)
  
  ),Dependencies991),
  
  flatten(Dependencies991,Dependencies992),
  sort(Dependencies992,Dependencies99),
  %trace,
   lc_tests(Lc_tests),

%trace,
((
findall([Tokens2,Tokens1]
,(member(Repository1a,Dependencies99),
 %trace,
 working_directory1(_,A1),

foldr(string_concat,["../../Github_lc/tests_",Repository1a,".txt"],K211),
 
%trace,
 %open_file_s
 %trace,
 member(%file,
 [K211|File2A1],Lc_tests),
 %File2A1=[_,Content1],
 %findall(*,(member([P,Tokens_i,Tokens_f],File2A1),
 File2A1=[Tokens2,Tokens1]),Tokens3),
 
 %trace,
 
 findall(%[
 AT2z%,",\n"]
 ,(member([AT2,_],Tokens3),foldr(string_concat,AT2,AT2z1),
 term_to_atom(AT2z,AT2z1%AT232
 )),AT22),%flatten(AT22,AT2x),%)),AT22),
 %append(AT24,[_],AT2x),
 %foldr(string_concat,AT24,AT235),
 %foldr(string_concat,["[",AT235,"]"],AT232),
 %term_to_atom(AT231,AT22%AT232
 %),
 foldr(append,AT22%AT231
 ,AT233),
 %trace,
 pp0_1(AT233,AT234),
 %term_to_atom(AT234,AT2341),
 split_string(AT234,"\n","\n",AT23),

%trace,
 %trace,
 findall(%[
 AT1z%,",\n"]
 ,(member([_,AT1],Tokens3),foldr(string_concat,AT1,AT1z1),
 term_to_atom(AT1z,AT1z1%AT132
 )),AT12),%flatten(AT12,AT1x),%)),AT12),
 %append(AT14,[_],AT1x),
 %foldr(string_concat,AT14,AT135),
 %foldr(string_concat,["[",AT135,"]"],AT132),
 %term_to_atom(AT131,AT12%AT132
 %),
 foldr(append,AT12%AT131
 ,AT133),
 %trace,
 pp0_1(AT133,AT134),
 %term_to_atom(AT134,AT1341),
 split_string(AT134,"\n","\n",AT13)

 %,trace
 )->true;(writeln("fault"),fail)),

%trace,
% 
%writeln(merge2(AT23,AT13,T4)),
 merge2(AT23,AT13,T4),%),T5),
 %writeln(merge2(AT23,AT13,T4)),
%trace,
% get all files, choose ones that are deps of a rep

 (success(1)->fail;true),
%trace,
	foldr(string_concat,["rm -rf ../private2/luciancicd-testing/"],Command3),
 	catch(bash_command(Command3,_), _, (foldr(string_concat,["Warning."%%"Error: Can't clone ",User3,"/",Repository3," repository on GitHub."
	],Text4),writeln1(Text4),abort
 	)),

 % find deps
 %trace,

 findall(_%Results%[PZ,FZ,T10]
 ,(
 %writeln(member(T44,T4)),
 member(T44,T4),
  %writeln(member(T44,T4)),

 findall([T51,"\n"],member(T51,T44),T522),%append(T522,[_],T52),
 flatten(T522,T53),
 foldr(string_concat,T53,T5),
 term_to_atom(T7,T5),split_into_lp_files(T7,T8),
 
   (success(1)->fail;true),

 writeln2(""),writeln2("**********"),
writeln2(["Installing Combination"]),


 findall(_,(member([[[n, comment], ["File delimiter", PZ, FZ]]|T10],T8),

  (success(1)->fail;true),


 writeln2(""),%writeln("**********"),
writeln2(["Installing",PZ, FZ%Repository1
]),

 %pwd,
  working_directory1(_,A1),
%pwd,

 	%writeln(["Installing dependency path",PZ,"file"%dependency"
 	%,FZ]),

 % path
 %trace,
 string_concat("../../Github_lc/",PZ1,PZ),
 foldr(string_concat,[LCTD,"/",PZ1%,"/"
 ],K212),

 % only want some reps files
 (exists_directory_s(LCTD)->true;make_directory_s(LCTD)),
 %(exists_directory_s(K212)->true;make_directory_s(K212)),
 make_directory_recursive_s(LCTD,PZ1),

 %trace,

 working_directory1(_,K212),
 %trace,
 % clear dir ***
 pp_lp2p0(T10,T11),
 %findall(_,(member([K2,Mod_time52],Mod_times),
open_s(FZ,write,S0),
write(S0,T11),close(S0)
%),_),!.

 ),_%T6
 ),
 
 %),_),
 
 % take apart, save repos
 % delete build/rep afterwards

%get needed reps
%findall(Results,(member(Repository1,Dependencies9),

%(Repository1="b"->trace;true),

%member([Repository1,Dependencies7],Dependencies6),
%findall(_,(member(Repository1,Dependencies7),
%writeln(["Installing required repository",Repository1]),

%lppm_install_luciancicd(LPPM_registry_term1,"luciangreen",Repository1),%),_),

%trace,
%pwd,
%notrace,
% test non-interactive algorithms
%trace,

 (success(1)->fail;true),

writeln2(["Running tests"]),
%trace,
findall(Results2,(member(Repository1b,Dependencies99),

working_directory1(_,A1),
 
foldr(string_concat,["../private2/luciancicd-cicd-tests/tests_",Repository1b,".txt"],Test_script_path),
(catch(open_file_s(Test_script_path,Tests),_,
(writeln2(["Cannot find",Test_script_path]),fail%,abort
))->

(%trace,
%working_directory1(_,A1), %***
working_directory1(A,A),
findall(Result,(member([Go_path1,File,Command],Tests),
foldr(string_concat,["../private2/luciancicd-testing/",%Repository1,
%"/",
Go_path1],Go_path),
((working_directory1(_,A),

%trace, %***
 %(exists_directory_s(LCTD)->true;make_directory_s(LCTD)),

 make_directory_recursive_s(LCTD,Go_path1),

working_directory1(_,Go_path),

% *** Change path to swipl if necessary

term_to_atom(Command,Command1),

string_concat(Repository1b,Go_path1a,Go_path1),
split_string(Go_path1a,"/","/",Go_path3),
(Go_path3=[_Go_path4]->Go_path5="";(Go_path3=[_|Go_path6],atomic_list_concat(Go_path6,'/',Go_path7),string_concat(Go_path7,"/",Go_path5))),
foldr(string_concat,["#!/usr/bin/swipl -f -q\n\n",":-include('",Go_path5,File,"').\n",":- initialization(catch(main, Err, handle_error(Err))).\n\nhandle_error(_Err):-\n  halt(1).\n","main :-\n    ",Command1,", nl,\n    halt.\n","main :- halt(1).\n"],String),
%trace,
%working_directory1(_,A),
foldr(string_concat,[%"../private2/luciancicd-testing/",Repository1b,"/",Go_path5,
"testcicd.pl"],GP),
%string_concat(Go_path,"testcicd.pl",GP),
open_s(GP,write,S1),
write(S1,String),close(S1),
foldr(string_concat,["chmod +x ",GP,"\n","swipl -f -q ./",GP],S3)%,
,catch(bash_command(S3,_), _, (foldr(string_concat,["Warning."%%"Error: Can't clone ",User3,"/",Repository3," repository on GitHub."
	],_Text4),%writeln1(Text4),
	fail%abort
 	))
%Command
)->(Result=success);Result=fail),
writeln12([Go_path1,File,Command,Result])),Results2)


)
;
true)

),Results3)

,flatten(Results3,Results3a),(forall(member(Result,Results3a),Result=success)->(retractall(success(_)),assertz(success(1)));true)

%),_)
),_Results1),
%trace,
%flatten(Results1,Results2),
%Results2=Results21,
%findall(Result4,(member(Result4,Results2),not(var(Result4))),Results21),
(success(1)%(forall(member(Result3,Results21),(not(var(Result3)),Result3=success))%,not(Results21=[])
->

% Only save mod times if all tests passed
(
/*
	foldr(string_concat,["rm -rf ../private2/luciancicd-data/"],Command31),
 	catch(bash_command(Command31,_), _, (foldr(string_concat,["Warning."%%"Error: Can't clone ",User3,"/",Repository3," repository on GitHub."
	],Text41),writeln1(Text41),abort
 	)),

(exists_directory('../private2/luciancicd-data')->true;make_directory('../private2/luciancicd-data')),
*/
findall(_,(member([K21,Mod_time521],Mod_times2),
open_s(K21,write,S21),
write(S21,Mod_time521),close(S21)
),_),




move_to_repository_or_back,


ci,
ci_end,

writeln2("All tests were successful."),
home_dir(HD),
working_directory1(_,HD)


)
;((true%not(Results21=[])
->(writeln2("1 or more tests failed.")

);true))
))),

success(S000),
working_directory1(A22,A22),

repositories_paths([Path]),
working_directory1(_,Path),
 (exists_directory_s("../lc_logs/")->true;make_directory_s("../lc_logs/")),
%trace,
log(Log),
term_to_atom(Log,Log1),
%Log1=[Log],
time1(Time),foldr(string_concat,["../lc_logs/log",Time,".txt"],Log_file_name),
open_s(Log_file_name,write,S21T),
write(S21T,[S000,Log1]),close(S21T),

working_directory1(_,A22)
.


make_directory_recursive_s(LCTD,PZ1) :-
 split_string(PZ1,"/","/",PZ2),
 delete(PZ2,"",PZ3),
 make_directory_recursive_s(LCTD,"",%PZ4,
 PZ3).
 
make_directory_recursive_s(_,_,%_,
[]) :- !.
make_directory_recursive_s(LCTD,PZ5,%PZ4,
PZ) :-
 PZ=[PZ6|PZ7],
 foldr(string_concat,[LCTD,"/",PZ5,PZ6%,"/"
 ],K212),
 (exists_directory_s(K212)->true;make_directory_s(K212)),
 foldr(string_concat,[PZ5,"/",PZ6,"/"
 ],PZ8),
 make_directory_recursive_s(LCTD,PZ8,%PZ4,
PZ7),!.

ci_end:-


 lc_tests(Tests),
 
 home_dir(AAA),
 working_directory1(_,AAA),
 
 (exists_directory('../../Github_lc')->
 
 (		time1(Time),
 %get_time(TS),stamp_date_time(TS,date(Year,Month,Day,Hour1,Minute1,Seconda,_A,_TZ,_False),local),
	foldr(string_concat,["../../Github_lc",Time,"/"],Folder1),
	%concat_list3(File1,[".txt"],File2),

mv_lc("../../Github_lc/",Folder1)
 %foldr(string_concat,[%"scp -pr ../../Github_lc/ ","rsync -av --exclude=\".*\"  ../../Github_lc/ ",Folder1],Command314),
 	%catch(bash_command(Command314,_), _, (foldr(string_concat,["Warning."%%"Error: Can't clone ",User3,"/",Repository3," repository on GitHub."],Text41),writeln1(Text41),abort))
 	);
 	(
 	%exists_directory('../../Github_lc')->true;
%make_directory('../../Github_lc')
true)),

working_directory1(Old_D1,Old_D1),
working_directory1(_,"../../Github_lc/"),


foldr(string_concat,[%"scp -pr ../../Github_lc/ ",
 "rm -f * */* */*/* */*/*/*"
 %Folder1
 ],Command315),
 	catch(bash_command(Command315,_), _, (foldr(string_concat,["Warning."%%"Error: Can't clone ",User3,"/",Repository3," repository on GitHub."
	],_Text42)%,writeln1(Text42)%,abort
 	)),
 	
 	working_directory1(_,Old_D1),

 	% The modified Prolog programs are saved
% - reset dirs, make folders x files have been cleaned from folders
%trace,
findall(_,(member([K21|Tests521],Tests),
term_to_atom(Tests521,Tests522),
open_s(K21,write,S21),
write(S21,Tests522),close(S21)
),_),

modification_dates(Mod_times),

clear_mod_dates,

findall(_,(member([K2,Mod_time52],Mod_times),
open_s(K2,write,S),
write(S,Mod_time52),close(S)
),_),!.


repositories_paths(Paths) :-
 repositories_paths1(Paths1),
 findall(Paths2,(member(Paths3,Paths1),
 ((string_concat(_Paths4,"/",Paths3),
 Paths2=Paths3)->true;
 string_concat(Paths3,"/",Paths2))),Paths),!.
 
omit_paths(Paths) :-
 omit_paths1(Paths1),
 findall(Paths2,(member(Paths3,Paths1),
 ((string_concat(Paths2,"/",Paths3))->true;
 (Paths3=Paths2))),Paths),!.
 
repositories_paths1([
%"../../GitHub/"
"../../GitHub2/"
%"reps/"
%"e/"
]).

omit_paths1([
"private2"
%"b" % omits a/b/
]).


modification_dates(Mod_time) :-

working_directory1(A,A),

(exists_directory('../private2/luciancicd-data')->true;make_directory('../private2/luciancicd-data')),

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

findall(Mod_time1,(member([D,K31],K01),

working_directory1(_,Old_D),

working_directory1(_,D),

%member(K2,K31),

%exists_directory(K2),

process_directory(K31,%_G,
 %Omit,%
 true,
 Mod_time1)%),Mod_time)
 ),Mod_time2),
 foldr(append,Mod_time2,Mod_time),
 
 working_directory1(_,A)

 ,!.

process_directory(K,%G,
 Top_level,%Mod_time1,
 Mod_time61) :-

%G=K,
%/*
findall(K4,(member(K1,K), directory_files(K1,F),
	delete_invisibles_etc(F,G),
%*/
findall(Mod_time3,(member(H,G),not(string_concat("dot",_,H)),

%not(member(H,Omit)),


foldr(string_concat,[K1,H],H1),

% if a file then find modification date
% if a folder then continue finding files in folder
(exists_directory(H1)->

(string_concat(H1,"/",H2),
process_directory([H2],%[H],
 false,%[],%Omit % only omit top level dirs xx
 %Mod_time1,
 Mod_time3)
 %foldr(append,Mod_time31,Mod_time3)
 );

(time_file(H1,Mod_time4),
%trace,
%append(Mod_time1,[[H1,Mod_time4]],Mod_time3)))
Mod_time3=[[H1,Mod_time4]]))

),Mod_time5),%trace,
foldr(append,Mod_time5,Mod_time51),

%Mod_time5=Mod_time51,

(Top_level=true%not(Omit=[]) % at top level
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

(%not(Omit=[])->
Top_level=true->
Mod_time6=Mod_time61;
foldr(append,Mod_time6,Mod_time61)),

!.


	%find_all_depending_luciancicd(LPPM_registry_term1,Repository1,Dependencies,Dependencies) :- !.
	find_all_depending_luciancicd(LPPM_registry_term1,Repository1,Dependencies7,Dependencies72) :-
	find_all_depending_luciancicd(LPPM_registry_term1,Repository1,Dependencies7,[],Dependencies72),!.
	find_all_depending_luciancicd(LPPM_registry_term1,Repository1,Dependencies7,D,Dependencies72) :-
((member([User1,Repository1,_Description1,_Dependencies1],LPPM_registry_term1),
not(member(Repository1,D)))->
(findall(Dependencies5,(member([User1,Repository2,_Description,Dependencies2],LPPM_registry_term1),
member([User1,Repository1],Dependencies2),
append(D,[Repository1],D2),
find_all_depending_luciancicd(LPPM_registry_term1,Repository2,[],D2,Dependencies4),
foldr(append,[Dependencies7,Dependencies4],Dependencies5)

),Dependencies3),
append([Repository1],Dependencies3,Dependencies6),
flatten(Dependencies6,Dependencies72));
Dependencies72=[]),
%flatten(Dependencies71,Dependencies72),
!.

%****  change later
lppm_get_registry_luciancicd(LPPM_registry_term1) :-
	catch(phrase_from_file_s(string(LPPM_registry_string), "../List-Prolog-Package-Manager/lppm_registry.txt"),_,(writeln1("Error: Cannot find ../List-Prolog-Package-Manager/lppm_registry.txt"),abort)),

term_to_atom(LPPM_registry_term1,LPPM_registry_string).

working_directory1(A1,B1) :-
 (string(A1)->atom_string(A,A1);A=A1),
 (string(B1)->atom_string(B,B1);B=B1),
 term_to_atom(working_directory(A,B),Atom),
 	catch(working_directory(A,B), _, (foldr(string_concat,["Error on ",Atom%%"Error: Can't clone ",User3,"/",Repository3," repository on GitHub."
	],Text41),writeln1(Text41),abort
 	)).
 	
split_into_lp_files(T7,T10) :-
 split_into_lp_files(T7,[],_T8,[],T9),
 delete(T9,[],T10),!.
 
split_into_lp_files([],B1,_B2,C1,C2) :-
 append(C1,[B1],C2),!.
split_into_lp_files(A,B,C,B1,C1) :-
 A=[D|E],
 not(D=[[n,comment],["File delimiter",_P,_F1]]),
 append(B,[D],F),
 split_into_lp_files(E,F,C,B1,C1),!.
split_into_lp_files(A,B,C,B1,C1) :-
 A=[D|E],
 D=[[n,comment],["File delimiter",_P,_F1]],
 append(B1,[B],B2),
 split_into_lp_files(E,[D],C,B2,C1),!.


pp0_1(A,B):-
 (%false%
 pp0(A,B)
 ->true;
 lines_to_comments(A,B)).
 
lines_to_comments(A,B) :-
 %term_to_atom(A,A1),
 split_string(A,"\n\r","\n\r",C),
 findall([[[n,comment],[D]]],member(D,C),B).
 
clear_mod_dates :-

working_directory1(A1,A1),
working_directory1(_,"../private2/luciancicd-data/"),

foldr(string_concat,[%"scp -pr ../../Github_lc/ ",
 "rm -f *"
 %Folder1
 ],Command315),
 	catch(bash_command(Command315,_), _, (foldr(string_concat,["Warning."%%"Error: Can't clone ",User3,"/",Repository3," repository on GitHub."
	],_Text42)%,writeln1(Text42)%,abort
 	)),
 	

working_directory1(_,A1).

check_repositories_paths :-
 repositories_paths(Paths),
 (not(Paths=[_])->
 (writeln("Only one repository path can be processed at at time."),abort);
 true),!.

get_time1 :-

	get_time(TS),stamp_date_time(TS,date(Year,Month,Day,Hour1,Minute1,Seconda,_A,_TZ,_False),local),
	foldr(string_concat,["-",Year,"-",Month,"-",Day,"-",Hour1,"-",Minute1,"-",Seconda],Time),
	
	retractall(time1(_)),
	assertz(time1(Time)).

writeln2(A) :- writeln12(A). 
/*
log(B),foldr(string_concat,[B,A,"\n"],C),
	retractall(log(_)),
	assertz(log(C)).
*/
writeln12(A) :- log(B),term_to_atom(A,A1),foldr(string_concat,[B,A1,"\n"],C),
	retractall(log(_)),
	assertz(log(C)),
	writeln1(A).

