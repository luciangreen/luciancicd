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
:-include('find_dependencies.pl').
:-include('settings.pl').

:-dynamic lc_tests/1.
:-dynamic home_dir/1.
:-dynamic ci_fail/1.
:-dynamic time1/1.
:-dynamic log/1.
:-dynamic pred_list/1.
:-dynamic pred_list_v/1.
:-dynamic success/1.
:-dynamic success_tmp/1.
:-dynamic test_n/1.
:-dynamic diff_html_n/1.
:-dynamic tests_preds3/1.

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
	
	retractall(diff_html_n(_)),
	assertz(diff_html_n(1)),

	retractall(test_n(_)),
	assertz(test_n(0)),

	retractall(success_tmp(_)),
	assertz(success_tmp([])),

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
->writeln2("There are no modifications to repositories to test.");

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
%trace,
 (success(1)->fail;true),
 %success_tmp(Tmp31),(forall(member(Tmp4,Tmp31),Tmp4=1)->true;fail),

	foldr(string_concat,["rm -rf ../private2/luciancicd-testing/"],Command312),
 	catch(bash_command(Command312,_), _, (foldr(string_concat,["Warning."%%"Error: Can't clone ",User3,"/",Repository3," repository on GitHub."
	],Text412),writeln1(Text412),abort
 	)),

(exists_directory_s(LCTD)->true;make_directory_s(LCTD)),


user(User1),

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
%((
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
 
 % id changed repos xx, get Tests - run tests from main file in Repositories
 
 % run find deps
 
 % find names, arities
 
 %trace,
%pwd,

 %append(AT233,AT133,AT333),
%trace, 
merge_files(AT233,AT133,AT333),
 % * merge, copy of new or old from start, into files, place same name, arity preds together
 % put same pred name arity together or at end if new
 % use split into lp files
 
 %trace,
 findall(H,(
 
 member(Dep99,Dependencies99), %* make future depends 99s [dep99]
 
 %(Dep99="b"->trace;true),
 
 %Dep991=[Dep99],
 %trace,
 working_directory1(_,A1),
 
 read_main_file(Dep99,H%_,Dep99_name,Dep99_arity
 )),H1),
 
 foldr(append,H1,H2),
 sort(H2,H3),
 
 retractall(pred_list(_)),
 assertz(pred_list([]%Dependencies7d
 )),

 %trace,
 findall(_,(
 member([Dep99,_,Dep99_name,Dep99_arity],H3),

%trace,

find_dependencies(Dep99_name,Dep99_arity,AT333,Dependencies7d,Pred_numbers0),

%trace, 
 %length(AT333,AT333L),
 %numbers(AT333L,1,[],AT333N),
 
 % New ones
  %trace,
  

 length(AT333,AT333L),
 numbers(AT333L,1,[],AT333N3),
 findall(AT233N1,(member(AT233N1,AT333N3),
 get_item_n(AT333,AT233N1,AT233N2),
 member(AT233N2,AT133)),AT233N),
 
 findall(AT233N1,(member(AT233N1,AT333N3),
 get_item_n(AT333,AT233N1,AT233N2),
 member(AT233N2,AT233)),AT233N_old),

 %length(AT233,AT233L)
 %numbers(AT233L,1,[],AT233N),
 /*

 

 ((member([Pred_name1|Rest2],AT1331),
 pred_rest(Arity1,Rest2,Lines2))->
 (append(AT333,)(T10,T11,[],T12),
 delete(AT1331,[[[n, comment], [["File delimiter", PZ, FZ]]]|T11],AT1333));
 (T12=T10,AT1331=AT1333)),
 append(AT333,[[[n, comment], [["File delimiter", PZ, FZ]]]|T12],AT3332),
 merge_files3(AT2333,AT1333,AT3332,AT3331).

pred_rest(Arity1,Rest) :-
*/
 
 % group clauses
 
 delete(Pred_numbers0,[[n, query_box_1], _, _],Pred_numbers),
 group_clauses(Dependencies7d,Pred_numbers,Dependencies7d1),

 %length(AT133,AT133L),
 %numbers(AT133L,1,[],AT133N),

 % assign old or new labels to deps
 %trace,
  findall(LD1,(member(Dependencies7d2,Dependencies7d1),
 (Dependencies7d2=[loop1,Loop1a]->
 (findall([ON,CN,PN],(member(Loop1b,Loop1a),Loop1b=[CN,PN],(((member(PN,AT233N),member(PN,AT233N_old))->member(ON,[new,old]);(member(PN,AT233N)->ON=new;ON=old)))),Loop1c),LD1=[loop1,Loop1c]);
 (Dependencies7d2=[CN,PN],(((member(PN,AT233N),member(PN,AT233N_old))->member(ON,[new,old]);(member(PN,AT233N))->ON=new;ON=old),LD1=[ON,CN,PN])))),Dependencies7d3),
 
%trace,
 (once(member([[n, comment], 1, Comment_pred_ns3],Pred_numbers))->true;Comment_pred_ns3=[]),

 (once(member([":-", 1, Includes_pred_ns],Pred_numbers))->true;
 Includes_pred_ns=[]),

% Find new comments

%(Test_n1=2->trace;true), 

findall(AT133N1,(member(AT133N1,AT333N3),
get_item_n(AT333,AT133N1,[[n,comment]|_])),Comment_pred_ns),
 
  append(Comment_pred_ns,Includes_pred_ns,Comment_pred_ns2),
%findall(Comment_pred_n,(member(Comment_pred_n,Comment_pred_ns),(member(Comment_pred_n,AT233N))),Comment_pred_ns2),

 % group into old, new clauses, loops

 %trace,
 
   findall(LD1A,(member(Dependencies7d2,Dependencies7d3),
 (Dependencies7d2=[loop1,Loop1a]->LD1A=Loop1a;LD1A=Dependencies7d2)),Dependencies7d5),
 group_into_clauses1(Comment_pred_ns2,Dependencies7d5,[],Dependencies7d4),


/*
 % delete(Dependencies7d1,Comment_pred_ns,LD2) - delete all comments
 findall(LD1,(member(Dependencies7d2,Dependencies7d4),*
 (Dependencies7d2=[loop1,Loop1a]->
 (findall(Loop1b,(member(Loop1b,Loop1a),Loop1b=[ON,CN,PN],not(member(PN,Comment_pred_ns))),Loop1c),LD1=[loop1,Loop1c]);
 (Dependencies7d2=[ON,CN,PN],not(member(PN,Comment_pred_ns))->LD1=Dependencies7d2)))),LD21),
*/

 % Choose predicates to test
%trace,

(Dependencies7d4=[]->Dependencies7d6=[];
append([[[old,Old_a],[new,New_a]]],Dependencies7d6,Dependencies7d4)),
 findall([new,_,Comment_pred_ns21],member(Comment_pred_ns21,Comment_pred_ns2),Comment_pred_ns22),%*
 
 append(New_a,Comment_pred_ns22,Comment_pred_ns23),

 append(Old_a,Comment_pred_ns22,Comment_pred_ns24),
 
  append([[[old,Comment_pred_ns24],[new,Comment_pred_ns23]]],Dependencies7d6,Dependencies7d7),

%trace,

 findall(_,(
 %trace,

 %success_tmp(Tmp32),(forall(member(Tmp4,Tmp32),Tmp4=1)->true;fail),
 %trace,
 
 
%trace,
 append(Curr_preds,_,Dependencies7d7%LD21
 ),
 not(Curr_preds=[]),

 (success(1)->fail;true),
 %trace,
%writeln( append(Curr_preds,_,Dependencies7d7)),
 %trace,

 %length(Curr_preds,Curr_preds_L),
 %length(Dependencies7d7,Dependencies7d7_L),
 %writeln(append(Curr_preds_L,_,Dependencies7d7_L)),

 %(Curr_preds_L=2->trace;true),
 /*
 %append(Curr_preds,Comment_pred_ns2,LD4) - append new comments ** ld4 has no loops xx
findall(LD31,(member(LD3,Dependencies7d4),LD3=[ON,CN,PN],(member(PN,Curr_preds)->LD31=LD3;

 (LD3=[loop1,Loop1a]->
 (findall(Loop1b,(member(Loop1b,Loop1a),Loop1b=[ON,CN,PN],member(PN,Comment_pred_ns2)),Loop1c),LD31=[loop1,Loop1c]);
(LD3=[ON,CN,PN],member(PN,Comment_pred_ns2)->LD31=LD3))

)),LD4),
*/
 %Curr_preds=[Curr_preds2],
 append(_Curr_preds1,[Curr_pred_n],Curr_preds),
 % cpn can include loop1
 


 %findall(LD51,(member([[old,_],[new,New_a]],Curr_preds%LD4
 %),member(get_item_n(Dependencies7d,LD5,LD51)),AT1331),
%loop* x
%trace,
 Curr_pred_n=[[old,Old_a4],[new,New_a4]],
%trace,
%trace, 
 %list_to_set(Old_a,Old_a1),
 %list_to_set(New_a,New_a1),
 findall([LD6,LD7,LD8],member([LD7,LD8,LD6],Old_a4),Old_a2),
 sort(Old_a2,Old_a3),
 findall([LD7,LD8,LD6],member([LD6,LD7,LD8],Old_a3),Old_a1),

 findall([LD6,LD7,LD8],member([LD7,LD8,LD6],New_a4),New_a2),
 sort(New_a2,New_a3),
 findall([LD7,LD8,LD6],member([LD6,LD7,LD8],New_a3),New_a1),


%trace,

 findall(LD52,(%member(LD51,Old_a%LD4
 %),
 member([_,_,LD5],Old_a1),get_item_n(AT333,LD5,LD52)),AT2331c),

 findall(LD52,(%member(LD51,New_a%LD4
 %),
 member([_,_,LD5],New_a1),get_item_n(AT333,LD5,LD52)),AT1331c),
 %trace,
 %list_to_set1(AT2331c1,AT2331c),
 %list_to_set1(AT1331c1,AT1331c),
 
%loop* x

/*
 findall(LD52,(member(Old_a1,Curr_preds1),member(LD51,Old_a1%LD4
 ),member([_,_,LD5],LD51),member(get_item_n(Dependencies7d,LD5,LD52)),AT2331),


 findall(LD52,(member(New_a1,Curr_preds1),member(LD51,New_a1%LD4
 ),member([_,_,LD5],LD51),member(get_item_n(Dependencies7d,LD5,LD52)),AT1331),
*/

 % merge, build, test each level of preds, saving progress
 
 % merge only curr pred, put back in list, save changes
 
 % comments
 %pwd,
 %trace,
 %trace,
 pp0_1(AT2331c,AT234),
 %term_to_atom(AT234,AT2341),
 split_string(AT234,"\n","\n",AT23),

 pp0_1(AT1331c,AT134),
 %term_to_atom(AT134,AT1341),
 split_string(AT134,"\n","\n",AT13),

 %,trace
 %)->true;(writeln("fault"),fail)),


%trace,
% 
%writeln(merge2(AT23,AT13,T4)),
 merge2(AT23,AT13,T4),%),T5),
 %writeln(merge2(AT23,AT13,T4)),
%trace,
% get all files, choose ones that are deps of a rep

 (success(1)->fail;true),
 %success_tmp(Tmp33),(forall(member(Tmp4,Tmp33),Tmp4=1)->true;fail),
%trace,
	/*foldr(string_concat,["rm -rf ../private2/luciancicd-testing/"],Command3),
 	catch(bash_command(Command3,_), _, (foldr(string_concat,["Warning."%%"Error: Can't clone ",User3,"/",Repository3," repository on GitHub."
	],Text4),writeln1(Text4),abort
 	)),
*/
 % find deps
 %trace,

 findall(_%Results%[PZ,FZ,T10]
 ,(
 %writeln(member(T44,T4)),
 member(T44,T4),

 (success(1)->fail;true),
%trace,
 % (Curr_preds_L=2->trace;true),
 %foldr(string_concat,T44,T48),
 pred_list(Pred_list2),

 foldr(string_concat,T44,T451),

 catch(term_to_atom(T49,T451),_,fail),
 %pp0(T49,T47),


 put_in_nums(T49,AT333,T491), % leave exact comments, includes x

 append(Pred_list2,T491,T47),

 sort(T47,T471), % leave comments, includes x
 findall(T472,member([_,T472],T471),T473), % strip nums
 
 pp0(T473,T50),

 
 %pp0_1(T46,T47),
 %term_to_atom(AT234,AT2341),
 split_string(T50,"\n","\n",T45),


  %writeln(member(T44,T4)),

/*
 pred_list(Pred_list2),
 %trace,
 ((%false,
 not(Pred_list2=[]),not(Pred_list2="[]"),
 not(T44=[]),not(T44="[]"))->
 
 (Pred_list2=["["|Pred_list222],
 append(Pred_list223,["]"],Pred_list222),
 T44=["["|T444],
 foldr(append,[["["],Pred_list223,[","],T444],T45),notrace);

 %foldr(append,[Pred_list2,[Poss_comma],T44],T45);
 (foldr(append,[Pred_list2,T44],T45))),
 notrace, % assuming T44 is a list of preds
 */

 findall([T51,"\n"],member(T51,T45),T522),%append(T522,[_],T52),
 flatten(T522,T53),
 foldr(string_concat,T53,T5),
 term_to_atom(T7,T5),split_into_lp_files(T7,T8),
 
   (success(1)->fail;true),
 %success_tmp(Tmp34),(forall(member(Tmp4,Tmp34),Tmp4=1)->true;fail),

 writeln2(""),writeln2("**********"),
writeln2(["Installing Combination"]),

	test_n(Test_n),
	Test_n1 is Test_n+1,
	retractall(test_n(_)),
	assertz(test_n(Test_n1)),
	%writeln([test_n1,Test_n1]),

 %test_n(Test_n0),
 %(Test_n0=1->trace;true),
	
	%(Test_n1=5->trace;true),
	
 findall(_,(member([[[n, comment], [["File delimiter", PZ, FZ]]]|T10],T8),

  (success(1)->fail;true),
 %success_tmp(Tmp35),(forall(member(Tmp4,Tmp35),Tmp4=1)->true;fail),


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
%<<<<<<< Updated upstream
%=======
 %pp_lp2p0(T10,T11),
%>>>>>>> Stashed changes
 lp2p1(T10,T11),
 %findall(_,(member([K2,Mod_time52],Mod_times),
open_s(FZ,write,S0),
write(S0,T11),close(S0)

%writeln([write(FZ,T11)])

%),_),!.

 ),_%T6
 ),
 %***
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
 %success_tmp(Tmp36),(forall(member(Tmp4,Tmp36),Tmp4=1)->true;fail),

writeln2(["Running tests"]),
%trace,
findall(H4,(member(Repository1b,Dependencies99),

findall([Repository1b,Main_file1],member([Repository1b,Main_file1,_,_],H3),H4)),H5),
%writeln([member(Repository1b,Dependencies99)]),

foldr(append,H5,H61),
sort(H61,H6),
%trace,
%Repository1b=Dep99,

findall(Results2,(member([_,_Main_file],H6),%member(Repository1b,Dependencies99),

(success(1)->fail;true),

working_directory1(_,A1),
 
findall(Tests_a,(member([Repository1b1,_],H6), 
foldr(string_concat,["../private2/luciancicd-cicd-tests/tests_",Repository1b1,".txt"],Test_script_path),
(catch(open_file_s(Test_script_path,Tests_a),_,
(writeln2(["Cannot find",Test_script_path]),fail%,abort
)))),Tests_b),
%trace,
foldr(append,Tests_b,Tests),%->

((
%trace,
%working_directory1(_,A1), %***
working_directory1(A,A),




append(AT2331c,AT1331c,AT3331c),

tests_pred2(Tests,AT3331c,Tests01),
%trace,
%Tests=Tests01,
sort1(Tests01,Tests0),
%writeln([tests0,Tests0]),
%notrace
%trace,

findall(Result,(member([Go_path1,File,Command],Tests0),
Repository1b=Go_path1,
%trace,
(true->%tests_pred(AT1331c,Command)->
(

%foldr(string_concat,["../private2/luciancicd-testing/",%Repository1,
%Go_path1],Go_path),

foldr(string_concat,["../private2/luciancicd-testing/",Repository1b,"/"],_Go_path3),

%foldr(string_concat,["../private2/luciancicd-testing/",%Repository1,
%"/",
%Go_path1,"/main_file.txt"],Go_path2),

%(catch(open_file_s(Go_path2,[Main_file1,_,_]),_,
%(writeln2(["Cannot find",Test_script_path]),(writeln(["Missing main_file.txt in " ,Go_path1,"/"]),abort)%,abort
%))),
%trace,
working_directory1(_,A),
%read_main_file(Repository1b,Main_file1%,_,_
%),

%atom_string(Main_file1,Main_file),

((working_directory1(_,A),

%trace, %***
 %(exists_directory_s(LCTD)->true;make_directory_s(LCTD)),

 make_directory_recursive_s(LCTD,Go_path1),

working_directory1(_,LCTD),
%working_directory1(_,Go_path),
working_directory1(_,Go_path1),

% *** Change path to swipl if necessary

term_to_atom(Command,Command1),

/*
string_concat(Repository1b,Go_path1a,Go_path1),
split_string(Go_path1a,"/","/",Go_path3),
(Go_path3=[_Go_path4]->Go_path5="";(Go_path3=[_|Go_path6],atomic_list_concat(Go_path6,'/',Go_path7),string_concat(Go_path7,"/",Go_path5))),
*/

%:-initialization(catch(call_with_time_limit(1,main),Err,handle_error(Err))).

foldr(string_concat,["#!/usr/bin/swipl -g main -q\n\n",":-include('../",Repository1b,"/",%Go_path5,
File%File
,"').\n","handle_error(_Err):-\n  halt(1).\n","main :-\n    catch(call_with_time_limit(1,(",Command1,")), Err, handle_error(Err)), nl,\n    halt.\n","main :- halt(1).\n"],String),
%trace,
%working_directory1(_,A),
foldr(string_concat,[%"../private2/luciancicd-testing/",Repository1b,"/",Go_path5,
"testcicd.pl"],GP),
%string_concat(Go_path,"testcicd.pl",GP),
open_s(GP,write,S1),
write(S1,String),close(S1),
foldr(string_concat,["chmod +x ",GP,"\n","swipl -g main -q ./",GP],S3),%,

 %(Test_n0=5->trace;true),

/*
catch(call_with_time_limit(7,bash_command(S3,_)),_,(foldr(string_concat,["Warning."%%"Error: Can't clone ",User3,"/",Repository3," repository on GitHub."
	],_Text4),%writeln1(Text4),
	fail%abort
 	))
*/
%/*
catch(bash_command(S3,_), _, (foldr(string_concat,["Warning."%%"Error: Can't clone ",User3,"/",Repository3," repository on GitHub."
	],_Text4),%writeln1(Text4),
	fail%abort
 	))
 	%*/
%Command
)->((Result=success,
%trace,

retractall(pred_list(_)),
assertz(pred_list(T471))
%)

));(Result=fail%,trace
)),
writeln12([Go_path1,File,Command,Result])
);Result=fail)
),Results2)

,flatten(Results2,Results2a),(forall(member(Result,Results2a),Result=success)->(retractall(success(_)),assertz(success(1)));true)

%, (Test_n0=4->trace;true)



%;
%true
)))

,Results3)



%,flatten(Results3,Results3a),(forall(member(Result,Results3a),Result=success)->(success_tmp(Tmp),append(Tmp,[1],Tmp1),retractall(success_tmp(_)),assertz(success_tmp(Tmp1)));
%(success_tmp(Tmp),append(Tmp,[0],Tmp1),retractall(success_tmp(_)),assertz(success_tmp(Tmp1))))

,flatten(Results3,Results3a),(forall(member(Result,Results3a),Result=success)->(retractall(success(_)),assertz(success(1)));true)


%),_)
),_Results1),

%trace,
/*
 pred_list_v(T8),
 pred_list(Pred_list),
 append(Pred_list,T8,Pred_list2),
 retractall(pred_list(_)),
 assertz(pred_list(Pred_list2%Dependencies7d
 ))
 */
 (success(0)->(writeln2("Current predicate set failed."),retractall(success(_)),assertz(success(1)),fail%,abort,working_directory1(_,A1)
 );(writeln2("Current predicate set passed."),%trace,
 retractall(success(_)),assertz(success(0))
 ))

),Result4)

,length(Dependencies7d7,Dependencies7d7L),
length(Result4,Dependencies7d7L)

),Result5),

%trace,
%flatten(Results1,Results2),
%Results2=Results21,
%findall(Result4,(member(Result4,Results2),not(var(Result4))),Results21),


 %success_tmp(_Tmp37),(true%forall(member(Tmp4,Tmp37),Tmp4=1)
 %->(retractall(success(_)),assertz(success(1)));
%(retractall(success(_)),assertz(success(0)))),

((length(H3,H3L),
length(Result5,H3L))
%success(1)%(forall(member(Result3,Results21),(not(var(Result3)),Result3=success))%,not(Results21=[])
->

% Only save mod times if all tests passed
(working_directory1(_,A1),
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

working_directory1(_,A1),

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
 PZ3),!.
 
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
	],Text41),writeln1(Text41)%fail%abort
 	)).
 	
split_into_lp_files(T7,T10) :-
 split_into_lp_files(T7,[],_T8,[],T9),
 delete(T9,[],T10),!.
 
split_into_lp_files([],B1,_B2,C1,C2) :-
 append(C1,[B1],C2),!.
split_into_lp_files(A,B,C,B1,C1) :-
 A=[D|E],
 not(D=[[n,comment],[["File delimiter",_P,_F1]]]),
 append(B,[D],F),
 split_into_lp_files(E,F,C,B1,C1),!.
split_into_lp_files(A,B,C,B1,C1) :-
 A=[D|E],
 D=[[n,comment],[["File delimiter",_P,_F1]]],
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
 findall([[n,comment],[D]],member(D,C),B).
 
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
 (writeln2("Only one repository path can be processed at a time."),abort);
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

group_clauses(Dependencies7,Pred_numbers,Clause_Ns3) :-
 length(Pred_numbers,Pred_numbers_L),
 numbers(Pred_numbers_L,1,[],Pred_numbers_Ns),
 findall([Pred_numbers_N,A,B,C],(member(Pred_numbers_N,Pred_numbers_Ns),get_item_n(Pred_numbers,Pred_numbers_N,[A,B,C])),Pred_numbers2),
 
 findall(D3,(member(D,Dependencies7),
 (D=[loop1,D1]->(findall([N1,D2],(member(D2,D1),
 member([N1,A,B,C],Pred_numbers2),member(D2,C)),ND1),D3=[loop1,ND1]);
 (member([N,A,B,C],Pred_numbers2),member(D,C),D3=[N,D]))),Clause_Ns),
 
 collect_clauses_in_loops(Clause_Ns,Clause_Ns1),
 move_non_loop_clauses_to_loop(Clause_Ns1,Clause_Ns3),!.
 
 %findall(M,(member(G,Clause_Ns2),
 %(G=[loop1,H]->(findall(J,member([_,J],H),L),M=[loop1,L]);G=[_,M])),Clause_Ns3),!.
 
collect_clauses_in_loops(C,B) :-
 sub_term_wa([loop1,_],C,A),
 collect_clauses_in_loops1(A,[],D),
 foldr(put_sub_term_wa_ae,D,C,B).

collect_clauses_in_loops1([],A,A) :- !.
collect_clauses_in_loops1(A,B,C) :-
 A=[[Add,[loop1,List]]|D],
 collect_clauses_in_loops1(List,D,[],E),
 append(B,[[Add,[loop1,List]]],F),
 collect_clauses_in_loops1(E,F,C).

collect_clauses_in_loops1(_,[],F,F) :- !.
collect_clauses_in_loops1(List1,D,F,E) :-
 D=[[Add,[loop1,List2]]|G],
 %subtract(List2,List1,List3),
foldr(collect_clauses_in_loops2,List1,List2,List3),

 append(F,[[Add,[loop1,List3]]],H),
 collect_clauses_in_loops1(List1,G,H,E).
 
collect_clauses_in_loops2([N1,A],NDs1,NDs2) :-
 (member([N1,_],NDs1)->delete(NDs1,[N1,_A2],NDs2);
 append(NDs1,[[N1,A]],NDs2)).
 

move_non_loop_clauses_to_loop(C,B) :-
 %sub_term_wa([loop1,_],C,A),
 %findall([loop1,D],(member([loop1,D],C)),A),
 move_non_loop_clauses_to_loop1(C,C,B1%,[],D%,[],NLC
 ),delete(B1,["&del"],B).
 % delete non loop clauses
 %foldr(put_sub_term_wa_ae,D,C,B).
 
move_non_loop_clauses_to_loop1([],A,A%,D,D%,NLC,NLC
) :- !.
move_non_loop_clauses_to_loop1(C,A1,A2%,D1,D2%,NLC1,NLC2
) :-
 C=[E|F],
 (E=[loop1,_]->(A1=A2);%get_n_item(A1,E,N),put_item_n(A1,N,append(A1,[E],A2));
 (move_non_loop_clauses_to_loop2(E,A1,A1,A3),
 move_non_loop_clauses_to_loop1(F,A3,A2))).%,D1,D2%,NLC1,NLC2


move_non_loop_clauses_to_loop2(_,[],A,A) :- !.
move_non_loop_clauses_to_loop2(E,A1,A2,A4) :-
 E=[N,E1],
 A1=[%[Add,
 F%[loop1,B]%]
 |C],
 (F=[loop1,B]->
 (member([N,_E2],B)->(append(B,[[N,E1]],B1),%delete(A1,[loop1,B],A3),
 get_n_item(A2,F,N1),put_item_n(A2,N1,[loop1,B1],A31),
 get_n_item(A31,[N,E1],N2),put_item_n(A31,N2,["&del"],A3)
 %append(A2,[%[Add,
 %[loop1,B1]]%]
 %,A4)
 );
 (A2=A3%append(A2,[%[Add,
 %[loop1,B]]%]
 %,A3)
 %move_non_loop_clauses_to_loop2(E,C,A3,A4))));
 ));
 A2=A3%append(A2,[F],A3)
 ),
 move_non_loop_clauses_to_loop2(E,C,A3,A4).
 

%group_into_clauses1(A,B,C):-forall(member(C,A),A=[loop1,_]),append(A,B,C),!.
group_into_clauses1(_,[],A,A) :- !.
group_into_clauses1(Comment_pred_ns,A,B,F) :-
 A=[C|D],
 (C=[loop1,E]->(append(D,E,B11),G=D,
 group_into_clauses1(Comment_pred_ns,B11,[],B1));
 (C=[_ON,CN,PN],
 (member(PN,Comment_pred_ns)->
 (G=D,H=[[old,[]],[new,[C]]]);
 (append(D,A
 ,G2),list_to_set(G2,G1),group_into_clauses([_, CN, _], G1, G1, G%[]
 , []%A
 , H1),
 group_into_old_new(H1,H))),
 append(B,[H],B1))),
 group_into_clauses1(Comment_pred_ns,G,B1,F).

group_into_clauses(_,[],
NDs1,NDs1,NDs2,NDs2) :- !.
group_into_clauses([A2,N1,B2],NDs0,
NDs1,NDs2,NDs3,NDs4) :-
 NDs0=[[A3,N10,B3]|NDs01],%member([_,N1,_],NDs1),
 ((N10=N1,member([A3,N1,B3],NDs1))->(delete(NDs1,[A3,N1,B3],NDs20),
 append(NDs3,[[A3,N1,B3]],NDs40));
 (NDs3=NDs40,NDs1=NDs20%append(NDs1,[[A2,N1,B2]],NDs20)
 )),
 group_into_clauses([A2,N1,B2],NDs01,
 NDs20,NDs2,NDs40,NDs4),!.


group_into_old_new(H1,H) :-
 findall([old,A,B],member([old,A,B],H1),H2),
 findall([new,A,B],member([new,A,B],H1),H3),
 H=[[old,H2],[new,H3]].

/*
tests_pred(AT1331c,Command) :-
 member([N|VE],AT1331c),
 N=[_,N1],
 ((VE=[V]->true;(VE=[V|_]))->length(V,Arity);Arity=0),
 square_to_round(List,Command),
 functor(Item,N1,Arity),
 member(Item,List),!.
*/

tests_pred2(Tests,AT3331c,Tests0) :-
 %writeln2(["Contains predicates: "]),
 retractall(tests_preds3(_)),
 assertz(tests_preds3([])),
findall([Go_path1,File,Command],(member([N|VE],AT3331c),
 N=[_,N1],
 ((VE=[V]->true;(VE=[V|_]))->length(V,Arity);Arity=0),
 member([Go_path1,File,Command],Tests),
 square_to_round(List,Command),
 functor(Item,N1,Arity),
 (not(N1=comment)->
 (tests_preds3(TP),
 append(TP,[[N1,Arity]],TP1),
 retractall(tests_preds3(_)),
 assertz(tests_preds3(TP1)))
 ;true),
 member(Item,List)),Tests0),
 tests_preds3(TP2),
 sort(TP2,TP3),
 writeln2(["Contains predicates: ",TP3]),%writeln2(""),
 !.


%find_first_pred(Dep99,H%File,Dep99_name,Dep99_arity
%) :-
%read_main_file(Dep99,G),
%findall([B,C],member([_,B,C],G),H),!.
%[A,B,C]=[File,Dep99_name,Dep99_arity],!.

read_main_file(Dep99,G) :-
working_directory1(A1,A1),

repositories_paths([Path]),

foldr(string_concat,[Path,Dep99,
%"/",
%Go_path1,
"/main_file.txt"],Go_path2),

(catch(open_file_s(Go_path2,A),_,
(writeln2(["Cannot find",Go_path2]),(writeln(["Missing main_file.txt in " ,Dep99,"/"]),abort)%,abort
))),

findall([Dep99,B,C,D],(member([B,E],A),member([C,D],E)),G),
%atom_string(Dep99_name1,Dep99_name),

working_directory1(_,A1),
!.

%merge_files(_AT233,AT3331,AT3331) :- !.
merge_files(AT233,AT133,AT3331) :-
 split_into_lp_files(AT233,AT2331),
 split_into_lp_files(AT133,AT1331),
 merge_files2(AT2331,AT1331,[],AT333),
 foldr(append,AT333,AT3332),
 % keep separate tests (sometimes with duplicates %A=1), remove other dups
 %trace,
 list_to_set1(AT3332,AT3331).
 
merge_files2([],AT1331,AT333,AT3331) :-
 append(AT333,AT1331,AT3331),!.
merge_files2(AT2331,AT1331,AT333,AT3331) :-
 AT2331=[[[[n, comment], [["File delimiter", PZ, FZ]]]|T10]|AT2333],
 (member([[[n, comment], [["File delimiter", PZ, FZ]]]|T11],AT1331)->
 (merge_files3(T10,T11,T12),
 delete(AT1331,[[[n, comment], [["File delimiter", PZ, FZ]]]|T11],AT1333));
 (T12=T10,AT1331=AT1333)),
 append(AT333,[[[[n, comment], [["File delimiter", PZ, FZ]]]|T12]],AT3332),
 merge_files2(AT2333,AT1333,AT3332,AT3331).

merge_files3([],AT1331,AT1331%,AT3331
) :-
 %append(AT333,AT1331,AT13331),
 !.
merge_files3(AT2331,AT1331,AT333%,AT3331
) :-
%writeln1(merge_files3(AT2331,AT1331,AT333)),
 AT2331=[[Pred_name1|Rest1]|_AT2333],
 pred_rest(Arity1,Rest1,_Lines1),
 findall([Pred_name1|Rest3],(member([Pred_name1|Rest3],AT2331),
 pred_rest(Arity1,Rest3,_Lines3)),Ps),
 subtract(AT2331,Ps,Ps2),
 %((%trace,
% append(C2,D2,AT2331),append(Ps,E2,D2),foldr(append,[C2,E2],Ps2))->true;AT2331=Ps2),!,

 reverse(AT1331,AT1332),
 ((append(A,B,AT1332),
 append([[Pred_name1|Rest4]],C,B),!,
 pred_rest(Arity1,Rest4,_Lines2)
 )->
 (%trace,
 reverse(A,A1),reverse(C,C1),foldr(append,[C1,[[Pred_name1|Rest4]],Ps,A1],AT1334));
 append(AT1331,Ps,AT1334)),
 merge_files3(Ps2,AT1334,AT333),!.%,AT3331) :-

put_in_nums(T49,AT333,T491) :- % leave exact comments, includes x
 /*findall(*,(member([Pred_name1|Rest1],T49),
 pred_rest(Arity1,Rest1,_Lines2),
 get_n_item(AT333,)
 )))
 */
 length(AT333,AT333L),
 numbers(AT333L,1,[],AT333N),
 findall([AT333N1,AT333Item],(member(AT333N1,AT333N),
 get_item_n(AT333,AT333N1,AT333Item),
 member(AT333Item,T49)),T491),!.

list_to_set1(A,B) :-
 list_to_set1(A,[],B),!.
 
list_to_set1([],A,A) :- !.
list_to_set1(A,B,C) :-
 A=[D|E],
 (E=[]->append(B,[D],C);
 ((D=[[n,comment],[String]],string(String),string_strings(String,C1),contains_assignment(C1))->
 (E=[E1|E2],
 ((E1=[[n,comment],[String1]],string(String1),string_strings(String1,C2),contains_assignment(C2))->(append(B,[D],G),list_to_set1(E2,G,C));(append(B,[D],G),list_to_set1(E,G,C))));
 
 (%E=F,%
 ((append(E3,E4,E),
 append([D],E5,E4),
 foldr(append,[E3,E5],F))->true;
 E=F),
 %delete(E,D,F),
 append(B,[D],G),
 list_to_set1(F,G,C)))),!.
 
contains_assignment(C1) :-
append(_A1,Bx,C1),append(_E6,Dxx,Bx),append(E61,Dxx1,Dxx),(append(["."],_Exx,Dxx1)),foldr(string_concat,E61,E612),sub_string(E612,_,_,_,"=").
%subtract1(A,B,C) :-
% subtract1(A,B,[],C),!.

/*
subtract1(A,[],A) :- !.
subtract1(A,B,G) :-
 B=[D|E],
 ((append(C2,D2,A),append([D],E2,D2),!)->true;A=E2),subtract1(E2,E,H),foldr(append,[C2,H],G).
 %subtract1(F,E,G),!.
*/

/*
subtract2(A,[],A) :- !.
subtract2(A,B,G) :-
 B=[D|E],
 
 (D=[[n,comment]|_]->
 A=C;%append(A,[D],G);%)(((append(C2,D2,A),append([D],E2,D2),!)->true;(C2=[],A=E2)),subtract2(E2,E,H),foldr(append,[C2,H],G));
 delete(A,D,C)),subtract2(C,E,G),!.
 */
 
/*
sublist(D,A,F) :-
 append(C2,D2,A),append([D],E2,D2),foldr(append,[C2,E2],F),!.
*/
sort1(Tests01,Tests0) :-
 sort1(Tests01,[],Tests0),!.
sort1([],B,B) :- !.
sort1(A,B,C) :-
 A=[D|E],
 delete(E,D,F),
 append(B,[D],G),
 sort1(F,G,C).

