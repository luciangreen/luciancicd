gh2tmp :- 
 working_directory1(A,A),
 (time1(T1)->true;get_time1),
 repositories_paths1([Path]),
 working_directory1(_,Path),
%(exists_directory('../gh2_tmp')->true;make_directory('../gh2_tmp')),
 (exists_directory('../gh2_tmp')->
 (time1(T),string_concat('../gh2_tmp',T,O2),string_concat(O2,"/",O3),working_directory1(_,Path),mv_lc("./",O3));true),%make_directory_s(O)),

 %rm_lc("../gh2_tmp/*"),
 %trace,
 %mv_lc("./","../gh2_tmp/"),
 %rm_lc("./*"),
 working_directory1(_,A),!.

tmp2gh :- !.


% rm after
luciancicd(At_start,Max,CICD,Start_files,End_files) :-
%trace,
 retractall(c(_)),
 assertz(c(CICD)),
 retractall(fail_if_greater_than_n_changes2(_)),
 assertz(fail_if_greater_than_n_changes2(Max)),
 (At_start=true->
 % overwrites existing tests_c.txt, leaves the new one behind for bug checking
 gh2tmp;true),
 working_directory1(A,A),
 repositories_paths1([Path]),
 working_directory1(_,Path),
 (At_start=true->
 % overwrites existing tests_c.txt, leaves the new one behind for bug checking
 (foldr(string_concat,[%"scp -pr ../../Github_lc/ ",
 "rm -f ../Github_lc/*"
 %Folder1
 ],Command315),
 	catch(bash_command(Command315,_), _, (foldr(string_concat,["Warning."%%"Error: Can't clone ",User3,"/",Repository3," repository on GitHub."
	],_Text42)%,writeln1(Text42)%,abort
 	))
 );true),
 
 findall(_,(member([File_name,Contents],Start_files),
 truncate_path(File_name,P,F2),
 working_directory1(A1,A1),
 (exists_directory(P)->true;
 make_directory_recursive_s("./",P)),
 working_directory1(_,P),
 save_file_s(F2,Contents),
 working_directory1(_,A1) 
 ),_),
 working_directory1(_,A),

 
 luciancicd,

 (success1(0)->
 (
 output_path([O]),
 working_directory1(_,O),
 find_files("./",F),
 
 findall([File_name,Contents1],(member([File_name,Contents],F),
 remove_end_comments2(Contents,Contents1)),F0),
 
 %trace,

 findall([File_name1,Contents],(member([File_name,Contents],End_files),
 (string_concat("./",_,File_name)->File_name1=File_name;
 (%trace,
 string_concat("./",File_name,File_name1)))),End_files1),

 working_directory1(_,A),
 
 (At_start=true->
 % overwrites existing tests_c.txt, leaves the new one behind for bug checking
 tmp2gh;true),
 
 retractall(fail_if_greater_than_n_changes2(_)),

 msort(F0,F1A),
 msort(End_files1,F1B),
 
 writeln1(["Result",F1A]),
 writeln1(["Correct Result",F1B]),
 F1A=F1B
 );(working_directory1(_,A),
 (At_start=true->
 % overwrites existing tests_c.txt, leaves the new one behind for bug checking
 tmp2gh;true),

 fail)),
 
 
 !.
