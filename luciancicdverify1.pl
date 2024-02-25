gh_init2 :-

 (exists_file_s("luciancicd.pl")->true;(writeln("Error: Please quit Prolog, reload and run luciancicd."),abort)),

working_directory1(A1000,A1000),
 (time1(_T1)->true;get_time1),
	
	(home_dir1(HD)->true;true),

(var(HD)->(retractall(home_dir1(_)),assertz(home_dir1(A1000)));true),

repositories_paths([RP_1]),

 (exists_directory_s(RP_1)->true;make_directory_s(RP_1)),

working_directory1(_,A1000),

output_path([OP_1]),

 (exists_directory_s(OP_1)->true;make_directory_s(OP_1)),

(exists_directory_s("../private2/")->true;make_directory_recursive_s("./","../private2")),


working_directory1(_,A1000).

gh_init(At_start) :-
working_directory1(A1000,A1000),

	gh_init2,
output_path([OP_1]),

 (exists_directory_s("../private2/luciancicd-cicd-tests")->true;make_directory_recursive_s("./","../private2/luciancicd-cicd-tests")),

working_directory1(_,A1000),

 working_directory1(A,A),
 repositories_paths1([Path]),
 working_directory1(_,Path),

 (exists_directory('../gh2_tmp2')->
 (time1(T),string_concat('../gh2_tmp2',T,O2),string_concat(O2,"/",O3),%working_directory1(_,Path),
 
 O4=O3);(make_directory('../gh2_tmp2'),O4="../gh2_tmp2/")),%make_directory_s(O)),
 %string_concat("../../",Path2,Path),
 %string_concat(Path3,"/",Path2),
 string_concat("../../",OP_2,OP_1),
 string_concat(OP_3,"/",OP_2),
 foldr(string_concat,[O4,%"/",
 OP_2],PX11),
 foldr(string_concat,[O4,%"/",
 "Github_lc/"],PX21),
 %mv_lc(PX,O4),
 
 
 (exists_directory(PX11)->true;
 (%trace,
 make_directory_recursive_s("./",PX11))),
 (exists_directory(PX21)->true;
 (%trace,
 make_directory_recursive_s("./",PX21))),
  
  %trace,
 foldr(string_concat,["scp -r ../",OP_3,"/ ",O4,"",OP_3,"/."],PX1),
 foldr(string_concat,["scp -r ","../Github_lc/"," ",O4,"","Github_lc/","."],PX2),
 %mv_lc(PX,O4),
 
 	catch(bash_command(PX1,_), _, (foldr(string_concat,["Warning."%%"Error: Can't clone ",User3,"/",Repository3," repository on GitHub."
	],_Text42)%,writeln1(Text42)%,abort
 	)),

 	catch(bash_command(PX2,_), _, (foldr(string_concat,["Warning."%%"Error: Can't clone ",User3,"/",Repository3," repository on GitHub."
	],_Text42)%,writeln1(Text42)%,abort
 	)),
 
foldr(string_concat,[%"scp -pr ../../Github_lc/ ",
 "rm -rf ../",OP_2,"*"
 %Folder1
 ],Command315),
 	catch(bash_command(Command315,_), _, (foldr(string_concat,["Warning."%%"Error: Can't clone ",User3,"/",Repository3," repository on GitHub."
	],_Text42)%,writeln1(Text42)%,abort
 	)),


 (At_start=true->
 (foldr(string_concat,[%"scp -pr ../../Github_lc/ ",
 "rm -rf ../Github_lc/*"
 %Folder1
 ],Command316),
 	catch(bash_command(Command316,_), _, (foldr(string_concat,["Warning."%%"Error: Can't clone ",User3,"/",Repository3," repository on GitHub."
	],_Text42)%,writeln1(Text42)%,abort
 	)));true),
 working_directory1(_,A),
 	!.
 
gh2tmp :- 
%trace,
 working_directory1(A,A),
 (time1(_T1)->true;get_time1),
 repositories_paths1([Path]),
 %trace,
 working_directory1(_,Path),
 %trace,
 %pwd,
%(exists_directory('../gh2_tmp')->true;make_directory('../gh2_tmp')),
 (exists_directory('../gh2_tmp')->
 %trace,
 (time1(T),string_concat('../gh2_tmp',T,O2),string_concat(O2,"/",O3),%working_directory1(_,Path),
 O4=O3,make_directory(O4));(%trace,
 O4="../gh2_tmp/"),make_directory_s(O4)),%(O)),
 %trace,
 foldr(string_concat, ["scp -pr ./ ",O4,"."],O41),
 
 catch(bash_command(O41, O42), _, (foldr(string_concat, ["Warning."], _), writeln1("Couldn't back up repositories."), abort)),

%trace,

 string_concat("../",Path1,Path),
 string_concat("../",Path2,Path1),
 working_directory1(_,"../"),

 foldr(string_concat, ["rm -rf ",Path2],O43),
 
 catch(bash_command(O43, O44), _, (foldr(string_concat, ["Warning."], _), writeln1("Couldn't back up repositories."), abort)),
%trace,
%pwd,
 foldr(string_concat, ["mkdir ",Path2],O45),
 
 catch(bash_command(O45, O46), _, (foldr(string_concat, ["Warning."], _), writeln1("Couldn't back up repositories."), abort)),
 %mv_lc("./",O4),
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
