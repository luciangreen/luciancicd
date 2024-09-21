luciancicd_ws1 :-
%trace,
home_dir1(A),
working_directory1(_,A),

repositories_paths([RP]),

output_path([OP]),

foldr(string_concat,[RP],To_m_1),
foldr(string_concat,[OP],R1),


 find_files(To_m_1,Tests1),
 %);Tests1=[]),


 find_files(R1,RTests),

 working_directory1(_,A),
 
findall1([T1a,BA1],(member([T1,BA],RTests),remove_end_comments2(BA,BA1),string_concat(R1,T1a,T1)),R110),


 To=R1,

term_to_atom(R110,R1101),
term_to_atom(Tests1,Tests11),
term_to_atom(RTests,RTests1),
term_to_atom(R1,R11),
term_to_atom(To_m_1,To_m_11),
%term_to_atom(Repository_root_path,Repository_root_path1),
%term_to_atom(Repository,Repository1),
%term_to_atom(Gitl_data_path1,Gitl_data_path11),
%term_to_atom(N,N1),
%term_to_atom(R1,R1A),
%term_to_atom(N_path,N_path1),
term_to_atom(To,To1),

%trace,

foldr(string_concat,["#!/usr/bin/swipl -g main -q\n\n",":-include('../gitl/gitl.pl').\n","handle_error(_Err):-\n  halt(1).\n","main :-\n    catch((sd2(",R1101,",",Tests11,",",RTests1,",",R11,",",To_m_11,",_,_,_,_,_,_,",To1,",HTML),term_to_atom(HTML,HTML1), write(HTML1)),Err, handle_error(Err)), nl,\n    halt.\n","main :- halt(1).\n"],String),

foldr(string_concat,[%"../private2/luciancicd-testing/",Repository1b,"/",Go_path5,
"tmp.pl"],GP),
%string_concat(Go_path,"testcicd.pl",GP),
open_s(GP,write,S1),
write(S1,String),close(S1),
sleep1(2),
foldr(string_concat,["chmod +x ",GP,"\n","swipl -g main -q ./",GP],S3),%,

((catch(bash_command(S3,HTML), _, (foldr(string_concat,["Warning."%%"Error: Can't clone ",User3,"/",Repository3," repository on GitHub."
	],_Text4),%writeln1(Text4),
	fail%abort
 	)),term_to_atom(HTML2,HTML))%sd2(R110,Tests1,RTests,R1,To_m_1,Repository_root_path,Repository,Gitl_data_path1,N,R1,N_path,To,HTML)
 ->HTML1=HTML2;
 HTML1="Identical"),
delete_tmp,
 working_directory1(_,A),

 	time1(Time),
 	%diff_html_n(Diff_html_n),

working_directory1(A1,A1),

%repositories_paths([RP]),

working_directory1(_,RP),

	(exists_directory_s("../lc_logs/")->true;make_directory_s("../lc_logs/")),
	 
foldr(string_concat,["../lc_logs/diff_html",Time,%"-",Diff_html_n,
".html"],File1),

	%Diff_html_n1 is Diff_html_n+1,
	%retractall(diff_html_n(_)),
	%assertz(diff_html_n(Diff_html_n1)),

	
	string_concat("<b>Diff output</b><br>These are the changes.<br><br>"%Key</b><br><table bgcolor=\"green\"><tr><td>Insertion</td></tr></table><br><table bgcolor=\"red\"><tr><td>Deletion</td></tr></table><br>"
	,HTML1,HTML3),
 save_file_s(File1,HTML3),
 
 working_directory1(_,A1),



 !.