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
 
findall([T1a,BA],(member([T1,BA],RTests),string_concat(R1,T1a,T1)),R110),


 To=R1,

(sd2(R110,Tests1,RTests,R1,To_m_1,Repository_root_path,Repository,Gitl_data_path1,N,R1,N_path,To,HTML)
 ->HTML1=HTML;
 HTML1="Identical"),

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

	
	string_concat("<b>Diff output</b><br>These are the changes.<br><b>"%Key</b><br><table bgcolor=\"green\"><tr><td>Insertion</td></tr></table><br><table bgcolor=\"red\"><tr><td>Deletion</td></tr></table><br>"
	,HTML1,HTML3),
 save_file_s(File1,HTML3),
 
 working_directory1(_,A1),



 !.