% mv luciancicd-testing->luciancicd-testing tmp
% mv gh*->luciancicd-testing
% mv luciancicd-testing tmp->gh*

move_to_repository_or_back :-

 working_directory1(AAA,AAA),

 %retractall(home_dir(_)),assertz(home_dir(AAA)),

 LCTD="../private2/luciancicd-testing/",
 LCTD2="../private2/luciancicd-testing-tmp/",
 repositories_paths1([R|_]),

 (exists_directory_s(LCTD2)->true;make_directory_s(LCTD2)),
 
 mv_lc(LCTD,LCTD2),
 mv_lc(R,LCTD),
 mv_lc(LCTD2,R),
 
 rm_lc(LCTD2),
 
 working_directory1(_,AAA),!.



 
mv_lc(From,To) :-

 foldr(string_concat,[%"scp -pr ../../Github_lc/ ",
 "rsync -av --exclude=\".*\"  ",From,%"../../Github_lc/ ",
 " ",
 To],Command314),
 	catch(bash_command(Command314,_), _, (foldr(string_concat,["Warning."%%"Error: Can't clone ",User3,"/",Repository3," repository on GitHub."
	],Text41),writeln1(Text41),abort
 	)),!. 

rm_lc(Item) :-

foldr(string_concat,[%"scp -pr ../../Github_lc/ ",
 "rm -rf ",Item
 %Folder1
 ],Command315),
 	catch(bash_command(Command315,_), _, (foldr(string_concat,["Warning."%%"Error: Can't clone ",User3,"/",Repository3," repository on GitHub."
	],_Text42)%,writeln1(Text42)%,abort
 	)),!.
