% mv luciancicd-testing->luciancicd-testing tmp
% mv gh*->luciancicd-testing
% mv luciancicd-testing tmp->gh*

move_to_repository_or_back :-

 working_directory1(AAA,AAA),

 %retractall(home_dir(_)),assertz(home_dir(AAA)),

 LCTD="../luciancicd-testing/",
 LCTD2="../luciancicd-testing-tmp/",
 repositories_paths(K),
 output_path([O]),
 %rm_lc(O),
 (exists_directory_s(O)->
 (time1(T),string_concat(O1,"/",O),string_concat(O1,T,O2),string_concat(O2,"/",O3),mv_lc(O,O3));make_directory_s(O)),
 
/* omit_paths(Omit),

 (exists_directory_s(LCTD2)->true;make_directory_s(LCTD2)),
 

findall1([K1,G4],(member(K1,K), directory_files(K1,F),
	delete_invisibles_etc(F,G),

%findall1(H,(member(H,G),not(string_concat("dot",_,H)),

subtract(G,Omit,G1),

findall1([G3,G31],(member(G2,G1),foldr(string_concat,[LCTD,K1,G2,"/"],LCTDa),
foldr(string_concat,[LCTD2,K1,G2,"/"],LCTD2a),
foldr(string_concat,[K1,G2,"/"],K1a),
*/
 %mv_lc(LCTDa,LCTD2a),
 %mv_lc(K1a,LCTDa),
 %mv_lc(LCTD2a,K1a)
 mv_lc(LCTD,LCTD2),

findall1(_,(member(K1,K),
 %mv_lc(K1,LCTD),
 mv_lc(LCTD2,O),
 
 string_concat(O,"*/testcicd.pl",R1),
 
 rm_lc(R1)
 
 ),_),
%),_G4)
%findall1(G3,(member(G2,G1),foldr(string_concat,[LCTD2,G2,"/"],G3)),LCTD2_G4),


 %mv_lc(R,LCTD),
 %mv_lc(LCTD2,R)

%not(member(G,Omit))

%),_K01),
 
 rm_lc(LCTD2),
 
 
 working_directory1(_,AAA),!.



 
mv_lc(From,To) :-
 foldr(string_concat,[%"scp -pr ../../Github_lc/ ",
 "scp -pr "
 %"rsync -av --ignore-existing --remove-source-files --exclude=\".*\"  "
 ,From,%"../../Github_lc/ ",
 " ",
 To,%," && \\","\n","rsync -av --delete `mktemp -d`/ ",From
 "\n","rm -rf ",From,"*"],Command314),
 	catch(bash_command1(Command314,_), _, (foldr(string_concat,["Warning."%%"Error: Can't clone ",User3,"/",Repository3," repository on GitHub."
	],Text41),writeln1(Text41),abort
 	)),!. 

rm_lc(Item) :-

foldr(string_concat,[%"scp -pr ../../Github_lc/ ",
 "rm -rf ",Item
 %Folder1
 ],Command315),
 	catch(bash_command1(Command315,_), _, (foldr(string_concat,["Warning."%%"Error: Can't clone ",User3,"/",Repository3," repository on GitHub."
	],_Text42)%,writeln1(Text42)%,abort
 	)),!.
