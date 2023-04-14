lppm_install_luciancicd(LPPM_registry_term1,User1,Repository1) :-
	%%lppm_get_manifest(User1,Repository1,_Description,Dependencies1),
	%lppm_get_registry(LPPM_registry_term1),
	member([User1,Repository1,_Description1,Dependencies1],LPPM_registry_term1),
	(%%repeat,
	concat_list(["Please enter path to install ",User1,"/",Repository1," to: (e.g. ../ to install at the same level as List Prolog Package Manager)."],_Text2),
	%writeln1(Text2),%read_string(user_input, "\n", "\r", _, Path1),
	
	Path1="../private2/luciancicd-testing/",
	%(working_directory(_,Path1)->true;(concat_list(["Warning: ",Path1," doesn't exist."],Text3),writeln1(Text3),fail))),
	
	%catch((true, call_with_time_limit(1,
		find_all_dependencies(LPPM_registry_term1,%[[User1,Repository1]],%%,Description,Dependencies1
	Dependencies1,[],Dependencies1a)
		,
		%)),
 %         time_limit_exceeded,
  %        (concat_list(["Error: Cycle in lppm_registry.txt: ",Dependencies1],Note_a),writeln(Note_a),abort)),
  
	append([[User1,Repository1%%,Description,Dependencies1
	]],Dependencies1a,Dependencies2),

  sort(Dependencies2,Dependencies2b),
	
	
	%trace,
	%writeln(Dependencies2b),
	findall(_,(member(Dependency2,Dependencies2b),Dependency2=[_User3,Repository3],
	%concat_list(["git clone https://github.com/",User3,"/",Repository3,".git"],Command3),
	%trace,
	repositories_paths([K]),
	concat_list(["scp -pr ",K,Repository3," ",Path1,Repository3],Command3),
 	catch((bash_command(Command3,_),!), _, (concat_list(["Warning."%%"Error: Can't clone ",User3,"/",Repository3," repository on GitHub."
 	],Text4),writeln1(Text4)%%,abort
 	))),_)),!.