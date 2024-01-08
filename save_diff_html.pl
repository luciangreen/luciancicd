save_diff_html(After3) :-
 correspondences(Corr),
 %trace,
 findall(["<table bgcolor=\"",Colour,"\"><tr><td>",Change,Item2,"</td></tr></table>"],(member(Item%[[n,comment],[Item]]
 ,After3),
 (string(Item)->
 (numbers_to_term([Item],Corr,[],Item2),Colour="white",Change="");
 ((%trace,
 (Item=[I,Item3],(I=i->true;I=p)))->
 (%trace,
 numbers_to_term([Item3],Corr,[],Item2),Colour="green",Change="Insertion: ");
 ((%trace,
 Item=[d,Item3])->
 (%trace,
 numbers_to_term([Item3],Corr,[],Item2),Colour="red",Change="Deletion: ")
 /*;
 (Item=[c,Item_a,Item_b]->
 (numbers_to_term(Item_a,Corr,[],Item2a),
 numbers_to_term(Item_b,Corr,[],Item2b),
 %trace,
 %term_to_atom(Item2a,Item2a1),
 %term_to_atom(Item2b,Item2b1),
 %foldr(string_concat,
 term_to_atom([Item2b,' -> ',Item2a],Item2),
 Colour="yellow",Change="Change: "))
 */
 )))),HTML),
 flatten(HTML,HTML1),
 foldr(string_concat,HTML1,HTML2),
 %term_to_atom(HTML1,HTML2),

 	time1(Time),
 	diff_html_n(Diff_html_n),
	(exists_directory_s("../../lc_logs/")->true;make_directory_s("../../lc_logs/")),
	 
foldr(string_concat,["../../lc_logs/diff_html",Time,"-",Diff_html_n,".html"],File1),

	Diff_html_n1 is Diff_html_n+1,
	retractall(diff_html_n(_)),
	assertz(diff_html_n(Diff_html_n1)),

	
	string_concat("<b>Diff output</b><br>These are the changes combinations were taken from.<br><b>Key</b><br><table bgcolor=\"green\"><tr><td>Insertion</td></tr></table><br><table bgcolor=\"red\"><tr><td>Deletion</td></tr></table><br>",HTML2,HTML3),
 save_file_s(File1,HTML3).

