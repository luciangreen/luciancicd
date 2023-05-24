save_diff_html(After3) :-
 correspondences(Corr),
 
 findall(["<table bgcolor=\"",Colour,"\"><tr><td>",Item2,"</td></tr></table>"],(member(Item,After3),
 (string(Item)->
 (numbers_to_term([Item],Corr,[],Item2),Colour="white");
 ((%trace,
 Item=[[i,_],[Item3]])->
 (%trace,
 numbers_to_term([Item3],Corr,[],Item2),Colour="green");
 ((%trace,
 Item=[[d,_],[Item3]])->
 (%trace,
 numbers_to_term([Item3],Corr,[],Item2),Colour="red");
 (Item=[[c,_],Item_a,Item_b]->
 (numbers_to_term(Item_a,Corr,[],Item2a),
 numbers_to_term(Item_b,Corr,[],Item2b),
 %trace,
 %term_to_atom(Item2a,Item2a1),
 %term_to_atom(Item2b,Item2b1),
 %foldr(string_concat,
 term_to_atom([Item2b,' -> ',Item2a],Item2),
 Colour="yellow")))))),HTML),
 flatten(HTML,HTML1),
 foldr(string_concat,HTML1,HTML2),
 %term_to_atom(HTML1,HTML2),
 	get_time(TS),stamp_date_time(TS,date(Year,Month,Day,Hour1,Minute1,Seconda,_A,_TZ,_False),local),
	foldr(string_concat,["../../diff_html_",Year,Month,Day,Hour1,Minute1,Seconda,".html"],File1),
	
	string_concat("<b>File Diff HTML</b><br>These are the changes combinations were taken from.<br>",HTML2,HTML3),
 save_file_s(File1,HTML3).

