%abc,ab to aabbc

/*
merge3([],_,C,C).
merge3([D|A],B,C1,C2) :-
 subtract(B,[D],E),
 %subtract(E,B,F),
 append(C1,E,C3),
 merge3(A,E,C3,C2).
*/

/*
:- use_module(library(pairs)).

sort_atoms_by_n(Atoms, ByOrder) :-
        %map_list_to_pairs(nth, Atoms, Pairs),
        keysort(Atoms%Pairs
        , Sorted),
        pairs_values(Sorted, ByOrder).
*/

/*
concatenate([], L, L).
concatenate([X|L1], L2, [X|L3]) :-
    concatenate(L1, L2, L3).

transform_lists(List1, List2, Result) :-
    concatenate(List1, List2, Result).
*/

:-dynamic(merge31/1).

% merge3([a,a,b,a],[a,b,c],A).
% A = [a, a, a, b, b, a, c].

merge3(A,B,Q) :-
 retractall(merge31(_)),
 assertz(merge31(A)),
 findall([[Y],Z],(member(Y,B),Y=[_,[N|_]|_],Y1=[_,[N|_]|_],findall(Y1,(merge31(A1),member(Y1,A1),delete(A1,Y1,A2),retractall(merge31(_)),
 assertz(merge31(A2))),Z)),Q1),
 foldr(append,Q1,Q2),
 foldr(append,Q2,Q3),
 merge31(R),foldr(append,[Q3,R],Q),!.

merge_files1a(A,B,C) :-
 split_into_lp_files(A,AT2331),
 split_into_lp_files(B,AT1331),
 merge_files2b(AT2331,AT1331,[],AT333),
 foldr(append,AT333,C),!.

merge_files2b([],B,C,D) :- append(C,B,D),!.
merge_files2b(A,B,C,D) :-
  A=[[[[n, comment], [["File delimiter", PZ, FZ]]]|T10]|E],
  (member([[[n, comment], [["File delimiter", PZ, FZ]]]|T11],B)->
 (merge3(T10,T11,T12),
 %merge_files3(T10,T11,T12),
 delete(B,[[[n, comment], [["File delimiter", PZ, FZ]]]|T11],B1));
 (T12=T10,B=B1)),
 append(C,[[[[n, comment], [["File delimiter", PZ, FZ]]]|T12]],A2),
 %*/
 merge_files2b(E,B1,A2,D),!.
 

