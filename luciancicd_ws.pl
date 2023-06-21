:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).
:- use_module(library(http/html_write)).

% we need this module from the HTTP client library for http_read_data
:- use_module(library(http/http_client)).
:- http_handler('/luciancicd', luciancicd_web_form, []).

%:-include('../SSI/ssi.pl').
%:-include('../SSI/ssi.pl').
%:-include('luciancicd1_pl.pl').

%:- include('files/listprolog.pl').

:-dynamic num1/1.

luciancicd_server(Port) :-
        http_server(http_dispatch, [port(Port)]).

	/*
	browse http://127.0.0.1:8000/
	This demonstrates handling POST requests
	   */

	   luciancicd_web_form(_Request) :-
retractall(html_api_maker_or_terminal(_)),
assertz(html_api_maker_or_terminal(html
 %terminal
 )),
 
 retractall(num1(_)),assertz(num1(1)),
			
			
			working_directory(A3,A3),
			working_directory(_,'../../lc_logs/'),
																										              format('Content-type: text/html~n~n', []),
																			
data(Header,Footer),

format(Header,[]),

writeln("<h1 id=\"#Top\">Log</h1><br><br>"),
directory_files("./",F),
	delete_invisibles_etc(F,G),
	
	findall([N,H,H1,F1],(member(H,G),open_string_file_s(H,F11),atomic_list_concat(A,"\n",F11),atomic_list_concat(A,"<br>",F1),
	get_num(N),foldr(string_concat,["<a href=\"#",N,"\">",
	H,"</a>",
	"<br><br>"],H1)%,writeln(H1)
	),J),
	
	findall(_,(member([_,_,H1,_],J),writeln(H1)),_),
	
	findall(_,(member([N,H,_,F1],J),foldr(string_concat,["<h2 id=\"",N,"\">",H,"</h2><a href=\"#Top\">Top</a><br>",F1,"<br><br>"],H2),writeln(H2)),_),
%Debug=off,

	%test_open_types_cases(4,Query,Types,Modes,Functions),

%international_lucianpl([lang,"en"],Debug,Query,Types,Modes,Functions,_Result),
%p2lpconverter([file,"../private/la_com_ssi1.pl"],List3),

%testopen_cases(8,[[n,test]],List3),
	%test(1,Query,Functions,Result),

% Form and HTML Table
%test1(Functions),	
%Query=[[n,test]],
	%luciancicd_test(List3),
	%para(List3),
	%international_lucianpl([lang,"en"],Debug,[[n,luciancicd]],List3,_Result1),
			working_directory(_,A3),


format(Footer,[])

																								      .

						%term_to_atom(Debug2,'off'),
%term_to_atom(Query2,Query1),
%term_to_atom(Functions2,Functions1),

%international_interpret([lang,"en"],Debug2,Query2,Functions2,Result),
																														%%format('</p><p>========~n', []),
																															%%portray_clause
																															%portray_clause(result),
																																																															%%writeln1(Data),

%format('</p>').


data(Header,Footer) :-

Header='<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 3.2//EN">
<html lang="en">
  <head>
    <meta http-equiv="content-type" content="text/html; charset=UTF-8">
    <meta charset="utf-8">
    <title>State Saving Interpreter</title>
    <style type="text/css"> 
<!-- 

A:link {text-decoration: none;} 
A:visited {text-decoration: none;} 
A:hover {text-decoration: underline;} 

img {
 height: auto;
 max-width: 100%;
 object-fit: contain;
} 

table {table-layout: fixed; width: 100%;}

td {word-wrap: break-word;}

--> 
  </style>
    <meta name="viewport" content="width=device-width, initial-scale=1">

  </head>
  <body style="background-color: rgb(255, 239, 227);">

   
    <div style="text-align: center;">
      <table width="80%">
        <tbody>
          <tr>
            <td>
              <p>',

Footer='</p>
            </td>
          </tr>
        </tbody>
      </table>
      <br>

    <br>
  </body>
</html>'.

get_num(A) :- num1(A),retractall(num1(_)),A1 is A+1,assertz(num1(A1)).