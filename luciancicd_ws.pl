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
			
			working_directory(_,'../../lc_logs/'),
																										              format('Content-type: text/html~n~n', []),
																			
data(Header,Footer),

format(Header,[]),

writeln("<b>Log</b><br><br>"),
directory_files("./",F),
	delete_invisibles_etc(F,G),
	
	findall(_,(member(H,G),foldr(string_concat,[%"<a href=\"",H,"\">",
	H,%"</a>"
	"<br><br>"],H1),writeln(H1)),_),
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
