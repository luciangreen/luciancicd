% main_v2.pl
%
% Main entry point for Lucian CI/CD v2 (Prolog Implementation)
% ===========================================================
%
% This file provides the main entry point for the enhanced Prolog-based
% CI/CD system with repository versioning and change combination features.
%

:- include('luciancicd_v2.pl').

% Main entry point - runs the full CI/CD v2 cycle
main :- 
    write('🚀 Lucian CI/CD v2 - Starting...'), nl,
    validate_v2_config,
    luciancicd_v2.

% Alternative main for testing
main_test :-
    write('🧪 Lucian CI/CD v2 - Testing Mode...'), nl,
    test_luciancicd_v2.

% Main for setup only
main_setup :-
    write('🔧 Lucian CI/CD v2 - Setup Mode...'), nl,
    set_up_luciancicd_v2.

% Main for status check
main_status :-
    write('📋 Lucian CI/CD v2 - Status Check...'), nl,
    luciancicd_v2_status.