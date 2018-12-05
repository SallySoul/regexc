:- module(statemachine,
  [
    ast_nfa/2
  ]).

:- use_module(library(nb_set)).

/** <module> statemachine

For the purposes of this module, a finite automaton is (Q, T, I, F) where:

Q: A finite set of states, {s_i}
T: A finite set of input transitions, {s_i X i -> s_j}
E: A finite set of epislon transitions, {s_i -> s_j}
I: An initial state
F: A set of accepting states

We assume that all finite Automotan here share the same set in input symbols, bytes.
For the purposes of specifying input in transitions we have three options.

byte(Byte),
range(Min, Max),
any.

Also note that a finite automaton is non-determinisitic unless E = []. 

@author Sally Soul
@license MIT
*/

ast_nfa(Root_Node, NFA) :-
  % We use non-backtracking sets to construct the NFA
  empty_nb_set(NFA_States),
  empty_nb_set(NFA_Transitions),
  empty_nb_set(NFA_Empty_Transitions),
  empty_nb_set(NFA_Final_States),

  % We only need the states and transitions for the construction though
  Partial_NFA = (NFA_States, NFA_Transitions, NFA_Empty_Transitions),

  % We use ast_nfa_r to recursivley build the NFA from the AST
  % Each recurrence returns an nfa (a sub nfa, if you will) that 
  % is composed of a subset of the partial NFA. Each sub-NFA can 
  % be refered to by a starting state, and an ending state
  Sub_NFA = (Start_State, Final_State),

  % Start our recursive construction
  ast_nfa_r(Root_Node, Partial_NFA, 0, _, Sub_NFA),

  % Create the final states set, and we have our finished NFA
  add_nb_set(Final_State, NFA_Final_States),
  NFA = (NFA_States, NFA_Transitions, NFA_Empty_Transitions, Start_State, NFA_Final_States).


%
% The following are ast_char_r definitions for each ast node type
% I attempt to document the goal including a ASCII diagram of the 
% resulting state machine. For example: 
% A -- x --> B
% This reads, from state A, transition to State B on input x

%
% ast_char(X)
% state(N) -- X --> state(N+1)
%
ast_nfa_r(
  ast_char(X),
  Partial_NFA,
  Current_Index,
  Next_Index,
  (State_Start, State_Final)
) :-
  Partial_NFA = (NFA_States, NFA_Transitions, _),

  State_Start = state(Current_Index),
  Next_Index_1 is Current_Index + 1,
  State_Final = state(Next_Index_1),

  add_nb_set(State_Start, NFA_States),
  add_nb_set(State_Final, NFA_States),
  add_nb_set((State_Start, X, State_Final), NFA_Transitions ),

  Next_Index is Current_Index + 1.

%
% ast_wildcard
% state(N) -- wildcard --> state(N+1)
%
ast_nfa_r(
  ast_wildcard,
  Partial_NFA,
  Current_Index,
  Next_Index,
  (State_Start, State_Final)
) :-
  Partial_NFA = (NFA_States, NFA_Transitions, _),

  State_Start = state(Current_Index),
  Next_Index_1 is Current_Index + 1,
  State_Final = state(Next_Index_1),

  add_nb_set(State_Start, NFA_States),
  add_nb_set(State_Final, NFA_States),
  add_nb_set((State_Start, wildcard, State_Final), NFA_Transitions ),

  Next_Index is Current_Index + 1.


/*
%
% ast_occurance(Ast_Node, none, none)
% state
%
ast_nfa_r(
  ast_wildcard,
  Partial_NFA,
  Current_Index,
  Next_Index,
  (State_Start, State_Final)
) :-
  Partial_NFA = (NFA_States, NFA_Transitions, _),

  State_Start = state(Current_Index),
  Next_Index_1 is Current_Index + 1,
  State_Final = state(Next_Index_1),

  add_nb_set(State_Start, NFA_States),
  add_nb_set(State_Final, NFA_States),
  add_nb_set((State_Start, wildcard, State_Final), NFA_Transitions ),

  Next_Index is Current_Index + 1.

*/
state_to_dot(Stream, state(N)) :-
  format(Stream, "\t~w;~n", N).

states_to_dot(Stream, NFA_States) :-
  nb_set_to_list(NFA_States, States),
  maplist(state_to_dot(Stream), States).

transition_to_dot(Stream, (state(Start_State), Input, state(Final_State))) :-
  format(Stream, "\t~w -> ~w [label=\"~w\"];~n", [Start_State, Final_State, Input]).

transitions_to_dot(Stream, NFA_Transitions) :-
  nb_set_to_list(NFA_Transitions, Transitions),
  maplist(transition_to_dot(Stream), Transitions).

empty_transition_to_dot(Stream, (state(Start_State), state(Final_State))) :-
  format(Stream, "\t~w -> ~w [label=\"ε\"];~n", [Start_State, Final_State]).

empty_transitions_to_dot(Stream, NFA_Transitions) :-
  nb_set_to_list(NFA_Transitions, Transitions),
  maplist(transition_to_dot(Stream), Transitions).

final_state_to_dot(Stream, state(N)) :-
  format(Stream, "\t~w [shape=doublecircle];~n", [N]).

final_states_to_dot(Stream, NFA_Final_States) :-
  nb_set_to_list(NFA_Final_States, Final_States),
  maplist(final_state_to_dot(Stream), Final_States).

start_state_to_dot(Stream, state(N)) :-
  format(Stream, "\t~w [shape=box];~n", [N]).

nfa_to_dot(Stream, NFA) :-
  NFA = (NFA_States, NFA_Transitions, NFA_Empty_Transitions, Start_State, NFA_Final_States),
  writeln(Stream, "digraph NFA {"),
  states_to_dot(Stream, NFA_States),
  transitions_to_dot(Stream, NFA_Transitions),
  empty_transitions_to_dot(Stream, NFA_Empty_Transitions),
  final_states_to_dot(Stream, NFA_Final_States),
  start_state_to_dot(Stream, Start_State),
  writeln(Stream, "}").
