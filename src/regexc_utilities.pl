:- module(regexc_utilities,
  [
    enumeration/2,
    write_to_file_once/3,
    file_diff/3
  ]).

/** <module> util

This module contains some helpful predicates that can be shared / don't belong anywhere else

@author Sally Soul
@license MIT
*/

%! enumeration(+List:list, +Enumerated_List:list) is semidet.
%
% This relates a list to a list of tuples with the element and their index.
enumeration([], []).
enumeration(Ls, Es) :- enumeration_r(Ls, Es, 0).
enumeration_r([], [], _).
enumeration_r([L|Ls], [(L, C)|Es], C) :- N is C + 1, enumeration_r(Ls, Es, N).

%! write_to_file_once(:Goal, +Path, -Deterministic) is det.
%
% This predicate will open the file at Path for writing and call Goal with that Output Stream.
% The Goal should normally be called like `goal(..., Output_Stream)`.
%
% Note that the goal will only be called once, but we return whether the predicate was
% Deterministic
write_to_file_once(Goal, Path, Deterministic) :-
  setup_call_cleanup(
    open(Path, write, File_Output),
    once((
      call(Goal, File_Output),
      deterministic(Deterministic)
    )),
    close(File_Output)).

%! file_diff(+Path_1, +Path_2, -Diff) is det.
%
% This predicate just shells out to git diff.
% I use it for testing.
% This predicate asserts that both Paths must exist.
file_diff(Path_1, Path_2, Diff) :-
  absolute_file_name(Path_1, Absolute_Path_1),
  absolute_file_name(Path_2, Absolute_Path_2),
  assertion(exists_file(Absolute_Path_1)),
  assertion(exists_file(Absolute_Path_2)),
  process_create(
    "/usr/bin/env",
    ["git", "diff", Absolute_Path_1, Absolute_Path_2],
    [process(Diff_PID), stdout(pipe(Diff_Output))]
  ),
  process_wait(
    Diff_PID,
    Status,
    [timeout(1)]
  ),
  assertion(Status = exit(0)),
  read_string(Diff_Output, _, Diff).

:- begin_tests(regexc_utilities).

test_enumeration(Input, Correct_Output) :-
  enumeration(Input, Output),
  assertion(Output = Correct_Output).

test(enumeration) :-
  Enumerated_Lists = [
    (
      [],
      []
    ),
    (
      ["a"],
      [("a", 0)]
    ),
    (
      [3, 2, 1],
      [(3, 0), (2, 1), (1, 2)]
    ),
    (
      ["a", 0, "b", "c"],
      [("a", 0), (0, 1), ("b", 2), ("c", 3)]
    )
  ],
  forall(member((Input, Correct_Output), Enumerated_Lists),
    assertion(test_enumeration(Input, Correct_Output))
  ).

:- end_tests(regexc_utilities).
