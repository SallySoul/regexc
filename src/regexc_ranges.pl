:- module(regexc_ranges,
  [
    bound_leq/2,
    bound_geq/2,
    contained_in/2
  ]).

/** <module> regexc_ranges

This module contains predicates meant for dealing with character ranges.

Character ranges are how inputs into the statmachine are stored. A range is:

```
range(Min, Max)
```

where Min and Max are inclusive bounds. They can either be `bound(:integer)`, `bound_min`, or `bound_max`.
*/

%! bound_leq(Lower_Bound, Greater_Bound) is det.
%
bound_leq(bound_min, _).
bound_leq(_, bound_max).
bound_leq(bound_min, bound_min).
bound_leq(bound_max, bound_max).
bound_leq(bound(Lower_Bound), bound(Upper_Bound)) :-
  Lower_Bound =< Upper_Bound.

%! bound_geq(Greater_Bound, Lower_Bound) is det.
%
bound_geq(Greater_Bound, Lower_Bound) :- bound_leq(Lower_Bound, Greater_Bound).

%! contained_in(+Sub_Range, +Super_Range) is non-det.
%
% Note that this assumes that each range's bounds are correctly ordered
contained_in(Sub_Range, Super_Range) :-
  Sub_Range = range(Sub_Min, Sub_Max),
  Super_Range = range(Super_Min, Super_Max),
  bound_geq(Sub_Min, Super_Min),
  bound_leq(Sub_Max, Super_Max).

%!lower_bounds(+Ranges:list, -Lower_Bounds:list)
lower_bounds([], []).
lower_bounds([range(Lower_Bound, _) | Ranges_Tail], [Lower_Bound | Lower_Bounds_Tail]) :-
  lower_bounds(Ranges_Tail, Lower_Bounds_Tail).

%!lower_bounds(+Ranges:list, -Lower_Bounds:list)
upper_bounds([], []).
upper_bounds([range(_, Upper_Bound) | Ranges_Tail], [Upper_Bound | Upper_Bounds_Tail]) :-
  upper_bounds(Ranges_Tail, Upper_Bounds_Tail).

%! partition_ranges(+Ranges:list, -Partions:list)
%
% Take a list of ranges and break it into a non-overlapping partition
/*
parition_ranges(Ranges, Partion) :-
  lower_bounds(Ranges, Lower_Bounds),
  upper_bounds(Ranges, Upper_Bounds),
*/

:- begin_tests(regexc_ranges).

test(bound_leq_correct) :-
  Correct_Pairs = [
    bound_min - X,
    X - bound_max,
    bound(0) - bound(0),
    bound(0) - bound(1),
    bound(10000) - bound(10001),
    bound(10000) - bound_max,
    bound_min - bound(0)
  ],
  forall(member(Lesser-Greater, Correct_Pairs),
    assertion(bound_leq(Lesser, Greater))
  ).

test(bound_leq_incorrect) :-
  Incorrect_Pairs = [
  bound_max - bound_min,
  bound(21) - bound(20)
  ],
  forall(member(Lesser-Greater, Incorrect_Pairs),
    assertion(\+(bound_leq(Lesser, Greater)))
  ).

test(contained_in_correct) :-
  Correct_Pairs = [
    range(bound_min, bound_max) - range(bound_min, bound_max),
    range(bound(3), bound(7)) - range(bound(3), bound(7)),
    range(bound(3), bound(7)) - range(bound(3), bound(8)),
    range(bound(3), bound(7)) - range(bound(2), bound(7)),
    range(bound(3), bound(7)) - range(bound(0), bound(90)),
    range(bound(2), bound(3)) - range(bound_min, bound_max)
  ],
  forall(member(Sub_Range-Super_Range, Correct_Pairs),
    assertion(contained_in(Sub_Range, Super_Range))).

test(contained_in_incorrect) :-
  Incorrect_Pairs = [
    range(bound(0), bound(90)) - range(bound(3), bound(7)),
    range(bound_min, bound_max) - range(bound(2), bound(3))
  ],
  forall(member(Sub_Range-Super_Range, Incorrect_Pairs),
    assertion(\+(contained_in(Sub_Range, Super_Range)))).

:- end_tests(regexc_ranges).
