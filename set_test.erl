-module(set_test).
-include_lib("eunit/include/eunit.hrl").

-define(SET_MODULE, set).
-record(io, {i, o}).

-define(BIND_IO(F), #io{i = Value, o = Expected} = F()).
-define(BIND_IO(S1, F), #io{i = S1} = F()).
-define(BIND_IO(S1, S2, F), #io{i = S1, o = S2} = F()).
-define(BIND_IO2(F), {#io{i = Set1, o = List1}, #io{i = Set2, o = List2}} = F()).
-define(BIND_IO2(S1, S2, F), {#io{i = S1}, #io{i = S2}} = F()).
-define(BIND_IO2(S1, S2, L1, L2, F), {#io{i = S1, o = L1}, #io{i = S2, o = L2}} = F()).

-define(NIL, nil).

-define(LIST, ?SET_MODULE:toList).
-define(TOSET, ?SET_MODULE:toSet).
-define(INSERT(S, E), ?SET_MODULE:insert(S, E)).
-define(DELETE(S, E), ?SET_MODULE:delete(S, E)).
-define(PREC(S, E), ?SET_MODULE:prec(S, E)).
-define(SUCC(S, E), ?SET_MODULE:succ(S, E)).
-define(UNION(S1, S2), ?SET_MODULE:union(S1, S2)).
-define(DIFF(S1, S2), ?SET_MODULE:diff(S1, S2)).
-define(EQUALS(S1, S2), ?SET_MODULE:equals(S1, S2)).
-define(MAX(S), ?SET_MODULE:max(S)).
-define(MIN(S), ?SET_MODULE:min(S)).
-define(MAP(S, F), ?SET_MODULE:map(S, F)).
-define(FOLDL(S, F, A0), ?SET_MODULE:foldl(S, F, A0)).
-define(FILTER(S, F), ?SET_MODULE:filter(S, F)).
-define(CARD(S), ?SET_MODULE:card(S)).
-define(ISIN(S, E), ?SET_MODULE:isin(S, E)).
-define(ALL(S, F), ?SET_MODULE:all(S, F)).
-define(ANY(S, F), ?SET_MODULE:any(S, F)).
-define(PRODUCT(S1, S2, F), ?SET_MODULE:product(S1, S2, F)).

%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%

% We expect the sets from different fixtures to be different
% We expect the fixtures to be constant -> return sets with the same elements each time the fixture is evaluated

emptySet() ->
  ?SET_MODULE:newSet().

nonEmptySet() ->
  #io{i = ?TOSET([-1, 1, 2, 3, 5, 7]), o = [-1, 1, 2, 3, 5, 7]}.

nonEmptySet1() ->
  #io{i = ?TOSET([-1, 4, 5, 9, 15, 42, 31231]), o = [-1, 4, 5, 9, 15, 42, 31231]}.

nonEmptySet2() ->
  #io{i = ?TOSET([-17, -16, 0, 5, 42, 9831, 777998]), o = [-17, -16, 0, 5, 42, 9831, 777998]}.

distinctNonEmptySets() ->
  {
    #io{i = ?TOSET([-17, -16, 0, 5, 42, 9832, 77799]), o = [-17, -16, 0, 5, 42, 9832, 77799]},
    #io{i = ?TOSET([-20, -5, 1, 3, 44, 8908, 11111111]), o = [-20, -5, 1, 3, 44, 8908, 11111111]}
  }.

setAndNonEmptySubset() ->
  {
    #io{i = ?TOSET([-17, -16, -5, 0, 3, 5, 42, 8908, 9831, 777998, 11111111]), o = [-17, -16, -5, 0, 3, 5, 42, 8908, 9831, 777998, 11111111]},
    #io{i = ?TOSET([-5, 3, 8908]), o = [-5, 3, 8908]}
  }.


emptyList() ->
  [].

orderedUniqueList() ->
  #io{i = [-1, 1, 2, 3, 5, 7], o = [-1, 1, 2, 3, 5, 7]}.

orderedUniqueListDesc() ->
  #io{i = [7, 5, 2, 3, 1, -1], o = [-1, 1, 2, 3, 5, 7]}.

unorderedUniqueList() ->
  #io{i = [3, 5, 2, -1, 7, 1], o = [-1, 1, 2, 3, 5, 7]}.

unorderedList() ->
  #io{i = [3, 5, 3, 3, 7, 1, 7, 3, -1, 7, 2, 7, 7], o = [-1, 1, 2, 3, 5, 7]}.

uglyList() ->
  #io{i = [a, 1, b, 5, 5, -1, 2, {abc}, [1, 2, 3, 4], 7, -1, 3], o = [-1, 1, 2, 3, 5, 7]}.

fun1Identity(X) ->
  X.

fun1PreserveOrder(X) ->
  X + 10.

fun1ReverseOrder(X) ->
  -X.

fun1RandomizeOrder(X) ->
  erlang:phash2(X).

fun2Minus(A, B) ->
  A - B.

pred1_1(X) ->
  X rem 2 == 0.

pred1_2(X) ->
  X < 17.

pred1_3(_X) ->
  false.

pred1_4(_X) ->
  true.

fun2_1(A, B) ->
  A + B.

fun2_2(A, _B) ->
  A.

fun2_3(_A, _B) ->
  1.

fun2_4(_A, _B) ->
  a.

%%%%%%%%%%%%%%
%%% newSet %%%
%%%%%%%%%%%%%%

newSet_test_() ->
  {inparallel, [newSet_two_empty_sets(), newSet_empty_set_is_empty()]}.

newSet_two_empty_sets() ->
  Set1 = ?SET_MODULE:newSet(),
  Set2 = ?SET_MODULE:newSet(),
  {"An empty set can be created and is equals to a different empty set", ?_assertEqual(Set1, Set2)}.

newSet_empty_set_is_empty() ->
  Set = emptySet(),
  {"An empty set is really empty", ?_assertEqual([], ?LIST(Set))}.

%%%%%%%%%%%%%
%%% toSet %%%
%%%%%%%%%%%%%

toSet_test_() ->
  {inparallel, [toSet_empty(), toSet_ordered_unique(), toSet_ordered_unique_desc(), toSet_unordered_unique(), toSet_unordered(), test_toSet_ugly()]}.

toSet_empty() ->
  List = emptyList(),
  {"Creating set from an empty list creates an empty set", ?_assertEqual([], ?LIST(?TOSET(List)))}.

toSet_ordered_unique() ->
  ?BIND_IO(fun orderedUniqueList/0),
  {"Creating set from a list of ordered unique integers will create a set of these integers", ?_assertEqual(Expected, ?LIST(?TOSET(Value)))}.

toSet_ordered_unique_desc() ->
  ?BIND_IO(fun orderedUniqueListDesc/0),
  {"Creating set from a list of descending ordered unique integers creates an ascending ordered set of these integers", ?_assertEqual(Expected, ?LIST(?TOSET(Value)))}.

toSet_unordered_unique() ->
  ?BIND_IO(fun unorderedUniqueList/0),
  {"Creating set from an unordered list of unique integers creates an ascending ordered set of these integers", ?_assertEqual(Expected, ?LIST(?TOSET(Value)))}.

toSet_unordered() ->
  ?BIND_IO(fun unorderedList/0),
  {"Creating set from an unordered list of duplicate integers creates an ascending ordered set of unique integers", ?_assertEqual(Expected, ?LIST(?TOSET(Value)))}.

test_toSet_ugly() ->
  ?BIND_IO(fun uglyList/0),
  {"Creating set from a list created an ascending ordered set of unique integers", ?_assertEqual(Expected, ?LIST(?TOSET(Value)))}.

%%%%%%%%%%%%%%
%%% insert %%%
%%%%%%%%%%%%%%

insert_test_() ->
  {inparallel, [insert_into_empty(), insert_invalid_element(), insert_valid_element_in_nonempty_set()]}.

insert_into_empty() ->
  Set = emptySet(),
  {"Inserting integer into an empty list should return a set with this element", ?_assertEqual([42], ?LIST(?INSERT(Set, 42)))}.

insert_invalid_element() ->
  EmptySet = emptySet(),
  ?BIND_IO(Set, fun nonEmptySet/0),
  {"Inserting invalid element into set should result in error", [
    ?_assertError(function_clause, ?LIST(?INSERT(EmptySet, a))),
    ?_assertError(function_clause, ?LIST(?INSERT(EmptySet, 1.2))),
    ?_assertError(function_clause, ?LIST(?INSERT(EmptySet, "ada"))),
    ?_assertError(function_clause, ?LIST(?INSERT(EmptySet, {1}))),
    ?_assertError(function_clause, ?LIST(?INSERT(Set, a))),
    ?_assertError(function_clause, ?LIST(?INSERT(Set, 1.2))),
    ?_assertError(function_clause, ?LIST(?INSERT(Set, "ada"))),
    ?_assertError(function_clause, ?LIST(?INSERT(Set, {1})))
  ]}.

insert_valid_element_in_nonempty_set() ->
  ?BIND_IO(fun nonEmptySet/0),
  {"Inserting valid element into non empty set should return the previous set with the new element inserted ignoring duplicates", [
    ?_assertEqual(ensureElementInOrderedList(Expected, 4), ?LIST(?INSERT(Value, 4))),
    ?_assertEqual(ensureElementInOrderedList(Expected, 42), ?LIST(?INSERT(Value, 42))),
    ?_assertEqual(ensureElementInOrderedList(Expected, 8309280931), ?LIST(?INSERT(Value, 8309280931))),
    ?_assertEqual(ensureElementInOrderedList(Expected, 1), ?LIST(?INSERT(Value, 1))),
    ?_assertEqual(ensureElementInOrderedList(Expected, -7312313120931), ?LIST(?INSERT(Value, -7312313120931))),
    ?_assertEqual(ensureElementInOrderedList(Expected, -7312313120931), ?LIST(?INSERT(?INSERT(Value, -7312313120931), -7312313120931)))
  ]}.

%%%%%%%%%%%%%%
%%% delete %%%
%%%%%%%%%%%%%%

delete_test_() ->
  {inparallel, [delete_from_empty(), delete_invalid_element(), delete_nonexisting_element(), delete_existing_element()]}.

delete_from_empty() ->
  Set = emptySet(),
  {"Deleting integer from an empty list should return the original set", ?_assertEqual([], ?LIST(?DELETE(Set, 42)))}.

delete_invalid_element() ->
  EmptySet = emptySet(),
  ?BIND_IO(Set, fun nonEmptySet/0),
  {"Deleting invalid element from set should result in error", [
    ?_assertError(function_clause, ?LIST(?DELETE(EmptySet, a))),
    ?_assertError(function_clause, ?LIST(?DELETE(EmptySet, 1.2))),
    ?_assertError(function_clause, ?LIST(?DELETE(EmptySet, "ada"))),
    ?_assertError(function_clause, ?LIST(?DELETE(EmptySet, {1}))),
    ?_assertError(function_clause, ?LIST(?DELETE(Set, a))),
    ?_assertError(function_clause, ?LIST(?DELETE(Set, 1.2))),
    ?_assertError(function_clause, ?LIST(?DELETE(Set, "ada"))),
    ?_assertError(function_clause, ?LIST(?DELETE(Set, {1})))
  ]}.

delete_nonexisting_element() ->
  ?BIND_IO(fun nonEmptySet/0),
  {"Deleting element not present in the set should return the same set as it was before removal", [
    ?_assertEqual(removeElementFromOrderedList(Expected, 3), ?LIST(?DELETE(setWithoutElement(Value, 3), 3))),
    ?_assertEqual(removeElementFromOrderedList(Expected, -1), ?LIST(?DELETE(setWithoutElement(Value, -1), -1))),
    ?_assertEqual(removeElementFromOrderedList(Expected, 42), ?LIST(?DELETE(setWithoutElement(Value, 42), 42))),
    ?_assertEqual(removeElementFromOrderedList(Expected, 3128309189301), ?LIST(?DELETE(setWithoutElement(Value, 3128309189301), 3128309189301))),
    ?_assertEqual(removeElementFromOrderedList(Expected, -3128309189301), ?LIST(?DELETE(setWithoutElement(Value, -3128309189301), -3128309189301))),
    ?_assertEqual(removeElementFromOrderedList(Expected, 0), ?LIST(?DELETE(setWithoutElement(Value, 0), 0)))
  ]}.

delete_existing_element() ->
  ?BIND_IO(fun nonEmptySet/0),
  {"Deleting element present in the set should return a new set with the element removed", [
    ?_assertEqual(removeElementFromOrderedList(Expected, 3), ?LIST(?DELETE(setWithElement(Value, 3), 3))),
    ?_assertEqual(removeElementFromOrderedList(Expected, -1), ?LIST(?DELETE(setWithElement(Value, -1), -1))),
    ?_assertEqual(removeElementFromOrderedList(Expected, 42), ?LIST(?DELETE(setWithElement(Value, 42), 42))),
    ?_assertEqual(removeElementFromOrderedList(Expected, 3128309189301), ?LIST(?DELETE(setWithElement(Value, 3128309189301), 3128309189301))),
    ?_assertEqual(removeElementFromOrderedList(Expected, -3128309189301), ?LIST(?DELETE(setWithElement(Value, -3128309189301), -3128309189301))),
    ?_assertEqual(removeElementFromOrderedList(Expected, 0), ?LIST(?DELETE(setWithElement(Value, 0), 0)))
  ]}.

%%%%%%%%%%%%
%%% prec %%%
%%%%%%%%%%%%

prec_test_() ->
  {inparallel, [prec_in_empty(), prec_invalid_element(), prec_nonexisting_element(), prec_existing_element(), prec_existing_element_at_the_begining_of_set()]}.

prec_in_empty() ->
  EmptySet = emptySet(),
  {"Search for the predecessor of any element in an empty set must return nil", ?_assertEqual(?NIL, ?PREC(EmptySet, 42))}.

prec_invalid_element() ->
  EmptySet = emptySet(),
  ?BIND_IO(Set, fun nonEmptySet/0),
  {"Searching for the predecessor of an invalid element must result in error", [
    ?_assertError(function_clause, ?PREC(EmptySet, a)),
    ?_assertError(function_clause, ?PREC(EmptySet, 1.2)),
    ?_assertError(function_clause, ?PREC(EmptySet, "ada")),
    ?_assertError(function_clause, ?PREC(EmptySet, {1})),
    ?_assertError(function_clause, ?PREC(Set, a)),
    ?_assertError(function_clause, ?PREC(Set, 1.2)),
    ?_assertError(function_clause, ?PREC(Set, "ada")),
    ?_assertError(function_clause, ?PREC(Set, {1}))
  ]}.

prec_nonexisting_element() ->
  ?BIND_IO(Set, fun nonEmptySet/0),
  {"Searching for the predecessor of an element not present in the set must return nil", [
    ?_assertEqual(?NIL, ?PREC(setWithoutElement(Set, 3), 3)),
    ?_assertEqual(?NIL, ?PREC(setWithoutElement(Set, -1), -1)),
    ?_assertEqual(?NIL, ?PREC(setWithoutElement(Set, 42), 42)),
    ?_assertEqual(?NIL, ?PREC(setWithoutElement(Set, 3128309189301), 3128309189301)),
    ?_assertEqual(?NIL, ?PREC(setWithoutElement(Set, -3128309189301), -3128309189301)),
    ?_assertEqual(?NIL, ?PREC(setWithoutElement(Set, 0), 0))
  ]}.

prec_existing_element() ->
  ?BIND_IO(Set, fun nonEmptySet/0),
  {"Searching for the predecessor of an element present in the set that has a non-nil predecessor must return the predecessor", [
    ?_assertEqual(2, ?PREC(setWithElement(setWithElement(Set, 3), 2), 3)),
    ?_assertEqual(-2, ?PREC(setWithElement(setWithElement(Set, -1), -2), -1))
  ]}.

prec_existing_element_at_the_begining_of_set() ->
  {"Searching for the predecessor of an element present in the set but that is the first element of the set must return nil", [
    ?_assertEqual(?NIL, ?PREC(?TOSET([6, 7, 9, 42, 3912318301809]), 6))
  ]}.

%%%%%%%%%%%%
%%% succ %%%
%%%%%%%%%%%%

succ_test_() ->
  {inparallel, [succ_in_empty(), succ_invalid_element(), succ_nonexisting_element(), succ_existing_element(), succ_existing_element_at_the_end_of_set()]}.

succ_in_empty() ->
  EmptySet = emptySet(),
  {"Searching for the successor of any element in an empty set must return nil", ?_assertEqual(?NIL, ?SUCC(EmptySet, 42))}.

succ_invalid_element() ->
  EmptySet = emptySet(),
  ?BIND_IO(Set, fun nonEmptySet/0),
  {"Searching for the successor of an invalid element must result in error", [
    ?_assertError(function_clause, ?SUCC(EmptySet, a)),
    ?_assertError(function_clause, ?SUCC(EmptySet, 1.2)),
    ?_assertError(function_clause, ?SUCC(EmptySet, "ada")),
    ?_assertError(function_clause, ?SUCC(EmptySet, {1})),
    ?_assertError(function_clause, ?SUCC(Set, a)),
    ?_assertError(function_clause, ?SUCC(Set, 1.2)),
    ?_assertError(function_clause, ?SUCC(Set, "ada")),
    ?_assertError(function_clause, ?SUCC(Set, {1}))
  ]}.

succ_nonexisting_element() ->
  ?BIND_IO(Set, fun nonEmptySet/0),
  {"Searching for the successor of an element not present in the set must return nil", [
    ?_assertEqual(?NIL, ?SUCC(setWithoutElement(Set, 3), 3)),
    ?_assertEqual(?NIL, ?SUCC(setWithoutElement(Set, -1), -1)),
    ?_assertEqual(?NIL, ?SUCC(setWithoutElement(Set, 42), 42)),
    ?_assertEqual(?NIL, ?SUCC(setWithoutElement(Set, 3128309189301), 3128309189301)),
    ?_assertEqual(?NIL, ?SUCC(setWithoutElement(Set, -3128309189301), -3128309189301)),
    ?_assertEqual(?NIL, ?SUCC(setWithoutElement(Set, 0), 0))
  ]}.

succ_existing_element() ->
  ?BIND_IO(Set, fun nonEmptySet/0),
  {"Searching for the successor of an element present in the set that has a non-nil successor must return the successor", [
    ?_assertEqual(4, ?SUCC(setWithElement(setWithElement(Set, 3), 4), 3)),
    ?_assertEqual(0, ?SUCC(setWithElement(setWithElement(Set, -1), 0), -1))
  ]}.

succ_existing_element_at_the_end_of_set() ->
  {"Searching for the successor of an element present in the set but that is the last element of the set must return nil", [
    ?_assertEqual(?NIL, ?SUCC(?TOSET([6, 7, 9, 42, 3912318301809]), 3912318301809))
  ]}.


%%%%%%%%%%%%%
%%% union %%%
%%%%%%%%%%%%%

union_test_() ->
  {inparallel, [union_of_empty(), union_with_one_empty(), union_of_nonempty_with_distinct_elements(), union_of_nonempty_with_same_elements()]}.

union_of_empty() ->
  EmptySet1 = emptySet(),
  EmptySet2 = emptySet(),
  {"Union of two empty sets must be empty set", ?_assertEqual([], ?LIST(?UNION(EmptySet1, EmptySet2)))}.

union_with_one_empty() ->
  EmptySet = emptySet(),
  ?BIND_IO(fun nonEmptySet/0),
  {"Union of two sets from which one is empty must return set equal to the one non-empty set", [
    ?_assertEqual(Expected, ?LIST(?UNION(EmptySet, Value))),
    ?_assertEqual(Expected, ?LIST(?UNION(Value, EmptySet)))
  ]}.

union_of_nonempty_with_distinct_elements() ->
  {"Union of two sets with distinct elements must return a set with all elements from both sets", [
    ?_assertEqual([1, 3, 5, 42], ?LIST(?UNION(?TOSET([1, 3]), ?TOSET([5, 42])))),
    ?_assertEqual([1, 3, 5, 42], ?LIST(?UNION(?TOSET([1, 5]), ?TOSET([3, 42])))),
    ?_assertEqual([1, 3, 5, 42], ?LIST(?UNION(?TOSET([3, 5]), ?TOSET([1, 42]))))
  ]}.

union_of_nonempty_with_same_elements() ->
  {"Union of two sets with some or all elements being the same must return a set with all unique elements from both sets", [
    ?_assertEqual([1, 3, 5, 42], ?LIST(?UNION(?TOSET([1, 3, 5, 42]), ?TOSET([1, 3, 5, 42])))),
    ?_assertEqual([1, 3, 5, 42], ?LIST(?UNION(?TOSET([1, 3, 5]), ?TOSET([3, 5, 42])))),
    ?_assertEqual([1, 3, 5, 42], ?LIST(?UNION(?TOSET([3, 42]), ?TOSET([1, 5, 42]))))
  ]}.

%%%%%%%%%%%%
%%% diff %%%
%%%%%%%%%%%%

diff_test_() ->
  {inparallel, [diff_both_empty(), diff_first_empty(), diff_second_empty(), diff_distinct(), diff_subset(), diff_same()]}.

diff_both_empty() ->
  EmptySet1 = emptySet(),
  EmptySet2 = emptySet(),
  {"Diff of two empty sets must be and empty set", ?_assertEqual([], ?LIST(?DIFF(EmptySet1, EmptySet2)))}.

diff_first_empty() ->
  EmptySet = emptySet(),
  ?BIND_IO(Set1, fun nonEmptySet/0),
  ?BIND_IO(Set2, fun nonEmptySet1/0),
  ?BIND_IO(Set3, fun nonEmptySet2/0),
  {"Diff of two sets where the first set is empty must result in an empty set", [
    ?_assertEqual([], ?LIST(?DIFF(EmptySet, Set1))),
    ?_assertEqual([], ?LIST(?DIFF(EmptySet, Set2))),
    ?_assertEqual([], ?LIST(?DIFF(EmptySet, Set3)))
  ]}.

diff_second_empty() ->
  EmptySet = emptySet(),
  ?BIND_IO(Set1, List1, fun nonEmptySet/0),
  ?BIND_IO(Set2, List2, fun nonEmptySet1/0),
  ?BIND_IO(Set3, List3, fun nonEmptySet2/0),
  {"Diff of two sets where the second set is empty must result in the first set", [
    ?_assertEqual(List1, ?LIST(?DIFF(Set1, EmptySet))),
    ?_assertEqual(List2, ?LIST(?DIFF(Set2, EmptySet))),
    ?_assertEqual(List3, ?LIST(?DIFF(Set3, EmptySet)))
  ]}.

diff_distinct() ->
  ?BIND_IO2(fun distinctNonEmptySets/0),
  {"Diff of two distinct sets is the first set", [
    ?_assertEqual(List1, ?LIST(?DIFF(Set1, Set2))),
    ?_assertEqual(List2, ?LIST(?DIFF(Set2, Set1)))
  ]}.

diff_subset() ->
  ?BIND_IO2(fun setAndNonEmptySubset/0),
  {"Diff of a set and its subset is the first set - the second set", ?_assertEqual(listsDiff(List1, List2), ?LIST(?DIFF(Set1, Set2)))}.

diff_same() ->
  ?BIND_IO(Set1@a, fun nonEmptySet/0), ?BIND_IO(Set1@b, fun nonEmptySet/0),
  ?BIND_IO(Set2@a, fun nonEmptySet1/0), ?BIND_IO(Set2@b, fun nonEmptySet1/0),
  ?BIND_IO(Set3@a, fun nonEmptySet2/0), ?BIND_IO(Set3@b, fun nonEmptySet2/0),
  {"Diff of a set with the same set must be an empty set", [
    ?_assertEqual([], ?LIST(?DIFF(Set1@a, Set1@b))),
    ?_assertEqual([], ?LIST(?DIFF(Set2@a, Set2@b))),
    ?_assertEqual([], ?LIST(?DIFF(Set3@a, Set3@b)))
  ]}.

%%%%%%%%%%%%%%
%%% equals %%%
%%%%%%%%%%%%%%

equals_test_() ->
  {inparallel, [equals_both_empty(), equals_one_empty(), equals_non_empty_same_set(), equals_non_empty_different_sets()]}.

equals_both_empty() ->
  EmptySet1 = emptySet(),
  EmptySet2 = emptySet(),
  {"Two empty sets are equal", ?_assertEqual(true, ?EQUALS(EmptySet1, EmptySet2))}.

equals_one_empty() ->
  EmptySet = emptySet(),
  ?BIND_IO(NonEmptySet, fun nonEmptySet/0),
  {"An empty set is not equals with a non empty set", [
    ?_assertEqual(false, ?EQUALS(EmptySet, NonEmptySet)),
    ?_assertEqual(false, ?EQUALS(NonEmptySet, EmptySet))
  ]}.

equals_non_empty_same_set() ->
  ?BIND_IO(Set1@a, fun nonEmptySet/0), ?BIND_IO(Set1@b, fun nonEmptySet/0),
  ?BIND_IO(Set2@a, fun nonEmptySet1/0), ?BIND_IO(Set2@b, fun nonEmptySet1/0),
  ?BIND_IO(Set3@a, fun nonEmptySet2/0), ?BIND_IO(Set3@b, fun nonEmptySet2/0),
  {"A non empty set is equal with itself", [
    ?_assertEqual(true, ?EQUALS(Set1@a, Set1@b)),
    ?_assertEqual(true, ?EQUALS(Set2@a, Set2@b)),
    ?_assertEqual(true, ?EQUALS(Set3@a, Set3@b))
  ]}.

equals_non_empty_different_sets() ->
  ?BIND_IO(Set1, fun nonEmptySet/0),
  ?BIND_IO(Set2, fun nonEmptySet1/0),
  ?BIND_IO(Set3, fun nonEmptySet2/0),
  ?BIND_IO2(Set4, Set5, fun distinctNonEmptySets/0),
  ?BIND_IO2(Set6, Set7, fun setAndNonEmptySubset/0),
  {"Two distinct sets are not equal", [
    ?_assertEqual(false, ?EQUALS(Set1, Set2)),
    ?_assertEqual(false, ?EQUALS(Set2, Set3)),
    ?_assertEqual(false, ?EQUALS(Set3, Set4)),
    ?_assertEqual(false, ?EQUALS(Set4, Set5)),
    ?_assertEqual(false, ?EQUALS(Set5, Set6)),
    ?_assertEqual(false, ?EQUALS(Set6, Set7)),
    ?_assertEqual(false, ?EQUALS(Set7, Set1))
  ]}.

%%%%%%%%%%%
%%% max %%%
%%%%%%%%%%%

max_test_() ->
  {inparallel, [max_empty(), max_nonempty()]}.

max_empty() ->
  EmptySet = emptySet(),
  {"Max of an empty set is nil", ?_assertEqual(?NIL, ?MAX(EmptySet))}.

max_nonempty() ->
  ?BIND_IO(Set1, List1, fun nonEmptySet/0),
  ?BIND_IO(Set2, List2, fun nonEmptySet1/0),
  ?BIND_IO(Set3, List3, fun nonEmptySet2/0),
  ?BIND_IO2(Set4, Set5, List4, List5, fun distinctNonEmptySets/0),
  ?BIND_IO2(Set6, Set7, List6, List7, fun setAndNonEmptySubset/0),
  {"Max of an non-empty set is the max element of the set", [
    ?_assertEqual(listMax(List1), ?MAX(Set1)),
    ?_assertEqual(listMax(List2), ?MAX(Set2)),
    ?_assertEqual(listMax(List3), ?MAX(Set3)),
    ?_assertEqual(listMax(List4), ?MAX(Set4)),
    ?_assertEqual(listMax(List5), ?MAX(Set5)),
    ?_assertEqual(listMax(List6), ?MAX(Set6)),
    ?_assertEqual(listMax(List7), ?MAX(Set7))
  ]}.

%%%%%%%%%%%
%%% min %%%
%%%%%%%%%%%

min_test_() ->
  {inparallel, [min_empty(), min_nonempty()]}.

min_empty() ->
  EmptySet = emptySet(),
  {"Min of an empty set is nil", ?_assertEqual(?NIL, ?MIN(EmptySet))}.

min_nonempty() ->
  ?BIND_IO(Set1, List1, fun nonEmptySet/0),
  ?BIND_IO(Set2, List2, fun nonEmptySet1/0),
  ?BIND_IO(Set3, List3, fun nonEmptySet2/0),
  ?BIND_IO2(Set4, Set5, List4, List5, fun distinctNonEmptySets/0),
  ?BIND_IO2(Set6, Set7, List6, List7, fun setAndNonEmptySubset/0),
  {"Max of an non-empty set is the max element of the set", [
    ?_assertEqual(listMin(List1), ?MIN(Set1)),
    ?_assertEqual(listMin(List2), ?MIN(Set2)),
    ?_assertEqual(listMin(List3), ?MIN(Set3)),
    ?_assertEqual(listMin(List4), ?MIN(Set4)),
    ?_assertEqual(listMin(List5), ?MIN(Set5)),
    ?_assertEqual(listMin(List6), ?MIN(Set6)),
    ?_assertEqual(listMin(List7), ?MIN(Set7))
  ]}.

%%%%%%%%%%%
%%% map %%%
%%%%%%%%%%%

map_test_() ->
  {inparallel, [map_empty(), map_non_empty()]}.

map_empty() ->
  EmptySet = emptySet(),
  {"Map on an empty set should return an empty list", ?_assertEqual([], ?MAP(EmptySet, fun fun1Identity/1))}.

map_non_empty() ->
  ?BIND_IO(Set1, List1, fun nonEmptySet/0),
  ?BIND_IO(Set2, List2, fun nonEmptySet1/0),
  ?BIND_IO(Set3, List3, fun nonEmptySet2/0),
  {"Map on a non-empty set should return a list of the individual elements of the set after applying the function", [
    ?_assertEqual(listMap(List1, fun fun1Identity/1), ?MAP(Set1, fun fun1Identity/1)),
    ?_assertEqual(listMap(List2, fun fun1Identity/1), ?MAP(Set2, fun fun1Identity/1)),
    ?_assertEqual(listMap(List3, fun fun1Identity/1), ?MAP(Set3, fun fun1Identity/1)),
    ?_assertEqual(listMap(List1, fun fun1RandomizeOrder/1), ?MAP(Set1, fun fun1RandomizeOrder/1)),
    ?_assertEqual(listMap(List2, fun fun1RandomizeOrder/1), ?MAP(Set2, fun fun1RandomizeOrder/1)),
    ?_assertEqual(listMap(List3, fun fun1RandomizeOrder/1), ?MAP(Set3, fun fun1RandomizeOrder/1)),
    ?_assertEqual(listMap(List1, fun fun1ReverseOrder/1), ?MAP(Set1, fun fun1ReverseOrder/1)),
    ?_assertEqual(listMap(List2, fun fun1ReverseOrder/1), ?MAP(Set2, fun fun1ReverseOrder/1)),
    ?_assertEqual(listMap(List3, fun fun1ReverseOrder/1), ?MAP(Set3, fun fun1ReverseOrder/1)),
    ?_assertEqual(listMap(List1, fun fun1PreserveOrder/1), ?MAP(Set1, fun fun1PreserveOrder/1)),
    ?_assertEqual(listMap(List2, fun fun1PreserveOrder/1), ?MAP(Set2, fun fun1PreserveOrder/1)),
    ?_assertEqual(listMap(List3, fun fun1PreserveOrder/1), ?MAP(Set3, fun fun1PreserveOrder/1))
  ]}.

%%%%%%%%%%%%%
%%% foldl %%%
%%%%%%%%%%%%%

foldl_test_() ->
  {inparallel, [foldl_empty(), foldl_non_empty()]}.

foldl_empty() ->
  EmptySet = emptySet(),
  {"Left fold on an empty set should return an the Acc0", ?_assertEqual(0, ?FOLDL(EmptySet, fun fun2Minus/2, 0))}.

foldl_non_empty() ->
  ?BIND_IO(Set1, List1, fun nonEmptySet/0),
  ?BIND_IO(Set2, List2, fun nonEmptySet1/0),
  ?BIND_IO(Set3, List3, fun nonEmptySet2/0),
  {"Left fold on a non-empty set should return the folded result", [
    ?_assertEqual(listFoldL(List1, fun fun2Minus/2, 0), ?FOLDL(Set1, fun fun2Minus/2, 0)),
    ?_assertEqual(listFoldL(List2, fun fun2Minus/2, 0), ?FOLDL(Set2, fun fun2Minus/2, 0)),
    ?_assertEqual(listFoldL(List3, fun fun2Minus/2, 0), ?FOLDL(Set3, fun fun2Minus/2, 0))
  ]}.

%%%%%%%%%%%%%%
%%% filter %%%
%%%%%%%%%%%%%%

filter_test_() ->
  {inparallel, [filter_empty(), filter_non_empty()]}.

filter_empty() ->
  EmptySet = emptySet(),
  {"Filtering and empty set should return an empty set", [
    ?_assertEqual([], ?LIST(?FILTER(EmptySet, fun pred1_1/1))),
    ?_assertEqual([], ?LIST(?FILTER(EmptySet, fun pred1_2/1))),
    ?_assertEqual([], ?LIST(?FILTER(EmptySet, fun pred1_3/1))),
    ?_assertEqual([], ?LIST(?FILTER(EmptySet, fun pred1_4/1)))
  ]}.

filter_non_empty() ->
  ?BIND_IO(Set1, List1, fun nonEmptySet/0),
  ?BIND_IO(Set2, List2, fun nonEmptySet1/0),
  ?BIND_IO(Set3, List3, fun nonEmptySet2/0),
  {"Filtering non-empty set should return a set containing only the elements that conform to the predicate", [
    ?_assertEqual(listFilter(List1, fun pred1_1/1), ?LIST(?FILTER(Set1, fun pred1_1/1))),
    ?_assertEqual(listFilter(List2, fun pred1_1/1), ?LIST(?FILTER(Set2, fun pred1_1/1))),
    ?_assertEqual(listFilter(List3, fun pred1_1/1), ?LIST(?FILTER(Set3, fun pred1_1/1))),
    ?_assertEqual(listFilter(List1, fun pred1_2/1), ?LIST(?FILTER(Set1, fun pred1_2/1))),
    ?_assertEqual(listFilter(List2, fun pred1_2/1), ?LIST(?FILTER(Set2, fun pred1_2/1))),
    ?_assertEqual(listFilter(List3, fun pred1_2/1), ?LIST(?FILTER(Set3, fun pred1_2/1))),
    ?_assertEqual(listFilter(List1, fun pred1_3/1), ?LIST(?FILTER(Set1, fun pred1_3/1))),
    ?_assertEqual(listFilter(List2, fun pred1_3/1), ?LIST(?FILTER(Set2, fun pred1_3/1))),
    ?_assertEqual(listFilter(List3, fun pred1_3/1), ?LIST(?FILTER(Set3, fun pred1_3/1))),
    ?_assertEqual(listFilter(List1, fun pred1_4/1), ?LIST(?FILTER(Set1, fun pred1_4/1))),
    ?_assertEqual(listFilter(List2, fun pred1_4/1), ?LIST(?FILTER(Set2, fun pred1_4/1))),
    ?_assertEqual(listFilter(List3, fun pred1_4/1), ?LIST(?FILTER(Set3, fun pred1_4/1)))
  ]}.

%%%%%%%%%%%%
%%% card %%%
%%%%%%%%%%%%

card_test_() ->
  {inparallel, [card_empty(), card_non_empty()]}.

card_empty() ->
  EmptySet = emptySet(),
  {"Cardinality of an empty set is 0", ?_assertEqual(0, ?CARD(EmptySet))}.

card_non_empty() ->
  ?BIND_IO(Set1, List1, fun nonEmptySet/0),
  ?BIND_IO(Set2, List2, fun nonEmptySet1/0),
  ?BIND_IO(Set3, List3, fun nonEmptySet2/0),
  {"Cardinality of a non-empty set is the number of elements in the set", [
    ?_assertEqual(listCard(List1), ?CARD(Set1)),
    ?_assertEqual(listCard(List2), ?CARD(Set2)),
    ?_assertEqual(listCard(List3), ?CARD(Set3))
  ]}.

%%%%%%%%%%%%
%%% isin %%%
%%%%%%%%%%%%

isin_test_() ->
  {inparallel, [isin_empty(), isin_non_empty_non_existent(), isin_non_empty_existent()]}.

isin_empty() ->
  EmptySet = emptySet(),
  {"Element does not exists in an empty set", ?_assertEqual(false, ?ISIN(EmptySet, 1))}.

isin_non_empty_non_existent() ->
  ?BIND_IO(Set1, fun nonEmptySet/0),
  {"An element that is not in the set is not in the set", [
    ?_assertEqual(false, ?ISIN(setWithoutElement(Set1, 4), 4)),
    ?_assertEqual(false, ?ISIN(setWithoutElement(Set1, -5), -5)),
    ?_assertEqual(false, ?ISIN(setWithoutElement(Set1, 42), 42)),
    ?_assertEqual(false, ?ISIN(setWithoutElement(Set1, 1), 1))
  ]}.

isin_non_empty_existent() ->
  ?BIND_IO(Set1, fun nonEmptySet/0),
  {"An element that is in the set is in the set", [
    ?_assertEqual(true, ?ISIN(setWithElement(Set1, 41231), 41231)),
    ?_assertEqual(true, ?ISIN(setWithElement(Set1, -5989), -5989)),
    ?_assertEqual(true, ?ISIN(setWithElement(Set1, 42), 42)),
    ?_assertEqual(true, ?ISIN(setWithElement(Set1, 999), 999))
  ]}.

%%%%%%%%%%%
%%% all %%%
%%%%%%%%%%%

all_test_() ->
  {inparallel, [all_empty(), all_non_empty()]}.

all_empty() ->
  EmptySet = emptySet(),
  {"An empty set always conforms to the 'all' predicate", [
    ?_assertEqual(true, ?ALL(EmptySet, fun pred1_1/1)),
    ?_assertEqual(true, ?ALL(EmptySet, fun pred1_2/1)),
    ?_assertEqual(true, ?ALL(EmptySet, fun pred1_3/1)),
    ?_assertEqual(true, ?ALL(EmptySet, fun pred1_4/1))
  ]}.

all_non_empty() ->
  ?BIND_IO(Set1, List1, fun nonEmptySet/0),
  ?BIND_IO(Set2, List2, fun nonEmptySet1/0),
  ?BIND_IO(Set3, List3, fun nonEmptySet2/0),
  {"All on a non-empty set suceeds only if all elements of the set conform to the predicate", [
    ?_assertEqual(listAll(List1, fun pred1_1/1), ?ALL(Set1, fun pred1_1/1)),
    ?_assertEqual(listAll(List2, fun pred1_1/1), ?ALL(Set2, fun pred1_1/1)),
    ?_assertEqual(listAll(List3, fun pred1_1/1), ?ALL(Set3, fun pred1_1/1)),
    ?_assertEqual(listAll(List1, fun pred1_2/1), ?ALL(Set1, fun pred1_2/1)),
    ?_assertEqual(listAll(List2, fun pred1_2/1), ?ALL(Set2, fun pred1_2/1)),
    ?_assertEqual(listAll(List3, fun pred1_2/1), ?ALL(Set3, fun pred1_2/1)),
    ?_assertEqual(listAll(List1, fun pred1_3/1), ?ALL(Set1, fun pred1_3/1)),
    ?_assertEqual(listAll(List2, fun pred1_3/1), ?ALL(Set2, fun pred1_3/1)),
    ?_assertEqual(listAll(List3, fun pred1_3/1), ?ALL(Set3, fun pred1_3/1)),
    ?_assertEqual(listAll(List1, fun pred1_4/1), ?ALL(Set1, fun pred1_4/1)),
    ?_assertEqual(listAll(List2, fun pred1_4/1), ?ALL(Set2, fun pred1_4/1)),
    ?_assertEqual(listAll(List3, fun pred1_4/1), ?ALL(Set3, fun pred1_4/1))
  ]}.

%%%%%%%%%%%
%%% any %%%
%%%%%%%%%%%

any_test_() ->
  {inparallel, [any_empty(), any_non_empty()]}.

any_empty() ->
  EmptySet = emptySet(),
  {"An empty set never conforms to the 'any' predicate", [
    ?_assertEqual(false, ?ANY(EmptySet, fun pred1_1/1)),
    ?_assertEqual(false, ?ANY(EmptySet, fun pred1_2/1)),
    ?_assertEqual(false, ?ANY(EmptySet, fun pred1_3/1)),
    ?_assertEqual(false, ?ANY(EmptySet, fun pred1_4/1))
  ]}.

any_non_empty() ->
  ?BIND_IO(Set1, List1, fun nonEmptySet/0),
  ?BIND_IO(Set2, List2, fun nonEmptySet1/0),
  ?BIND_IO(Set3, List3, fun nonEmptySet2/0),
  {"Any on a non-empty set suceeds only if at least one element of the set conforms to the predicate", [
    ?_assertEqual(listAny(List1, fun pred1_1/1), ?ANY(Set1, fun pred1_1/1)),
    ?_assertEqual(listAny(List2, fun pred1_1/1), ?ANY(Set2, fun pred1_1/1)),
    ?_assertEqual(listAny(List3, fun pred1_1/1), ?ANY(Set3, fun pred1_1/1)),
    ?_assertEqual(listAny(List1, fun pred1_2/1), ?ANY(Set1, fun pred1_2/1)),
    ?_assertEqual(listAny(List2, fun pred1_2/1), ?ANY(Set2, fun pred1_2/1)),
    ?_assertEqual(listAny(List3, fun pred1_2/1), ?ANY(Set3, fun pred1_2/1)),
    ?_assertEqual(listAny(List1, fun pred1_3/1), ?ANY(Set1, fun pred1_3/1)),
    ?_assertEqual(listAny(List2, fun pred1_3/1), ?ANY(Set2, fun pred1_3/1)),
    ?_assertEqual(listAny(List3, fun pred1_3/1), ?ANY(Set3, fun pred1_3/1)),
    ?_assertEqual(listAny(List1, fun pred1_4/1), ?ANY(Set1, fun pred1_4/1)),
    ?_assertEqual(listAny(List2, fun pred1_4/1), ?ANY(Set2, fun pred1_4/1)),
    ?_assertEqual(listAny(List3, fun pred1_4/1), ?ANY(Set3, fun pred1_4/1))
  ]}.

%%%%%%%%%%%%%%%
%%% product %%%
%%%%%%%%%%%%%%%

product_test_() ->
  {inparallel, [product_both_empty(), product_one_empty(), product_non_empty()]}.

product_both_empty() ->
  EmptySet1 = emptySet(),
  EmptySet2 = emptySet(),
  {"Product of two empty sets in an empty set", ?_assertEqual([], ?LIST(?PRODUCT(EmptySet1, EmptySet2, fun fun2_1/2)))}.

product_one_empty() ->
  EmptySet = emptySet(),
  ?BIND_IO(Set1, fun nonEmptySet/0),
  ?BIND_IO(Set2, fun nonEmptySet1/0),
  ?BIND_IO(Set3, fun nonEmptySet2/0),
  {"Product of an empty set and a non-empty set is always an empty set", [
    ?_assertEqual([], ?LIST(?PRODUCT(EmptySet, Set1, fun fun2_1/2))),
    ?_assertEqual([], ?LIST(?PRODUCT(EmptySet, Set2, fun fun2_1/2))),
    ?_assertEqual([], ?LIST(?PRODUCT(EmptySet, Set3, fun fun2_1/2))),
    ?_assertEqual([], ?LIST(?PRODUCT(Set1, EmptySet, fun fun2_1/2))),
    ?_assertEqual([], ?LIST(?PRODUCT(Set2, EmptySet, fun fun2_1/2))),
    ?_assertEqual([], ?LIST(?PRODUCT(Set3, EmptySet, fun fun2_1/2))),
    ?_assertEqual([], ?LIST(?PRODUCT(EmptySet, Set1, fun fun2_2/2))),
    ?_assertEqual([], ?LIST(?PRODUCT(EmptySet, Set2, fun fun2_2/2))),
    ?_assertEqual([], ?LIST(?PRODUCT(EmptySet, Set3, fun fun2_2/2))),
    ?_assertEqual([], ?LIST(?PRODUCT(Set1, EmptySet, fun fun2_2/2))),
    ?_assertEqual([], ?LIST(?PRODUCT(Set2, EmptySet, fun fun2_2/2))),
    ?_assertEqual([], ?LIST(?PRODUCT(Set3, EmptySet, fun fun2_2/2))),
    ?_assertEqual([], ?LIST(?PRODUCT(EmptySet, Set1, fun fun2_3/2))),
    ?_assertEqual([], ?LIST(?PRODUCT(EmptySet, Set2, fun fun2_3/2))),
    ?_assertEqual([], ?LIST(?PRODUCT(EmptySet, Set3, fun fun2_3/2))),
    ?_assertEqual([], ?LIST(?PRODUCT(Set1, EmptySet, fun fun2_3/2))),
    ?_assertEqual([], ?LIST(?PRODUCT(Set2, EmptySet, fun fun2_3/2))),
    ?_assertEqual([], ?LIST(?PRODUCT(Set3, EmptySet, fun fun2_3/2))),
    ?_assertEqual([], ?LIST(?PRODUCT(EmptySet, Set1, fun fun2_4/2))),
    ?_assertEqual([], ?LIST(?PRODUCT(EmptySet, Set2, fun fun2_4/2))),
    ?_assertEqual([], ?LIST(?PRODUCT(EmptySet, Set3, fun fun2_4/2))),
    ?_assertEqual([], ?LIST(?PRODUCT(Set1, EmptySet, fun fun2_4/2))),
    ?_assertEqual([], ?LIST(?PRODUCT(Set2, EmptySet, fun fun2_4/2))),
    ?_assertEqual([], ?LIST(?PRODUCT(Set3, EmptySet, fun fun2_4/2)))
  ]}.

product_non_empty() ->
  ?BIND_IO(Set1, List1, fun nonEmptySet/0),
  ?BIND_IO(Set2, List2, fun nonEmptySet1/0),
  ?BIND_IO(Set3, List3, fun nonEmptySet2/0),
  {"Product of two non empty sets is a set created from the result of applying a function to all element pairs such, that the first element of the pair is from the first set and the second element of the pair is from the second pair", [
    ?_assertEqual(listsProduct(List1, List2, fun fun2_1/2), ?LIST(?PRODUCT(Set1, Set2, fun fun2_1/2))),
    ?_assertEqual(listsProduct(List2, List1, fun fun2_1/2), ?LIST(?PRODUCT(Set2, Set1, fun fun2_1/2))),
    ?_assertEqual(listsProduct(List1, List3, fun fun2_1/2), ?LIST(?PRODUCT(Set1, Set3, fun fun2_1/2))),
    ?_assertEqual(listsProduct(List3, List1, fun fun2_1/2), ?LIST(?PRODUCT(Set3, Set1, fun fun2_1/2))),
    ?_assertEqual(listsProduct(List2, List3, fun fun2_1/2), ?LIST(?PRODUCT(Set2, Set3, fun fun2_1/2))),
    ?_assertEqual(listsProduct(List3, List2, fun fun2_1/2), ?LIST(?PRODUCT(Set3, Set2, fun fun2_1/2))),
    ?_assertEqual(listsProduct(List1, List1, fun fun2_1/2), ?LIST(?PRODUCT(Set1, Set1, fun fun2_1/2))),
    ?_assertEqual(listsProduct(List2, List2, fun fun2_1/2), ?LIST(?PRODUCT(Set2, Set2, fun fun2_1/2))),
    ?_assertEqual(listsProduct(List3, List3, fun fun2_1/2), ?LIST(?PRODUCT(Set3, Set3, fun fun2_1/2))),

    ?_assertEqual(listsProduct(List1, List2, fun fun2_2/2), ?LIST(?PRODUCT(Set1, Set2, fun fun2_2/2))),
    ?_assertEqual(listsProduct(List2, List1, fun fun2_2/2), ?LIST(?PRODUCT(Set2, Set1, fun fun2_2/2))),
    ?_assertEqual(listsProduct(List1, List3, fun fun2_2/2), ?LIST(?PRODUCT(Set1, Set3, fun fun2_2/2))),
    ?_assertEqual(listsProduct(List3, List1, fun fun2_2/2), ?LIST(?PRODUCT(Set3, Set1, fun fun2_2/2))),
    ?_assertEqual(listsProduct(List2, List3, fun fun2_2/2), ?LIST(?PRODUCT(Set2, Set3, fun fun2_2/2))),
    ?_assertEqual(listsProduct(List3, List2, fun fun2_2/2), ?LIST(?PRODUCT(Set3, Set2, fun fun2_2/2))),
    ?_assertEqual(listsProduct(List1, List1, fun fun2_2/2), ?LIST(?PRODUCT(Set1, Set1, fun fun2_2/2))),
    ?_assertEqual(listsProduct(List2, List2, fun fun2_2/2), ?LIST(?PRODUCT(Set2, Set2, fun fun2_2/2))),
    ?_assertEqual(listsProduct(List3, List3, fun fun2_2/2), ?LIST(?PRODUCT(Set3, Set3, fun fun2_2/2))),

    ?_assertEqual(listsProduct(List1, List2, fun fun2_3/2), ?LIST(?PRODUCT(Set1, Set2, fun fun2_3/2))),
    ?_assertEqual(listsProduct(List2, List1, fun fun2_3/2), ?LIST(?PRODUCT(Set2, Set1, fun fun2_3/2))),
    ?_assertEqual(listsProduct(List1, List3, fun fun2_3/2), ?LIST(?PRODUCT(Set1, Set3, fun fun2_3/2))),
    ?_assertEqual(listsProduct(List3, List1, fun fun2_3/2), ?LIST(?PRODUCT(Set3, Set1, fun fun2_3/2))),
    ?_assertEqual(listsProduct(List2, List3, fun fun2_3/2), ?LIST(?PRODUCT(Set2, Set3, fun fun2_3/2))),
    ?_assertEqual(listsProduct(List3, List2, fun fun2_3/2), ?LIST(?PRODUCT(Set3, Set2, fun fun2_3/2))),
    ?_assertEqual(listsProduct(List1, List1, fun fun2_3/2), ?LIST(?PRODUCT(Set1, Set1, fun fun2_3/2))),
    ?_assertEqual(listsProduct(List2, List2, fun fun2_3/2), ?LIST(?PRODUCT(Set2, Set2, fun fun2_3/2))),
    ?_assertEqual(listsProduct(List3, List3, fun fun2_3/2), ?LIST(?PRODUCT(Set3, Set3, fun fun2_3/2))),

    ?_assertEqual(listsProduct(List1, List2, fun fun2_4/2), ?LIST(?PRODUCT(Set1, Set2, fun fun2_4/2))),
    ?_assertEqual(listsProduct(List2, List1, fun fun2_4/2), ?LIST(?PRODUCT(Set2, Set1, fun fun2_4/2))),
    ?_assertEqual(listsProduct(List1, List3, fun fun2_4/2), ?LIST(?PRODUCT(Set1, Set3, fun fun2_4/2))),
    ?_assertEqual(listsProduct(List3, List1, fun fun2_4/2), ?LIST(?PRODUCT(Set3, Set1, fun fun2_4/2))),
    ?_assertEqual(listsProduct(List2, List3, fun fun2_4/2), ?LIST(?PRODUCT(Set2, Set3, fun fun2_4/2))),
    ?_assertEqual(listsProduct(List3, List2, fun fun2_4/2), ?LIST(?PRODUCT(Set3, Set2, fun fun2_4/2))),
    ?_assertEqual(listsProduct(List1, List1, fun fun2_4/2), ?LIST(?PRODUCT(Set1, Set1, fun fun2_4/2))),
    ?_assertEqual(listsProduct(List2, List2, fun fun2_4/2), ?LIST(?PRODUCT(Set2, Set2, fun fun2_4/2))),
    ?_assertEqual(listsProduct(List3, List3, fun fun2_4/2), ?LIST(?PRODUCT(Set3, Set3, fun fun2_4/2)))
  ]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS TEST %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

helpers_test_() ->
  {inparallel, [
    test_setWithoutElement(),
    test_setWithElement(),
    test_ensureElementInOrderedList(),
    test_removeElementFromOrderedList(),
    test_listsDiff(),
    test_listsProduct()
  ]}.

test_setWithoutElement() ->
  % These will likely fail if ?SET_MODULE:toSet or ?SET_MODULE:toList are not working correctly since we use them in the functions
  [
    ?_assertEqual([], ?LIST(setWithoutElement(?TOSET([]), 42))),
    ?_assertEqual([], ?LIST(setWithoutElement(?TOSET([42]), 42))),
    ?_assertEqual([1, 3, 5], ?LIST(setWithoutElement(?TOSET([1, 3, 5, 42]), 42))),
    ?_assertEqual([1, 3, 5, 999], ?LIST(setWithoutElement(?TOSET([1, 3, 5, 42, 999]), 42))),
    ?_assertEqual([9999, 99999], ?LIST(setWithoutElement(?TOSET([42, 9999, 99999]), 42))),
    ?_assertEqual([1, 3, 5], ?LIST(setWithoutElement(?TOSET([1, 3, 5]), 42)))
  ].

test_setWithElement() ->
  % These will likely fail if ?SET_MODULE:toSet or ?SET_MODULE:toList are not working correctly since we use them in the functions
  [
    ?_assertEqual([42], ?LIST(setWithElement(?TOSET([]), 42))),
    ?_assertEqual([42], ?LIST(setWithElement(?TOSET([42]), 42))),
    ?_assertEqual([-1, 3, 5, 42], ?LIST(setWithElement(?TOSET([-1, 3, 5, 42]), 42))),
    ?_assertEqual([-1, 3, 5, 42, 999], ?LIST(setWithElement(?TOSET([-1, 3, 5, 42, 999]), 42))),
    ?_assertEqual([42, 9999, 99999], ?LIST(setWithElement(?TOSET([42, 9999, 99999]), 42))),
    ?_assertEqual([-1, 3, 5, 42], ?LIST(setWithElement(?TOSET([-1, 3, 5]), 42))),
    ?_assertEqual([-1, 3, 5, 42, 999], ?LIST(setWithElement(?TOSET([-1, 3, 5, 999]), 42))),
    ?_assertEqual([42, 9999, 99999], ?LIST(setWithElement(?TOSET([9999, 99999]), 42)))
  ].

test_ensureElementInOrderedList() ->
  [
    ?_assertEqual([42], ensureElementInOrderedList([], 42)),
    ?_assertEqual([42], ensureElementInOrderedList([42], 42)),
    ?_assertEqual([-1, 3, 5, 42], ensureElementInOrderedList([-1, 3, 5, 42], 42)),
    ?_assertEqual([-1, 3, 5, 42, 999], ensureElementInOrderedList([-1, 3, 5, 42, 999], 42)),
    ?_assertEqual([42, 9999, 99999], ensureElementInOrderedList([42, 9999, 99999], 42)),
    ?_assertEqual([-1, 3, 5, 42], ensureElementInOrderedList([-1, 3, 5], 42)),
    ?_assertEqual([-1, 3, 5, 42, 999], ensureElementInOrderedList([-1, 3, 5, 999], 42)),
    ?_assertEqual([42, 9999, 99999], ensureElementInOrderedList([9999, 99999], 42))
  ].

test_removeElementFromOrderedList() ->
  [
    ?_assertEqual([], removeElementFromOrderedList([], 42)),
    ?_assertEqual([], removeElementFromOrderedList([42], 42)),
    ?_assertEqual([-1, 3, 5], removeElementFromOrderedList([-1, 3, 5, 42], 42)),
    ?_assertEqual([-1, 3, 5, 999], removeElementFromOrderedList([-1, 3, 5, 42, 999], 42)),
    ?_assertEqual([9999, 99999], removeElementFromOrderedList([42, 9999, 99999], 42)),
    ?_assertEqual([-1, 3, 5], removeElementFromOrderedList([-1, 3, 5], 42)),
    ?_assertEqual([-1, 3, 5, 999], removeElementFromOrderedList([-1, 3, 5, 999], 42)),
    ?_assertEqual([9999, 99999], removeElementFromOrderedList([9999, 99999], 42))
  ].

test_listsDiff() ->
  [
    ?_assertEqual([], listsDiff([],[])),
    ?_assertEqual([1, 3, 5, 42], listsDiff([1, 3, 5, 42],[])),
    ?_assertEqual([], listsDiff([],[1, 3, 5, 42])),
    ?_assertEqual([1, 3], listsDiff([1, 3],[5, 42])),
    ?_assertEqual([1, 5], listsDiff([1, 5],[3, 42])),
    ?_assertEqual([], listsDiff([1, 3, 5, 42],[1, 3, 5, 42])),
    ?_assertEqual([1, 5], listsDiff([1, 3, 5, 42],[3, 42]))
  ].

test_listsProduct() ->
  [
    ?_assertEqual([], listsProduct([], [], fun (A, B) -> A + B end)),
    ?_assertEqual([], listsProduct([1, 3, 5, 42], [], fun (A, B) -> A + B end)),
    ?_assertEqual([], listsProduct([], [1, 3, 5, 42], fun (A, B) -> A + B end)),
    ?_assertEqual([2, 3, 11, 12, 20, 21, 22, 30, 31], listsProduct([1, 2, 10, 11], [1, 10, 20], fun (A, B) -> A + B end))
  ].

%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%

listsProduct(List1, List2, F) ->
  S1 = sofs:set(List1),
  S2 = sofs:set(List2),
  R = sofs:product(S1, S2),
  Pairs = sofs:to_external(R),
  Applied = lists:map(fun ({A, B}) -> F(A, B) end, Pairs),
  lists:usort(lists:filter(fun is_integer/1, Applied)).

listAny(List, F) ->
  lists:any(F, List).

listAll(List, F) ->
  lists:all(F, List).

listCard(List) ->
  length(List).

listFilter(List, F) ->
  lists:filter(F, List).

listFoldL(List, F, Acc0) ->
  lists:foldl(F, Acc0, List).

listMap(List, F) ->
  lists:map(F, List).

listMax(List) ->
  lists:max(List).

listMin(List) ->
  lists:min(List).

%% List1 - List2
listsDiff(List1, List2) ->
  ordsets:to_list(ordsets:subtract(ordsets:from_list(List1), ordsets:from_list(List2))).

setWithoutElement(Set, Ele) ->
  ?TOSET(removeElementFromOrderedList(?LIST(Set), Ele)).

setWithElement(Set, Ele) ->
  ?TOSET(ensureElementInOrderedList(?LIST(Set), Ele)).

ensureElementInOrderedList(List, Ele) ->
  ordsets:to_list(ordsets:add_element(Ele,ordsets:from_list(List))).

removeElementFromOrderedList(List, Ele) ->
  ordsets:to_list(ordsets:del_element(Ele,ordsets:from_list(List))).