%%%-------------------------------------------------------------------
%%% @author raethlo
%%% @copyright (C) 2015, raethlo
%%% @doc
%%%
%%% @end
%%% Created : 13. Mar 2015 11:29
%%%-------------------------------------------------------------------
-module(set).
-author(raethlo).

%% API
-export([newSet/0, toList/1, toSet/1, insert/2, delete/2, prec/2, succ/2, show/1,
  intersect/2, union/2, diff/2, equals/2 , min/1, max/1, map/2, foldl/3, filter/2,
  card/1, isin/2, all/2, any/2, product/3]).

%% SET CREATION FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

newSet() -> {set, []}.

toList({set, List}) ->
  List.

toSet(List) when is_list(List) ->
  {set,toSet_p(List, [])}.

toSet_p([], Acc) ->
  Acc;
toSet_p([H | T], Acc) when is_integer(H) ->
  toSet_p(T ,insert_p(Acc, H, []));
toSet_p([_|T], Acc)  ->
  toSet_p(T, Acc).


%% SET MANIPULATION FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

insert({set, List}, Ele) when is_integer(Ele) ->
  {set, insert_p(List, Ele, [])}.

insert_p([], Ele, Acc) ->
  reverse([Ele | Acc]);
insert_p([H | T], Ele, Acc) when Ele < H ->
  join_p(Acc, [Ele | [H | T]]);
insert_p([H | T], Ele, Acc) when Ele > H ->
  insert_p(T, Ele, [H | Acc]);
insert_p([H | T], Ele, Acc) when Ele == H ->
  join_p(Acc, [H | T]).


delete({set, List}, Ele) when is_integer(Ele) ->
  {set,delete_p(List, Ele, [])}.

delete_p([],_,[]) ->
  [];
delete_p([],_, Acc) ->
  reverse(Acc);
delete_p([H | T], Ele, Acc) when H == Ele ->
  join_p(Acc,T);
delete_p([H | T], Ele, Acc) ->
  delete_p(T, Ele, [H | Acc]).


prec({set,List}, Ele) when is_integer(Ele)->
  prec_p(List, Ele, nil).

prec_p([],_Ele, _) ->
  nil;
prec_p([H | T], Ele, Before) when Ele == H ->
  Before;
prec_p([H | T], Ele, _) ->
  prec_p(T, Ele, H).


succ({set,List},Ele) when is_integer(Ele) ->
  succ_p(List,Ele,nil).

succ_p([], _Ele, _) ->
  nil;
succ_p([H | _], Ele, Before) when Ele == Before ->
  H;
succ_p([H | T], Ele, _) ->
  succ_p(T, Ele, H).


show({set, List}) ->
  io:format("set contains: "),
  show_p(List).


show_p([]) ->
  io:format("~n");
show_p([H | []]) ->
  io:format("~p~n",[H]);
show_p([H | T]) ->
  io:format("~p,",[H]),
  show_p(T).



intersect({set,List1},{set,List2}) ->
  L = [X || X <- List1, isin_p(List2,X)],
  {set,L}.


union({set, List1}, {set, List2}) ->
  {set, union_p(List1, List2)}.

union_p([],List) ->
  List;
union_p([H | T], List) ->
  union_p(T, insert_p(List, H, [])).


diff({set,List1},{set,List2}) ->
  L = [X || X <- List1, not isin_p(List2, X)],
  {set,L}.


equals({set, List1}, {set, List2}) ->
  equals_p(List1, List2).

equals_p([],[]) ->
  true;
equals_p([], _List) ->
  false;
equals_p(_List, []) ->
  false;
equals_p([H1 | _T1], [H2 | _T2]) when H1 =/= H2 ->
  false;
equals_p([H1 | T1], [H2 | T2]) when H1 == H2 ->
  equals_p(T1,T2).


max({set, List}) ->
  max_p(List).

max_p([]) ->
  nil;
max_p([H | []]) ->
  H;
max_p([_H | T]) ->
  max_p(T).


min({set,[]}) ->
  nil;
min({set,[H | _]}) ->
  H.


map({set, List}, Fnc) when is_function(Fnc, 1) ->
  L = [Fnc(X) || X <- List],
  toSet_p(L,[]).


foldl({set, List}, Fnc, Acc0) when is_function(Fnc,2) ->
  foldl_p(List, Fnc, Acc0).

foldl_p([],_Fnc,Acc) ->
  Acc;
foldl_p([H | T],Fnc, Acc) ->
  foldl_p(T, Fnc, Fnc(Acc, H)).


filter({set, List}, Fnc) when is_function(Fnc, 1) ->
  {set, [X || X <- List, Fnc(X)]}.


card({set, List}) ->
  card_p(List, 0).

card_p([], Counter) ->
  Counter;
card_p([_ | T], Counter) ->
  card_p(T, Counter + 1).


isin({set, List}, Ele) when is_integer(Ele) ->
  isin_p(List,Ele).

isin_p([], _Ele) ->
  false;
isin_p([H | _T], Ele) when H == Ele ->
  true;
isin_p([_ | T], Ele) ->
  isin_p(T, Ele).

all({set, List}, Fnc) when is_function(Fnc,1) ->
  all_p(List,Fnc).

all_p([],_Fnc) ->
  true;
all_p([H | T], Fnc) ->
  Res  = Fnc(H),
  if not Res -> false;
    true -> all_p(T,Fnc)
  end.

any({set, List}, Fnc) when is_function(Fnc, 1) ->
  any_p(List, Fnc).

any_p([], _Fnc) ->
  false;
any_p([H | T], Fnc) ->
  Res = Fnc(H),
  if Res -> true;
    true -> any_p(T, Fnc)
  end.


product({set, List1}, {set, List2}, Fnc) when is_function(Fnc,2) ->
  L = [ Fnc(X,Y) || X <- List1, Y <- List2 ],
  {set, toSet_p(L,[])}.


%%% HELPERS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%takes in ordered lists and makes them one
join(List1, List2) when is_list(List1), is_list(List2) ->
  join_p(reverse(List1), List2).


%% expects that the first list is descending and the other is ascending
%% should work vice-versa
join_p([], List) ->
  List;
join_p([H | T], List) ->
  join_p(T, [H | List]).


reverse(List) when is_list(List) ->
  reverse_p(List, []).

reverse_p([], Acc) ->
  Acc;
reverse_p([H | T], Acc) ->
  reverse_p(T, [H | Acc]).