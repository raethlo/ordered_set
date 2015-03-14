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
-export([newSet/0, toList/1, toSet/1, insert/2, delete/2]).

%% SET CREATION FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

newSet() -> {set, []}.

toList({set, List}) ->
  List.

toSet(List) when is_list(List) ->
  toSet_p(List, []).

toSet_p([], Acc) ->
  {set, Acc};
toSet_p([H | T], Acc) when is_number(H) ->
  toSet_p(T ,insert_p(Acc, H, []));
toSet_p([_|T], Acc)  ->
  toSet_p(T ,Acc).


%% SET MANIPULATION FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

insert({set, List}, Ele) when is_number(Ele) ->
  {set, insert_p(List, Ele, [])}.

insert_p([], Ele, Acc) ->
  reverse([Ele | Acc]);
insert_p([H | T], Ele, Acc) when Ele < H ->
  join_p(Acc, [Ele | [H | T]]);
insert_p([H | T], Ele, Acc) when Ele > H ->
  insert_p(T, Ele, [H | Acc]);
insert_p([H | T], Ele, Acc) when Ele == H ->
  join(Acc, [H | T]).


delete({set, List}, Ele) when is_number(Ele) ->
  {set,delete_p(List, Ele, [])}.

delete_p([],_,[]) ->
  [];
delete_p([],_, Acc) ->
  reverse(Acc);
delete_p([H | T], Ele, Acc) when H == Ele ->
  join_p(Acc,T);
delete_p([H | T], Ele, Acc) ->
  delete_p(T, Ele, [H | Acc]).

%%% HELPERS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%takes in ordered lists and makes them one
join(List1, List2) when is_list(List1), is_list(List2) ->
  join_p(reverse(List1), List2).


%% expects that the first list is descending and the other is ascending
%% should work vice-versa
join_p([], List) ->
  List;
join_p(List, []) ->
  List;
join_p([H | T], List) ->
  join_p(T, [H | List]).


reverse(List) when is_list(List) ->
  reverse_p(List, []).

reverse_p([], Acc) ->
  Acc;
reverse_p([H | T], Acc) ->
  reverse_p(T, [H | Acc]).