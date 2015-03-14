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
-export([newSet/0, insert/2, reverse/1, join/2]).
%% -compile(export_all). %% replace with -export() later, for God's sake!


newSet() -> {set,[]}.

toList({set,List}) ->
  List.


%% SET MANIPULATION FUNCTIONS

insert({set, List}, Ele) when is_number(Ele) ->
  insert_p(List, Ele, []).

insert_p([],Ele,Acc) ->
  {set,reverse([Ele | Acc])};
insert_p( [H | T] , Ele, Acc) when Ele < H ->
  List = join(Acc, [Ele | [H| T]] ),
  {set, List};
insert_p( [H | T], Ele, Acc) when Ele > H ->
  insert_p(T, Ele, [H | Acc]);
insert_p([H | T], Ele, Acc) when Ele == H ->
  {set,join(Acc, [H | T])}.

%% HELPERS

%%
join(List1,List2) when is_list(List1),is_list(List2) ->
  join_p(reverse(List1),List2).

join_p([], List) ->
  List;
join_p(List, []) ->
  List;
join_p([H | T], List) ->
  join_p(T, [H | List]).



reverse(List) when is_list(List) ->
  reverse_p(List,[]).

reverse_p([],Acc) ->
  Acc;
reverse_p([H | T],Acc) ->
  reverse_p(T,[ H | Acc ]).