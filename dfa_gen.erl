-module(dfa_gen).

-export([gen/0]).


re2nfa([], _From, _To, Count, Table) ->
    {Count, Table};
re2nfa([H|T], From, To, Count, Table) ->
    {From1, Count1, Table1} =
	lists:foldl(
	  fun (E, {F, Cnt, Tab}) ->
		  {Cnt1, Tab1} = re2nfa(E, F, Cnt, Cnt+1, Tab),
		  {Cnt, Cnt1, Tab1}
	  end,
	  {From, Count, Table},
	  H),
    re2nfa(T, From, To, Count1, [{From1, {e, To}}|Table1]);
re2nfa({'?', E}, From, To, Count, Table) ->
    re2nfa(E, From, To, Count, [{From, {e, To}}|Table]);
re2nfa({'+', E}, From, To, Count, Table) ->
    {Count1, Table1} = re2nfa(E, Count, To, Count+1, [{From, {e, Count}}|Table]),
    {Count1, [{To, {e, Count}}|Table1]};
re2nfa({'*', E}, From, To, Count, Table) ->
    re2nfa(E, To, To, Count, [{From, {e, To}}|Table]);
re2nfa(E, From, To, Count, Table) ->
    {Count, [{From, {E, To}}|Table]}.


lex_rules_to_nfa(REs) ->
    {_, Table} =
	lists:foldl(
	  fun ({I, RE}, {Count, Tab}) ->
		  re2nfa(RE, 0, {rule, I}, Count, Tab)
	  end,
	  {1, []},
	  lists:zip(lists:seq(0, length(REs)-1), REs)),
    lists:reverse(Table).


nfa_move(States, Symbol, NFA) ->
    lists:usort(
      lists:foldl(
	fun (State, NewStates) ->
		proplists:get_all_values(
		  Symbol,
		  proplists:get_all_values(State, NFA)) ++ NewStates
	end,
	[],
	States)).


e_closure([], States, _NFA) ->
    States;
e_closure(NewStates, States, NFA) ->
    NextStates =
	lists:filter(
	  fun (State) ->
		  not lists:member(State, States)
	  end,
	  nfa_move(NewStates, e, NFA)),
    e_closure(NextStates, lists:usort(NextStates++States), NFA).

e_closure(States, NFA) ->
    e_closure(States, States, NFA).


powerset(States, Symbol, NFA) ->
    e_closure(nfa_move(States, Symbol, NFA), NFA).


nfa2dfa([], PowerSets, _Count, Table, _Symbols, _NFA) ->
    {PowerSets, Table};
nfa2dfa([{DFAState, NFAStates}|Rest], PowerSets, Count, Table, Symbols, NFA) ->
    {States, PowerSets1, Count1, Table1} =
	lists:foldl(
	  fun (Symbol, {NewStates, Sets, Cnt, Tab}) ->
		  case powerset(NFAStates, Symbol, NFA) of
		      [] ->
			  {NewStates, Sets, Cnt, Tab};
		      PowerSet ->
			  case proplists:get_value(PowerSet, PowerSets) of
			      undefined ->
				  {NewStates ++[{Cnt, PowerSet}],
				   [{PowerSet, Cnt}|Sets],
				   Cnt+1,
				   [{DFAState, {Symbol, Cnt}}|Tab]};
			      NextState ->
				  {NewStates, Sets, Cnt, [{DFAState, {Symbol, NextState}}|Tab]}
			  end
		  end
	  end,
	  {[], PowerSets, Count, Table},
	  Symbols),
    nfa2dfa(Rest++States, PowerSets1, Count1, Table1, Symbols, NFA).


nfa2dfa(NFA) ->
    Symbols = lists:usort([S || {_,{S,_}} <- NFA, S =/= e]),
    NFAStates = e_closure(lists:usort([0]), NFA),
    {PowerSets, Table} = nfa2dfa([{0, NFAStates}], [{NFAStates, 0}], 1, [], Symbols, NFA),
    {lists:reverse(Table), lists:reverse([{V,K} || {K,V} <- PowerSets])}.


dfa_move(State, Symbol, DFA) ->
    proplists:get_value(Symbol, proplists:get_all_values(State, DFA)).


index_of(Elem, [Elem|_], Count) ->
    Count;
index_of(Elem, [_|T], Count) ->
    index_of(Elem, T, Count+1).

index_of(Elem, List) ->
    index_of(Elem, List, 0).


refine_dfa_groups(GroupOfState, Transitions) ->
    TransitionsToGroup =
	lists:map(
	  fun ({State, States}) ->
		  {State,
		   lists:map(
		     fun (S)
			 when is_integer(S) ->
			     proplists:get_value(S, GroupOfState);
			 (S) ->
			     S
		     end,
		     [State|States])}
	  end,
	  Transitions),

    Groups = lists:usort([ Group || {_, Group} <- TransitionsToGroup ]),

    GroupOfState1 =
	lists:keymap(
	  fun (Group) ->
		  index_of(Group, Groups)
	  end,
	  2,
	  TransitionsToGroup),

    case GroupOfState1 of
	GroupOfState ->
	    GroupOfState;
	_ ->
	    refine_dfa_groups(GroupOfState1, Transitions)
    end.


minimize_dfa(DFA, PowerSets) ->
    Symbols = lists:usort([S || {_,{S,_}} <- DFA]),

    States = lists:usort([From || {From, _} <- DFA] ++ [To || {_, {_, To}} <- DFA]),

    Terminals = [{K,S} || {K, V} <- PowerSets, S <- V, not is_integer(S)],

    GroupOfState = [{S, 0} || S <- States],

    Transitions =
	lists:map(
	  fun (State) ->
		  {State,
		   [dfa_move(State, Symbol, DFA) || Symbol <- Symbols] ++ [proplists:get_all_values(State, Terminals)]}
	  end,
	  States),

    GroupOfState1 = refine_dfa_groups(GroupOfState, Transitions),

    DFA1 = lists:usort(
	     [{proplists:get_value(State0, GroupOfState1), {Symbol, proplists:get_value(State1, GroupOfState1)}}
	      || {State0, {Symbol, State1}} <- DFA ]),
    StatesOfGroup = [{G,S} || {S,G} <- GroupOfState1 ],
    Groups = lists:usort(proplists:get_keys(StatesOfGroup)),
    PowerSets1 =
	lists:map(
	  fun (G) ->
		  {G,
		   lists:umerge(
		     [ proplists:get_value(S, PowerSets)
		       || S <- proplists:get_all_values(G, StatesOfGroup) ])}
	  end,
	  Groups),
    {DFA1, PowerSets1}.


start_state(PowerSet) ->
    [S] = [K || {K,V} <- PowerSet, lists:member(0, V)],
    S.

match_state(PowerSet) ->
    [ {K, lists:usort([S || {rule, S} <- V])} || {K, V} <- PowerSet].

states(DFA) ->
    lists:usort([From || {From, _} <- DFA] ++ [To || {_, {_, To}} <- DFA]).


format_dfa(dot, DFA, PowerSet) ->
    lists:flatten(
      [ io_lib:format("~p -> ~p [label=\"~p\"];~n", [From, To, S]) || {From, {S, To}} <- DFA]);

format_dfa(python, DFA, PowerSet) ->
    lists:flatten(
      [io_lib:format("START = ~p~n~n", [start_state(PowerSet)]),
       "MATCH = {\n",
       string:join([ io_lib:format("  ~p: [~s]", [K, string:join([io_lib:format("~p", [S]) || S <- V], ", ")])
	 || {K, V} <- match_state(PowerSet)], ",\n"),
       "}\n\n",
       "DFA = {\n",
       string:join(
	 [io_lib:format(
	    "  ~p: {~s}",
	    [State, string:join([io_lib:format("~p: ~p", [S, To]) || {S, To} <- proplists:get_all_values(State, DFA) ], ", ")])
	  || State <- states(DFA)],
	 ",\n"),
       "}\n"
      ]);

format_dfa(ruby, DFA, PowerSet) ->
    lists:flatten(
      [
       io_lib:format("START = ~p~n~n", [start_state(PowerSet)]),
       "MATCH = {\n",
       string:join([ io_lib:format("  ~p=> [~s]", [K, string:join([io_lib:format("~p", [S]) || S <- V], ", ")])
	 || {K, V} <- match_state(PowerSet)], ",\n"),
       "}\n\n",
       "DFA = {\n",
       string:join(
	 [io_lib:format(
	    "  ~p=> {~s}",
	    [State, string:join([io_lib:format("~p=> ~p", [S, To]) || {S, To} <- proplists:get_all_values(State, DFA) ], ", ")])
	  || State <- states(DFA)],
	 ",\n"),
       "}\n"
      ]);
format_dfa(javascript, DFA, PowerSet) ->
    lists:flatten(
      [
       io_lib:format("{~n  START: ~w,~n", [start_state(PowerSet)]),
       "  MATCH: {\n",
       string:join(
	 [io_lib:format("    '~p': [~s]", [K, string:join([io_lib:format("~p", [S]) || S <- V], ", ")])
	  || {K, V} <- match_state(PowerSet) ], ",\n"),
       "},\n\n",
       "  DFA: {\n",
       string:join(
	 [io_lib:format(
	    "  '~p': {~s}",
	    [State, string:join( [ io_lib:format("'~p': ~p", [S, To]) || {S, To} <- proplists:get_all_values(State, DFA) ], ", ") ])
	     || State <- states(DFA) ],
	 ",\n"),
       "}\n}"
      ]).


group(code) ->
    lists:map(
      fun (C) ->
	      if (C == 9) or (C == 10) or (C == 13) or (C == 32) ->
		      0;
		 C == $, ->
		      1;
		 C == $; ->
		      2;
		 C == $| ->
		      3;
		 C == $( ->
		      4;
		 C == $) ->
		      5;
		 C == ${ ->
		      6;
		 C == $} ->
		      7;
		 C >= $A, C =< $Z ->
		      8;
		 C >= $a, C =< $z ->
		      9;
		 C == $_ ->
		      10;
		 C >= $0, C=< $9 ->
		      10;
		 true ->
		      -1
	      end
      end,
      lists:seq(0, 127)).


rule(code) ->
    %% var: [A-Z][_A-Za-z0-9]*
    %% atom: a-z[_A-Za-z0-9]*
    %%
    %% group  0: SPACE \r \n \t
    %% group  1: ,
    %% group  2: ;
    %% group  3: |
    %% group  4: (
    %% group  5: )
    %% group  6: {
    %% group  7: }
    %% group  8: A-Z
    %% group  9: a-z
    %% group 10: _0-9

    %% pseudo group
    %% group 11: proc
    %% group 12: when
    %% group 13: channel
    %% group 14: in
    %% group 15: send
    %% group 16: to
    %% group 17: recv
    %% group 18: from
    %% group 19: button
    %% group 20: attach
    %% group 21: at
    %% group 22: detach
    %% group 23: wait
    %% group 24: set
    %% group 25: title
    %% group 26: style

    [
     [[{'+', [[0]]}]],
     [[1]],
     [[2]],
     [[3]],
     [[4]],
     [[5]],
     [[6]],
     [[7]],
     [[8, {'*', [[8], [9], [10]]}]],
     [[9, {'*', [[8], [9], [10]]}]]
    ].


gen(Lang, Name) ->
    Rule = rule(Name),
    NFA = lex_rules_to_nfa(Rule),
    {DFA, PowerSet} = nfa2dfa(NFA),
    {DFA1, PowerSet1} = minimize_dfa(DFA, PowerSet),
    format_dfa(Lang, DFA1, PowerSet1).


gen() ->
    io:format("~s~n", [gen(javascript, code)]),
    io:format("~w~n", [group(code)]).
