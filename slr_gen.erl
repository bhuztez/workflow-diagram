-module(slr_gen).

-export([gen/0]).

udelta([], Delta, _Found) ->
    Delta;
udelta([Head|Tail], Delta, Found) ->
    Delta1 =
	case lists:member(Head, Delta) or lists:member(Head, Found) of
	    true ->
		Delta;
	    false ->
		Delta ++ [Head]
	end,
    udelta(Tail, Delta1, Found).


iterate(Fun, Delta, Found) ->
    case udelta(Fun(Delta), [], Found) of
	[] ->
	    Found;
	New ->
	    iterate(Fun, New, Found ++ New)
    end.

iterate(Fun, Items) ->
    iterate(Fun, Items, Items).

closure(ItemSet, Grammar) ->
    iterate(
      fun (Delta) ->
	      lists:append(
		lists:map(
		  fun
		      ({_, _, [{nt, Name}|_]}) ->
			  lists:map(
			    fun (P) -> {Name, P, P} end,
			    proplists:get_all_values(Name, Grammar));
		      ({_, _, _}) ->
			  []
		  end,
		  Delta))
      end,
      ItemSet).


first({t, T}, _Grammar) ->
    [{t, T}];
first({nt, NT}, Grammar) ->
    lists:append(
      lists:map(
	fun
	    ({_, [{t, T}|_], _}) ->
		       [{t, T}];
	    ({_, _, _}) ->
		       []
	       end,
	closure(
	  lists:map(
	    fun (Prod) -> {NT, Prod, Prod} end,
	    proplists:get_all_values(NT, Grammar)),
	  Grammar))).


goto(ItemSet, Symbol, Grammar) ->
    closure(
      lists:append(
	lists:map(
	  fun
	      ({Name, Prod, [Head|Tail]})
		when Head == Symbol ->
			 [{Name, Prod, Tail}];
	      ({_, _, _}) ->
			 []
		 end,
	  ItemSet)),
      Grammar).


next_syms(ItemSet) ->
    lists:foldl(
      fun
	  ({_, _, []}, List) ->
		       List;
	  ({_, _, [Head|_]}, List) ->
		       case lists:member(Head, List) of
			   true ->
			       List;
			   false ->
			       List ++ [Head]
		       end
	       end,
      [],
      ItemSet).


goto(ItemSet, Grammar) ->
    lists:map(
      fun (Symbol) ->
	      goto(ItemSet, Symbol, Grammar)
      end,
      next_syms(ItemSet)).


items(Grammar) ->
    {prime, Prod} = proplists:lookup(prime, Grammar),
    C = closure([{prime, Prod, Prod}], Grammar),
    iterate(
      fun (Delta) ->
	      lists:append(
		lists:map(
		  fun (ItemSet) -> goto(ItemSet, Grammar) end,
		  Delta))
      end,
      [C]).


index_of(_E, _N, []) ->
    error;
index_of(E, N, [E|_]) ->
    N;
index_of(E, N, [_|T]) ->
    index_of(E, N+1, T).

index_of(Elem, List) ->
    index_of(Elem, 0, List).



follow(Name, Grammar) ->
    Names =
	iterate(
	  fun (Delta) ->
		  lists:map(
		    fun ({K,_}) -> K end,
		    lists:filter(
		      fun ({_, Prod}) ->
			      case lists:last(Prod) of
				  {nt, N} ->
				      lists:member(N, Delta);
				  _ ->
				      false
			      end
		      end,
		      Grammar))
	  end,
	  [Name]),

    Symbols =
	lists:append(
	  lists:map(
	    fun ({_, Prod}) ->
		    L = length(Prod),
		    lists:append(
		      [ first(Rule, Grammar) ||
			  {{nt, N}, Rule} <- lists:zip(lists:sublist(Prod, 1, L-1), lists:sublist(Prod, 2, L)),
			  lists:member(N, Names) ])
	    end,
	    Grammar)),

    case lists:member(prime, Names) of
	true ->
	    Symbols ++ [{t, -1}];
	false ->
	    Symbols
    end.


shift_table(ItemSet, ItemSets, Grammar) ->
    I = index_of(ItemSet, ItemSets),
    lists:append(
      lists:map(
	fun
	    ({t, _}=Symbol) ->
		J = index_of(goto(ItemSet, Symbol, Grammar), ItemSets),
		[{I, {Symbol, {shift, J}}}];
	    (_) ->
		[]
	end,
	next_syms(ItemSet))).


goto_table(ItemSet, ItemSets, Grammar) ->
    I = index_of(ItemSet, ItemSets),
    lists:append(
      lists:map(
	fun
	    ({nt, _}=Symbol) ->
		J = index_of(goto(ItemSet, Symbol, Grammar), ItemSets),
		[{I, {Symbol, {goto, J}}}];
	    (_) ->
		[]
	end,
	next_syms(ItemSet))).


reduce_table(ItemSet, ItemSets, Grammar) ->
    I = index_of(ItemSet, ItemSets),

    lists:append(
      lists:map(
	fun
	    ({Name, Prod, []}) ->
		lists:map(
		  fun (Symbol) ->
			  {I, {Symbol, {reduce, index_of({Name, Prod}, Grammar)}}}
		  end,
		  follow(Name, Grammar));
	    (_) ->
		[]
	end,
	ItemSet)).


action_table(ItemSets, Grammar) ->
    lists:append(
      lists:map(
	fun (ItemSet) ->
		lists:append(
		  [shift_table(ItemSet, ItemSets, Grammar),
		   reduce_table(ItemSet, ItemSets, Grammar),
		   goto_table(ItemSet, ItemSets, Grammar)])
	end,
	ItemSets)).


grammar(code) ->
    [{prime,     [{nt, 'Pdefs'}]},
     {'Pdefs',   [{nt, 'Pdefs'}, {nt, 'Pdef'}]},
     {'Pdefs',   [{nt, 'Pdef'}]},
     {'Pdef',    [{t, 11}, {nt, 'Invoke'}, {t, 6}, {nt, 'Seq'}, {t, 7}]},
     {'P',       [{nt, 'Invoke'}]},
     {'P',       [{t, 12}, {nt, 'Invoke'}]},
     {'P',       [{t, 13}, {nt, 'Varlist'}, {t, 14}, {t, 6}, {nt, 'Seq'}, {t, 7}]},
     {'P',       [{t, 19}, {nt, 'Varlist'}, {t, 14}, {t, 6}, {nt, 'Seq'}, {t, 7}]},
     {'P',       [{t, 15}, {t, 8}, {t, 16}, {t, 8}]},
     {'P',       [{t, 17}, {t, 8}, {t, 18}, {t, 8}]},
     {'P',       [{t, 20}, {t, 8}, {t, 21}, {t, 9}]},
     {'P',       [{t, 22}, {t, 8}]},
     {'P',       [{t, 23}, {t, 8}]},
     {'P',       [{t, 24}, {t, 8}, {t, 25}, {t, 9}]},
     {'P',       [{t, 24}, {t, 8}, {t, 26}, {t, 9}]},
     {'P',       [{t, 6}, {nt, 'Seq'}, {t, 7}]},

     {'Seq',     [{nt, 'Branch'}, {t, 2}, {nt, 'Seq'}]},
     {'Seq',     [{nt, 'Branch'}]},

     {'Branch',  [{nt, 'P'}, {t, 3}, {nt, 'Branch'}]},
     {'Branch',  [{nt, 'P'}]},

     {'Invoke',  [{t, 9}, {t, 4}, {t, 5}]},
     {'Invoke',  [{t, 9}, {t, 4}, {nt, 'Varlist'}, {t, 5}]},
     {'Varlist', [{nt, 'Varlist'}, {t, 1}, {t, 8}]},
     {'Varlist', [{t, 8}]}
    ].


gen(Name, Lang) ->
    Grammar = grammar(Name),

    C = items(Grammar),
    Table = action_table(C, Grammar),
    States = lists:seq(0, length(C)-1),

    ActionTable =
	lists:map(
	 fun (State) ->
		 {State, [{T, Action} || {{t, T}, Action} <- proplists:get_all_values(State, Table)]}
	 end,
	 States),

    GotoTable =
	lists:map(
	  fun (State) ->
		 {State, [{NT, Goto} || {{nt, NT}, Goto} <- proplists:get_all_values(State, Table)]}
	  end,
	  States),

    format_slr(Lang, ActionTable, GotoTable).


format_slr(python, ActionTable, GotoTable) ->
    lists:flatten(
      ["ACTIONS = [\n",
       string:join(
	 [ io_lib:format(
	     "    {~s}",
	     [string:join(
		[io_lib:format(
		   "~p: ~s",
		   [Symbol,
		    case Action of
			{shift, N} ->
			    io_lib:format("('shift', ~p)", [N]);
			{reduce, N} ->
			    io_lib:format("('reduce', ~p)", [N])
		    end])
		 || {Symbol, Action} <- Actions], ", ")])
	   || {State, Actions} <- ActionTable ],
	",\n"),
       "]\n\n",
       "GOTOS = [\n",
       string:join(
	 [ io_lib:format(
	     "    {~s}",
	     [string:join(
		[io_lib:format(
		   "~p: ~s",
		   [Symbol,
		    case Goto of
			{goto, N} ->
			    io_lib:format("('goto', ~p)", [N])
		    end])
		 || {Symbol, Goto} <- Gotos], ", ")])
	   || {State, Gotos} <- GotoTable ],
	 ",\n"),
       "]\n\n"
      ]);
format_slr(ruby, ActionTable, GotoTable) ->
    lists:flatten(
      ["ACTIONS = [\n",
       string:join(
	 [ io_lib:format(
	     "    {~s}",
	     [string:join(
		[io_lib:format(
		   "~p => ~s",
		   [Symbol,
		    case Action of
			{shift, N} ->
			    io_lib:format("[:shift, ~p]", [N]);
			{reduce, N} ->
			    io_lib:format("[:reduce, ~p]", [N])
		    end])
		 || {Symbol, Action} <- Actions], ", ")])
	   || {State, Actions} <- ActionTable ],
	",\n"),
       "]\n\n",
       "GOTOS = [\n",
       string:join(
	 [ io_lib:format(
	     "    {~s}",
	     [string:join(
		[io_lib:format(
		   ":~s => ~s",
		   [Symbol,
		    case Goto of
			{goto, N} ->
			    io_lib:format("[:goto, ~p]", [N])
		    end])
		 || {Symbol, Goto} <- Gotos], ", ")])
	   || {State, Gotos} <- GotoTable ],
	 ",\n"),
       "]\n\n"
      ]);
format_slr(javascript, ActionTable, GotoTable) ->
    lists:flatten(
      ["ACTIONS: [\n",
       string:join(
	 [ io_lib:format(
	     "    {~s}",
	     [string:join(
		[io_lib:format(
		   "'~p': ~s",
		   [Symbol,
		    case Action of
			{shift, N} ->
			    io_lib:format("['shift', ~p]", [N]);
			{reduce, N} ->
			    io_lib:format("['reduce', ~p]", [N])
		    end])
		 || {Symbol, Action} <- Actions], ", ")])
	   || {State, Actions} <- ActionTable ],
	",\n"),
       "],\n\n",
       "GOTOS: [\n",
       string:join(
	 [ io_lib:format(
	     "    {~s}",
	     [string:join(
		[io_lib:format(
		   "'~s': ~s",
		   [Symbol,
		    case Goto of
			{goto, N} ->
			    io_lib:format("['goto', ~p]", [N])
		    end])
		 || {Symbol, Goto} <- Gotos], ", ")])
	   || {State, Gotos} <- GotoTable ],
	 ",\n"),
       "],\n\n"
      ]).

format_grammar(python, Grammar) ->
    lists:flatten(
      [
       "GRAMMAR = [\n",
       string:join(
	 [
	  io_lib:format(
	    "    ('~s', [~s])",
	    [Name,
	     string:join(
	       [
		io_lib:format(
		  case N of
		      N when is_integer(N) ->
			  "('~s', ~w)";
		      N ->
			  "('~s', '~s')"
		  end,
		  [T, N])
		|| {T, N} <- Prod],
	       ", ")
	    ])
	  || {Name, Prod} <- Grammar ],
	 ",\n"),
       "]\n\n"
      ]);
format_grammar(ruby, Grammar) ->
    lists:flatten(
      [
       "GRAMMAR = [\n",
       string:join(
	 [
	  io_lib:format(
	    "    [:~s, [~s]]",
	    [Name,
	     string:join(
	       [
		io_lib:format(
		  case N of
		      N when is_integer(N) ->
			  "[:~s, ~w]";
		    N ->
			  "[:~s, :~s]"
		  end,
		  [T, N])
	      || {T, N} <- Prod],
	       ", ")
	    ])
	  || {Name, Prod} <- Grammar ],
	 ",\n"),
       "]\n\n"
      ]);
format_grammar(javascript, Grammar) ->
    lists:flatten(
      [
       "GRAMMAR: [\n",
       string:join(
	 [
	  io_lib:format(
	    "    ['~s', [~s]]",
	    [Name,
	     string:join(
	       [
		io_lib:format(
		  case N of
		      N when is_integer(N) ->
			  "['~s', ~w]";
		      N ->
			  "['~s', '~s']"
		  end,
		  [T, N])
		|| {T, N} <- Prod],
	       ", ")
	    ])
	  || {Name, Prod} <- Grammar ],
	 ",\n"),
       "]\n\n"
      ]).



gen() ->
    io:format("~s~n", [gen(code, javascript)]),
    io:format("~s~n", [format_grammar(javascript, grammar(code))]).
