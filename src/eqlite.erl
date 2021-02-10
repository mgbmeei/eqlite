-module(eqlite).

-export([ init/0
        , init/1
        , get_query/1
        , get_info/1
        , list_queries/0
        ]).

-define(EQLITE_EXT, ".eqlite").
-define(EQLITE_TAB, eqlite_table).

init() ->
  init(default_script_directory()).

init(Directory) ->
  Table = new_table(?EQLITE_TAB),
  Files = find_eqlite_files(Directory),
  Queries = parse_eqlite_files(Files),
  file_eqlite_queries(Queries, Table).

get_query(Query) ->
  case ets:lookup(?EQLITE_TAB, Query) of
    [{Query, _Info, Statement}] -> Statement;
    _ -> undefined
  end.

get_info(Query) ->
  case ets:lookup(?EQLITE_TAB, Query) of
    [{Query, Info, _Statement}] -> Info;
    _ -> undefined
  end.

list_queries() ->
  Qs = ets:foldl(fun({Query, _, _}, Acc) -> [Query | Acc] end,
                 [], ?EQLITE_TAB),
  lists:sort(Qs).


%%%%%%%%%%%%%%%%%%%%%%%%
%% INTERNAL FUNCTIONS %%
%%%%%%%%%%%%%%%%%%%%%%%%


new_table(Name) ->
  case ets:info(Name) of
    undefined ->
      ets:new(Name, [named_table, set, public, {read_concurrency, true}]);
    _ ->
      Name
  end.

default_script_directory() ->
  code:priv_dir(eqlite).

find_eqlite_files(Directory) ->
  Wildcard = filename:join(Directory, "**/*" ++ ?EQLITE_EXT),
  filelib:wildcard(Wildcard).

parse_eqlite_files(Files) ->
  lists:foldl(fun(Filepath, Acc) ->
                  {ok, FileIO} = file:open(Filepath, [read]),
                  maps:merge(parse_file(FileIO), Acc) end,
              #{}, Files).

parse_file(FileIO) ->
  parse_line(FileIO, file:read_line(FileIO), #{}, []).

parse_line(FileIO, eof, Acc, _CurrentQuery) ->
  file:close(FileIO),
  Acc;

parse_line(FileIO, {ok, Line}, Acc, CurrentQuery) ->
  {NewCurrentQuery, NewAcc} =
    case string:trim(Line, leading) of
      "-- :" ++ Rest ->                         % query name
        [RawQueryName | Info] = string:split(Rest, " "),
        QueryName = list_to_atom(string:trim(RawQueryName)),
        NewMap = maps:put(QueryName, #{ data => [],
                                        info => string:trim(Info) }, Acc),
        {QueryName, NewMap};
      "--" ++ _ ->                              % comment line
        {CurrentQuery, Acc};
      "" ->                                     % blank line
        {CurrentQuery, Acc};
      Line ->                                   % line of code
        CurrentQueryMap = maps:get(CurrentQuery, Acc),
        CurrentLines = maps:get(data, CurrentQueryMap, []),
        NewLines = CurrentLines ++ Line,
        NewQueryMap = maps:put(data, NewLines, CurrentQueryMap),
        {CurrentQuery, maps:put(CurrentQuery, NewQueryMap, Acc)}
      end,
  parse_line(FileIO, file:read_line(FileIO), NewAcc, NewCurrentQuery).


%% Given a map of queries like
%%   #{ query_name => #{info => [], data => []}
%% insert {query_name, info, data} into the ETS table.
file_eqlite_queries(Queries, Table) ->
  Iterator = maps:iterator(Queries),
  query_iterator(maps:next(Iterator), Table).

query_iterator(none, _Table) ->
  ok;
query_iterator({K, V, Iterator}, Table) ->
  ets:insert(Table, {K, maps:get(info, V, []), maps:get(data, V, [])}),
  query_iterator(maps:next(Iterator), Table).
