-module(rebar3_archive_plugin).

-export([init/1,
         do/1,
         format_error/1]).

-define(PROVIDER, archive).
-define(DEPS, [compile]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
  Provider = providers:create([
                               {name, ?PROVIDER},
                               {module, ?MODULE},
                               {bare, true},
                               {deps, ?DEPS},
                               {example, "rebar3 archive"},
                               {opts, []},
                               {short_desc, "Generate ez archive."},
                               {desc, desc()}
                              ]),
  {ok, rebar_state:add_provider(State, Provider)}.

desc() ->
  "Generate Erlang code archive http://erlang.org/doc/man/code.html".

do(State) ->
  Providers = rebar_state:providers(State),
  Cwd = rebar_state:dir(State),
  rebar_hooks:run_project_and_app_hooks(Cwd, pre, ?PROVIDER, Providers, State),
  rebar_api:info("Building archive...", []),
  Res = case rebar_state:project_apps(State) of
          [App] ->
            archive(State, App);
          _ ->
            {error, {?MODULE, no_main_app}}
        end,
  rebar_hooks:run_project_and_app_hooks(Cwd, post, ?PROVIDER, Providers, State),
  Res.

archive(State, App) ->
  Vsn = rebar_app_info:original_vsn(App),
  Name = rebar_app_info:name(App),

  Dir = to_string([Name, "-", Vsn]),

  Priv = filelib:wildcard(filename:join(rebar_app_info:priv_dir(App), "**/*"), file),
  Ebin = filelib:wildcard(filename:join(rebar_app_info:ebin_dir(App), "*.{beam,app}"), file),

  Files = to_list(Dir, "ebin", Ebin) ++ to_list(Dir, "priv", Priv),

  {ok, _} = zip:create(Dir ++ ".ez", Files),

  {ok, State}.

to_list(Dir, Type, Files) ->
  lists:filtermap(fun(File) ->
                      case file:read_file(File) of
                        {ok, Bin} ->
                          Split = lists:reverse(filename:split(File)),
                          TypeIndex = string:str(Split, [Type]),
                          Path = filename:join(lists:reverse(lists:sublist(Split, TypeIndex))),
                          {true, {filename:join([Dir, Path]), Bin}};
                        {error, Reason} ->
                          rebar_api:warn("Can't read file: ~p", [Reason]),
                          false
                      end
                  end, Files).

to_string(List) ->
  binary_to_list(iolist_to_binary(List)).

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
