-module(rest_update_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    Dispatch = cowboy_router:compile([
        {'_', [
               {"/list", db_update_handler, [list]},
               {"/get/:record_id", db_update_handler, [get]},
               {"/create", db_update_handler, [create]},
               {"/update/:record_id", db_update_handler, [update]},
               {"/delete/:record_id", db_update_handler, [delete]},
               {"/help", db_update_handler, [help]},
               {"/", db_update_handler, [help]}
              ]}
    ]),
    {ok, _} = cowboy:start_clear(my_http_listener, 100,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),
%~     {ok, Recordsfilename} = application:get_env(rest_update, records_file_name),
%~     {ok, Statefilename} = application:get_env(rest_update, state_file_name),
%~     dets:open_file(records_db, [{file, Recordsfilename}, {type, set}]),
%~     dets:open_file(state_db, [{file, Statefilename}, {type, set}]),
	rest_update_sup:start_link().

stop(_State) ->
    io:fwrite("Stopping and closing db~n", []),
%~     dets:close(records_db),
%~     dets:close(state_db),
	ok.
