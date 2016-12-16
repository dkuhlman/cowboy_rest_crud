#!/usr/bin/env escript

%%
%% synopsis:
%%     Create new records and state DETS files.
%% usage:
%%     create_table.escript <db-file-stem>
%% example:
%%     # create Data/test01_records.dets and Data/test01_state.dets 
%%     create_table.escript Data/test01
%%     

main([Filename]) ->
    Filename1 = io_lib:format("~s_records.dets", [Filename]),
    Filename2 = io_lib:format("~s_state.dets", [Filename]),
    dets:open_file(record_tab, [{file, Filename1}, {type, set}]),
    dets:open_file(record_state_tab, [{file, Filename2}, {type, set}]),
    dets:insert(record_state_tab, {current_id, 1}),
    dets:close(record_state_tab),
    dets:close(record_tab);
main(["--help"]) ->
    usage();
main([]) ->
    usage().

usage() ->
    io:fwrite("usage: create_table.escript <db-file-stem>~n", []).
