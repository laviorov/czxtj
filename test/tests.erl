-module(tests).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

forward(Name)->
    application:ensure_all_started(czxtj),
    N = atom_to_list(Name),
    {ok, S} = file:read_file("test/data/"++N++".xml"),
    {ok, T} = file:read_file("test/data/"++N++".json"),
    czxtj:xml2json(S) =:= T.
    
backward(Name)->
    application:ensure_all_started(czxtj),
    N = atom_to_list(Name),
    {ok, S} = file:read_file("test/data/"++N++".json"),
    {ok, T} = file:read_file("test/data/"++N++".xml"),
    czxtj:json2xml(S) =:= T.


all_test_()->
     [?_assert(forward(forward)),
      ?_assert(forward(encoding)),
      ?_assert(backward(back)),
      ?_assert(backward(backroot))].
