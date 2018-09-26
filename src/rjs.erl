-module(rjs).
-compile([no_native]).

-on_load(init/0).
-export([encode/1, decode/1]).

encode(Data) ->
    nif_encode(Data).

decode(Data) when is_binary(Data) ->
    nif_decode(Data);
decode(Data) ->
    Bin = erlang:iolist_to_binary(Data),
    nif_decode(Bin).

init() ->
    PrivDir = code:priv_dir(?MODULE),
    erlang:load_nif(filename:join(PrivDir, "crates/rjs/librjs"), 0).

-define(NOT_LOADED, not_loaded(?LINE)).

not_loaded(Line) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, Line}]}).

nif_encode(_Data) ->
    ?NOT_LOADED.

nif_decode(_Data) ->
    ?NOT_LOADED.
