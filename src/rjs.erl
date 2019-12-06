-module(rjs).
-compile([no_native]).

-on_load(init/0).
-export([encode/1, decode/1, decode/2]).

encode(Data) ->
    nif_encode(Data).

decode(Data) ->
    decode(Data, []).

decode(Bin, Opts) when is_binary(Bin) ->
    nif_decode(Bin, Opts);

decode(Data, Opts) ->
    Bin = erlang:iolist_to_binary(Data),
    nif_decode(Bin, Opts).

init() ->
    PrivDir = code:priv_dir(?MODULE),
    erlang:load_nif(filename:join(PrivDir, "crates/rjs/librjs"), 0).

-define(NOT_LOADED, not_loaded(?LINE)).

not_loaded(Line) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, Line}]}).

nif_encode(_Data) ->
    ?NOT_LOADED.

nif_decode(_Data, _Opts) ->
    ?NOT_LOADED.
