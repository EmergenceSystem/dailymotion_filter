%%%-------------------------------------------------------------------
%%% @doc Dailymotion video search agent.
%%%
%%% Deduplication by URL is handled upstream by the Emquest pipeline.
%%%
%%% === Capability cascade ===
%%%
%%%   base_capabilities/0 extends em_filter:base_capabilities().
%%%
%%% Handler contract: handle/2 (Body, Memory) -> {RawList, Memory}.
%%% @end
%%%-------------------------------------------------------------------
-module(dailymotion_filter_app).
-behaviour(application).

-export([start/2, stop/1]).
-export([handle/2, base_capabilities/0]).

-define(API_URL, "https://api.dailymotion.com/videos").
-define(FIELDS,  "id,title,url").
-define(LIMIT,   20).

%%====================================================================
%% Capability cascade
%%====================================================================

-spec base_capabilities() -> [binary()].
base_capabilities() ->
    em_filter:base_capabilities() ++ [<<"dailymotion">>, <<"video">>, <<"media">>].

%%====================================================================
%% Application behaviour
%%====================================================================

start(_StartType, _StartArgs) ->
    em_filter:start_agent(dailymotion_filter, ?MODULE, #{
        capabilities => base_capabilities()
    }),
    {ok, self()}.

stop(_State) ->
    em_filter:stop_agent(dailymotion_filter).

%%====================================================================
%% Agent handler
%%====================================================================

handle(Body, Memory) when is_binary(Body) ->
    {generate_embryo_list(Body), Memory};
handle(_Body, Memory) ->
    {[], Memory}.

%%====================================================================
%% Search and processing
%%====================================================================

generate_embryo_list(JsonBinary) ->
    {Value, Timeout} = extract_params(JsonBinary),
    Url = lists:concat([
        ?API_URL,
        "?search=", uri_string:quote(Value),
        "&fields=", ?FIELDS,
        "&limit=",  integer_to_list(?LIMIT)
    ]),
    case httpc:request(get, {Url, []},
                       [{timeout, Timeout * 1000}],
                       [{body_format, binary}]) of
        {ok, {{_, 200, _}, _, RespBody}} ->
            parse_api_response(RespBody);
        _ ->
            []
    end.

extract_params(JsonBinary) ->
    try json:decode(JsonBinary) of
        Map when is_map(Map) ->
            Value   = binary_to_list(maps:get(<<"value">>, Map,
                          maps:get(<<"query">>, Map, <<"">>))),
            Timeout = case maps:get(<<"timeout">>, Map, undefined) of
                undefined            -> 10;
                T when is_integer(T) -> T;
                T when is_binary(T)  -> binary_to_integer(T)
            end,
            {Value, Timeout};
        _ ->
            {binary_to_list(JsonBinary), 10}
    catch
        _:_ -> {binary_to_list(JsonBinary), 10}
    end.

parse_api_response(Body) ->
    try json:decode(Body) of
        #{<<"list">> := Videos} when is_list(Videos) ->
            lists:filtermap(fun build_embryo/1, Videos);
        _ ->
            []
    catch
        _:_ -> []
    end.

build_embryo(#{<<"url">> := Url, <<"title">> := Title})
        when is_binary(Url), is_binary(Title), byte_size(Title) > 0 ->
    {true, #{
        <<"properties">> => #{
            <<"url">>    => Url,
            <<"resume">> => Title
        }
    }};
build_embryo(_) ->
    false.
