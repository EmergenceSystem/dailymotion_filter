%%%-------------------------------------------------------------------
%%% @doc Dailymotion video search agent using the public API.
%%%
%%% Announces capabilities to em_disco on startup and maintains a
%%% memory of URLs already returned so duplicate videos across
%%% successive queries are filtered out.
%%%
%%% Handler contract: `handle/2' (Body, Memory) -> {RawList, NewMemory}.
%%% Memory schema: `#{seen => #{binary_url => true}}'.
%%% @end
%%%-------------------------------------------------------------------
-module(dailymotion_filter_app).
-behaviour(application).

-export([start/2, stop/1]).
-export([handle/2]).

-define(API_URL, "https://api.dailymotion.com/videos").
-define(FIELDS,  "id,title,url").
-define(LIMIT,   20).

-define(CAPABILITIES, [
    <<"dailymotion">>,
    <<"video">>,
    <<"media">>
]).

%%====================================================================
%% Application behaviour
%%====================================================================

start(_StartType, _StartArgs) ->
    em_filter:start_agent(dailymotion_filter, ?MODULE, #{
        capabilities => ?CAPABILITIES,
        memory       => ets
    }).

stop(_State) ->
    em_filter:stop_agent(dailymotion_filter).

%%====================================================================
%% Agent handler
%%====================================================================

handle(Body, Memory) when is_binary(Body) ->
    Seen    = maps:get(seen, Memory, #{}),
    Embryos = generate_embryo_list(Body),
    Fresh   = [E || E <- Embryos, not maps:is_key(url_of(E), Seen)],
    NewSeen = lists:foldl(fun(E, Acc) ->
        Acc#{url_of(E) => true}
    end, Seen, Fresh),
    {Fresh, Memory#{seen => NewSeen}};

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
            Value   = binary_to_list(maps:get(<<"value">>,   Map, <<"">>)),
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

-spec url_of(map()) -> binary().
url_of(#{<<"properties">> := #{<<"url">> := Url}}) -> Url;
url_of(_) -> <<>>.
