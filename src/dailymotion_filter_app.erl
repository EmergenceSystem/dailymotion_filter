%%%-------------------------------------------------------------------
%%% @doc Dailymotion video search filter using the public API.
%%%
%%% Uses the Dailymotion API endpoint:
%%%   GET https://api.dailymotion.com/videos
%%%       ?search=<query>&fields=id,title,url&limit=20
%%%
%%% Returns a list of embryo maps with the video URL and title.
%%% @end
%%%-------------------------------------------------------------------
-module(dailymotion_filter_app).
-behaviour(application).

-export([start/2, stop/1]).
-export([handle/1]).

-define(API_URL, "https://api.dailymotion.com/videos").
-define(FIELDS,  "id,title,url").
-define(LIMIT,   20).

%%====================================================================
%% Application behaviour
%%====================================================================

start(_StartType, _StartArgs) ->
    em_filter:start_filter(dailymotion_filter, ?MODULE).

stop(_State) ->
    em_filter:stop_filter(dailymotion_filter).

%%====================================================================
%% Filter handler — returns a list of embryo maps
%%====================================================================

handle(Body) when is_binary(Body) ->
    generate_embryo_list(Body);
handle(_) ->
    [].

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

%%--------------------------------------------------------------------
%% API response parsing
%%
%% Dailymotion returns:
%%   { "list": [ {"id":"x...", "title":"...", "url":"https://..."}, ... ] }
%%--------------------------------------------------------------------

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
