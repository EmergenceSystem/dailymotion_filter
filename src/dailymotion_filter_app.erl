-module(dailymotion_filter_app).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
%% Handler callbacks
-export([handle/1]).

-define(SEARCH_URL, "https://www.dailymotion.com/search/").

%%% ===================================================================
%%% Application behavior
%%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, Port} = em_filter:find_port(),
    em_filter_sup:start_link(dailymotion_filter, ?MODULE, Port).

stop(_State) ->
    ok.

%% @doc Handle incoming requests from the filter server.
%% This function is called by em_filter_server through Wade.
%% @param Body The request body (JSON binary or string)
%% @return JSON response as binary or string
handle(Body) when is_binary(Body) ->
    handle(binary_to_list(Body));

handle(Body) when is_list(Body) ->
    io:format("Bing Filter received body: ~p~n", [Body]),
    EmbryoList = generate_embryo_list(list_to_binary(Body)),
    Response = #{embryo_list => EmbryoList},
    jsone:encode(Response);

handle(_) ->
    jsone:encode(#{error => <<"Invalid request body">>}).

%%% ===================================================================
%%% Private functions
%%% ===================================================================

generate_embryo_list(JsonBinary) ->
    try
        case jsone:decode(JsonBinary, [{keys, atom}]) of
            Search when is_map(Search) ->
                Value = binary_to_list(maps:get(value, Search, <<"">>)),
                TimeoutBin = maps:get(timeout, Search, <<"10">>),
                Timeout = parse_timeout(TimeoutBin),
                EncodedSearch = uri_string:quote(Value),
                SearchUrl = lists:concat([?SEARCH_URL, EncodedSearch, "/videos"]),
                io:format("[DM] Performing search with URL: ~s~n", [SearchUrl]),
                Headers = [{"User-Agent", "Mozilla/5.0"}],
                case httpc:request(get, {SearchUrl, Headers}, [{timeout, Timeout * 1000}], [{body_format, binary}]) of
                    {ok, {{_, 200, _}, _, Body}} ->
                        io:format("[DM] HTTP 200 received. Body length: ~p bytes~n", [byte_size(Body)]),
                        extract_video_links_from_html(Body, Timeout);
                    {ok, {{_, StatusCode, StatusText}, _, _}} ->
                        io:format("[DM][ERROR] Dailymotion HTTP error ~p ~s~n", [StatusCode, StatusText]),
                        [];
                    {error, Reason} ->
                        io:format("[DM][ERROR] HTTP request failed: ~p~n", [Reason]),
                        []
                end;
            _ ->
                io:format("[DM][ERROR] Invalid JSON structure in request~n", []),
                []
        end
    catch
        ErrorType:ErrorReason ->
            io:format("[DM][ERROR] generate_embryo_list: ~p:~p~n", [ErrorType, ErrorReason]),
            []
    end.

parse_timeout(Bin) when is_binary(Bin) ->
    try list_to_integer(binary_to_list(Bin)) catch _:_ -> 10 end;
parse_timeout(Int) when is_integer(Int) -> Int;
parse_timeout(_) -> 10.

%% --- Extraction principale adaptée à la structure réelle Dailymotion ---
extract_video_links_from_html(Html, TimeoutSecs) ->
    try
        io:format("[DM] HTML parsing...~n", []),
        ParsedHtml = mochiweb_html:parse(Html),
        io:format("[DM] HTML parsed. Preview: ~s~n", [binary:part(Html, 0, min(1000, byte_size(Html)))]),
        StartTime = erlang:system_time(millisecond),
        Timeout = TimeoutSecs * 1000,
        VideoCards = find_video_cards_in_dom(ParsedHtml),
        io:format("[DM] Found ~p video cards in DOM~n", [length(VideoCards)]),
        VideoLinks = [find_first_video_link_in_card(Card) || Card <- VideoCards],
        ValidLinks = [L || L <- VideoLinks, L =/= none],
        io:format("[DM] Found ~p valid video links in video cards~n", [length(ValidLinks)]),
        Embryos = process_video_links(ValidLinks, StartTime, Timeout, []),
        io:format("[DM] Successfully extracted ~p video embryos~n", [length(Embryos)]),
        Embryos
    catch
        ErrorType:ErrorReason:Stack ->
            io:format("[DM][ERROR] extract_video_links_from_html: ~p:~p~n~p~n", [ErrorType, ErrorReason, Stack]),
            []
    end.

%% Trouve tous les <div data-testid="video-card"> dans le DOM
find_video_cards_in_dom(Node) ->
    find_video_cards_in_dom(Node, []).

find_video_cards_in_dom({<<"div">>, Attrs, Children}, Acc) ->
    case lists:keyfind(<<"data-testid">>, 1, Attrs) of
        {<<"data-testid">>, <<"video-card">>} ->
            [{<<"div">>, Attrs, Children} | lists:foldl(fun(C, A) -> find_video_cards_in_dom(C, A) end, Acc, Children)];
        _ ->
            lists:foldl(fun(C, A) -> find_video_cards_in_dom(C, A) end, Acc, Children)
    end;
find_video_cards_in_dom({_Tag, _Attrs, Children}, Acc) when is_list(Children) ->
    lists:foldl(fun(C, A) -> find_video_cards_in_dom(C, A) end, Acc, Children);
find_video_cards_in_dom(_Other, Acc) -> Acc.

%% Trouve le premier <a href="/video/..."> descendant d'un video-card
find_first_video_link_in_card({_, _, Children}) ->
    find_first_video_link_in_card_rec(Children);
find_first_video_link_in_card(_) -> none.

find_first_video_link_in_card_rec([]) -> none;
find_first_video_link_in_card_rec([{<<"a">>, Attrs, AChildren}|Rest]) ->
    case lists:keyfind(<<"href">>, 1, Attrs) of
        {<<"href">>, Link} when is_binary(Link) ->
            case binary:match(Link, <<"/video/">>) of
                {0, _Len} ->
                    {<<"a">>, Attrs, AChildren};
                _ ->
                    find_first_video_link_in_card_rec(Rest)
            end;
        _ ->
            find_first_video_link_in_card_rec(Rest)
    end;

find_first_video_link_in_card_rec([{_Tag, _Attrs, Children}|Rest]) ->
    case find_first_video_link_in_card_rec(Children) of
        none -> find_first_video_link_in_card_rec(Rest);
        Res -> Res
    end;
find_first_video_link_in_card_rec([_Other|Rest]) ->
    find_first_video_link_in_card_rec(Rest).

%% Transforme chaque <a> en embryo, avec timeout
process_video_links([], _StartTime, _Timeout, Acc) ->
    lists:reverse(Acc);
process_video_links([LinkElement | Rest], StartTime, Timeout, Acc) ->
    CurrentTime = erlang:system_time(millisecond),
    case CurrentTime - StartTime >= Timeout of
        true ->
            io:format("[DM] Processing timeout reached, returning ~p results~n", [length(Acc)]),
            lists:reverse(Acc);
        false ->
            case create_video_embryo_safe(LinkElement) of
                skip -> process_video_links(Rest, StartTime, Timeout, Acc);
                Embryo -> process_video_links(Rest, StartTime, Timeout, [Embryo | Acc])
            end
    end.

create_video_embryo_safe({<<"a">>, Attrs, Children}) ->
    try
        case lists:keyfind(<<"href">>, 1, Attrs) of
            {<<"href">>, Link} when is_binary(Link), Link =/= <<>> ->
                Title = extract_text_content_safe(Children),
                io:format("[DM] Video: ~s | Title: ~s~n", [Link, Title]),
                #{
                    properties => #{
                        <<"url">> => <<"https://www.dailymotion.com", Link/binary>>,
                        <<"resume">> => Title
                    }
                };
            _ -> skip
        end
    catch
        _:_ -> skip
    end;
create_video_embryo_safe(_) -> skip.

extract_text_content_safe(Elements) ->
    try extract_text_content_safe(Elements, <<>>) catch _:_ -> <<>> end.

extract_text_content_safe([], Acc) -> string:trim(Acc);
extract_text_content_safe([TextNode|Rest], Acc) when is_binary(TextNode) ->
    extract_text_content_safe(Rest, << Acc/binary, TextNode/binary >>);
extract_text_content_safe([{_Tag, _Attrs, Children}|Rest], Acc) when is_list(Children) ->
    ChildText = extract_text_content_safe(Children, <<>>),
    extract_text_content_safe(Rest, << Acc/binary, ChildText/binary >>);
extract_text_content_safe([_Other|Rest], Acc) ->
    extract_text_content_safe(Rest, Acc).

