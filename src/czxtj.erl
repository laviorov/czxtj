-module(czxtj).
-compile(export_all).

-spec xml2json(binary()) -> binary().
xml2json(Xmlb)->
    try erlsom:simple_form(Xmlb, [{output_encoding, 'utf8'}, {nameFun, fun name_to_binary/3}]) of
        {ok, Xml, _Rest} ->
            jiffy:encode(convert_xml(Xml))
    catch
        {error,"Encoding CP-1250 not supported"} ->
            xml2json(iconv(Xmlb, <<"utf8">>))
    end.

name_to_binary(Name, _, _) -> 
    erlang:list_to_binary(Name). 

iconv(B, _) ->
   error("not impl"). 

convert_xml(E={Name, _Attrs, _Childs})->
    {[{Name, convert_xml_element(E)}]}.

convert_xml_element({_Name, [], [Text]}) when is_binary(Text) ->
    Text;
convert_xml_element({_Name, Attrs, Children})->
   {lists:map(fun attr_name_to_json/1, Attrs) ++ convert_children(Children)}. 

attr_name_to_json({K, V}) -> {<<"@", K/binary>>, V}.
   
%% convert_children(Children)-> 
%%     Grouped = lists:foldl(
%%                 fun (B, M) when is_binary(B) -> map_list_append(<<"#text">>, B, M);
%%                     (E={Name, _, _}, M) -> map_list_append(Name, convert_xml_element(E), M)
%%                 end, #{}, Children),
%%     Res = maps:fold(fun
%%                   (K, [E], M) -> M#{K => E};
%%                   (K, L, M) when is_list(L) -> M#{K => lists:reverse(L)}
%%               end, #{}, Grouped),
%%     maps:to_list(Res).
convert_children(Children)-> 
    Grouped = lists:foldl(
                fun (B, M) when is_binary(B) -> map_list_append(<<"#text">>, B, M);
                    (E={Name, _, _}, M) -> map_list_append(Name, convert_xml_element(E), M)
                end, #{}, Children),
    Res = maps:fold(
            fun (K, [E], M) -> [{K, E}|M];
                (K, L, M) when is_list(L) -> [{K, lists:reverse(L)}|M]
            end, [], Grouped),
    lists:reverse(Res).

map_list_append(K, V, M)->
    M#{K => [V | maps:get(K, M, [])]}.



    
%%%------------------------------------------------------------
-spec json2xml(binary()) -> binary().
json2xml(Jsonb) ->
    Json = jiffy:decode(Jsonb),
    {ok, Doc} = erlsom:write(convert_json(Json), [{output, binary}]),
    Prolog = xml_prolog(),
    <<Prolog, Doc/binary>>. 

xml_prolog() -> <<"<?xml version=\"1.0\" encoding=\"UTF-8\" ?>">>.

convert_json({[E={_Name, _Rest}]}) ->
    convert_json_element(E);
convert_json({L}) when length(L)>1 ->
    {root_name(), [], lists:map(fun convert_json_element/1, L)}.

convert_json_element({Name, Text}) when is_binary(Text) -> 
    {Name, [], [Text]};
convert_json_element({Name, {Rest}})-> 
    {RawAttrs, RawTexts, RestJson} = 
        lists:foldl(
          fun (E={<<"@", _/binary>>, _}, {As, Ts, R}) -> {[E|As], Ts, R};
              (E={<<"#", _/binary>>, _}, {As, Ts, R}) -> {As, [E|Ts], R};
              (E, {As, Ts, R}) -> {As, Ts, [E|R]}
          end, {[],[],[]}, Rest),
    Attrs = lists:map(
              fun({<<"@", K/binary>>, V}) -> 
                      {K, V} end, 
              RawAttrs), 
    Texts = element(2, lists:unzip(RawTexts)),
    RestXml = lists:map(fun convert_json_element/1, RestJson),
    {Name, Attrs, Texts++RestXml}.


root_name() -> <<"doc">>.
