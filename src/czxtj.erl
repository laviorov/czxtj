-module(czxtj).
-compile(export_all).

-spec xml2json(binary()) -> binary().
xml2json(Xmlb)->
    Converted = case detect_encoding(Xmlb) of
        <<"UTF-8">> -> Xmlb;
        Enc -> iconv:convert(Enc, <<"UTF-8">>, Xmlb)
    end,
    {xml, _Ver, _Enc, Xml} = exomler:decode_document(Converted),
    jiffy:encode(convert_xml(Xml)).

convert_xml(E={Name, _Attrs, _Childs})->
    {[{Name, convert_xml_element(E)}]}.

convert_xml_element({_Name, [], [Text]}) when is_binary(Text) ->
    Text;
convert_xml_element({_Name, Attrs, Children})->
   {lists:map(fun attr_name_to_json/1, Attrs) ++ convert_children(Children)}. 

attr_name_to_json({K, V}) -> {<<"@", K/binary>>, V}.
   
convert_children(Children)-> 
    Grouped = lists:foldl(
                fun (B, M) when is_binary(B) -> map_list_append(<<"#text">>, B, M);
                    %({xmlel, _Name, [], []}, M) -> M;  %%discard empty tag
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
detect_encoding(<<"\xFF\xFE",     _/binary>>) -> <<"UTF-16LE">>;
detect_encoding(<<"\xFE\xFF",     _/binary>>) -> <<"UTF-16BE">>;
detect_encoding(<<"\xEF\xBB\xBF", _/binary>>) -> <<"UTF-8">>;
detect_encoding(B) -> 
    case re:run(B, "<?xml[^>]+encoding=\"([^\"]+)\"", [{capture, [1], binary}]) of
        nomatch -> error("can not detect character encoding");
        {match, [Encoding]} -> Encoding
    end.
    
%%%------------------------------------------------------------
-spec json2xml(binary()) -> binary().
json2xml(Jsonb) ->
    Json = jiffy:decode(Jsonb),
    Doc = exomler:encode(convert_json(Json)),
    Prolog = xml_prolog(),
    <<Prolog/binary, Doc/binary>>. 

xml_prolog() -> <<"<?xml version=\"1.0\" encoding=\"UTF-8\" ?>">>.

convert_json({[E={_Name, _Rest}]}) ->
    convert_json_element(E);
convert_json({L}) when length(L)>1 ->
    {root_name(), [], lists:map(fun convert_json_element/1, L)}.

convert_json_element({Name, {[]}}) -> 
    {Name, [], []};
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

root_name() -> 
    {ok, Root} = application:get_env(czxtj, root_name),
    Root.
