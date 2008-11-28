-module (prims).
-export ([start/1]).

-record (vertex, {num, adj_list=[], min_distance=inf, parent=null, 
                  min_adj_list=[], is_in_Q=true}).

start (Filename) ->
    {ok, File} = file:open (Filename, read),
    VertexList = populate_vertex_list (File),
    MinQueue = lists:sort([V#vertex.num || V <- VertexList]),    
    Tree = find_min_tree (MinQueue, VertexList),
    %print_path (Tree),
    io:format ("~w~n", [Tree]),
    file:close (File).   

print_path ([]) ->
    ok;
print_path ([H |T]) ->
    io:format ("~w~n", [hd(list:reverse(H#vertex.min_adj_list))]).

populate_vertex_list (File) ->
    populate_vertex_list (File, []).
populate_vertex_list (File, VertexList) ->
    case io:get_line (File, '') of
        eof ->
            VertexList;
        "\n" ->
            populate_vertex_list (File, VertexList);
        Line ->
            [{V1, _}, {V2, _}, {Weight, _}] = lists:map (fun (X) -> 
                                                                 string:to_integer(X) 
                                                         end, 
                                                         string:tokens (Line, " ")),
            NewVertexList = update_vertex_list (V1, V2, Weight, VertexList),
            NewNewVertexList = update_vertex_list (V2, V1, Weight, NewVertexList),
            io:format ("~w ~w ~w~n", [V1, V2, Weight]),
            populate_vertex_list (File, NewNewVertexList)
    end.

update_vertex_list (V1, V2, Weight, VertexList) ->
    update_vertex_list (V1, V2, Weight, VertexList, []).  

update_vertex_list (V1, V2, Weight, [], NewVertexList) ->
    [#vertex {num=V1, adj_list=[{V2, Weight}]} | NewVertexList];
update_vertex_list (V1, V2, Weight, 
                 [#vertex{num=V1} = OldVertex | T], NewVertexList) -> 
    NewVertex = OldVertex#vertex {
                  adj_list=[{V2, Weight} | OldVertex#vertex.adj_list]},
    [NewVertex | T ++ NewVertexList];
update_vertex_list (V1, V2, Weight, [H | T], NewVertexList) ->
    update_vertex_list (V1, V2, Weight, T, [H | NewVertexList]).
      
find_min_tree ([], VertexList) -> 
    VertexList;                           
find_min_tree ([VertexNum | T], VertexList) ->    
    {NewVertex, NewVertexList} = update_vertex(VertexNum, VertexList, false),
    case NewVertex#vertex.parent of
        null ->
            NewNewVertexList1 = NewVertexList,
            nothing;
        ParentNum ->
            NewNewVertexList = update_min_adj_list (ParentNum, NewVertexList, VertexNum),
            NewNewVertexList1 = update_min_adj_list (VertexNum, NewNewVertexList, ParentNum)
    end,
    {NewNewNewVertexList, NewMinQueue} = update_adjacent (NewVertex, NewVertex#vertex.adj_list, 
                                                       NewNewVertexList1, T),
    find_min_tree (NewMinQueue, NewNewNewVertexList).

get_vertex (VertexNum, []) ->
    #vertex {num=VertexNum};
get_vertex (VertexNum, [#vertex{num=VertexNum} = V | _T]) ->
    V;
get_vertex (VertexNum, [_H|T]) ->
    get_vertex (VertexNum, T).

update_vertex (Vertex, VertexList, Value) ->
    update_vertex (Vertex, VertexList, Value, []).

update_vertex (Vertex, [], _, NewVertexList) ->
    {#vertex {num=Vertex}, NewVertexList};
update_vertex (Vertex, [#vertex{num=Vertex} = V | T], 
               Value, NewVertexList) ->
    NewVertex = V#vertex {is_in_Q=Value},
    {NewVertex, NewVertexList ++ [NewVertex | T]};
update_vertex (Vertex, [H|T], Value, NewVertexList) ->
    update_vertex (Vertex, T, Value, [H | NewVertexList]).

update_adjacent (_, [], VertexList, MinQueue) ->
    {VertexList, MinQueue};
update_adjacent (Vertex, [{AdjVertexNum, Weight} | T], VertexList, MinQueue) ->
    AdjVertex = get_vertex (AdjVertexNum, VertexList),
    if
        AdjVertex#vertex.is_in_Q, Weight < AdjVertex#vertex.min_distance -> 
            % set parent of adjacent to latest_addition
            % set min_distance of adjacent to weight-function
            % (latest_addition, adjacent)
            % update adjacent in Q, order by min_distance
            {NewAdjVertex, NewVertexList} = set_parent_and_min_dist (AdjVertex#vertex.num, VertexList,
                                                     Vertex#vertex.num),             
            NewMinQueue = update_queue (NewAdjVertex#vertex.num, NewAdjVertex#vertex.min_distance, lists:delete(NewAdjVertex#vertex.num, MinQueue)),
            update_adjacent (Vertex, T, NewVertexList, NewMinQueue);
        true ->
            update_adjacent (Vertex, T, VertexList, MinQueue)
    end.

get_edge_weight (Vertex, AdjVertexNum) ->
    case lists:keysearch (AdjVertexNum, 1, Vertex#vertex.adj_list) of
        {value, {_, Weight}} ->
            Weight;
        false ->
            0
    end.

set_parent_and_min_dist (AdjVertexNum, VertexList, VertexNum) ->
    set_parent_and_min_dist (AdjVertexNum, VertexList, VertexNum, []).    
set_parent_and_min_dist (AdjVertexNum, [#vertex{num=AdjVertexNum} = V | T], 
                         VertexNum, NewVertexList) ->    
    EdgeWeight = get_edge_weight (get_vertex (VertexNum, T++NewVertexList), AdjVertexNum),
    NewVertex = V#vertex {parent=VertexNum, min_distance=EdgeWeight},
    {NewVertex, NewVertexList ++ [NewVertex | T]};
set_parent_and_min_dist (AdjVertexNum, [H|T], VertexNum, NewVertexList) ->    
    set_parent_and_min_dist (AdjVertexNum, T, VertexNum, [H | NewVertexList]).

update_queue (VertexNum, MinDist, MinQueue) ->
    update_queue (VertexNum, MinDist, MinQueue, []).

update_queue (VertexNum, _, [], NewMinQueue) ->
    NewMinQueue ++ [VertexNum];              
update_queue (VertexNum, MinDist, [H|T], NewMinQueue) ->
    if
        H == inf ; MinDist < H ->
            NewMinQueue ++ [VertexNum] ++ [H|T];              
        true ->
            update_queue (VertexNum, MinDist, T, NewMinQueue++[H])
    end.

update_min_adj_list (ParentNum, VertexList, VertexNum) ->
    update_min_adj_list (ParentNum, VertexList, VertexNum, []).

update_min_adj_list (_, [], _, NewVertexList) ->
    NewVertexList;
update_min_adj_list (ParentNum, [#vertex{num=ParentNum} = V | T], 
               VertexNum, NewVertexList) ->
    MinAdjList = V#vertex.min_adj_list,
    NewVertex = V#vertex {min_adj_list=[VertexNum | MinAdjList]},
    NewVertexList ++ [NewVertex | T];
update_min_adj_list (ParentNum, [H|T], VertexNum, NewVertexList) ->
    update_min_adj_list (ParentNum, T, VertexNum, [H | NewVertexList]).

