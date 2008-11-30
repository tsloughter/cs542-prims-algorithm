-module (prims).
-export ([start/1]).

%%% A vertex contains a vertex number, a list of adjacent vertices
%%% its minimum distance, its parent's vertex number, in the tree, 
%%% its minimum adjacent list of vertices and boolean for if it is 
%%% in the queue.
-record (vertex, {num, adj_list=[], min_distance=inf, parent=null, 
                  min_adj_list=[], is_in_Q=true}).

%%% Takes the name of the graph file
%%% Populates a list of vertices by reading in the file
%%% Finds and prints the minimum spanning tree
start (Filename) ->
    {ok, File} = file:open (Filename, read),
    VertexList = populate_vertex_list (File),
    MinQueue = lists:sort([V#vertex.num || V <- VertexList]),    
    Tree = find_min_tree (MinQueue, VertexList),
    print_path (Tree),
    %io:format ("~w~n", [Tree]),
    file:close (File).   


print_path (List) ->
    Cost = lists:foldr(fun(X, Y) -> 
                               if
                                   X#vertex.parent == null ->
                                       Y;
                                   true ->                                       
                                       X#vertex.min_distance+Y 
                               end 
                       end, 
                       0, List),
    io:format ("Cost of tree: ~w~n", [Cost]),
    io:format ("Edges picked:~n"),
    print_edges (List).

print_edges ([]) ->
    ok;
print_edges ([H |T]) when H#vertex.parent == null ->
    print_edges (T);
print_edges ([H |T]) ->
    io:format ("(~w, ~w) cost: ~w~n", [H#vertex.parent, H#vertex.num, 
                                       H#vertex.min_distance]),
    print_edges (T).

populate_vertex_list (File) ->
    io:get_line (File, ''), %% throw away first line
    populate_vertex_list (File, []).
populate_vertex_list (File, VertexList) ->
    case io:get_line (File, '') of
        eof ->
            VertexList;
        "\n" ->
            %%% if line is blank, skip it
            populate_vertex_list (File, VertexList);
        Line ->
            %%% parse out the vertex numbers and weight of the edges
            [{V1, _}, {V2, _}, {Weight, _}] = lists:map (fun (X) -> 
                                                                 string:to_integer(X) 
                                                         end, 
                                                         string:tokens (Line, " ")),
            %%% add vertices and edges to the list of vertices
            NewVertexList = update_vertex_list (V1, V2, Weight, VertexList),
            NewNewVertexList = update_vertex_list (V2, V1, Weight, NewVertexList),
            %io:format ("~w ~w ~w~n", [V1, V2, Weight]),
            populate_vertex_list (File, NewNewVertexList)
    end.

%%% Add vertex V1 to the list of vertices if it doesn't already exist
%%% Also add the edge to that vertices list of adjacent nodes
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
      
%%% The main function for building the min spanning tree
%%% Takes a queue of vector numbers and the list of vector records
%%% 
find_min_tree ([], VertexList) -> 
    VertexList;                           
find_min_tree ([VertexNum | T], VertexList) ->    
    %%% get vertex record and set that it is no longer in the queue
    {NewVertex, NewVertexList} = update_vertex(VertexNum, VertexList, false),
    case NewVertex#vertex.parent of
        null ->
            %%% no parent? don't do anything
            NewNewVertexList1 = NewVertexList,
            nothing;
        ParentNum ->
            %%% the vertex has a parent, update the min adj list for the vertex and the parent
            NewNewVertexList = update_min_adj_list (ParentNum, NewVertexList, VertexNum),
            NewNewVertexList1 = update_min_adj_list (VertexNum, NewNewVertexList, ParentNum)
    end,
    %%% 
    {NewNewNewVertexList, NewMinQueue} = update_adjacent (NewVertex, NewVertex#vertex.adj_list, 
                                                       NewNewVertexList1, T),
    find_min_tree (NewMinQueue, NewNewNewVertexList).

%%% Returns a vertex from the vertex list based on its vertex number
get_vertex (VertexNum, []) ->
    #vertex {num=VertexNum};
get_vertex (VertexNum, [#vertex{num=VertexNum} = V | _T]) ->
    V;
get_vertex (VertexNum, [_H|T]) ->
    get_vertex (VertexNum, T).

%%% Returns a vertex and sets that it is not in the queue anymore
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

%%% Recursives over the adjacent vertices of the given vertex
%%% If the vertex is in the queue (has not been visited) and the 
%%% weight of the edge to it is less than the current min distance 
%%% set the parent of the adjacent vertex to the vertex passed in and 
%%% the min distance of the adajacent vertex to the weight. Finally 
%%% update the queue with the modified vertices and recurse.
update_adjacent (_, [], VertexList, MinQueue) ->
    {VertexList, MinQueue};
update_adjacent (Vertex, [{AdjVertexNum, Weight} | T], VertexList, MinQueue) ->
    AdjVertex = get_vertex (AdjVertexNum, VertexList),
    if
        AdjVertex#vertex.is_in_Q, Weight < AdjVertex#vertex.min_distance -> 
            {NewAdjVertex, NewVertexList} = set_parent_and_min_dist (AdjVertex#vertex.num, VertexList,
                                                     Vertex#vertex.num),             
            NewMinQueue = update_queue (NewAdjVertex#vertex.num, NewAdjVertex#vertex.min_distance, lists:delete(NewAdjVertex#vertex.num, MinQueue), NewVertexList),
            update_adjacent (Vertex, T, NewVertexList, NewMinQueue);
        true ->
            update_adjacent (Vertex, T, VertexList, MinQueue)
    end.

%%% returns the weight of an edge
get_edge_weight (Vertex, AdjVertexNum) ->
    case lists:keysearch (AdjVertexNum, 1, Vertex#vertex.adj_list) of
        {value, {_, Weight}} ->
            Weight;
        false ->
            0
    end.

%%% Sets the parent of the adjacent vertex to the vertex passed in and 
%%% the min distance of the adajacent vertex to the weight.
set_parent_and_min_dist (AdjVertexNum, VertexList, VertexNum) ->
    set_parent_and_min_dist (AdjVertexNum, VertexList, VertexNum, []).    
set_parent_and_min_dist (AdjVertexNum, [#vertex{num=AdjVertexNum} = V | T], 
                         VertexNum, NewVertexList) ->    
    EdgeWeight = get_edge_weight (get_vertex (VertexNum, T++NewVertexList), AdjVertexNum),
    NewVertex = V#vertex {parent=VertexNum, min_distance=EdgeWeight},
    {NewVertex, NewVertexList ++ [NewVertex | T]};
set_parent_and_min_dist (AdjVertexNum, [H|T], VertexNum, NewVertexList) ->    
    set_parent_and_min_dist (AdjVertexNum, T, VertexNum, [H | NewVertexList]).

%%% Updates the queue by inserting the vertexnum to its correct spot
update_queue (VertexNum, MinDist, MinQueue, VertexList) ->
    update_queue (VertexNum, MinDist, MinQueue, [], VertexList).

update_queue (VertexNum, _, [], NewMinQueue, _VertexList) ->
    NewMinQueue ++ [VertexNum];              
update_queue (VertexNum, MinDist, [H|T], NewMinQueue, VertexList) ->
    Vertex = get_vertex (H, VertexList),
    Weight = Vertex#vertex.min_distance,
    if
        Weight == inf ; MinDist < Weight ->
            %%% if H is infinite or its distance is ver the min distance
            %%% insert the vertex there in the queue
            NewMinQueue ++ [VertexNum] ++ [H|T];              
        true ->
            update_queue (VertexNum, MinDist, T, NewMinQueue++[H], VertexList)
    end.

%%% Adds the parent to the vertexes minimum adjacent list and 
%%% returns the new list of vertices
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

