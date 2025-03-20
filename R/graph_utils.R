

.is_tree <- function(graph, root){
  if(igraph::is_tree(graph) && igraph::is_directed(graph)){
    # Check if the root has no parents
    length(igraph::neighbors(graph, root, mode = "in")) == 0
  }else{
    FALSE
  }
}


.make_tree <- function(graph, root){
  if(.is_tree(graph, root)){
    graph
  }else{
    stopifnot(igraph::is_tree(graph))
    stopifnot(root %in% igraph::V(graph)$name)

    graph <- graph |>
      .igraph_as_undirected() |>
      igraph::simplify()

    bfs_result <- igraph::bfs(graph, root = root, order = TRUE, father = TRUE)
    # Remove root as it has no father
    nodes <- bfs_result$order[-1]$name
    fathers <- bfs_result$father[nodes]$name
    if(length(nodes) == 0){
      .singleton_igraph(root)
    }else{
      igraph::graph_from_data_frame(data.frame(fathers, nodes), directed = TRUE)
    }
  }
}


.igraph_as_undirected <- function(...){
  if(utils::packageVersion("igraph") < "2.1.0"){
    igraph::as.undirected(...)
  }else{
    igraph::as_undirected(...)
  }
}

.singleton_igraph <- function(element){
  graph <- igraph::make_empty_graph(n=0)
  graph <- igraph::add_vertices(graph, nv = 1, name = element)
  graph
}
