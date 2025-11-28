

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

.graph_entry_exit_times <- function(g, root){
  tin  <- integer(igraph::vcount(g))
  names(tin) <- igraph::V(g)$name
  tout <- integer(igraph::vcount(g))
  names(tout) <- igraph::V(g)$name
  time <- 1
  in_cb <- function(graph, node, ...) {
    tin[node["vid"]] <<- time
    time <<- time + 1
    FALSE
  }
  out_cb <- function(graph, node, ...) {
    tout[node["vid"]] <<- time
    time <<- time + 1
    FALSE
  }
  igraph::dfs(g, root, mode = "out", in.callback = in_cb, out.callback = out_cb)
  list(entry = tin, exit = tout)
}

.check_tree_compatible <- function(tree1, tree1_root, tree2, tree2_root){
  stopifnot(.is_tree(tree1, tree1_root))
  stopifnot(.is_tree(tree2, tree2_root))

  inout_times1 <- .graph_entry_exit_times(tree1, tree1_root)
  inout_times2 <- .graph_entry_exit_times(tree2, tree2_root)
  all_nodes <- intersect(names(igraph::V(tree1)), names(igraph::V(tree2)))
  df <- expand.grid(n1=all_nodes, n2=all_nodes, stringsAsFactors = FALSE)

  df$parent_child1 <- inout_times1$entry[df$n1] <= inout_times1$entry[df$n2] & inout_times1$exit[df$n2] <= inout_times1$exit[df$n1]
  df$parent_child2 <- inout_times2$entry[df$n1] <= inout_times2$entry[df$n2] & inout_times2$exit[df$n2] <= inout_times2$exit[df$n1]

  if(any(df$parent_child1 != df$parent_child2)){
    mismatch <- which(df$parent_child1 != df$parent_child2)
    i <- mismatch[1]
    error_message <- paste0("Detected ", length(mismatch), " problems between trees. The first is: \n",
                            "tree1 defines '", df$n1[i], if(df$parent_child1[i]) "' to be" else "' not to be", " a parent of '", df$n2[i], "' whereas ",
                            "tree2 defines '", df$n1[i], if(df$parent_child2[i]) "' to be" else "' not to be", " a parent of '", df$n2[i], "'.")
    stop(error_message)
  }
  invisible(TRUE)
}

