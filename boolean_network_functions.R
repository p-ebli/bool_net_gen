library(BoolNet)
library(gtools)

#########################
# 1. Canonical Matrix Hash
#########################
get_canonical_matrix_hash <- function(mat) {
  n     <- nrow(mat)
  perms <- gtools::permutations(n, n, 1:n)
  hashes <- apply(perms, 1, function(p) {
    paste(mat[p, p], collapse = ",")
  })
  min(hashes)
}

#########################
# 2. Unique Adjacency Matrices
#########################
generate_unique_adjacency_matrices <- function(n) {
  vals   <- c(-1, 0, 1)
  combos <- expand.grid(rep(list(vals), n * n))
  mats   <- lapply(seq_len(nrow(combos)), function(i) 
    matrix(as.numeric(combos[i, ]), nrow = n, byrow = FALSE)
  )
  hashes <- vapply(mats, get_canonical_matrix_hash, character(1))
  mats[!duplicated(hashes)]
}

#########################
# 3. Boolean Expression Generation
#########################
generate_boolean_expressions <- function(adj_matrix) {
  nodes <- colnames(adj_matrix)
  if (is.null(nodes)) nodes <- LETTERS[1:nrow(adj_matrix)]

  lapply(nodes, function(node) {
    act <- nodes[adj_matrix[, node] == 1]
    inh <- nodes[adj_matrix[, node] == -1]
    act_str <- if (length(act)) paste(act, collapse = " | ") else ""
    inh_str <- if (length(inh)) paste(paste0("!", inh), collapse = " & ") else ""
    if (nzchar(act_str) && nzchar(inh_str)) {
      paste(act_str, "&", inh_str)
    } else if (nzchar(act_str)) {
      act_str
    } else if (nzchar(inh_str)) {
      inh_str
    } else {
      "FALSE"
    }
  }) -> exprs
  names(exprs) <- nodes
  exprs
}

#########################
# 4. Rule File Builder
#########################
build_rule_file <- function(expressions, path) {
  rules <- vapply(names(expressions), function(g) {
    expr <- expressions[[g]]
    if (expr == "FALSE")
      paste0(g, ", ", g, " & ", g)       # self-loop
    else
      paste0(g, ", ", expr)
  }, character(1))

  writeLines(c("targets, functions", rules), con = path)
}

#########################
# 5. Network Analysis Helper
#########################
analyze_network <- function(file_path) {
  net <- loadNetwork(file_path, bodySeparator = ",")
  attr <- getAttractors(net, type = "synchronous", returnTable = TRUE)
  list(
    attractors      = attr$attractors,
    transitionTable = getTransitionTable(attr)
  )
}
