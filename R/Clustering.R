# Copyright 2021 Observational Health Data Sciences and Informatics
#
# This file is part of ClusteringPlayground
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# library(dplyr)

#' Title
#'
#' @param outputFolder 
#' @param algorithm 
#'
#' @return
#' @export
#'
#' @examples
clusterPatients <- function(outputFolder, algorithm = "umap-dbscan") {
  idxToRowId <- readRDS(file.path(outputFolder, "idxToRowId.rds"))
  distances <- readRDS(file.path(outputFolder, "distances.rds"))
  
  if (algorithm == "umap-dbscan") {
    umapSettings <- umap::umap.defaults
    umapSettings$metric <- "cosine"
    umapSettings$n_components <- 5
    lowDimensional <- umap::umap(distances, input = "dist", config = umapSettings)
    k <-  dbscan::kNNdist(lowDimensional$layout, k = 5 - 1)
    eps <- quantile(k, 0.95)
    # dbscan::kNNdistplot(lowDimensional$layout, k = 5 - 1)
    clustering <- dbscan::dbscan(lowDimensional$layout, eps = eps, minPts = 100)
    labels <- tibble(rowId = idxToRowId$rowId,
                     label = sprintf("Cluster %d", clustering$cluster))
  } else if (algorithm == "dbscan") {
    # distances[1:10, 1:10]
    distances <- as.dist(distances)
    ggplot2::ggplot(tibble(distance = as.numeric(distances)), ggplot2::aes(x = .data$distance)) +
      ggplot2::geom_density(alpha = 0.5, fill = "#000000")
    quantile(distances, 0.5)
    clustering <- dbscan::dbscan(distances, eps = 0.7, minPts = 100)
    labels <- tibble(rowId = idxToRowId$rowId,
                     label = sprintf("Cluster %d", clustering$cluster))
  } else {
    distances <- 1 - distances
    # distances <- 1 / distances - 1
    # distances <- 1 / distances^2 - 1
    
    
    idx <- lower.tri(distances, diag = FALSE)
    g <- igraph::make_full_graph(nrow(distances))
    igraph::E(g)$weight <- distances[idx]
    
    start <- Sys.time()
    if (algorithm == "louvain") {
      clustering <- igraph::cluster_louvain(g)
    } else if (algorithm == "walktrap") {
      clustering <- igraph::cluster_walktrap(g)
    } else if (algorithm == "slm") {
      clustering <- slm.community(g = g)  
    }
    delta <- Sys.time() - start
    ParallelLogger::logInfo("Clustering took ", signif(delta, 3), " ", attr(delta, "units"))
    
    labels <- tibble(rowId = idxToRowId$rowId,
                     label = sprintf("Cluster %d", clustering$membership))
  }
  saveRDS(labels, file.path(outputFolder, "clusters.rds"))
}

slm.community <- function(g,
                          e.weight = "weight",
                          modularity = 1,
                          resolution = 1,
                          algorithm = 3,
                          nrandom = 10,
                          iterations = 10,
                          randomseed = 0,
                          print = 1) {
  # memorySize <- "–Xmx10000m"
  # stackSize <- "-Xss1000k"
  nodes <- igraph::as_data_frame(g, what = "vertices")
  edges <- igraph::as_data_frame(g, what = "edges")
  nodes$seq <- 1:nrow(nodes)
  edges$from <- nodes[edges$from, "seq"] - 1
  edges$to <- nodes[edges$to, "seq"] - 1
  edges <- edges[, c("from", "to", e.weight)]
  
  inputFile <- tempfile(pattern = "input", fileext = ".network")
  outputFile <- tempfile(pattern = "output", fileext = ".cluster")
  
  write.table(edges,
              file = inputFile,
              sep = "\t",
              row.names = FALSE,
              col.names = FALSE)
  
  on.exit(unlink(inputFile))
  
  # Downloaded from http://www.ludowaltman.nl/slm/
  pathToJar <- system.file("java", "ModularityOptimizer.jar", package = "ClusteringPlayground")
  
  args <-c("-jar",
           pathToJar,
           inputFile,
           outputFile,
           modularity,
           resolution,
           algorithm,
           nrandom,
           iterations,
           randomseed,
           print)
  
  res <- sys::exec_wait(cmd = "java", args = args) 
  if (file.exists(outputFile)) {
    #success
    member <- read.table(outputFile, header = FALSE)[, 1]
    unlink(outputFile)
  } else{
    #failed
    member <- (seq_along(nodes$name) - 1)
  }
  
  res <- NULL
  res$membership <- (member + 1)
  res$vcount <- igraph::vcount(g)
  res$names <- igraph::V(g)$name
  res$algorithm <- "Smart Local Moving by Ludo Waltman and Nees Jan van Eck"
  class(res) <- "communities"
  return(res)
}
