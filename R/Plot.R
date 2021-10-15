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
# labels <- readRDS(file.path(outputFolder, "labels,rds"))
plot2D <- function(outputFolder, labels) {
  distances <- readRDS(file.path(outputFolder, "distances.rds"))
  
  ParallelLogger::logInfo("Computing 2D coordinates")
  map <- umap::umap(distances, input = "dist")
  
  points <- data.frame(x = map$layout[, 1],
                       y = map$layout[, 2])
  
  # Add labels
  idxToRowId <- readRDS(file.path(outputFolder, "idxToRowId.rds"))
  labels <- labels %>%
    inner_join(idxToRowId, by = "rowId") %>%
    arrange(.data$idx)
  
  points$label <- "Other"
  points$label[labels$idx] <- labels$label
  
  ParallelLogger::logInfo("Plotting")
  plot <- ggplot2::ggplot(points, ggplot2::aes(x = .data$x, y = .data$y, color = .data$label)) +
    ggplot2::geom_point(shape = 16, alpha = 0.4) +
    ggplot2::theme(panel.background = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   axis.title = ggplot2::element_blank(),
                   legend.background = ggplot2::element_blank(),
                   legend.key = ggplot2::element_blank(),
                   legend.title = ggplot2::element_blank())
  
  ggplot2::ggsave(filename = file.path(outputFolder, "plot.png"))
}

labelByCovariate <- function(outputFolder,
                              conceptIds = c(201254, 201826)) {
  covariateData <- FeatureExtraction::loadCovariateData(file.path(outputFolder, "CovariateData.zip"))
  
  conceptPriority <- data.frame(priority = 1:length(conceptIds),
                                conceptId = conceptIds)
  labels <- covariateData$covariateRef %>%
    inner_join(conceptPriority, copy = TRUE, by = "conceptId") %>%
    select(.data$covariateId, .data$covariateName, .data$priority) %>%
    inner_join(covariateData$covariates, by = "covariateId") %>%
    select(.data$rowId, label = .data$covariateName, .data$priority) %>%
    arrange(.data$rowId, .data$priority) %>%
    collect()
  
  labels <- labels[!duplicated(labels$rowId), ]
  labels$label <- gsub(".*index: ", "", labels$label)
  labels <- labels %>%
    select(-.data$priority)
  saveRDS(labels, file.path(outputFolder, "labels,rds"))
}
