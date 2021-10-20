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


#' Title
#'
#' @param outputFolder 
#' @param labels 
#'
#' @return
#' @export
characterizeClusters <- function(outputFolder, labels) {
  covariateData <- FeatureExtraction::loadCovariateData(file.path(outputFolder, "CovariateData.zip"))
  totalPersonCount <- as.numeric(nrow(labels))
  
  idfs <- covariateData$covariates %>%
    inner_join(select(labels, .data$rowId), copy = TRUE, by = "rowId") %>%
    group_by(.data$covariateId) %>%
    summarise(count = n()) %>%
    mutate(idf = log(totalPersonCount / count)) %>%
    select(.data$covariateId, .data$idf) %>%
    collect()
  
  # subset <- split(labels, labels$label)[[2]]
  characterize <- function(subset) {
    ParallelLogger::logInfo(sprintf("Characterizing cluster '%s'", subset$label[1]))
    subsetCd <- FeatureExtraction::filterByRowId(covariateData, subset$rowId)
    subsetCd <- FeatureExtraction::aggregateCovariates(subsetCd)
    subsetCd <- subsetCd$covariates %>%
      filter(.data$covariateId != 900000010802) %>%
      inner_join(idfs, copy = TRUE, by = "covariateId") %>%
      mutate(tfIdf = .data$averageValue * .data$idf) %>%
      inner_join(select(subsetCd$covariateRef, c("covariateId", "covariateName")), by = "covariateId") %>%
      collect() %>%
      mutate(covariateName = gsub("^.*index: ", "", .data$covariateName),
             percent = sprintf("%0.0f", .data$averageValue * 100))  %>%
      mutate(covariateName = if_else(nchar(.data$covariateName) > 60,
                     paste(substr(.data$covariateName, 1, 60), "..."),
                     .data$covariateName))
    
    table <- bind_rows(
      tibble(v1 = c("", "%"),
             v2 = c(subset$label[1], "Most frequent"),
             v3 = c("", "%"),
             v4 = c("", "Most informative")),
      bind_cols(subsetCd %>%
                  slice_max(order_by = .data$averageValue, n = 8, with_ties = FALSE) %>%
                  select(v1 = .data$percent,
                         v2 = .data$covariateName),
                subsetCd %>%
                  slice_max(order_by = .data$tfIdf, n = 8, with_ties = FALSE) %>%
                  select(v3 = .data$percent,
                         v4 = .data$covariateName)),
      tibble(v1 = "",
             v2 = "",
             v3 = "",
             v4 = ""))
    return(table)
  }
  labels <- labels %>%
    filter(.data$label != "Cluster 0")
  table <- lapply(split(labels, labels$label), characterize)
  table <- bind_rows(table)
  write.csv(table, file.path(outputFolder, "Characterization.csv"), row.names = FALSE, col.names = FALSE)
}
