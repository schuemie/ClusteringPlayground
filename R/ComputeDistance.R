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
#' @param minFeaturesPerPerson 
#' @param minPersonsPerFeature 
#'
#' @return
#' @export
computeDistance <- function(outputFolder,
                            minFeaturesPerPerson = 100,
                            minPersonsPerFeature = 10) {
  covariateData <- FeatureExtraction::loadCovariateData(file.path(outputFolder, "CovariateData.zip"))
  
  
  
  ParallelLogger::logInfo("Filter persons with few non-zero covariates")
  
  filteredPersons <- covariateData$covariates %>%
    group_by(.data$rowId) %>%
    summarise(count = n()) %>%
    filter(.data$count > minFeaturesPerPerson)  %>%
    select(.data$rowId)
  covariateData$filteredCovariates <- covariateData$covariates %>%
    inner_join(filteredPersons, by = "rowId")
  
  totalPersonCount <- filteredPersons %>%
    count() %>%
    pull()
  
  
  ParallelLogger::logInfo("Compute inverse document frequencies")
  idfs <- covariateData$filteredCovariates %>%
    group_by(.data$covariateId) %>%
    summarise(count = n()) %>%
    filter(.data$count > minPersonsPerFeature)  %>%
    mutate(idf = log(totalPersonCount / count)) %>%
    select(.data$covariateId, .data$idf)
  
  # Store as slam sparse matrix
  temp <- covariateData$filteredCovariates %>%
    inner_join(idfs, by = "covariateId") %>%
    select(.data$rowId, .data$covariateId, .data$idf) %>%
    collect()
  # (Make rowId and covariateId consecutive)
  idxToRowId <- unique(temp$rowId)
  idxToCovariateId <- unique(temp$covariateId)
  temp$rowId <- match(temp$rowId, idxToRowId)
  temp$covariateId <- match(temp$covariateId, idxToCovariateId)
  
  vectors <- slam::simple_triplet_matrix(i = temp$covariateId,
                                         j = temp$rowId,
                                         v = temp$idf)
  rm(temp)
  
  # saveRDS(vectors, file.path(outputFolder, "vectors.rds"))
  
  ParallelLogger::logInfo("Compute distance matrix")
  distances <- 1 - slam::crossprod_simple_triplet_matrix(vectors)/(sqrt(slam::col_sums(vectors^2) %*% t(slam::col_sums(vectors^2))))
  distances[is.nan(distances)] <- 1
  
  saveRDS(distances, file.path(outputFolder, "distances.rds"))
  
  idxToRowId <- data.frame(idx = 1:length(idxToRowId),
                           rowId = idxToRowId)
  saveRDS(idxToRowId, file.path(outputFolder, "idxToRowId.rds"))
}
