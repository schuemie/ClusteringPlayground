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

#' @export
constructFeatures <- function(connectionDetails,
                              cdmDatabaseSchema,
                              cohortDatabaseSchema,
                              cohortTable,
                              outputFolder,
                              oracleTempSchema = NULL) {
  if (!file.exists(outputFolder)) {
    warning("Folder '", outputFolder, "' not found. Attempting to create")
    dir.create(outputFolder)
  }
  
  # Create features for year before up to  year after cohort entry
  covariateSettings <- FeatureExtraction::createCovariateSettings(useDemographicsGender = TRUE,
                                                                  useDemographicsAgeGroup = TRUE,
                                                                  useConditionGroupEraLongTerm = TRUE,
                                                                  useDrugGroupEraLongTerm = TRUE,
                                                                  useProcedureOccurrenceLongTerm = TRUE,
                                                                  useObservationLongTerm = TRUE,
                                                                  useMeasurementLongTerm = TRUE,
                                                                  longTermStartDays = -365,
                                                                  endDays = 365)
  covariateData <- FeatureExtraction::getDbCovariateData(connectionDetails = connectionDetails,
                                                         oracleTempSchema = oracleTempSchema,
                                                         cohortDatabaseSchema = cohortDatabaseSchema,
                                                         cohortTable = cohortTable,
                                                         cohortId = 1,
                                                         cdmDatabaseSchema = cdmDatabaseSchema,
                                                         rowIdField = "row_id",
                                                         covariateSettings = covariateSettings)
  FeatureExtraction::saveCovariateData(covariateData, file.path(outputFolder, "CovariateData.zip"))
  
  
}

#' @export
constructFeaturesMetabolicDisease <- function(connectionDetails,
                                              cdmDatabaseSchema,
                                              cohortDatabaseSchema,
                                              cohortTable,
                                              outputFolder,
                                              oracleTempSchema = NULL) {
  if (!file.exists(outputFolder)) {
    warning("Folder '", outputFolder, "' not found. Attempting to create")
    dir.create(outputFolder)
  }
  
  # Create features for year before up to  year after cohort entry
  covariateSettings <- FeatureExtraction::createCovariateSettings(useConditionGroupEraLongTerm = TRUE,
                                                                  longTermStartDays = -365,
                                                                  endDays = 365,
                                                                  includedCovariateConceptIds = 436670, # Metabolic disease
                                                                  addDescendantsToInclude = TRUE)
  covariateData <- FeatureExtraction::getDbCovariateData(connectionDetails = connectionDetails,
                                                         oracleTempSchema = oracleTempSchema,
                                                         cohortDatabaseSchema = cohortDatabaseSchema,
                                                         cohortTable = cohortTable,
                                                         cohortId = 1,
                                                         cdmDatabaseSchema = cdmDatabaseSchema,
                                                         rowIdField = "row_id",
                                                         covariateSettings = covariateSettings)
  FeatureExtraction::saveCovariateData(covariateData, file.path(outputFolder, "CovariateData.zip"))
  
  
}
