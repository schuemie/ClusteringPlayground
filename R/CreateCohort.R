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
createCohort <- function(connectionDetails,
                         cdmDatabaseSchema,
                         cohortDatabaseSchema,
                         cohortTable,
                         sampleSize = 100000) {
  sql <- SqlRender::loadRenderTranslateSql(sqlFilename = "CreateCohort.sql",
                                           packageName = "ClusteringPlayground",
                                           dbms = connectionDetails$dbms,
                                           cohort_database_schema = cohortDatabaseSchema,
                                           cohort_table = cohortTable,
                                           cdm_database_schema = cdmDatabaseSchema,
                                           sample_size = format(sampleSize, scientific = FALSE),
                                           concept_id = 201820,
                                           min_obs_days_prior = 365,
                                           min_obs_days_after = 365)
  connection <- DatabaseConnector::connect(connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))
  
  DatabaseConnector::executeSql(connection, sql)
}

#' @export
createRandomVisitCohort <- function(connectionDetails,
                                    cdmDatabaseSchema,
                                    cohortDatabaseSchema,
                                    cohortTable,
                                    sampleSize = 100000) {
  sql <- SqlRender::loadRenderTranslateSql(sqlFilename = "CreateRandomVisitCohort.sql",
                                           packageName = "ClusteringPlayground",
                                           dbms = connectionDetails$dbms,
                                           cohort_database_schema = cohortDatabaseSchema,
                                           cohort_table = cohortTable,
                                           cdm_database_schema = cdmDatabaseSchema,
                                           sample_size = format(sampleSize, scientific = FALSE),
                                           min_obs_days_prior = 365,
                                           min_obs_days_after = 365)
  connection <- DatabaseConnector::connect(connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))
  
  DatabaseConnector::executeSql(connection, sql)
}
