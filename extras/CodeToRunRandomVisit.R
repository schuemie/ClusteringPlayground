library(ClusteringPlayground)

options(andromedaTempFolder = "d:/andromedaTemp")

connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = "redshift",
                                                                connectionString = keyring::key_get("redShiftConnectionStringOhdaMdcd"),
                                                                user = keyring::key_get("redShiftUserName"),
                                                                password = keyring::key_get("redShiftPassword"))

# The name of the database schema where the CDM data can be found:
cdmDatabaseSchema <- "cdm_truven_mdcd_v1734"

# The name of the database schema and table where the study-specific cohorts will be instantiated:
cohortDatabaseSchema <- "scratch_mschuemi"
cohortTable <- "cohort_for_clustering_2"

outputFolder <- "d:/temp/Clustering2"

createRandomVisitCohort(connectionDetails = connectionDetails,
                        cdmDatabaseSchema = cdmDatabaseSchema,
                        cohortDatabaseSchema = cohortDatabaseSchema,
                        cohortTable = cohortTable,
                        sampleSize = 10000)

constructFeatures(connectionDetails = connectionDetails,
                  cdmDatabaseSchema = cdmDatabaseSchema,
                  cohortDatabaseSchema = cohortDatabaseSchema,
                  cohortTable = cohortTable,
                  outputFolder = outputFolder)

computeDistance(outputFolder = outputFolder,
                minFeaturesPerPerson = 100,
                minPersonsPerFeature = 10)

labelByCovariate(outputFolder = outputFolder,
                 conceptIds = c(201254, 201826)) 

plot2D(outputFolder = outputFolder,
       labels = readRDS(file.path(outputFolder, "labels.rds")))

clusterPatients(outputFolder = outputFolder)

plot2D(outputFolder = outputFolder,
       labels = readRDS(file.path(outputFolder, "clusters.rds")))

characterizeClusters(outputFolder = outputFolder,
                     labels = readRDS(file.path(outputFolder, "clusters.rds")))

