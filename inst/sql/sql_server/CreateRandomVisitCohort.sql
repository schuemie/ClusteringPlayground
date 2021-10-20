{DEFAULT @sample_size = 100000}
{DEFAULT @min_obs_days_prior = 365}
{DEFAULT @min_obs_days_after = 365}

IF OBJECT_ID('@cohort_database_schema.@cohort_table', 'U') IS NOT NULL
	DROP TABLE @cohort_database_schema.@cohort_table;
	
--HINT DISTRIBUTE_ON_KEY(subject_id)
SELECT TOP @sample_size CAST(1 AS INT) AS cohort_definition_id,
	ROW_NUMBER() OVER (
		ORDER BY visit_occurrence.person_id,
			visit_start_date
		) AS row_id,
	visit_occurrence.person_id AS subject_id,
	visit_start_date AS cohort_start_date,
	visit_start_date AS cohort_end_date
INTO @cohort_database_schema.@cohort_table
FROM @cdm_database_schema.visit_occurrence
INNER JOIN @cdm_database_schema.observation_period
	ON visit_occurrence.person_id = observation_period.person_id
		AND visit_start_date >= observation_period_start_date
		AND visit_start_date <= observation_period_end_date
WHERE DATEDIFF(DAY, observation_period_start_date, visit_start_date) > @min_obs_days_prior
	AND DATEDIFF(DAY, visit_start_date, observation_period_end_date) > @min_obs_days_after
ORDER BY NEWID();


