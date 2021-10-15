{DEFAULT @sample_size = 100000}
{DEFAULT @min_obs_days_prior = 365}
{DEFAULT @min_obs_days_after = 365}
{DEFAULT @concept_id = 201820}

IF OBJECT_ID('@cohort_database_schema.@cohort_table', 'U') IS NOT NULL
	DROP TABLE @cohort_database_schema.@cohort_table;
	
--HINT DISTRIBUTE_ON_KEY(subject_id)
SELECT TOP @sample_size CAST(1 AS INT) AS cohort_definition_id,
	ROW_NUMBER() OVER (
		ORDER BY first_diagnosis.person_id,
			cohort_start_date
		) AS row_id,
	first_diagnosis.person_id AS subject_id,
	cohort_start_date,
	cohort_start_date AS cohort_end_date
INTO @cohort_database_schema.@cohort_table
FROM (
	SELECT person_id,
		MIN(condition_start_date) AS cohort_start_date
	FROM @cdm_database_schema.condition_occurrence
	INNER JOIN @cdm_database_schema.concept_ancestor
		ON condition_concept_id = descendant_concept_id
	WHERE ancestor_concept_id = @concept_id
	GROUP BY person_id
	) first_diagnosis
INNER JOIN @cdm_database_schema.observation_period
	ON first_diagnosis.person_id = observation_period.person_id
		AND cohort_start_date >= observation_period_start_date
		AND cohort_start_date <= observation_period_end_date
WHERE DATEDIFF(DAY, observation_period_start_date, cohort_start_date) > @min_obs_days_prior
	AND DATEDIFF(DAY, cohort_start_date, observation_period_end_date) > @min_obs_days_after
ORDER BY NEWID();


