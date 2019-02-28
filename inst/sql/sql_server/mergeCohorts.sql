/***********************************
File mergeCohorts.sql
***********************************/

IF OBJECT_ID('@result_database_schema.@merged_cohorts', 'U') IS NOT NULL
DROP TABLE @result_database_schema.@merged_cohorts;

CREATE TYPE prijnbeek_CohortList AS TABLE (cohort_id int)

SELECT *
INTO @result_database_schema.@merged_cohorts
FROM @cohort_database_schema.@cohort_table
where cohort_definition_id IN (SELECT cohort_id FROM @cohort_ids);
