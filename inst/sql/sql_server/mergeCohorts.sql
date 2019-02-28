/***********************************
File mergeCohorts.sql
***********************************/

IF OBJECT_ID('@result_database_schema.@merged_cohorts', 'U') IS NOT NULL
DROP TABLE @result_database_schema.@merged_cohorts;

SELECT *
INTO @result_database_schema.@merged_cohorts
FROM @cohort_database_schema.@cohort_table
where cohort_definition_id IN @cohort_ids;
