/***********************************
File mergeCohorts.sql
***********************************/

SELECT *
INTO @result_database_schema.@merged_cohorts
FROM @cohort_database_schema.@cohort_table
where cohort_definition_id IN @cohort_ids;
