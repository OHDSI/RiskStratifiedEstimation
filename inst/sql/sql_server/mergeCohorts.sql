/***********************************
File mergeCohorts.sql
***********************************/

IF OBJECT_ID('@result_database_schema.@merged_cohort_table', 'U') IS NOT NULL
DROP TABLE @result_database_schema.@merged_cohort_table;


SELECT @target_cohort_id AS cohort_definition_id,
  subject_id,
  cohort_start_date,
  cohort_end_date
INTO @result_database_schema.@merged_cohort_table
FROM @cohort_database_schema.@cohort_table
WHERE cohort_definition_id IN (@cohort_ids);
