/*********************************
  treatnmentCovariateSettings.sql
*********************************/

  IF OBJECT_ID('@result_database_schema.@cohort_attribute_table', 'U') IS NOT NULL
DROP TABLE @result_database_schema.@cohort_attribute_table;

IF OBJECT_ID('@result_database_schema.@attribute_definition_table', 'U') IS NOT NULL
DROP TABLE @result_database_schema.@attribute_definition_table;

SELECT 1 AS attribute_definition_id,
'Treated' AS attribute_name
INTO @result_database_schema.@attribute_definition_table;

SELECT 1 AS cohort_definition_id,
subject_id,
cohort_start_date,
1 AS attribute_definition_id,
1 AS value_as_number
INTO @result_database_schema.@cohort_attribute_table
FROM @cohort_database_schema.@cohort_table
WHERE cohort_definition_id = @treatment_cohort_id AND subject_id IN (SELECT subject_id FROM @result_database_schema.@merged_cohort_table)
;
