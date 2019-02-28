/*********************************
treatnmentCovariateSettings.sql
*********************************/

IF OBJECT_ID('@cohort_database_schema.@cohort_attribute_table', 'U') IS NOT NULL
DROP TABLE @cohort_database_schema.@cohort_attribute_table;

IF OBJECT_ID('@cohort_database_schema.@attribute_definition_table', 'U') IS NOT NULL
DROP TABLE @cohort_database_schema.@attribute_definition_table;

SELECT 1 AS attribute_definition_id,
'Treated' AS attribute_name
INTO @cohort_database_schema.@attribute_definition_table;

SELECT cohort_definition_id,
subject_id,
cohort_start_date,
1 AS attribute_definition_id,
1 AS value_as_number
INTO @cohort_database_schema.@cohort_attribute_table
FROM @cohort_database_schema.@merged_cohort_table mc
INNER JOIN @cohort_database_schema.@cohort_table cohort
ON mc.subject_id = cohort.subject_id
WHERE cohort_definition_id = @treatment_cohort_id
;
