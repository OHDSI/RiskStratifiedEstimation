/***********************************
File createTable.sql
***********************************/
IF OBJECT_ID('@result_database_schema.@target_cohort_table', 'U') IS NOT NULL
DROP TABLE @result_database_schema.@target_cohort_table;

CREATE TABLE @result_database_schema.@target_cohort_table(
    cohort_definition_id int,
    subject_id bigint,
    cohort_start_date date,
    cohort_end_date date
);
