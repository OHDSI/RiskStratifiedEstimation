RiskStratifiedEstimation
===========

Introduction
============
RiskStratifiedEstimation is an R package for implementing risk stratified analyses for the assessment of treatment effect heterogeneity in an observational database in the OMOP Common Data Model. The package combines functionality of [PatientLevelPrediction](https://github.com/OHDSI/PatientLevelPrediction) and [CohortMethod](https://github.com/OHDSI/CohortMethod) R packages.

The figure below illustrates the overall process of performing a risk-based analysis of treatment effect heterogeneity using the RiskStratifiedEstimation package. (A) Starting from a treatment (top), a comparator (bottom) and an outcome (middle) cohort we estimate the propensity scores on the entire target population. (B) We match patients on the propensity scores and estimate the prediction model. Since we match patients we develop the prediction model on smaller subset of the initial population and, therefore, the number of patients is smaller in B compared to A. (C) We apply the prediction model on the entire population (green: lower 25% of the risk distribution; yellow: patients with risk between 25% and 50% of the risk distribution; orange: patients with risk between 50% and 75% of the risk distribution; red: patients at risk higher than 75% of the risk distribution). (D) We separate in risk subgroups, here quarters. Within risk quarters propensity scores are estimated again and relative and absolute treatment effects are estimated.

  <img src="https://github.com/mi-erasmusmc/RiskStratifiedEstimation/blob/develop/vignettes/rsee_process.jpg" alt="CohortMethod propensity score plot" title="RiskStratifiedEstimation comparison plot" />
  
Features
========
- Extracts the necessary data from a database in OMOP Common Data Model format.
- Uses a large set of covariates for the prediction, the propensity and the outcome model, including for example all drugs, diagnoses, procedures, as well as age, comorbidity indexes, etc.
- Imports functionality from [PatientLevelPrediction](https://github.com/OHDSI/PatientLevelPrediction) package for the development of prediction models for risk stratification.
- Large scale regularized regression to fit the propensity and outcome models.
- Includes diagnostic functions for the prediction process, including calibration plots and ROC curves demostrating the performance of the developed prediction models in sub-populations of interest.
- Includes diagnostic functions for the estimation process, including propensity score distribution plots and plots showing covariate balance before and after performing a propensity score-based analysis.



Screenshots
===========
<table border = "">
<tr valign="top">
<td width = 33%>
  <img src="https://github.com/mi-erasmusmc/RiskStratifiedEstimation/blob/develop/vignettes/comparisonPlot.png" alt="CohortMethod propensity score plot" title="RiskStratifiedEstimation comparison plot" />
</td>
<td width = 33%>
 <img src="https://github.com/mi-erasmusmc/RiskStratifiedEstimation/blob/master/vignettes/KMPlot.jpeg" alt="CohortMethod covariate balance plot" title="RiskStratifiedEstimation weighted Kaplan-Meier estimate" />
</td>
<td width = 33%>
 <img src="https://github.com/mi-erasmusmc/RiskStratifiedEstimation/blob/master/vignettes/balancePlot.jpeg" alt="CohortMethod covariate balance plot" title="RiskStratifiedEstimation weighted Kaplan-Meier estimate" />
</td>
</tr><tr>
<td>Comparison plot across risk strata</td><td>Weighted Kaplan-Meier estimator</td><td>Covariate balance plot</td>
</tr>
</table>

Technology
============
RiskStratifiedEstimation is an R package, combining functionality from the CohortMethod and the PatientLevelPrediction packages.

System Requirements
============
Requires R (version 3.1.0 or higher). Installation on Windows requires [RTools](http://cran.r-project.org/bin/windows/Rtools/). Libraries used in RiskStratifiedEstimation require Java.

Dependencies
============
 * Cyclops
 * DatabaseConnector
 * SqlRender
 * FeatureExtraction
 * ParallelLogger
 * CohortMethod
 * PatientLevelPrediction

Getting Started
===============
1. On Windows, make sure [RTools](http://cran.r-project.org/bin/windows/Rtools/) is installed.
2. The DatabaseConnector and SqlRender packages require Java. Java can be downloaded from
<a href="http://www.java.com" target="_blank">http://www.java.com</a>. Once Java is installed, ensure that Java is being pathed correctly. Under environment variables in the control panel, ensure that the jvm.dll file is added correctly to the path.
3. In R, use the following commands to download and install RiskStratifiedEstimation:

  ```r
  install.packages("drat")
  drat::addRepo("OHDSI")
  install.packages("RiskStratifiedEstimation")
  ```

Getting Involved
=============
* Vignette: [Study example](https://github.com/mi-erasmusmc/RiskStratifiedEstimation/tree/master/inst/doc/StudyExample.pdf)
* Package manual: [RiskStratifiedEstimation.pdf](https://github.com/mi-erasmusmc/RiskStratifiedEstimation/blob/develop/extras/RiskStratifiedEstimation-manual.pdf)
* Developer questions/comments/feedback: <a href="http://forums.ohdsi.org/c/developers">OHDSI Forum</a>
* We use the <a href="../../issues">GitHub issue tracker</a> for all bugs/issues/enhancements

License
=======
RiskStratifiedEstimation is licensed under Apache License 2.0

Development
===========
RiskStratifiedEstimation is being developed in R Studio.
RiskStratifiedEstimation is still under development and should not be used yet to run risk stratified analyses.


