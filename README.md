RiskStratifiedEstimation
===========

Introduction
============
RiskStratifiedEstimation is an R package for implementing risk stratified analyses for the assessment of treatment effect heterogeneity in an observational database in the OMOP Common Data Model. The package combines functionality of PatientLevelPrediction and CohortMethod R packages.

Features
========
- Extracts the necessary data from a database in OMOP Common Data Model format.
- Uses a large set of covariates for the prediction, the propensity and the outcome model, including for example all drugs, diagnoses, procedures, as well as age, comorbidity indexes, etc.
- Imports all functionality from PatientLevelPrediction package for patient level predictions.
- Large scale regularized regression to fit the propensity and outcome models.
- Only inverse probability of treatment weighting (IPTW) is now available for balancing covariates.
- Right now only fixed truncation at quantiles of weight distribution is implemented for handling of extreme weights.
- Includes diagnostic functions, including propensity score distribution plots and plots showing covariate balance before and after weighting.
- Right now only (conditional) cox regression outcome models are available.

Screenshots
===========
<table border = "">
<tr valign="top">
<td width = 33%>
  <img src="https://github.com/mi-erasmusmc/RiskStratifiedEstimation/blob/master/vignettes/comparisonPlot.jpeg" alt="CohortMethod propensity score plot" title="RiskStratifiedEstimation comparison plot" />
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
Requires R (version 3.1.0 or higher). Installation on Windows requires [RTools](http://cran.r-project.org/bin/windows/Rtools/). Libraries used in CohortMethod require Java.

Dependencies
============
 * Cyclops
 * DatabaseConnector
 * SqlRender
 * OhdsiRTools
 * FeatureExtraction
 * CohortMethod
 * PatientLevelPrediction
 * survival

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
* Package manual: [RiskStratifiedEstimation.pdf](https://github.com/mi-erasmusmc/RiskStratifiedEstimation/blob/master/extras/RiskStratifiedEstimation-manual.pdf)
* Developer questions/comments/feedback: <a href="http://forums.ohdsi.org/c/developers">OHDSI Forum</a>
* We use the <a href="../../issues">GitHub issue tracker</a> for all bugs/issues/enhancements

License
=======
RiskStratifiedEstimation is licensed under Apache License 2.0

Development
===========
RiskStratifiedEstimation is being developed in R Studio.

RiskStratifiedEstimation is still under development and should not be used yet to run risk stratified analyses.

