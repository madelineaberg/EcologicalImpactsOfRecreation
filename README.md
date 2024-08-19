# EcologicalImpactsOfRecreation
These analysis files can be used to reproduce the analyzes in our manuscript: Aberg et al. In-press. Effects  of outdoor recreation on multiple vertebrate guilds in a fragmented sagebrush-steppe ecosystem. Journal of Wildlife Management. 

## EcologicalImpactsManuscript R Project
This R Project file can be used to organize the R scripts described below and the datasets used in the analyses. 

## Recreation Intensity Analyses
Create the recreational intensity variable that is used as a predictor throughout the ecological analyses. The R scripts associated with this step of the analysis are described below. This step does not need to be repeated if using the clean data, which already include recreational intensity. 
### EcologicalImpacts_01_RecreationIntensity
This R script contains code to map a Gaussian kernel density estimate of locations of recreation and extract recreational intensity variables for 1-km2 sites and nests.
### EcologicalImpacts_01a_CleanDriveTabData
This script cleans the observational data used in EcologicalImpacts_01_RecreationIntensity. It is not necessary to run if you are using a clean dataset.
### EcologicalImpacts_01b_RecreationIntensityFunctions
This scrip contains the functions required to run EcologicalImpacts_01_RecreationIntensity. 

## Ground Squirrel Abundance
Assess the relationship between recreational intensity and the abundance of Piute ground squirrels. The R scrips associated with this analysis are described below. 
### EcologicalImpacts_02_GroundSquirrelAnalysis
This script contains the code to prepare the ground squirrel count data and create an N-mixture model to assess the relationship. 
### EcologicalImpacts_02a_GroundSquirrelCleanData
This script cleans the observational data used in EcologicalImpacts_02_GroundSquirrelAnalysis. It is not necessary to run if you are using a clean dataset. 

## Raptor and Raven Abundance
Assess the relationship between recreational intensity and the abundance of raptors and ravens. The R scrips associated with this analysis are described below. 
### EcologicalImpacts_03_RaptorRaven
This script contains the code to prepare the raptor and raven count data and create an N-mixture model to assess the relationship. 
### EcologicalImpacts_03a_RaptorRavenCleanData
This script cleans the observational data used in EcologicalImpacts_03_RaptorRavenAnalysis. It is not necessary to run if you are using a clean dataset. 

### Breeding Bird Abundance
Assess the relationship between recreational intensity and the abundance of breeding birds (long-billed curlew and horned lark). The R scrips associated with this analysis are described below. 
### EcologicalImpacts_04_BreedingBirdAnalysis
This script contains the code to prepare the breeding bird count data, estimate abudnace with a distance sampling model, and compare abundance with a Poisson linear regression. 
### EcologicalImpacts_04a_BreedingBirdCleanData
This script cleans the observational data used in EcologicalImpacts_04_BreedingBirdlAnalysis. It is not necessary to run if you are using a clean dataset. 

### Breeding Bird Nest Success
Assess the relationship between recreational intensity and the nest success of breeding birds (long-billed curlew and horned lark). The R scrips associated with this analysis are described below. 
### EcologicalImpacts_05_NestSuccessAnalysis
This script contains the code to prepare the breeding bird nest data, estimate Mayfield nest success, and compare probability of nest success with a binomial linear regression. 
### EcologicalImpacts_05a_NestSuccessCleanData
This script cleans the observational data (horned lark nest success) used in EcologicalImpacts_05_NestSuccessAnalysis. It is not necessary to run if you are using a clean dataset. 
### EcologicalImpacts_05b_NestHabClean
This script cleans the observational data (horned lark nest habitat surveys) used in EcologicalImpacts_05_NestSuccessAnalysis. It is not necessary to run if you are using a clean dataset. 
### EcologicalImpacts_05c_LBCUclean
This script cleans the observational data (long-billed curlew nest success) used in EcologicalImpacts_05_NestSuccessAnalysis. It is not necessary to run if you are using a clean dataset. 
