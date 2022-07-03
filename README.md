# CIND820_Capstone


ASSESSMENT OF TORONTO CRIME DATA THROUGH EXPLORATORY DATA ANALYSIS AND CLASSIFICATION METHODS

This repository contains source code and related documents relevant to my capstone project in the Data Analytics, Big Data and Predictive Analytics Certificate Program at Toronto Metropolitan University.
The goal of this project is to assess crime within the city of Toronto, via the Toronto Major Crime Indicators dataset released by the Toronto Police Service, to identify the relevant factors associated with crime, in addition to crime prediction through the application of Decision Tree, Multinomial Logistic Regression and Naïve Bayes classification algorithms.  The comparative performance of the classification algorithms will also be assessed.

Research questions include:

	Which Toronto neighborhoods are the most violent and which the least violent (generally assessed as high crime versus low crime)?
	What are the general crime trends within the City of Toronto?  
	Are there recognizable temporal trends?  
	Are specific crime types concentrated within certain geographical areas?  
	Which neighborhoods are the most violent and which the least violent?

	Which neighborhood has the highest incidence of crime and which neighbourhood the lowest?

	Which classification algorithm(s) demonstrate the most potential for prediction of crime categories?

	Which variables most influence crime?

The primary dataset used for this project was the 2014 to 2021 Toronto Major Crime Indicator (MCI) data released by the Toronto Police Service and available through the Toronto Police Service Public Safety Data Portal (https://data.torontopolice.on.ca/search?q=crime).  Additional datasets include the Crime Severity Index weights for Canada (Statcan, 2021), geographic feature files (shapefiles) of Toronto Police patrol zones (https://open.toronto.ca/dataset/patrol-zones/) and Toronto neighbourhoods (https://open.toronto.ca/dataset/neighbourhoods/), and Toronto neighbourhood profiles (https://open.toronto.ca/dataset/neighbourhood-profiles/).

The following files are included in the repository:
1.	AultK CINDI820 Assignment 1 Abstract_Orig (pdf): The original abstract submitted describing project and rationale.

2.	AultK CINDI820 Assignment 2 LiteratureDataReview (pdf): The review of available literature detailing crime prediction through classification algorithms as well as a first pass review of the datasets.  Includes the updated abstract.

3.	AultK CINDI820 Assignment3 InitialResultsCode (pdf):  Descriptions of feature selection methods and results, SMOTE oversampling of the reduced dataset and initial classification of the dataset using J48 Decision Tree, Multinomial Logistic Regression and Naïve Bayes algorithms.

4.	Capstone_DataAssessment_EDA (R file): Contains the code to load the datasets as well as the evaluation, initial cleaning, and initial manual feature selection of the Toronto MCI dataset.

5.	CIND820_Assignment3_FeatureSelection_InitialResults (R file):  Contains the code to carry out feature selection, SMOTE balancing of the dataset and classification of the MCI dataset through implementation of Decision Tree (J48), Multinomial Logistic Regression, and Naïve Bayes algorithms.

6.	MCI_WorkingDataset (csv):  The cleaned dataset prior to final feature selection and SMOTE oversampling.

7.	CIND820_TechnicalReports_Compilation (pdf):  The combined code from the EDA and FeatureSelection R files in pdf format. 

