# NLP: Topic modelling on medical reports

Application of NLP in the public health sector

## Introduction
This work aims to provide an introductory concept of Topic modelling a branch of Natural Language Processing (NLP) in the medical field. Topic modelling also known as the unsupervised machine learning (UML) is a way of establishing trends, patterns in the text such that topics can be drawn from the textual data. We advance the mining of data in an under-researched field by means of exploring rich data (text) encountered in public health which may be readily accessible and analyzable by various stakeholders. 

### Study objectives
Firstly, AI generation and compilation of structured and unstructured data (text) from case (medical) reports from a given time frame.

Secondly, to carry out Natural Language Processing (NLP) specifically, topic modelling to observe the most prevalent signs and symptoms, diagnosis, treatment, prognosis and follow-ups in the health institution. 

## Text preprocessing
We perform pre-processing of the textual data by:
-lowering all cases to lower cases,
-removing all punctuation,
-removing all English stop words,
-removing numbers and
-tokenization.

## Data exploration
There was a generation of pictorials that enable us to have a picture of the most common signs and symptoms in the medical institution or region under consideration. This would be instrumental in the distribution of resources and the quantities likely needed in the institution(s) with time. Moreover, the frequencies can be an early indicator of any brewing pandemics.

## Model formulation
we built the Latent Dirichlet Allocation (LDA) topic model from the medical records data of patients signs and symptoms and ascertained the optimal number of topics for the data. Topic distributions for each patient and the sign/symptoms distributions on the topics were calculated and analysed, the *beta* and *theta* probabilities respectively.

Finally, we discuss the findings and provide a brief future course of action.

## Files
### Data
*patient_data.txt*

*patient_data.csv*

### Code
*data_processing.R*

*topic_modelling_medical-reports.Rmd*

*topic_modelling_medical-reports.html*

### Model output
*beta_probabilities.csv*
*theta_probabilities.csv*

## Author
Tawanda Gallan Chakuvinga



