This readme file was generated on 2022-08-11 by Angela Zhang


GENERAL INFORMATION

Description: RDI proportion of aquatic (animal and plant source) and land (animal source) based foods

DATA SOURCES

- [AFCD Sci](https://github.com/Aquatic-Food-Composition-Database/AFCD ) 
- [Daily Reference Intake](https://github.com/cfree14/nutrient_endowment/blob/b410c3c1cc9269e0268854873d661aeb25986aeb/data/ears/data/raw/recommended_intakes_individuals.pdf) 
    - [Scraped](https://github.com/cfree14/nutrient_endowment/blob/b410c3c1cc9269e0268854873d661aeb25986aeb/data/ears/data/dietary_reference_intake_data.Rds)
- [USDA Standard Reference Legacy](https://fdc.nal.usda.gov/download-datasets.html)

 

DATA & FILE OVERVIEW

File List: 

- relevant_nutrients.R: cleaning and merging of three dataset + code generating visualizations  
- USDA_cleaning.R: cleaning and merging script for USDA FoodData Central Standard Reference Database

Relationship between files, if important: 

- USDA_cleaning generates one of the datasets that will be merged in relevant_nutrients.R



METHODOLOGICAL INFORMATION

Description of methods used for collection/generation of data: <include links or references to publications or other documentation containing experimental design or protocols used in data collection>

Methods for processing the data: <describe how the submitted data were generated from the raw or collected data>

Instrument- or software-specific information needed to interpret the data: <include full name and version of software, and any necessary packages or libraries needed to run scripts>

Standards and calibration information, if appropriate: 

Environmental/experimental conditions: 

Describe any quality-assurance procedures performed on the data: 

People involved with sample collection, processing, analysis and/or submission: 


DATA-SPECIFIC INFORMATION FOR: [FILENAME]
<repeat this section for each dataset, folder or file, as appropriate>

Number of variables: 

Number of cases/rows: 

Variable List: <list variable name(s), description(s), unit(s) and value labels as appropriate for each>

Missing data codes: <list code/symbol and definition>

Specialized formats or other abbreviations used: 


THINGS LEFT TO DO
- Find missing nutrients in database that Chris Free used to creat RDI dataset 
- Verify that nutrients/analysis methods are comparable across three source databases
- DRI data is missing nutrients 
