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


THINGS LEFT TO DO
- Find missing nutrients in database that Chris Free used to creat RDI dataset 
- Verify that nutrients/analysis methods are comparable across three source databases
- DRI data is missing nutrients 
- DRI data Protein use g/kg or g?, for now removing protein from DRI 
- Add food part to USDA data (and other features in the AFCD)
- facet by relevant taxa 
