

1. The data process in this project uses [the `drake` package](https://ropenscilabs.github.io/drake-manual/).

2. All raw data are stored on [the iplant site](https://iplant.plantandfood.co.nz/project/P442060-13/Research/Forms/AllItems.aspx).



# Data 

1. Plant LECO data seems already been transferred to the biomass sheet. Need to confirm with Mike - **YES**

   a. trts 1-4 (SD1) got all components done for intermediate and final harvest 
   b. trts 5 & 6 (SD2 & 3) final harvest components only I think


2. [Soil data](https://iplant.plantandfood.co.nz/project/P442060-13/_layouts/15/WopiFrame.aspx?sourcedoc=/project/P442060-13/Research/PeaProtein_Soil_Test_Results_2018-19.xlsx&action=default): 
    
    a. Lincoln has 4 sets
    b. HB has 1 set 

3. Greenseeker and Sunscan data from Lincoln are avilable in the same [excel](https://iplant.plantandfood.co.nz/project/P442060-13/_layouts/15/WopiFrame.aspx?sourcedoc=/project/P442060-13/Research/Pea_Protein_GS_2018-19_Lincoln.xlsx&action=default)

  3a. Greenseeker data is incomplete for HB - Nathan has the raw data in the recon, will send them over. 
  3b. Sunscan data is called ceptometer in the excel sheet. 

4. TDR [data](https://iplant.plantandfood.co.nz/project/P442060-13/_layouts/15/WopiFrame2.aspx?sourcedoc=/project/P442060-13/Research/Peas2018_19_SWCirrigation_Lincoln_HB.xlsx&action=default)


# Objectives 

1. Summarise all data
2. EDA with visulisation 
3. Pull summarised data into APSIM input format


# To do 

identify the keys for combining all sheets:

  1. Site, Date and plot are the primary key?!
  2. Dates from different tables doesn't always match

add units to the biomass graphy


[use sql to keep the tidy data](https://jupyterhub.powerplant.pfr.co.nz/user/cflfcl/notebooks/00.jupyter_examples/UsingSQLiteWithBash.ipynb)

sqlite store date as Julian day with the origin date "1970-01-01"
need date conversion when retrive the date

```
gs_sql <- dbReadTable(con, "greenseeker")
gs_sql %>% 
  mutate(date = as.Date(c(17835), origin = "1970-01-01"))
```
