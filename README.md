# Climate and health benefits of a transition from gas to electric cooking
Repo supporting Gould et al. "Climate and health benefits of a transition from gas to electric cooking" (2023).

The materials in this repository enable replication of the main results, including figures, with the exception of results derived from customer-level electricity billing records which are subject to a private use data sharing agreement. 

If you find meaningful errors or have questions or suggestions, please contact Carlos Gould at gould.cf@gmail.com

# Organization of the repository

Scripts are data are primarily contained in this respository.

- **data**: inputs for analysis
- **scripts**: R code for analysis

## List of scripts

- **Parish_PEC_Analysis.R**: reads in combined data on parish-month PEC enrollment, electricity consumption and LPG consumption and conducts all related analyses for the paper, including the generation of Figures 1 and 3.
- **Hospitalizations_Analsyis.R**: reads in combined canton-month hospitalizations data, conducts all analyses and generates figures related to hospitalizations. Generates Figure 4

## List of datasets

-**ecaudor_electricity_combined.rds**: parish-month combined dataset on PEC enrollment and electricity consumption and relevant covariates for analysis
-**ecuador_lpg_consumption.csv**: nation-month dataset on LPG sales/consumption
-**ecuador_hospitalizations_combined.rds**: canton-month combined dataset on cause-coded hospitalizations and relevant covariates for analysis
-**icd_walk.csv**: crosswork for linking icd codes (and their variable names) to descriptive phrases

### Notes on obtaining raw data

Processed datasets available in this repository are derived from public use sources. 

- Yearly hospitalizations data can be obtained freely from https://www.ecuadorencifras.gob.ec/camas-y-egresos-hospitalarios/
- 
