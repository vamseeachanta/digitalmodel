The program should read stress rainflow annual cycle count from CSV file and calculate damage rate (1/year).


## Input file Description

An input file should contain the following sections:

- raw data
  - folder: specs\modules\fatigue-analysis\cummulative_damage\sample_damage_results

  - **File Naming Pattern**: `{config}_FC{###}_Strut{#}_{location_id}_damage_rate.csv`
    format: .csv
    column:
    - stress range (Mpa)
    - Cycles_Annual
    - Damage_Rate by stress range (1/year)
    - Total Damage Rate (1/year) by miners rule

- the fatigue condition occurrence metadata is:
  - specs\modules\fatigue-analysis\cummulative_damage\input\fatigue_seastates_production.csv

- methodology
  - yearly damage rate multiply by probability of occurrence of each sea state
  - miners rule
  - calculate fatigue life 
    - for each configuration, for each strut, at each location

## Output
  - **File Naming Pattern**: `{config}_FC{###}_Strut{#}_{location_id}_fatigue_life.csv`
    - **Output Folder**: specs\modules\fatigue-analysis\reference-seastate-scale-load\output\rainflow\stress_range
  - **Output File Type**: csv
  - output:
    - damage rate by fatigue condition
    - % contribution to total fatigue by fatigue condition
    - Total Damage Rate (1/year) by miners rule
    - fatigue life (years)
  - A plot showing damage rate by fatigue condition and cumulative damage rate
  - A summary of all files analyzed saved to .csv.