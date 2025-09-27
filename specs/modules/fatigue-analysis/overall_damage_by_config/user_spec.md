The program should calculate total damage for all configurations based on % of time of each configuration.


## Input file Description

An input file should contain the following sections:

- raw data
  - total fatigue damage file for each configuration, strut and location
    - specs\modules\fatigue-analysis\overall_damage_by_config\input\fatigue_life_summary.csv
    column:
    - Total Damage Rate (1/year)

- methodology
  - yearly damage rate multiply by duration percentage for each configuration
  - duration in:
    - specs\modules\fatigue-analysis\overall_damage_by_config\input\config_weightage.csv
  - miners rule
  - calculate fatigue life 
    - for strut and at each location

## Output


  - **Output Folder**: overall_damage
  - **Output File Type**: csv
  - output:
    - damage rate by configuration
    - % contribution to total fatigue by configuration
    - Total Damage Rate (1/year) by miners rule
    - fatigue life (years)

  - A plot showing damage rate by configuration and cumulative damage rate
