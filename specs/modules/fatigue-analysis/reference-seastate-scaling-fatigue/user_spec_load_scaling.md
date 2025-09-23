For a load scaling program, 

## Input file

the input file should contain the following sections:

- the reference seastate
  - reference seastate meta data file
    - specs\modules\fatigue-analysis\reference-seastate-scaling-fatigue\input\reference_seastate_definitions_sample.csv
  - timetrace data folder: specs\modules\fatigue-analysis\reference-seastate-scaling-fatigue\reference_data
- the fatigue seastate meta data csv file:
  - specs\modules\fatigue-analysis\reference-seastate-scaling-fatigue\input\fatigue_seastates_sample.csv

  - **Load Configuration Data**: For each of 4 vessel configurations
  - **Process Fatigue seastates**: For each fatigue seastate:
    - Select closest wind reference (by direction)
    - Select closest wave reference (by direction and Tp)

- Load scaling method
  - the load scaling parameters
    - **Wind Scaling**: (Vfatigue/vreference)²
    - **Wave Scaling**: Hs fatigue/Hs reference
    - Combine: Effective tension = scaled_wind + scaled_wave


- output
  - **File Naming Pattern**: `{config}_FC{###}_Strut{#}_{type}.csv`
  - **Output Folder**: specs\modules\fatigue-analysis\reference-seastate-scaling-fatigue\output
  - **Output File Type**: csv

