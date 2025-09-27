The program should read stress rainflow annual cycle count from CSV file and calculate damage rate (1/year).


## Input file Description

An input file should contain the following sections:

- raw data
  - folder: specs\modules\fatigue-analysis\stress_rainflow_to_damage\stress_rainflow

  - **File Naming Pattern**: `{config}_FC{###}_Strut{#}_{location_id}_stress_rainflow.csv`
    format: .csv
    column:
    - stress range (Mpa)
    - Cycles_Annual

- the location metdata is:
  - specs\modules\fatigue-analysis\stress_rainflow_to_damage\input\location_metadata.csv
  

- save curves from ABS publication in fatigue analysis module
  - reference: https://pub-rm20.apps.eagle.org/r/2/2020-06-01/Section-4-Fatigue-Design-Factors
  - run a parallel research to get fatigue curve data from other references, DNV, GL, etc.
- use SN Curve of ABS E curve, in air from above reference

- Apply thickness correction factor based on location thickness and reference thickness
  - reference thickness : 22 mm
  - thickness correction factor = (location thickness / reference thickness)^tk
  - tk: thickness exponent (check ABS reference for value)

## Output 
  - **File Naming Pattern**: `{config}_FC{###}_Strut{#}_{location_id}_damage_rate.csv`
    - **Output Folder**: specs\modules\fatigue-analysis\reference-seastate-scale-load\output\rainflow\stress_range
  - **Output File Type**: csv
  - output:
    - stress range (Mpa)
    - Cycles_Annual
    - Damage_Rate by stress range (1/year)
    - Total Damage Rate (1/year) by miners rule
  - A plot of Damage Rate vs Stress Range should be generated .
    - show against SN curve
    - give the best plot title, axis label, legend, etc.
  - A summary of all files analyzed saved to .csv.
