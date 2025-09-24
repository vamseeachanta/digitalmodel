The program should read tension data from a CSV file, apply a linear transformation using a lookup table to convert tension ranges to stress ranges, and output the results into new CSV files named according to a specific pattern.

For a data transformation program, 

## Input file Description

An input file should contain the following sections:

- tension data
  - folder: specs\modules\data-transformation\lookup-table\data
  - 
  - file naming pattern (fsts_FC001_Strut1_rainflow): `{config}_FC{###}_Strut{#}_rainflow.csv`
    format: .csv
    column: Range (kN)

- transformation
  - source: lookup
  - method: linear
  - lookup table: specs\modules\data-transformation\lookup-table\inputs\tension_range_to_stress_range_function.csv
    x: tension range (kN),
    y: stress range (Mpa)
  - other columns for metadata based filtering on file naming pattern:
    - location ID
    - config

## Output
  - **File Naming Pattern**: `{config}_FC{###}_Strut{#}_{location_id}_stress_rainflow.csv`
  - **Output Folder**: specs\modules\fatigue-analysis\reference-seastate-scale-load\output\rainflow\stress_range
  - **Output File Type**: csv
  - output: 
    - stress range (Mpa)
    - Cycles_Annual

