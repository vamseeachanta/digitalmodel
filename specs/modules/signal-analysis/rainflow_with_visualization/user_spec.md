For a load scaling program, 

## Input file Description

An input file should contain the following sections:

- input file
  - folder: specs\modules\signal-analysis\rainflow_with_visualization\timetrace
    format: .csv
    column: Scaled Tension (kN)

- rainflow counting method:
  - determine timetrace length using at each individual csv timetrace
  - provide rainflow counting parameters
    - **Number of bins**: 50
    - **Bin method**: logarithmic
    - **Min bin value**: 0.1
    - **Max bin value**: 1000

- fft method:
  - window type: Hanning
  - window time length (seconds): 500
  - overlap (%): 50

## Output
  - **File Naming Pattern**: `{config}_FC{###}_Strut{#}_{type}.csv`
  - **Output Folder**: specs\modules\fatigue-analysis\reference-seastate-scale-load\output
  - **Output File Type**: csv

- prepare rainflow counting results
  - folder: specs\modules\signal-analysis\rainflow_with_visualization\output
    format: .csv
    type: rainflow
    content: rainflow counting results

- prepare fft chart for the timetrace
  - folder: specs\modules\signal-analysis\rainflow_with_visualization\visualization
    format: .png
    type: fft
    content: fft chart of the timetrace

- Prepare a visualization of the rainflow counting results using a sample timetrace
  - folder: specs\modules\signal-analysis\rainflow_with_visualization\visualization
    format: .png
    type: rainflow
    content: similar to work performed in this repo in: "specs\modules\fatigue-analysis\reference-seastate-scaling-fatigue\rev_0\core\output\verification\step_by_step\figures\rainflow_visualization.png"

