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


## Revisions

### 2025-10-01, 9:42 AM

Updated visualization plots in spec and implementation:

**Changes to config.yml:**
- Deleted "polar_summary" plot section
- Replaced "polar_worst_case" plot with "bar_worst_case" (bar chart)
- Added new plot types:
  - `damage_contribution_worst_case_bar.png` - Bar chart for worst-case strut-location
  - `damage_contribution_worst_case_pie.png` - Pie chart for worst-case strut-location
  - `damage_worst_strut_all_locations.png` - Stacked bar comparing all locations for worst strut
  - `damage_worst_location_all_struts.png` - Stacked bar comparing all struts for worst location

**Changes to calculate_overall_damage.py:**
- Removed `_create_polar_summary_plot()` method
- Replaced `_create_polar_worst_case_plot()` with `_create_worst_case_bar_plot()`
- Added `_create_worst_case_pie_plot()` method
- Added `_create_worst_strut_stacked_bar_plot()` method
- Added `_create_worst_location_stacked_bar_plot()` method
- Updated `create_plots()` to call all new methods

**New Visualizations:**
1. Worst-case bar chart showing configuration contributions for the worst strut-location
2. Worst-case pie chart showing configuration contributions for the worst strut-location
3. Stacked bar chart for worst-case strut showing damage across all locations
4. Stacked bar chart for worst-case location showing damage across all struts

