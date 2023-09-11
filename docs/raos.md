# Introduction

To help QA RAOs

# Summary

- Check displacement RAOs
    - Utilize long seas RAOs criteria
    - Compare betwen RAOs 
- Check seastate RAOs by plotting
    - Compare RAOs 
        - at different locations (Extremes and middle)
        - at different wave frequencies
        - at different wave directions
        - Between different RAOs

## Key Assumptions

- For orcaflex RAO analysis, the program assumes that there is only 1 vessel defined. The index of the vessel is hard coded to generate plots.
- For single file:
    - RAO amplitude vs. period for all headings
    - RAO Phases should follow long period criteria (i.e. free riding vessel)
    - RAO amplitude should follow long period criteria (i.e. free riding vessel)

## Seastate RAOs Filtering

- Convert vessel .dat file with Seastate RAOs to .yml file
-  Run the digitalmodel program and provide the CoG and the structures dimensiosn relative to .yml. Coordinates should be the model coordinates of the vessel.
- Run the program with the .yml file and the digitalmodel output file.
- Copy digitalmodel output file into suitable format with a key and the output as value.
- Copy this into the main file to get the reduced seastate RAO file for further analysis.
- Also copy the updated filtered grid drawing file for visual purposes (and accuracy)


