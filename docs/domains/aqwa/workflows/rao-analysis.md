
## Introduction

Instructions to run AQWA RAOs using digitalmodel repository.

### Summary

### Run Instructions

General steps are given below:

- Run the program using input file - 1st time:
  - Define
    - CoG
    - weight
    - inertia, etc
  - Define additional nodes for the model (eg for motions, etc.)

- Manual- Create remaining deck files:
  - copy/rename wb_*01.deck to avoid overwriting by program
  - copy the output of node definitions to renamed *01.deck file
  - Combine files into .dat (DONE)
  - Run AQWA (DONE)
  - Verify out-of-balance forces.  (Manual - no need?)

- Run the program using input file - 2nd time:
  - Get RAOs (with no damping) (BatchFile)
  - Get RAOs output and identify peaks (Used AQWA Undamped Natural Period output)
  - Extract damping values (DONE)
  - Ensure frequency resoultion is sufficient around peaks (Manual process)
  - Define frequency independent damping values (DONE)

- Run the program using input file - 3rd time:
  - Rerun Diffraction analysis (Manual)
  - Extract RAOs (Script exisits)
  - Plot RAOs (Independent process)
  - Plot RAO comparisons (Independent process)

# TODO

- Verify result change with change in Draft and coG Z definition (Pending)
