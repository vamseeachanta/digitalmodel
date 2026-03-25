## Introduction

To run dynamic analysis

# From the Pretension Iteration

- Copy the includefile_* files from the pretension iteration to the seperate folder to avoid overwrite of the iterated files.
  - The fender should be set to the correct compression
  - The mooring lines should be set to the correct pretension
- The wind and current should be ramped from zero
- The analysis does not converge using vessel 6 DOF in static. The vessel may end up in a very wrong direction.
  - If necessary, use 3 DOF or none in static analysis to force the correct static position of the vessel at the start of the simulation.
  - Alternatively, also utilize the 6 DOF from static no load and use it for the main analysis. However, this will involve a bit of effort
  - So, an initial settling period of 1000 s is used to ensure the vessel mooring tensions are achieved before main dynamics are underway. This initial period is ignored  for postprocessing of the results.

# Static Analysis

The static mooring tensions are to be achieved.

## Dynamic Analysis

- Define appropraite wind, current and wave and other dynamics as necessary
- Run the dynamic analysis
- Postprocess the results to ensure that the mooring and fender forces are within acceptable limits

# Troubleshooting:

- If a lwl or hwl configuration does not run. Take the nearest line lengths and reiterate again to achieve convergence
