
## Introduction

Standardizing file preperation helps to:

- Understand each others work
- Easy to perform projects
- build bigger analysis teams

### Summary

<img src="flowcharts/aqwa_naming_.svg" width=400, height=auto/>

#### Filenaming Conventions

- The maximum length of the filename is 28 characters (32 with the extension).
- On Windows, file names are case-insensitive.

Naming comvention need to be simplified.

**#TODO** : convert to table and schematic for easier reference

- Analysis Type Conventions (2 characters):
  - Frequency Analysis: F_
  - Static Analysis: S_
  - Dynamic Analysis: D_

- WFRQ_02PMS_ad010_fst2_l015 : Limited frequency run (Done)
- WFRQ_ad010_fst2_l015 : Comprehensive frequency run (Done)
- STAT_ad010_fst2_l015 : Equilibrium run (postprocess?)
- Analysis Type (WFRQ, STAT/Stability, DYNA ) + Structure (02PMS) + Damping (ad010) + Structure (fst2) + Length (l015)
        Structure Conventions (5 characters) : 2PMS_, fst2_, l015_? (project specific?)
        Wave and Current (12 characters) (examples: W01_V01_C01_: ??
        Damping Conventions (4 charaters) : d00_, d08_, d10_, d22_ (examples)
          - small d Occurs in middle
        Frequency Analysis:
          - Simple : SIMP
          - Comprehensive : COMP

### Architecture

Project folder
