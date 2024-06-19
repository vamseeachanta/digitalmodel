
## Introduction

Standardizing file preperation helps to:

- Understand each others work
- Easy to perform projects
- build bigger analysis teams

### Summary

<img src="flowcharts/aqwa_naming_.svg" width=400, height=auto/>

#### .DAT Filenaming Conventions

- The maximum length of the filename is 28 characters (32 with the extension).
- On Windows, file names are case-insensitive.

Naming comvention need to be simplified.

**#TODO** : convert to table and schematic for easier reference

- Analysis Type Conventions (2 characters):
  - Frequency or Hydrodynamic diffraction Analysis:
    - F_ : Prefix for Frequency or hydrodynamic diffraction Analysis
    - F_S_* : Limited no.of Frequency used. Quick analysis for first pass sanity checks
    - F_C_* : Comprehensive frequency analysis. Used for final results.
  - Static Analysis:
    - S_ : Prefix for Static Analysis
  - Dynamic Analysis:
    - D_ : Prefix for Dynamic Analysis

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

# Introduction

Inputs data preparation

## Summary

- Individual files can be combined
- The spaces in between files is not effecting the program runs or the results
- The files can be combined in a particular order ONLY (NOT in any order?)

## Steps

## Layout

### General Modelling

Need a flowchart here

- Bathymetry
- vessel/Structure
- Other modelling stuff (Scripts?)

## General Notes

AQWA is a "script friendly" product. Some tools to use are:
a/ journaling
b/ JScript programming
c/ utilities like Aqwa reader.

**Read AQWAReader Manual**

## Post Processing

### Philosophy

Generally, the dev team tries to keep the LIS file format as constant as possible to avoid breaking any legacy customer scripts that are designed around it. Of course, it's not 100% guaranteed that it won't change in the future.
There are some aspects are left over from the older format of post processing and result storage, e.g. one set of data is spread over multiple tables because this is how much fits on a sheet of paper.
It came to their attention recently from other users as well as Vamsee that there is need to focus on post-processing for a release. There are some talks about a version that is more automation friendly when it comes to post processing as well as automated output from AqwaWB.
I communicated the Aqwa reader with the dev team and they are looking into getting in a single AqwaReader operation to output all RAO data in one hit. They might have to limit it to separate DOFs, but at least these will be constant (unlike numbers of freqs/dirs).
on the restart analysis as well for the time being the best we can do is the one procedure that is in place i.e. restart 4 5 reading decks 9-20.

### Key Files

| File Extension | Description |
| --- | --- |
| **INPUTS**|
| .DAT | AQWA data file |
| .LIN | AQWA Lines Plan |
| ,MSD | AQWA Mass Distribution file |
| **OUTPUTS**|
| .HYD | AQWA hydrodynamic database file |
| .LIS | AQWA output file |
| .PLT | AQWA plot file |

## Batch Processing

Note the following:

- No .dat filetype extension should be given.

"C:\Program Files\ANSYS Inc\v182\aqwa\bin\winx64\aqwa.exe" /nowind "I:\B1516\100 mph\15\LIBR_15"
"C:\Program Files\ANSYS Inc\v182\aqwa\bin\winx64\aqwa.exe" /nowind "I:\B1516\100 mph\30\LIBR_30"
"C:\Program Files\ANSYS Inc\v182\aqwa\bin\winx64\aqwa.exe" /nowind "I:\B1516\100 mph\45\LIBR_45"
"C:\Program Files\ANSYS Inc\v182\aqwa\bin\winx64\aqwa.exe" /nowind "I:\B1516\100 mph\60\LIBR_60"
...

## Pre Processing

Pre- Solving options. Ability to run Python scripts to automate input generation process.

### AQWA Reader | Command Line

- Command line

### Setting up ?

<code>

```bash
"C:\Program Files\ANSYS Inc\v222\aisol\workbench.bat" -cmd "C:\Program Files\ANSYS Inc\v222\aisol\bin\winx64\AqwaReader.exe"
aqwa -b -i inputfile.dat -o outputfile.dat
```

### CSV PLT Route Options

- PLT1: For Structure 1 Parameters vs. Frequency
- PLT2: Direction 1
- PLT3: Free Floating Position RAOs
- PLT4: Surge (X) RAOs.

```

cd C:\Program Files\ANSYS Inc\v222\aisol\bin\winx64\

CALL ..\..\workbench.bat -cmd AqwaReader --Type "Graphical" --InFile "K:\b1522_ctr07\wb\fst1_015_files\dp1\AQW\AQW\AQ\Analysis\Analysis.plt" --OutFile "C:\Users\vamseea\fst1_l015_ad000_rao_p_dx" --Format "csv" --PLT1 1 --PLT2 1 --PLT3 1 --PLT4 1
CALL ..\..\workbench.bat -cmd AqwaReader --Type "Graphical" --InFile "K:\b1522_ctr07\wb\fst1_015_files\dp1\AQW\AQW\AQ\Analysis\Analysis.plt" --OutFile "C:\Users\vamseea\fst1_l015_ad000_rao_p_dy" --Format "csv" --PLT1 1 --PLT2 1 --PLT3 1 --PLT4 2
CALL ..\..\workbench.bat -cmd AqwaReader --Type "Graphical" --InFile "K:\b1522_ctr07\wb\fst1_015_files\dp1\AQW\AQW\AQ\Analysis\Analysis.plt" --OutFile "C:\Users\vamseea\fst1_l015_ad000_rao_p_dz" --Format "csv" --PLT1 1 --PLT2 1 --PLT3 1 --PLT4 3

CALL ..\..\workbench.bat -cmd AqwaReader --Type "Graphical" --InFile "K:\b1522_ctr07\wb\fst1_015_files\dp1\AQW\AQW\AQ\Analysis\Analysis.plt" --OutFile "C:\Users\vamseea\fst1_l015_ad000_rao_p_rx" --Format "csv" --PLT1 1 --PLT2 1 --PLT3 1 --PLT4 4
CALL ..\..\workbench.bat -cmd AqwaReader --Type "Graphical" --InFile "K:\b1522_ctr07\wb\fst1_015_files\dp1\AQW\AQW\AQ\Analysis\Analysis.plt" --OutFile "C:\Users\vamseea\fst1_l015_ad000_rao_p_ry" --Format "csv" --PLT1 1 --PLT2 1 --PLT3 1 --PLT4 5
CALL ..\..\workbench.bat -cmd AqwaReader --Type "Graphical" --InFile "K:\b1522_ctr07\wb\fst1_015_files\dp1\AQW\AQW\AQ\Analysis\Analysis.plt" --OutFile "C:\Users\vamseea\fst1_l015_ad000_rao_p_rz" --Format "csv" --PLT1 1 --PLT2 1 --PLT3 1 --PLT4 6

```

```
"C:\Program Files\ANSYS Inc\v181\aisol\workbench.bat" -cmd "C:\Program Files\ANSYS Inc\v181\aisol\bin\winx64\AqwaReader.exe" --InFile Analysis.plt --OutFile "output\example" --Format "csv" --PLT1 1 --PLT2 1 --PLT3 1 --PLT4 1


"C:\Program Files\ANSYS Inc\v182\aisol\workbench.bat" -cmd "C:\Program Files\ANSYS Inc\v182\aisol\bin\winx64\AqwaReader.exe" --InFile Analysis.plt --OutFile "output\example" --Format "csv" --PLT1 1 --PLT2 1 --PLT3 1 --PLT4 1

```

cd C:\Program Files\ANSYS Inc\v222\aisol\bin\winx64\

..\..\workbench.bat -cmd AqwaReader --Type "Graphical" --InFile "<path_to_your_project>\<project_name>_files\dp0\AQW\AQW\AQ\Analysis\Analysis.plt" --OutFile "stdout" --Format "csv" --PLT1 1 --PLT2 5 --PLT3 1 --PLT4 5

```

### JScript | Workbench

- Used for workbench.
- Cubersome to churn through menus and make it work.
- JScript is for ANSYS as S... is  for Python UI Automation.

### AQL | Excel Interface

### Python Script

Yes, python interface available for AQWA to run post finish commands results?

<https://forum.ansys.com/forums/topic/aqwa-export-data-csv-file/>

# Questions

### LIS File
start:
N A T U R A L   F R E Q U E N C I E S / P E R I O D S   F O R   S T R U C T U R E

end:
H Y D R O D Y N A M I C   P A R A M E T E R S   F O R
