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

## Post Processing

## Key Files

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

## Pre Processing

Pre- Solving options. Ability to run Python scripts to automate input generation process.

### AQWA Reader | Command Line

- Command line

#### Setting up

<code>

```bash
"C:\Program Files\ANSYS Inc\v222\aisol\workbench.bat" -cmd "C:\Program Files\ANSYS Inc\v222\aisol\bin\winx64\AqwaReader.exe"
aqwa -b -i inputfile.dat -o outputfile.dat
```

PLT Options:

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

# Questions
