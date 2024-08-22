## Introduction

This document summarizes the AQWA typical errors and the way to troubleshoot them.

## Summary

- Viewing in Workbench
  - Latest message (error, warning, info etc.) are given at the top.
  - read from bottom to top
- Viewing in command mode
  - Open .MES file
  - read from top to bottom

### .MES File comparison

A typical comparison of errors is given below

| File.No | File.No | E01 | E02 |  E03 | W01 | W02 | W03 |
|------|------|-----|-----|------|-----|-----|-----|
| 1    |    |  X  |  -   |  -  |  X  |  X  | -  |
| 2    |    |  X  |  -   |  -  |  X  |  X  | -  |

Error and Warning List:
| Type | Code | Description |
|------|------|-------------|
| Error | E01 | Input Error - There was a failure to open the hydrodynamic data file due to a spelling error |
| Error | E02 | None |
| Warning | W01 | The number of requested cores often exceeds available licenses, causing it to be reduced |
| Warning | W02 | For Morison-type hull drag, the current at depth is ignored and the current at COG is used instead |
| Warning | W02 | Specified low-frequency damping is being withdrawn, and should be replaced with frequency-independent values using the FIDD card |
| Warning | W03 | Some modelling rule violations are ignored by the GOON option |

### Summary List

 #TODO summary list.

## Detailed Errors

### Deck 2 | "NON-DIFFRACTING STRUCTURE"

Search for "NON-DIFFRACTING STRUCTURE"
<code>
 ---------------------------------------------------------------- **** INPUT DATA ERROR ***READING DECK 2: NON-DIFFRACTING STRUCTURE #2 CANNOT BE IN AN INTERACTING SET
 ---------------------------------------------------------------------------------------------------------

</code>

**Explanation**

- In the geometry menu, structures are numbered from top to bottom in numerical order starting with 1. Note the external force python structure numbering starts at 0
- 2nd structure is non-diffracting. THerefore should be included in the "hydrodynamic diffraction" analysis.
- In workbench:
  - Select "Hydrodynamic Diffraction"
  - Select "Structure Interaction"
  - Select "Group of Structures"
  - In "Interacting Structure Group", select only diffracting structures i.e. unselect non-diffracting structures

### TimeStep or Natural periods not suitable

<code>
**** ERROR **** ABORTING- SOLUTION DIVERGED ON STRUCTURE 3 in X-Surge at time=4.80 step #48
 --------------------------------------------------------------------------------------------
**** ERROR **** YOUR TIME-STEP IS PROBABLY TOO BIG. For this time-step(0.100 secs) the natural periods of the system
                 throughout the analysis MUST BE GREATER THAN 1.00 seconds. i.e. Frequency LESS THAN 6.283 rad/sec.
                 PLEASE - Do NOT contact support unless you have: 1.Checked the system natural frequencies/periods.
                 2.Re-run with a smaller timestep. Support will require the results of these investigations BEFORE
                 replying
 ----------------------------------------------------------------------------------------------------------------------
</code>

###

<code>
 ELMAXF:SYSTEM LOGIC ERROR#1 - NEL=0 NNSI>0.<code>
</code>

**Explanation**

### TERMINATED WITH ERRORS: ERROR COUNT=    1

Analysis terminated without .MES file
Error is mentioned in .LIS file

<code>
 ----------------------------------------

FINISH DATE:03/06/24       TIME:03:36:45
 Total elapsed time (Seconds): 770774400.00
 ----------------------------------------

 ***************************************************
 **** TERMINATED WITH ERRORS: ERROR COUNT=    1 ***
 ***************************************************
</code>

**Explanation**

- Filename is too long.
- The maximum length of the filename is 28 characters (32 with the extension).
- On Windows, file names are case-insensitive.

### Damping Related Errors

<code>
       DGDP                 10.       10.       10. 1.71887e9 4.0107e10  572.9578
       *****
 **** INPUT DATA ERROR  :LINE 10693 USER-SPECIFIED LOW FREQUENCY DAMPING CANNOT BE USED FOR

                                    CONVOLUTION.
 
                                    USE FREQUENCY *INDEPENDENT* INPUT. E.G.FIDP,FIDD.
</code>

**Explanation**

- Background:
  - Error occurs when using the following options:
    - RESTART  4  5
    - RESTART  5  5

- Resolution:
  - Use frequency independent damping options
    - FIDP?
    - FIDD?

<code>
 **** ERROR **** CHKAD0: - CONVTH - T/H CONVOLUTION ERROR.
                 The low frequency added mass/damping (deck 9) has
                 been modified. It cannot be used in the time history
                 convolution - Structure#1

                 USE FREQUENCY INDEPENDENT ADDITIONAL ADDED
                 MASS/DAMPING. (See FIAM,FIDA,FIDP,FIDD input)
</code>
**Explanation**

- Background:
  - Error occurs when using the following options:
    - RESTART  1  5

- Resolution:
  - if running DRIF analysis, ensure WFRQ is used
<code>

JOB AQWA  DRIF  WFRQ  c07a21bd1a5b2414c4c6f713d4d6cd2fbf73b23f
</code>

**Explanation**

- Definition of damping is missing or overwritten by the program. Review damping section document.

### Fender

<code>
 Omni-directional Fenders can only be of Fixed Type, not Floating Type.
</code>
**Explanation**

### Irregular Wave Group

<code>
 More than one Irregular Wave Group exists, but only one is permitted. Suppress or delete surplus definitions.
</code>
