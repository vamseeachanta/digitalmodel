## Introduction

This document summarizes the AQWA typical errors and the way to troubleshoot them

### Summary

- Viewing in Workbench
  - Latest message (error, warning, info etc.) are given at the top.
  - read from bottom to top
- Viewing in command mode
  - Open .MES file
  - read from top to bottom

### Deck 2 | "NON-DIFFRACTING STRUCTURE"

Search for "NON-DIFFRACTING STRUCTURE"
<code>
**** INPUT DATA ERROR **** READING DECK 2: NON-DIFFRACTING STRUCTURE #2 CANNOT BE IN AN INTERACTING SET
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
