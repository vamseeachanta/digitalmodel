## Introduction

This document summarizes the AQWA typical warnings and the way to troubleshoot them.
Ignoring warnings can be a early sign of bigger problems that may be reasons for convergence errors and issues.

### Summary

- Viewing in Workbench
  - Latest message (error, warning, info etc.) are given at the top.
  - read from bottom to top
- Viewing in command mode
  - Open .MES file
  - read from top to bottom

### Input Data | Cores vs. License

<code>

**** INPUT DATA WARNING **** Requested number of cores, 12, is reduced to the number of available licenses, 4
 --------------------------------------------------------------------------------------------------------------

</code>

**Explanation**

- TBA

### Input Data | SEAG in Workbench vs. Command

<code>
**** MODELLING WARNING **** WHEN AQWA IS *NOT* RUN FROM WORKBENCH, ONLY 2 PARAMETERS (RESOLUTION) ARE ALLOWED ON THE
                             SEAG CARD. DEFAULT VALUES HAVE BEEN USED FOR THE GRID SIZE
 ----------------------------------------------------------------------------------------------------------------------
</code>

**Explanation**

- TBA

### Input Data | GOON Option

<code>
**** MODELLING WARNING **** 5 MODELLING RULE #1 VIOLATIONS IGNORED BY GOON OPTION
 ----------------------------------------------------------------------------------

</code>

**Explanation**

- This option allows the analysis to go on in spite of the modeling rule violations. Most of the
modeling errors will be turned into warnings by this option. Users are advised not to use this option unless the violations are minor and difficult to correct.

### Input Data | COG is not on the vertical line across COB

<code>

**** MODELLING WARNING **** COG is not on the vertical line across COB
 --------------------------------------------------------------------------------

</code>

**Explanation**

- Perform appropriate stability analysis and use calculated positions to avoid this problem.

### Input Data | Cores vs. License

<code>

**** ANALYSIS WARNING **** *.MSD not exist. Use PMAS input
 --------------------------------------------------------------------------------

</code>

**Explanation**

- Mass data is provided through either .MSD file or .dat file

### Other Warnings

**** INPUT DATA WARNING **** Without WFRQ in JOB card for AQWA-DRIFT run: Drift frequency responses ONLY in this
                              analysis
 ------------------------------------------------------------------------------------------------------------------

**** WARNING **** Previously defined additional structure stiffness information in the hydrodynamic database file
                   (*.HYD/*.RES) is set to be zero as Stage#2 is required by the current analysis task. New relevant
                   input in Deck 7 will be used
 ---------------------------------------------------------------------------------------------------------------------

 **** WARNING **** Previously defined additional stiffness for STR#1 in the hydrodynamic database file (*.HYD/*.RES) is
                   set to be zero as Stage#2 is required by the current analysis task. New relevant input in Deck 7
                   will be used.

**** INPUT DATA WARNING **** Additional structure stiffness has been defined for structure#5 , but additional
                              structure stiffness force at initial position has not been defined
 ---------------------------------------------------------------------------------------------------------------

**** ANALYSIS WARNING **** Negative generalized damping = -0.68 is detected at frequency =0.04 rad/sec, for the mode
                            with the 1st 4 dominant motions of Str#1 Z:0.97 , Str#1 RY:0.22 , Str#1 X:0.01 , Str#1
                            RX:-0.01
 ----------------------------------------------------------------------------------------------------------------------
