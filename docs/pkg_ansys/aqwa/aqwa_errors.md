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
Error: Solve aborted.
Error: The Aqwa Core Solver stopped unexpectedly.
Error: TERMINATED WITH ERRORS.
Error: READING DECK 2: **NON-DIFFRACTING STRUCTURE #2** CANNOT BE IN AN INTERACTING SET.
</code>

**Explanation**

- In the geometry menu, structures are numbered from top to bottom in numerical order starting with 1. Note the external force python structure numbering starts at 0
- 2nd structure is non-diffracting. THerefore should be included in the "hydrodynamic diffraction" analysis.
- In workbench:
  - Select "Hydrodynamic Diffraction"
  - Select "Structure Interaction"
  - Select "Group of Structures"
  - In "Interacting Structure Group", select only diffracting structures i.e. unselect non-diffracting structures
