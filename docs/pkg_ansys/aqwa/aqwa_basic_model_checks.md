## Basic Model Checks

THe basic model checks

### Summary

### Hydrostatic

- Weight, Gyradii etc. 
- Stability is part of ANSYS and will need to be checked to ensure runs are as expected.
- From raw model, automatically correct the 

### Hydrodynamic

- No load, no offset checks
  - X, Y and Z starting positions
  - find the equilibrium position and specify utilze for zero load
  - Check the structure positions, external loads etc.

- Repeat Stability analysis
  - Input correct FST position, (all structures)

- For structure where force is determined by external python code:
  - Going by pure force for determining position is difficult
  - Need to include a combination of spring and damper to get the equilibrium position
  - Provide additional force to get the overall force correct

- X, Y and Z starting positions
- Check if rotations are in deg or radians. Mark the plots accordingly

- **Stability** Run
  - Data taken from own software (ie. AQWA/GHS)
  - Position is input from GHS. Take it from AQWA.
  - Restoring force of the order 1e-8.

- WSP Dampener:
  - Deadband. How to capture it

- General: Troubleshooting
  - Get linter to work
  - Get logging to work

### Understanding Program Checks

- Change of version:
  - Check between 18.2, 2022R2 and 2024R1 for multiple runs
  - B1516 (Diffraction, Equilibrium and Wind etc.)
  - B1522 (Diffraction, Equilibrium, Dynamic analysis)

- Cut geoemtry vs No-cut geoemtry for all versions
  - Check between 18.2, 2022R2 and 2024R1

- Unit definition sensitivity. SI vs US Custmary units. See below run for details
  - K:\b1516\rev2\runs\ld_100MPH000WD_09moors.dat
    - * Hydrodynamic Solver Unit System : U.S. Customary: lb, ft [pdl]
  - K:\b1516\rev2\equilibrium\LIBR_NE.dat
    - * Hydrodynamic Solver Unit System : Metric: kg, m [N]

- How to restart AQWA runs in most efficient manner
  - on the restart analysis as well for the time being the best we can do is the one procedure that is in place i.e. restart 4 5 reading decks 9-20.
  - Refer to AQWA Reference Manual, Chapter 2

- Beta feature to fix 2 structures together in a DOF using 2024R1
  - See email from Alex Austin, DRD, subject "RE: ANSYS AQWA | Stability Run Assistance", May 20th 2024.
  - Alternatively use 2 fenders wtih high stiffness to simulate the same.

### External Force

- Perform the above hydrodynamic basic checks prior to producing results
-

### Animations

- How to load a solved .DAT file results into Workbench to get animations?
  - Easiest path
  - any WB script file to process multiple files for animations?
