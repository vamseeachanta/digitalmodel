## Basic Model Checks

THe basic model checks

### Summary

### Hydrostatic

Weight, Gyradii etc. Stability is part of ANSYS and will need to be checked to ensure runs are as expected.

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

- WSP Dampener:
  - Deadband. How to capture it

- General: Troubleshooting
  - Get linter to work
  - Get logging to work

### External Force

- Why do we get 4 values at each time step.
  - For now, take the final time step iteration value as the main value.
  - Check that multiple values are not for PMS3, PMS4, X and Y direction.
- dl-correction term
  - Do we need this?
