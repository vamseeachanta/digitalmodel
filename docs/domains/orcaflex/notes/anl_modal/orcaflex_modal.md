# Introduction

To help automate Orcaflex for a typical analysis

# Summary

The typical workflow to automate resonance analysis:
- Run Statics
- Run Modes
    - Can identify axial mode by looking at mode shapes (i.e. modes.modeDetails(modeIndex).shapeWrtGlobal, see python scripts)
- For Shear7 Modes files. 
    - Needs a very small background current to identify transverse direction to generate modes3D file.
- Assess shapes and identify axial mode 

### Notable Features

- Can compare 2 files in orcaflex (.dat files). File -> Compare
- Add pictures 1 on top of another i.e. import multiple models into same screen?


### Modal Analysis

- For 6D Buoy, the DegreesOfFreedomInStatics key for installation analysis can be X,Y,Z to cleraly identify vertical resposnance mode. Else, these modes will be mixed with other bending modes of the structure.
- Alternatively, the post process can be done to only look at X,Y,Z to avoid this confusion.

Example script:

<code>

LoadData SUT_DZ_30deg_200m.sim
SHEAR7MdsFile CraneWire#2 SUT_DZ_30deg_200m.mds 1 20

LoadData SUT_DZ_30deg_400m.sim
SHEAR7MdsFile CraneWire#2 SUT_DZ_30deg_400m.mds 1 20

</code>

