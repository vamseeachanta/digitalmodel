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

Example script:
<code>

LoadData SUT_DZ_30deg_200m.sim
SHEAR7MdsFile CraneWire#2 SUT_DZ_30deg_200m.mds 1 20

LoadData SUT_DZ_30deg_400m.sim
SHEAR7MdsFile CraneWire#2 SUT_DZ_30deg_400m.mds 1 20

</code>

