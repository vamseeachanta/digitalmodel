Key checks
- Mesh checks
  - In "Calculation & output" tab, check the "Perform validation of panel arrangement" box.
    - 3 parameters are available as follows
     - Min body volume
     - Max panel aspect ratio
     - min panels per wavelength
    - Details TBA
  - Mesh panel normals. 
    - Typically they should point into the hull 
    - If they are pointing in the wrong direction, it should be corrected in the meshing tool

- Mesh correction by OrcaFlex
 - Non planar mesh elements. Orcaflex can correct these. 
   - triangular elements with 3 points are in the same plane
   - quadrilateral elements with 4 points will be converted to triangular elements to be in plane. 

- Mesh Addition by OrcaFlex
  - Add interior surface panels to remove irregular frequency effects
  - If the draft of the vessel changes
    - the panels are automatically adjusted such that only the bottom panels are used for analysis. 
    - Adding the interior panels corrects the mesh below and above waterline and thus provides accurate results than AQWA.
    - 