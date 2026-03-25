# Objective

To summarize the steps to run the case study

## Summary

- Rerun with more processors. See sensitivity of run times. How to do this? Current runtime: 6 hours using 4 processors
- Also generate the water height along the hull plot (post processing)

# Run Errors

**Entry 'Umean' not found in dictionary in U.boundaryField.outflow**

Entry 'Umean' not found in dictionary "/home/vamsee/openfoam_others/wigley_hull/wigleyHull_LTS/0/U.boundaryField.outflow"
openfoam Entry 'Umean' not found in dictionary in U.boundaryField.outflow

Solution:
Added the "Umean           1.452;" line to the outflow boundary condition in 0_org/U file. See the updated file snippet below. This made it to run

<code>
    outflow
    {
        Umean           1.452;
        type            outletPhaseMeanVelocity;
        alpha           alpha.water;
        UnMean           $Umean;
        value           $internalField;
    }
</code>

    Other Downstream error caused by due to Umean error above

    **openfoam not creating 0 folders**

    <https://www.cfd-online.com/Forums/openfoam-pre-processing/217890-redistributepar-does-not-create-0-folder-new-processor-folders.html>

    **Summary**

    openfoam not creating 0 folders

    **Details**

    cannot find file "/home/vamsee/openfoam_others/wigley_hull/wigleyHull_LTS/processor0/0/p_rgh"
    cannot find file "/home/vamsee/openfoam_others/wigley_hull/wigleyHull_LTS/processor2/0/p_rgh"
    cannot find file "/home/vamsee/openfoam_others/wigley_hull/wigleyHull_LTS/processor3/0/p_rgh"

# Paraview PostProcess Errors

**Error in reading 0 in line 21**
See more details on error below. Did not resolve this error. Used the option to check "skip zero time" to avoid this error.
Alternatively can copy U file from another solution (sol_5000 or 500) folder to here.

<code>
ERROR: In ./VTK/IO/Geometry/vtkOpenFOAMReader.cxx, line 8654
vtkOpenFOAMReaderPrivate (0x5620750cb5a0): Error reading line 21 of /home/vamsee/openfoam_others/wigley_hull/0/U: Unexpected token 0
</code>

**Rename Solution file from sol_5000 to 5000**
Rename of solution file helps read the solution in paraview and can visualize the results for 5000s (i.e. end of simulation)

### Downstream error due to Umean error above

**openfoam not creating 0 folders**

<https://www.cfd-online.com/Forums/openfoam-pre-processing/217890-redistributepar-does-not-create-0-folder-new-processor-folders.html>

**Summary**

openfoam not creating 0 folders

**Details**

cannot find file "/home/vamsee/openfoam_others/wigley_hull/wigleyHull_LTS/processor0/0/p_rgh"
cannot find file "/home/vamsee/openfoam_others/wigley_hull/wigleyHull_LTS/processor2/0/p_rgh"
cannot find file "/home/vamsee/openfoam_others/wigley_hull/wigleyHull_LTS/processor3/0/p_rgh"

