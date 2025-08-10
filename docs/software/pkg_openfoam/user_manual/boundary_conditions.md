## Boudary Conditions (BCs)

Boundary Condition (BC) information is primarily stored in the 0/ directory for any case set up.

This directory holds information about each of the fields required to run a simulation (pressure, velocity, turbulence, etc.). We’ll use an example case that is running pimpleFoam with laminar flow, therefore the 0/ directory will have the following files:

<code>
0/
… U
… p
</code>

The boundary conditions that go into the 0/U file are also similar to the quantities that go into 0/p and other files:

- Within the 0/U file, each of the boundaries defined in the polyMesh/ folder will require a BC to enable the solver to run.
- Manual entry for each BC is an option, and can be suitable for simple simulations (small number of boundary faces and field files), however, this method will quickly become tedious as additional boundaries and field files are added.
- Additionally, if there are changes to the BCs, it becomes tedious and error prone to manually change each entry.

### Tip 1: Writing multiple boundary faces to the same boundary condition

- If you have multiple boundary faces that you want to apply the same BC to, you can write the BC information for the first face, and then write the same BC information for the remaining faces.
- For example, if you’re running a case that has two walls (i.e., “wall_front” and “wall_back”) and you’re interested in knowing the forces acting on each body, you would break the two walls into independant boundaries that can be read by the postProcessing function.
- Both boundaries use the same noSlip BC in the 0/U file, thus resulting in the same boundary being written twice as shown:

<code>
boundaryField
{
    wall_front
    {
        type    noSlip;
    }
    wall_back
    {
        type    noSlip;
    }

    ...

}
</code>

In the case where there a number of other boundaries in this file, we can condense the two inputs into a single one using the ‘|’ operator, given as:

<code>
boundaryField
{
    "(wall_front|wall_back)"
    {
        type    noSlip;
    }
    ...

}
</code>

Note that when using the ‘|’ operator there can be no spaces.There is no limit to the number of BCs that can be grouped together in this way. If you use a common naming convention for your boundaries, then you can use a more generalized method known as the ‘wildcard’ character or ‘*’, shown here:

<code>
boundaryField
{
    “wall_*”
    {
        type    noSlip;
    }
    ...

}
</code>

In this case the solver will look at all boundaries that start with “wall_” and apply the noSlip BC to them.

### Tip 2: Utilizing #includeEtc

This tip can be a huge time-saver for many of the miscellaneous boundaries that are commonly defined, such as symmetry, wedge, empty, and cyclicAMI. At the top of the boundary list, in any field file, you can add “#includeEtc “casedicts/setConstraintTypes”, as shown:

<code>
boundaryField
{
   #​includeEtc “casedicts/setConstraintTypes”

    ...

}
</code>

This line instructs OpenFOAM to look through the polyMesh/boundary file and automatically apply the boundaries based on their type. This can be especially useful for creating BC templates that reduce the effort to apply case specific boundary details.
For a closer look at which boundary types can be applied with this line, you can find the file at $FOAM_ETC/caseDicts/setConstrainTypes

### Tip 3: Reference variables

Reference variables can be used more broadly than I will discuss in this post, however, their overall purpose is to allow values that you wish to use repeatedly to be referenced in from a single location. You may have seen this used in several tutorials where the internalField entry is also applied at the inlet patch as shown below:

<code>
internalField    uniform 10;

boundaryField
{
   #​includeEtc “casedicts/setConstraintTypes”

    inlet
    {
        type    fixedValue;
        value   $internalField;
    }
    ...
}
</code>

When the OpenFOAM solver reads this BC, it will know to replace the inlet value with the internalField value. The $ preceding the variable name is the flag that informs OpenFOAM that it is looking at a reference variable. Reference variables are not limited to keywords in the boundary files, you can create your own named variables at the top of the file and reference them within the boundary fields, for example if you would like to specify a different inlet value than your internalField value, you can set your new inlet value using ‘inletValue’, shown as:

<code>
internalField    uniform 10;

inletValue    uniform 15;

boundaryField
{
   #​includeEtc “casedicts/setConstraintTypes”

    inlet
    {
        type    fixedValue;
        value   $inletValue;
    }
    ...
}
</code>

This will set the inlet value to 15, and the internal field value to 10. Reference variables are not limited to boundary files and can be applied to other dictionary files, such as a dynamicMeshDict, snappyHexMeshDict and many more.

### Tip 4: Always maintain a 0.orig

OpenFOAM has a habit of overwriting the boundary field files as it reads them. The result of this is OpenFOAM undo-ing all our clever coding practices, such as taking our boundary condition lists (i.e., “(wall_front|wall_back)”) and expanding them into individual inputs. The drawback of this is that we now would have to go in and explicitly change each individual BC if we need to change the value or type. To avoid this, it’s good practise to maintain a 0.orig directory that contains the basic input file structure, and can easily be used to generate a 0/ directory. This practise is showcased in many of the tutorial cases available, and is amazingly helpful at maintaining your case organization. So keep your 0.orig/ directory safe and clean.

## Geometry BCs

In OpenFOAM, BCs on the geometry are defined within the constant/polyMesh/boundary file.

- This file holds information about the boundary type (patch, symmetry, wall, etc.), which faces in the mesh belong to the boundary, and any other special information required.

## References

[openfoam-boundary-conditions-tips-and-tricks](<https://maplekeylabs.com/openfoam-boundary-conditions-tips-and-tricks/>)
