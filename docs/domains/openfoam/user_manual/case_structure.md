## OpenFoam Case Structure

An introductory summary and description of the OpenFOAM case structure is given in this document

## Summary

The case description is summarized into the following. For further understanding read the rest of this document.

### Directory Structure

- A sample directory layout for a solver that requires only pressure and velocity (e.g. an incompressible solver for laminar flow).
- Note the entries with a trailing / are directories, while those without are plain text files.
-

<code>

0/
...U
...p
constant/
...polyMesh/
...transportProperties
...turbulenceProperties
system/
...controlDict
...fvSchemes
...fvSolution

</code>

### Typical File

- A header is included in each OpenFOAM file

### 0 Directory

-The 0 directory is a special time directory that contains the following for the simulation:
    -  the initial condition(s) .
    - boundary conditions ?.

- Inside of this directory there will be one text file for each field that is required for the particular solver executable that is being run (e.g. U for velocity, p for pressure, etc.)
  - Each file contains four main sections, including the header section.
  - A header is included in each OpenFOAM file to give the parameters of the file, which can be checked by the solver prior to reading.
  - An example of the header section for the p file located in the 0 directory is below:
    - The first 7 lines are comments, which are included by convention, but are not strictly necessary. All OpenFOAM files can be commented using C++ syntax.
    - FoamFile{...} section contains several properties of the file that may be checked by the solver prior to reading the field.
    - The FoamFile section is followed by the field data, which is a list of values for each cell in the mesh.

    <code>
    /*--------------------------------*- C++ -*----------------------------------*\
  =========                 |
| \\      /  F ield         | OpenFOAM: The Open Source CFD Toolbox           |
|  \\    /   O peration     | Version:  6.x                                   |
|   \\  /    A nd           | Website:  <https://openfoam.org>                  |
|    \\/     M anipulation  |                                                 |
\*---------------------------------------------------------------------------*/
FoamFile
{
    version     2.0;
    format      ascii;
    class       volScalarField;
    object      p_rgh;
}
//* ************************************ //

    </code>

#### Dimensions and Units

- Correct specification of dimensions is important in OpenFOAM, since the code will check that dimensions are consistent when performing arithmetic operations, and will return an error if dimensions are incompatible.
- dimensions in OpenFOAM are specified using an array of 7 integers, where each integer represents the exponent on one of the fundamental base units.
- The 7 base units are given below.
      Position  Dimension  SI Unit
      1  Mass  Kilogram (kg)
      2  Length  Metre (m)
      3  Time  Second (s)
      4  Temperature  Kelvin (K)
      5  Quantity  Mole (mol)
      6  Current  Ampere (A)
      7  Luminous intensity  Candela (cd)

- An example of the dimension specification section for the p field is as follows. According to this table, the dimensions of pressure are m^2/s^2.
<code>
dimensions      [0 2 -2 0 0 0 0];
</code>

#### Initial Value Specification

- Following the specification of the dimensions is the specification of the initial value for the internal field, which is normally a statement of the form:
- This example would set the value of the field to a uniform value of 0 at every cell within the domain (taking the units that are specified by the dimensions keyword).
- Pressure being vector is specified this way.
  <code>
  internalField   uniform 0;
  </code>
- This type of statement would apply to any scalar field. For a vector field (e.g. velocity), the statement would look something like this:
  <code>
  internalField   uniform (0 0 0);
  </code>
- It is also possible to specify n-dimensional tensors in a similar manner, where each component must be specified individually.
- Normally, the keyword uniform is used when setting the initial values
- However, it is also possible to specify a separate value for each cell using the keyword nonuniform. Normally this is not done manually, but will come up when using data from a previous simulation as the initial condition for a subsequent one.

#### Boundary Condition Specification

- Uses the keyword boundaryField.
- This keyword is followed by a set of braces ({...}) that contain an entry for every "boundary patch" that is contained in the mesh
- A boundary patch is a named collection of faces that lie on the exterior boundary of the domain.
- An example and expanation is given elow
  - The name "outlet" in this case would correspond with the exact name of a boundary patch that is specified in the mesh.
  - Every boundary condition must specify a type and most require a value.
  - Different boundary conditions may require additional keywords to be specified.
  - This example is one of the simplest boundary conditions, "fixedValue", which will set the value on all faces of the patch outlet to a uniform value i.e. 0.

<code>
boundaryField
{
    outlet
    {
        type            fixedValue;
        value           uniform 0;
    }
    ...
}
</code>
- How to get the boundary conditions right? Banana test
  - The set of valid boundary conditions is dependent on the particular solver being used and the type of field being set (i.e. not all boundary conditions are applicable to both velocity and pressure fields).
  - One easy way to find out the set of valid boundary conditions is to attempt to run the case with an invalid type (the "banana test" as it has become known in the OpenFOAM community).
  - If the boundary condition type is invalid, the OpenFOAM solver will stop running the case and provide a list of all the valid boundary conditions.
  - For example, if the type is specified as banana for the velocity field in the pimpleFoam solver, the following will be output:
  <code>
  --> FOAM FATAL IO ERROR:
Unknown patchField type banana for patch type patch

Valid patchField types are :

85
(
SRFFreestreamVelocity
SRFVelocity
SRFWallVelocity
...
wedge
zeroGradient
)
</code>

- Note that there is nothing special about the word banana; any invalid word would produce the same result.

### Remaining Time Directories

- In addition to the 0 directory, the solver will output additional time directories that correspond to the solution fields at those particular times.
- The name of these directories will be the numerical value of the simulation time for which the solution is output, for example 0.001 or 10.
- The frequency of output is controlled by the controlDict file, which will be discussed later.
- In general, you will want to avoid writing these time directories too frequently to avoid spending too much time writing to disk and taking up a lot of hard drive space.

### Constant Directory

- The constant directory contains files that are related to the physics of the problem, including the mesh and any physical properties that are required for the solver.
- Always contains a subdirectory called "polyMesh":
  - which contains a number of files related to the mesh for the case.
  - The mesh files are not written by the user but created by a meshing utility.
- The constant directory normally contains a number of other files that are solver-specific. Some example files are "transportProperties" and "turbulenceProperties" for the pimpleFoam solver.
  - transportProperties file to specify the necessary physical properties.
  - As an example, the following would be contained in the transportProperties file for the pimpleFoam solver
    - i.e. fluid is to be treated as Newtonians, with a kinematic viscosity set to 1.0e-06 m^2/s.
    - The integers in square brackets define quantity units.
    - The name nu is specified by the solver; if a value with this name is not found, then the solver will fail.
  <code>
  transportModel  Newtonian;
  nu              [0 2 -1 0 0 0 0] 1.0e-06;
  </code>
  - The turbulenceProperties file specify turbulence properties and the associated parameters.
    - For laminar flow, this file is required and the keyword simulationType would be set to laminar

- To get more help while running, inputting an invalid option for transportModel will get the solver to print the list of available options, for example, see below:
- Each of these models can then be looked up in the OpenFOAM documentation to determine the required parameters.
<code>
7
(
BirdCarreau
Casson
CrossPowerLaw
HerschelBulkley
Newtonian
powerLaw
strainRateFunction
)
</code>

### System Directory

- Parameters related to running the simulation (e.g. timestep, linear solvers, etc.).
- Three required files: fvSchemes, fvSolution, and controlDict.
  - fvSchemes file: specifies discretization schemes information for the finite volume method. The schemes are below. Detailed explanation can be found in OpenFOAM tutorials.
    - ddtSchemes: specifies how each equation is integrated with respect to time
    - gradSchemes: specifies how the gradient of each field is calculated
    - divSchemes: specifies how divergence terms (e.g. convection term) are discretized
    - laplacianSchemes: specifies how Laplacian terms (e.g. diffusion term) are discretized
    - interpolationSchemes: specifies how interpolation from cell-centred values to face-centred values are computed
    - snGradSchemes: specifies how surface-normal gradient terms (e.g. heat flux terms) are discretized
  - fvSolution file: specifies how the discretized equations are to be solved.
    - list of linear solvers to solve each equation.
    - optional entry called relaxationFactors allows the user to set under-relaxation on a per-equation basis and to relax field variables between update steps.
    - Most solvers also require a dictionary to specify solver-specific parameters, e.g. for PIMPLE-based solvers:
      - This specifies a number of solver-specific correction parameters (i.e. nNonOrthogonalCorrectors, nCorrectors, and nOuterCorrectors) as well as parameters that allow the solver to fix the pressure at a specific point in the domain when none of the boundary conditions fix the pressure level (i.e. pRefCell and pRefValue).
      - This is the most basic PIMPLE sub-dictionary that can be specified.
      - There are a number of additional parameters that can be specified to further control the behaviour of the solver.
<code>
PIMPLE
{
    nNonOrthogonalCorrectors 1;
    nCorrectors              1;
    nOuterCorrectors         10;
    pRefCell                 0;
    pRefValue                0;
}
</code>
  - controlDict specifies:
    - how the solution should progress by specifying things like the timestep,
    - how often to write solution data, as well as more advanced items such as function objects that should be executed at runtime.
    - A basic controlDict file, neglecting the header, looks something like this:
    - The most common parameters to adjust in the controlDict:
      - deltaT: specifies the timestep of the simulation
      - endTime: specifies at what time to stop the simulation (assuming stopAt is set to endTime)
      - writeInterval: specifies how often (in simulation time) to write output files (assuming writeControl is set to runTime)
      - purgeWrite: species how many timestep directories to keep; when running a simulation to steady-state, this allows intermediate timestep files to be deleted if not needed, while being able to access them while the simulation runs
    - There are many other parameters that can be set in the controlDict, but these basic parameters are normally enough to get a simulation up and running.

<code>
  application      pimpleFoam;
  startFrom        latestTime;
  startTime        0;
  stopAt           endTime;
  endTime          36000;
  deltaT           300;
  writeControl     runTime;
  writeInterval    600;
  purgeWrite       2;
  writeFormat      ascii;
  writePrecision   6;
  writeCompression off;
  timeFormat       general;
</code>

- Additional files may be included such as
  - "blockMeshDict" if blockMesh used.
  - additional file(s) to control runtime post-processing activities

#### Meshing (move this?)

- The most basic meshing tool that comes with OpenFOAM is blockMesh, which is suitable for simple cases, but becomes difficult to use for more complicated geometries.
- OpenFOAM also includes a tool called snappyHexMesh, which can create meshes for objects with triangulated surfaces. Further, external meshing software can be used. Many meshing software packages can directly export the necessary files for the polyMesh directory. For those that cannot, OpenFOAM includes several utilities to convert mesh formats (e.g. the ANSYS .msh format).

### References

[understanding-the-openfoam-case-structure](<https://maplekeylabs.com/understanding-the-openfoam-case-structure/>)
