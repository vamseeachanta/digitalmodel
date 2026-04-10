# Data: Inertia

> Source: Orcina WebHelp (https://www.orcina.com/webhelp/)

$\newcommand{\Mxyz}{\mat{M}\_{\xyz}} %Mesh coordinates $

|  |  |
| --- | --- |
| Note: | If multiple bodies are defined then inertia data must be specified for each body. |

### Inertia specified by

Inertia for each body is specified using one of the following possible formulations:

* Radii of gyration (for a free-floating body)
* Matrix (for a general body)

### Inertia specified by radii of gyration (for a free-floating body)

This option can be used for a [displacement](Data,Bodies.htm#HydrostaticStiffnessMethod) body that is floating in equilibrium in the configuration defined by its [body mesh](Data,Bodies.htm#BodyMesh) and its [mesh position and orientation](Data,Bodies.htm#MeshPositionAndOrientation). The data requirements are:

* the vertical location of the centre of mass (in [global coordinates](Theory,Coordinatesystems.htm#GlobalCoordinates) $\GXYZ$)
* a matrix of radii of gyration (with respect to axes through the specified origin and parallel to the body axes $\Bxyz$)
* if the origin for the radii of gyration is **user specified**, the position of that origin (in [body coordinates](Theory,Coordinatesystems.htm#BodyCoordinates) $\Bxyz$)

Since the body is floating in equilibrium, the mass of the body must equal the mass of displaced water and the horizontal location of the centre of mass must equal that of the centre of buoyancy. These are inferred from the mesh and included in the [hydrostatic results](Results,Hydrostatics.htm).

### Inertia specified by matrix (for a general body)

This option allows you to specify the body's inertia in full generality. It can be used for a body in a floating equilibrium position, or a body whose position is not a floating equilibrium (e.g. a tension-leg platform, a structure fixed to the seabed, or a [sectional](Data,Bodies.htm#HydrostaticStiffnessMethod) body connected to another sectional body).

|  |  |
| --- | --- |
| Note: | If the body's position is not a simple hydrostatic (free-floating) equilibrium, the additional effects that contribute to the body's position must be captured somewhere in the model. Otherwise the calculation of [displacement RAOs](Results,DisplacementRAOs.htm) will not be valid. |

The data requirements are:

* the mass of the body
* the centre of mass vector (in [mesh coordinates](Theory,Coordinatesystems.htm#MeshCoordinates) $\Mxyz$)
* a moment of inertia tensor (with respect to axes through the specified origin and parallel to the body axes $\Bxyz$)
* if the origin for the inertia tensor is **user specified**, the position of that origin (in [body coordinates](Theory,Coordinatesystems.htm#BodyCoordinates) $\Bxyz$)

|  |  |
| --- | --- |
| Note: | In general, the body coordinates $\Bxyz$ and mesh coordinates $\Mxyz$ are different – they are identical only in [specific special cases](Theory,Coordinatesystems.htm#BodyCoordinatesEqualMeshCoordinates). |

|  |  |
| --- | --- |
| Warning: | When [importing](https://www.orcina.com/webhelp/OrcaFlex/Redirector.htm?Importinghydrodynamicdata,OrcaWave.htm) inertia data from OrcaWave into OrcaFlex, it is important to be aware that the inertia matrix is defined differently in the two programs: in OrcaFlex it is defined with respect to axes through the centre of mass and parallel to the mesh axes $\Mxyz$. The components of the inertia matrix can therefore differ between the two programs because the necessary reorientation and shift of origin will have been performed by the import process. |
