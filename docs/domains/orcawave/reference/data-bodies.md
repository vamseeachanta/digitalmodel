# Data: Bodies

> Source: Orcina WebHelp (https://www.orcina.com/webhelp/)

$\newcommand{\Mxyz}{\mat{M}\_{\xyz}} %Mesh coordinates $
$\newcommand{\SB}{S\_B} %body surface $
$\newcommand{\SI}{S\_I} %interior free surface $
$\newcommand{\SE}{S\_E} %dipole surface $
$\newcommand{\CWL}{C\_{WL}} %body waterline $
$\newcommand{\SF}{S\_F} %exterior free surface $
$\newcommand{\SCsub}{S\_{C,\textrm{sub}}} %submerged control surface $
$\newcommand{\SCfre}{S\_{C,\textrm{fs}}} %free surface control surface $
$\newcommand{\CCL}{C\_{CL}} %control line $

### Body name

For convenience you can enter a name for each body. These names appear in drop-down lists when you specify body data (other than position and orientation). OrcaWave also uses the body name in warning and error messages, and in many of the results.

If you have more than one body in the list, we recommend that you give them descriptive names.

### Mesh position and orientation

The position is specified in terms of the $X$, $Y$ and $Z$ coordinates of the [mesh origin](Theory,Coordinatesystems.htm).

Orientation is specified by giving heading, trim and heel angles, which are successive rotations (about rotated axes) that define the orientation of the mesh axes $\Mxyz$ relative to the global axes $\GXYZ$.

### Include

Determines whether or not the body is included in the analysis. For example, you can use this to switch quickly between different versions of the same body having different mesh files, or different position or orientation. In a multibody analysis this allows you temporarily to remove a body and simplify the analysis, which can help to solve problems if you have error messages or unexpected results.

### Global mesh symmetries

Symmetry planes present in the global mesh can significantly accelerate the diffraction calculation. OrcaWave indicates which symmetries, if any, are present in the global mesh based on the data supplied for each body. OrcaWave supports symmetry in the XZ plane, YZ plane or both XZ & YZ planes.

OrcaWave has the following requirements for a symmetry plane to be present in the global mesh:

* The symmetry plane must be present in each body with respect to its [body coordinates](Theory,Coordinatesystems.htm#BodyCoordinates) $\Bxyz$. This means that the [body mesh file](#BodyMesh) must have a corresponding symmetry plane. Further, the body's heel angle, trim angle and [body origin](#BodyOrigin) must all preserve the symmetry plane.
* The symmetry plane must be present in the overall model with respect to the [global coordinates](Theory,Coordinatesystems.htm#GlobalCoordinates) $\GXYZ$. This requires that each body's $X$ position, $Y$ position and heading angle must all preserve the symmetry plane.

In other words, a symmetry that is present in a body mesh file can be absent from the global mesh for the following reasons:

* The global mesh will have no symmetries if any included body has a non-zero heading angle.
* The global mesh will have no XZ symmetry if any included body has a non-zero heel angle, or a body origin with non-zero $Y$, or a mesh position with non-zero $Y$.
* The global mesh will have no YZ symmetry if any included body has a non-zero trim angle, or a body origin with non-zero $X$, or a mesh position with non-zero $X$.

|  |  |
| --- | --- |
| Note: | If a symmetry is present in a body mesh file but is absent from the global mesh for any of the reasons above, OrcaWave will construct the global mesh appropriately. You do **not** need to modify the mesh file you provide. |

## Body

Select a body in the drop-down list of names. All of the remaining data are specified for the body selected here.

## General properties

### Body origin

You can choose the origin of the [body coordinates](Theory,Coordinatesystems.htm#BodyCoordinates) $\Bxyz$ for the selected body. The body origin is used by OrcaWave during the calculation, and many [results](Results,Tables.htm) (e.g. added mass and damping, RAOs, QTFs) are reported in body coordinates $\Bxyz$. In theory, the body origin is completely arbitrary – a different origin gives physically equivalent results. The main difference is that, after importing diffraction results into OrcaFlex, the reference origins of the vessel-type data will be different. Nevertheless, it may be convenient to choose a specific body origin, e.g. for comparing with data from other sources.

* If you select **free surface** for the origin type, the body origin is determined by the body's [mesh position](#MeshPositionAndOrientation) data. It is defined as the point on the free surface of the water that is vertically above/below the [mesh origin](Theory,Coordinatesystems.htm).
* If you select **centre of mass** for the origin type, the body origin is determined by the body's [inertia data](Data,Inertia.htm).
* If you select **user specified** for the origin type, the position of the body origin can be specified explicitly in [mesh coordinates](Theory,Coordinatesystems.htm#MeshCoordinates) $\Mxyz$.

### Hydrostatics

Two data items pertain to OrcaWave's calculation of body [hydrostatic results](Results,Hydrostatics.htm).

The **hydrostatic integral method** is the method used to compute surface integrals for results such as [centre of buoyancy](Results,Hydrostatics.htm#CentreOfBuoyancy) and [hydrostatic stiffness matrix](Results,Hydrostatics.htm#HydrostaticStiffnessMatrix). The standard method is usually satisfactory. In more detail:

* The **standard** method evaluates the integrand at the centroid of each panel and multiplies this by panel area. This is consistent with the numerical integration elsewhere in the calculation, e.g. when computing influence matrices.
* **Analytic** integration over each panel is also possible for these simple integrands. This is more accurate, for a given mesh, but the benefit is often negligible because accuracy is still limited by mesh resolution (i.e. panel size). This method is included in the software principally to aid in-house testing, because it gives more precise self-consistency between calculations using equivalent mesh files.

The **hydrostatic stiffness method** method gives a choice between two quite different methods for body [hydrostatics](Results,Hydrostatics.htm):

* The **displacement** method is appropriate for most bodies, which we refer to as *displacement bodies*. This method uses Archimedes' principle, and requires that the wet body surface $\SB$ and interior surface $\SI$ together form a closed surface, as shown in the [geometry definitions](Theory,Governingequations.htm#GeometryNotation). For bodies such as catamarans, or platforms with distinct floating columns, each *component* of the body must be closed.
* The **sectional** method is a more general method which has no requirement for a closed body. It permits an analysis in which a single hull is divided into multiple *sectional bodies*, any of which may have an open-ended wet body surface $\SB$.

|  |  |
| --- | --- |
| Note: | Viewed in isolation, the wet body surface $\SB$ of a sectional body may be open-ended. However, the union of all sectional bodies in the model must yield one (or more) closed structure. |

### OrcaFlex import

Two data items pertain to importing OrcaWave results into OrcaFlex, a process by which OrcaWave bodies are mapped to OrcaFlex vessel types. These data items can be ignored if you are not intending to import the results of the diffraction analysis into OrcaFlex.

The **symmetry for OrcaFlex import** sets the symmetry of the imported vessel type in OrcaFlex. The default value is [global mesh symmetry](#GlobalMeshSymmetry), which takes into account the position, orientation and mesh file symmetry of each body included in the OrcaWave model. Global mesh symmetry is appropriate in most situations, however it may be useful to specify the symmetry explicitly. For example, to prescribe **circular** symmetry, which has no direct OrcaWave equivalent.

The **length for OrcaFlex import** sets the length of the imported vessel type. This is '~' by default, but should be set to a physical value: an OrcaFlex simulation will not run if any of its vessel types have length '~'.

## Body mesh

For the selected body you should specify:

* The **body mesh file** containing the panels that describe the wet body surface $\SB$. If your panels extend above the free surface ($Z=0$) then OrcaWave will automatically clip the mesh along the plane of the free surface. That is, panels above the free surface are removed and any panels which straddle the plane $Z=0$ are clipped to remove the portion above.

|  |  |
| --- | --- |
| Note: | In practice, most panels are either unmodified or removed when OrcaWave clips the mesh. Only panels having vertices both above and below the free surface by a distance greater than the [waterline Z tolerance](Data,Calculationandoutput.htm#CalculationTolerances) are clipped to remove a portion. In the case of [non-planar quadrilateral panels](Data,Calculationandoutput.htm#DivideNonPlanarPanels), OrcaWave also considers the mesh file data – if the data exactly touches the free surface (without crossing it) at a vertex or an edge, the panel will be either unmodified or removed and will not be clipped to remove a portion. |

|  |  |
| --- | --- |
| Warning: | Using OrcaWave to automatically clip the body mesh at the free surface is convenient but may lead to mesh quality issues (e.g. small or slender panels near $Z=0$). Using specialist software to produce a mesh restricted to $Z \le 0$ will give you more control over mesh quality. |

* The **format** from the list of [supported file formats](Data,Meshfileformats.htm).
* The **length units** used in the mesh file, so that the coordinates can be scaled to the [units system](Data,Model.htm#Units) used by OrcaWave.
* The **symmetry**, i.e. the symmetry planes (if any) of the panels in the body mesh file. OrcaWave supports symmetry in the xz plane, yz plane or both xz & yz planes.

|  |  |
| --- | --- |
| Notes: | If the body mesh file has a symmetry plane, the file should provide panels only on one side of that plane. |
|  | Some mesh file formats include information on symmetry planes within the file and some do not. OrcaWave requires you to specify the symmetry information in either case. |

* The **body number** to import, for mesh file formats that support definition of multiple bodies or structures.
* Whether or not to import **dry panels**, for mesh file formats that support the definition of dry panels, i.e. panels above the waterline.
* The identity of any **dipole panels**. The data is a list of [mesh file panel indices](Meshview.htm#ViewOptionsPanel), i.e. one-based indices into the panels that are imported from the body mesh file. Indices can be separated by any non-numeric characters and ranges are supported. For example, `1 2-4;8` is equivalent data to `1,2,3 4 8`

|  |  |
| --- | --- |
| Tip: | Use the [mesh view](Meshview.htm) to verify that your **dipole panels** data is identifying the correct panels. Include [mesh file panel indices](Meshview.htm#ViewOptionsPanel) in the drawing to visualise the appropriate indices. |

### Dipole panels

* Dipole panels data can only be specified if the [solve type](Data,Calculationandoutput.htm#SolveType) is *potential formulation only*. Dipole panels are [not available](Theory,Dipolepanels.htm#Restrictions) in combination with other solve types.
* [Dipole panels](Theory,Dipolepanels.htm) should describe surface(s) which are infinitely thin, wet on both sides, and rigidly connected to the rest of the body. Since they are wet on both sides, they must not lie flat on the seabed or the free surface.
* The direction of the normal vector is arbitrary on dipole panels and does not affect results.
* Dipole panels can lie on a symmetry plane, e.g. when describing a thin keel on the centreline of a boat.
* It is possible to analyse bodies which are exclusively comprised of thin surfaces $\SE$ and have no body surface $\SB$.

|  |  |
| --- | --- |
| Tip: | Where a thin surface $\SE$ meets the body surface $\SB$, it is best practice for the edges of dipole panels to meet edges of body panels, rather than grazing over the interior of body panels. This is because the pressure and velocity potential are [discontinuous](Theory,Dipolepanels.htm#IntegralEquations) across $\SE$. |

### Interior surface panels

Mesh panels covering $\SI$, the interior free surface(s) of a body, are required if you wish OrcaWave to [remove irregular frequency effects](Theory,Irregularfrequencies.htm#RemovingIrregularFrequencyEffects). These panels can be included in your body mesh file or you can ask OrcaWave to add them to your mesh. In either case we recommend you use the [mesh view](Meshview.htm) to check that $\SI$ is correctly panelled.

|  |  |
| --- | --- |
| Note: | Adding panels increases the size of the mesh and hence increases the run time of a calculation. It is not necessary to include $\SI$ panels if your wave frequencies are below the [first irregular frequency](Theory,Irregularfrequencies.htm#FirstIrregularFrequency). However, if you do not include $\SI$ panels in your mesh and a wave frequency exceeds the first irregular frequency, then you should carefully check your results for errors associated with irregular frequency effects. |

If interior surface panels are included in your body mesh file:

* The vertical component of each panel's centroid, $Z\_C$, must satisfy $-\textit{tol} \le Z\_C \le 0$ where $\textit{tol}$ is the [length tolerance.](Data,Calculationandoutput.htm#CalculationTolerances)
* They must be consistent with the heel, trim and $Z$ values of the [mesh position and orientation](#MeshPositionAndOrientation) (whose values will, in general, change the location and shape of $\SI$).
* The panels on $\SI$ should be approximately the same size as the panels on $\SB$.

|  |  |
| --- | --- |
| Note: | OrcaWave cannot analyse bodies that have part of the wet body surface $\SB$ in the plane of the free surface, i.e. a part with zero draught. All panels with zero draught are interpreted as $\SI$ panels. |

If you ask OrcaWave to add interior surface panels:

* The body mesh file must not contain any interior surface panels.
* OrcaWave offers you a choice of methods for generating the panels: [radial method](#RadialMethod) or [triangulation method](#TriangulationMethod).
* Body waterline(s) must be [closed](Data,Meshdetails.htm#Waterlines). The quality of the mesh generated by OrcaWave on $\SI$ is better if the waterline segments all have similar length.

|  |  |
| --- | --- |
| Warning: | Using OrcaWave to automatically add panels covering $\SI$ is convenient but may lead to mesh quality issues (e.g. if the waterline has some very short segments). Using specialist software to produce a mesh of $\SI$ will give you more control over mesh quality. |

### Interior surface panels for sectional bodies

In models containing [sectional](#HydrostaticStiffnessMethod) bodies, the waterline bounding a component of the interior surface may be shared between multiple bodies. Therefore there are some additional considerations for interior surface panels:

* If you want to provide the interior surface panels, they can be included in any of the body mesh files.
* If you want OrcaWave to add interior surface panels, you should specify to *add interior surface panels* for at least one of the bodies that has panels on the bounding waterline.
* OrcaWave will determine the method ([radial](#RadialMethod) or [triangulation](#TriangulationMethod)) by looping over the [segments](Data,Meshdetails.htm#Waterlines) of the bounding waterline to find the first segment belonging to a body with *add interior surface panels* specified. The method specified for this body is used for the relevant component of the interior surface.

### The radial method for generating interior surface panels

Each segment of the waterline around $\SI$ defines a radial sector, by connecting the two ends of the segment to the centroid of the waterline. Each radial sector begins as a large triangle and is then subdivided into quadrilateral panels.

An advantage of this method is that all the $\SI$ panels are coupled in a consistent way to the waterline panels of the body mesh – which is beneficial because the latter provide the [boundary condition](Theory,Irregularfrequencies.htm#RemovingIrregularFrequencyEffectsSourceFormulation) for $V(\vec X)$ in the source formulation. A disadvantage of this method is that it may create a large number of panels and/or slender panels with large [aspect ratio](Data,Meshdetails.htm#AspectRatio), particularly if the waterline has a large aspect ratio (e.g. a typical ship).

|  |  |
| --- | --- |
| Note: | If the interior free surface contains a moonpool, or its shape is highly concave (specifically, if the entire perimeter is not visible from the centroid), then the radial method breaks down. In these cases OrcaWave automatically switches to the triangulation method as a fallback. |

### The triangulation method for generating interior surface panels

A general-purpose triangulation algorithm is applied using (i) the end points of the waterline segments around the perimeter, and (ii) a regular lattice of points in the interior. In the interior of $\SI$ the lattice points predominate and, to minimise the number of panels added to the mesh, OrcaWave combines pairs of neighbouring right-angled triangles into rectangular panels.

An advantage of this method is that it can handle both moonpools and highly concave waterlines, whereas the radial method breaks down for those cases. In addition, it often performs better than the radial method for waterlines with a large aspect ratio (e.g. a typical ship).

## Control surface mesh

Mesh panels describing a **control surface** are required if your [quadratic load calculation method](Data,Calculationandoutput.htm#LoadCalculationMethods) includes control surface integration. These panels can be provided in a mesh file or you can ask OrcaWave to automatically generate them.

### Control surface geometry

A control surface surrounds the body but is separated from it. The control surface and body surface together enclose a finite volume of water. In the typical case of a floating body, the control surface includes a portion of the free surface exterior to the body waterlines(s) as shown in the figure.

|  |  |
| --- | --- |
| Figure: | Control surface mesh |

We define the following terminology:

$\SCsub$ is the submerged portion of the control surface. On $\SCsub$ the normal $\vec n$ points into the control volume.

$\SCfre$ is the free surface portion of the control surface. On $\SCfre$ the normal $\vec n$ points vertically down, $\vec{n}=-\vec{e}\_z$.

$\CCL$ is the **control line**, the intersection of $\SCsub$ with $\SCfre$.

It is important that the panels on $\SCsub$ and $\SCfre$ are correctly orientated. Use the [mesh view](Meshview.htm#ViewOptionsPanel) to check the direction of the normal vectors.

|  |  |
| --- | --- |
| Notes: | In a multibody analysis, the control surface must not intersect the body surface of another body. |
|  | If a body is fully submerged, the control surface can be either fully submerged or it can extend up to the free surface. In either case it should completely surround the body. |
|  | If a body is mounted on the seabed, $\SCsub$ should include panels on the seabed exterior to the body. |

### Omitting part of the control surface

A complete control surface, as defined above, is not always necessary. If you only require horizontal (i.e. surge, sway and yaw) components of [mean drift loads](Results,Quadraticloads.htm#MeanDriftLoads), and if your model does not include a [damping lid](Data,Calculationandoutput.htm#ResonanceDampingLid), the control surface need not include panels on the free surface or seabed. Omitting part(s) of the control surface will make the calculation of mean drift loads faster, but vertical components (i.e. heave, roll and pitch) will not be correctly computed.

A complete control surface is always necessary for [time-varying quadratic loads](Results,Quadraticloads.htm#Timevaryingquadraticloads).

### Control surface data

If your control surface is **defined by a mesh file**, you should provide a mesh file with the symmetry type indicated by the program. The control surface mesh file should specify panels in [body coordinates](Theory,Coordinatesystems.htm#BodyCoordinates) $\Bxyz$.

|  |  |
| --- | --- |
| Notes: | Care is required to prepare the mesh file in body coordinates. In general, the body coordinates $\Bxyz$ and mesh coordinates $\Mxyz$ are different – they are identical only in [specific special cases](Theory,Coordinatesystems.htm#BodyCoordinatesEqualMeshCoordinates). |
|  | In general, the shape of a body's waterlines is dependent on its $Z$ position, heel angle and trim angle. |
|  | The symmetry required in the control surface mesh file (indicated by the program) may differ from the symmetry of the body mesh file. E.g. if the latter has xz symmetry and the body's heel angle is non-zero. |

|  |  |
| --- | --- |
| Tip: | Use the [mesh view](Meshview.htm) to verify that your control surface mesh file has been imported and positioned as expected. |

If your control surface is **automatically generated**, you must specify a target panel size and the separation from the body. You can also choose whether or not to include free surface panels.

|  |  |
| --- | --- |
| Notes: | Panel size in the control surface mesh should be similar to the body mesh. Acceptable results can sometimes be obtained with slightly larger panels. |
|  | The separation between the control surface and body is arbitrary, in theory. In practice, a distance equivalent to 5-10 panels is often used. |
|  | OrcaWave cannot automatically generate a control surface for bottom-mounted bodies. |
|  | If you want automatically generated panels including the free surface, then all body waterlines must be [closed](Data,Meshdetails.htm#Waterlines). The quality of the mesh is better if the waterline segments all have similar length, comparable to the specified panel size. |

|  |  |
| --- | --- |
| Warning: | Using OrcaWave to automatically generate panels including $\SCfre$ is convenient but may lead to mesh quality issues (e.g. if the waterline has some very short segments). Using specialist software to produce a mesh of $\SCfre$ will give you more control over mesh quality. |

The automatically generated mesh can be saved as a WAMIT .gdf file by using the popup menu on the [mesh view](Meshview.htm) or [mesh details](Data,Meshdetails.htm) pages.
