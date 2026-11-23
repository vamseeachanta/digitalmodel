# OrcaWave Input Parameter Reference

Comprehensive reference for all YAML configuration parameters found across the
seven official OrcaWave example models (L01–L06). Each parameter entry lists the
key name, the value(s) observed across examples, and a short description. Where
a parameter appears only in specific examples, it is tagged accordingly.

---

## Parameter Categories

1. [Solver and Calculation Parameters](#solver-and-calculation-parameters)
2. [Environment Parameters](#environment-parameters)
3. [Drag Linearisation Parameters](#drag-linearisation-parameters)
4. [Mesh Quality Parameters](#mesh-quality-parameters)
5. [Output Control Parameters](#output-control-parameters)
6. [QTF Parameters](#qtf-parameters)
7. [Body Parameters](#body-parameters)
8. [Morison Element Parameters](#morison-element-parameters)
9. [Field Point Parameters](#field-point-parameters)
10. [Drawing Parameters](#drawing-parameters)
11. [Cross-Example Comparison Table](#cross-example-comparison-table)

---

## Solver and Calculation Parameters

Global keys that control the diffraction solver formulation and linear algebra
backend.

| Parameter | Observed Values | Description |
|-----------|----------------|-------------|
| `UnitsSystem` | `SI` | Units for the model. All examples use SI. |
| `SolveType` | `Potential and source formulations`, `Potential formulation only`, `Full QTF calculation` | Controls the boundary-element formulation. Potential-only (L04, L05) is faster but cannot compute source-distribution pressures. Full QTF (L06) activates the second-order difference/sum frequency solver. |
| `LoadRAOCalculationMethod` | `Both`, `Haskind` | Which load RAO methods to compute. `Both` computes both Haskind and pressure integration; `Haskind` computes Haskind only. |
| `PreferredLoadRAOCalculationMethod` | `Haskind` | When `LoadRAOCalculationMethod: Both`, selects which result is written to primary output. |
| `QuadraticLoadPressureIntegration` | `Yes`, `No` | Enables the pressure-integration path for quadratic (second-order) loads. Not present when `SolveType: Potential formulation only`. |
| `QuadraticLoadControlSurface` | `Yes`, `No` | Enables the control-surface path for quadratic load calculation. Requires a control surface mesh when `Yes`. |
| `QuadraticLoadMomentumConservation` | `No` | Enables the momentum-conservation path for quadratic loads. All examples set `No`. |
| `PreferredQuadraticLoadCalculationMethod` | `Control surface`, `Pressure integration` | Selects the primary quadratic load result when multiple methods are active. L01 uses control surface; L02 uses pressure integration (no CS mesh). |
| `HasResonanceDampingLid` | `No` | When `Yes`, adds a damping lid on the interior free surface to suppress irregular frequencies. All examples set `No`. |
| `DivideNonPlanarPanels` | `Yes`, `No` | Subdivides non-planar quadrilateral panels into triangles. L01 enables this; L02 disables it. |
| `LinearSolverMethod` | `Direct LU`, `Iterative AGS` | Factorisation method for the BEM influence matrix. Direct LU is robust; Iterative AGS scales better for large panel counts (L02/L03). |
| `LinearSolverMaxIterations` | `30` | Maximum iterations for the iterative solver. Present only when `LinearSolverMethod: Iterative AGS`. |
| `LinearSolverTolerance` | `1e-6` | Convergence tolerance for the iterative solver. Present only when `LinearSolverMethod: Iterative AGS`. |
| `ValidatePanelArrangement` | `No`, `Yes` | Runs geometric panel-overlap and gap checks before solving. L02/L03/L04/L05 enable this. |
| `Comments` | (free text) | Optional global model comment string. Only present in L03. |
| `RestartingFrom` | `*.owd` file path | Restarts computation from a previously saved `.owd` result, reusing first-order results for full QTF computation. L06-only. |

---

## Environment Parameters

Keys that define the physical environment and the excitation frequency/heading
grid.

| Parameter | Observed Values | Description |
|-----------|----------------|-------------|
| `WaterDepth` | `400`, `200` | Water depth in metres. L01 uses 400 m; L02/L03 use 200 m. |
| `WaterDensity` | `1.025` | Seawater density in t/m³ (tonnes per cubic metre). All examples use 1.025. |
| `WavesReferredToBy` | `period (s)` | Controls whether the excitation grid is specified as periods (s) or frequencies (rad/s or Hz). All examples use period. |
| `PeriodOrFrequency` | list of floats | The excitation period grid in seconds (when `WavesReferredToBy: period (s)`). L01: 24 values 4–22 s. L02: 32 values 3.75–30 s. L04/L05: 31 values 4–600 s (includes very long periods for sectional-body analysis). |
| `WaveHeading` | list of floats (degrees) | Wave heading angles in degrees. L01/L02: 9 headings 0–180 every 22.5°. L03/L04/L05: 16 headings 0–337.5 every 22.5°. |
| `QTFMinCrossingAngle` | `0` | Minimum crossing angle (degrees) between wave directions for QTF computation. Present in all non-restart examples. |
| `QTFMaxCrossingAngle` | `180` | Maximum crossing angle (degrees) for QTF computation. Present in all non-restart examples. |
| `MorisonFluidVelocity` | `Undisturbed incident wave` | Fluid velocity model used for Morison drag terms. All examples use undisturbed incident wave. |
| `HasWaveSpectrumForDragLinearisation` | `No`, `Yes` | When `Yes`, activates the JONSWAP wave spectrum for linearising drag. L02/L03 set `Yes`. |

---

## Drag Linearisation Parameters

Present only when `HasWaveSpectrumForDragLinearisation: Yes` (L02, L03). These
parameters define the JONSWAP spectrum used to compute drag linearisation
coefficients and the iteration procedure.

| Parameter | Observed Values | Description |
|-----------|----------------|-------------|
| `WaveType` | `JONSWAP` | Wave spectrum type for drag linearisation. |
| `WaveJONSWAPParametersMode` | `Partially specified` | Controls which JONSWAP parameters are user-supplied versus derived. |
| `WaveHs` | `7` | Significant wave height (m) for the linearisation spectrum. |
| `WaveTz` | `8` | Zero-crossing period (s) for the linearisation spectrum. |
| `WaveGamma` | `1` | JONSWAP peak enhancement factor. `1` recovers a Bretschneider/PM spectrum. |
| `SpectrumDiscretisationMethod` | `Equal energy` | Method for discretising the continuous spectrum into components. |
| `WaveNumberOfSpectralDirections` | `1` | Number of spreading directions for the wave spectrum. `1` means long-crested. |
| `WaveNumberOfComponents` | `200` | Number of frequency components used to represent the spectrum. |
| `WaveSpectrumMinRelFrequency` | `0.5` | Minimum frequency in the spectrum as a multiple of the peak frequency. |
| `WaveSpectrumMaxRelFrequency` | `10` | Maximum frequency in the spectrum as a multiple of the peak frequency. |
| `WaveSpectrumMaxComponentFrequencyRange` | `0.05` | Maximum frequency bandwidth of any single spectral component (rad/s). |
| `DragLinearisationMaxNumberOfIterations` | `50` | Maximum iterations for the drag linearisation convergence loop. |
| `DragLinearisationTolerance` | `1e-6` | Convergence tolerance for the drag linearisation iterations. |

---

## Mesh Quality Parameters

Tolerance and warning-level keys that govern geometric checks and mesh quality
reporting.

| Parameter | Observed Values | Description |
|-----------|----------------|-------------|
| `LengthTolerance` | `100e-9` | Absolute length tolerance (m) for geometric coincidence tests. |
| `WaterlineZTolerance` | `1e-6` | Tolerance (m) for classifying a panel vertex as on the waterline. |
| `WaterlineGapTolerance` | `1e-6` | Acceptable gap (m) at the waterline between adjacent panel edges. |
| `BodyVolumeWarningLevel` | `1e-12` | Relative error threshold for the mesh-volume hydrostatic check. Volumes within this tolerance are accepted without warning. |
| `PanelAspectRatioWarningLevel` | `25` | Panel aspect ratio above which a warning is issued. |
| `PanelsPerWavelengthWarningLevel` | `5` | Minimum number of panels per wavelength below which a resolution warning fires. |
| `PanelOverlapWarningLevel` | `100e-6`, `0.001` | Panel overlap distance (m) above which an overlap warning is issued. L02/L03 use 100e-6; L04/L05 use 0.001. Only present when `ValidatePanelArrangement: Yes`. |
| `PanelGapWarningLevel` | `100e-6`, `0.001` | Panel gap distance (m) above which a gap warning is issued. Same pattern as overlap level. |
| `PanelAngleWarningLevel` | `120` | Included angle (degrees) between adjacent panels above which a dihedral warning fires. L02/L03 only. **⚠ Not supported in OrcFxAPI 11.6** — strip before loading via API (see run scripts). |

---

## Output Control Parameters

Keys that select which optional result sets are written to the `.owr` output file.

| Parameter | Observed Values | Description |
|-----------|----------------|-------------|
| `OutputPanelPressures` | `No`, `Yes` | Writes per-panel hydrodynamic pressure to output. Enabled in L05 (the panel-pressures demonstration example). |
| `OutputPanelVelocities` | `No` | Writes per-panel fluid velocity to output. All examples set `No`. |
| `OutputBodyWireFrames` | `Yes` | Writes body wire-frame geometry to output for visualisation. All examples set `Yes`. |
| `OutputIntermediateResults` | `No`, `Yes` | Writes intermediate solver results (influence matrices, etc.) to output. Enabled in L04 and L05. |

---

## QTF Parameters

Full QTF and free-surface discretisation parameters. All entries in this section
are L06-only unless noted.

### Full QTF Calculation Control

| Parameter | Observed Values | Description |
|-----------|----------------|-------------|
| `QTFMinPeriodOrFrequency` | `0`, `4` | Minimum period (s) or frequency for QTF matrix output. `0` means no lower cutoff. |
| `QTFMaxPeriodOrFrequency` | `12`, `Infinity` | Maximum period (s) for QTF output. `Infinity` includes all computed frequencies. |
| `QTFFrequencyTypes` | `Sum frequencies` | Whether to compute sum-frequency, difference-frequency, or both QTF types. Both L06 examples use sum frequencies. |
| `IncludeMeanDriftFullQTFs` | `No`, `Yes` | Includes the mean-drift (zero-frequency difference) component in the QTF matrix. L06A: `No`; L06B: `Yes`. |
| `QTFCalculationMethod` | `Both`, `Direct method` | Which QTF calculation paths to activate. `Both` computes direct method and indirect potential; `Direct method` computes only the direct path. |
| `PreferredQTFCalculationMethod` | `Direct method` | Selects the primary QTF result when `QTFCalculationMethod: Both`. L06A only. |

### Free-Surface Panelled Zone

| Parameter | Observed Values | Description |
|-----------|----------------|-------------|
| `FreeSurfacePanelledZoneType` | `Automatically generated`, `Defined by mesh file` | Controls whether the free-surface panel mesh is generated internally or loaded from a file. L06A uses auto-generation; L06B uses an external mesh file. |
| `FreeSurfacePanelledZonePanelSize` | `1.25` | Target panel size (m) for the automatically generated free-surface mesh. L06A only. |
| `FreeSurfacePanelledZoneInnerRadius` | `85` | Inner radius (m) of the annular free-surface panel zone (excludes the body vicinity). L06A only. |
| `FreeSurfacePanelledZoneMeshFileName` | (empty string) | Path to the external free-surface mesh file. L06B only (file path may be empty if mesh is embedded). |
| `FreeSurfacePanelledZoneMeshFormat` | `Wamit gdf` | Format of the external free-surface mesh file. L06B only. |
| `FreeSurfacePanelledZoneMeshLengthUnits` | `m` | Length units of the external free-surface mesh. L06B only. |

### Free-Surface Quadrature Zone

| Parameter | Observed Values | Description |
|-----------|----------------|-------------|
| `FreeSurfaceQuadratureZoneNumberOfAnnuli` | `9` | Number of annular rings in the numerical quadrature zone beyond the panelled zone. L06A only. |
| `FreeSurfaceQuadratureZoneRadiusStep` | `48.67` | Radial step size (m) of each annular ring in the quadrature zone. L06A only. |
| `FreeSurfaceQuadratureZoneNumberOfRadialNodes` | `12` | Number of Gauss points per annulus in the radial direction. L06A only. |
| `FreeSurfaceQuadratureZoneNumberOfAzimuthalNodes` | `32` | Number of Gauss points per annulus in the azimuthal direction. L06A only. |
| `FreeSurfaceOuterCircleNumberOfSegments` | `197` | Number of boundary segments on the outer circle of the free-surface domain. L06A only. |
| `FreeSurfaceAsymptoticZoneExpansionOrder` | `24` | Order of the asymptotic expansion used beyond the numerical quadrature zone. L06A only. |

---

## Body Parameters

Per-body keys, placed under each body entry in the `Bodies` list. All bodies
share the core set; some keys appear only for specific body configurations.

### Identity and Mesh

| Parameter | Observed Values | Description |
|-----------|----------------|-------------|
| `BodyName` | string | Name label for the body, referenced by `BodyConnectionParent`. |
| `BodyIncludedInAnalysis` | `Yes` | Whether this body participates in the diffraction solve. |
| `BodyMeshFileName` | string | Path to the body panel mesh file. |
| `BodyMeshFormat` | string | Format of the body mesh (e.g., `OrcaFlex obj`, `Wamit gdf`). |
| `BodyMeshLengthUnits` | `m` | Length units of the mesh file coordinates. |
| `BodyMeshPosition` | `[x, y, z]` | Global position of the mesh origin (m). |
| `BodyMeshAttitude` | `[Rx, Ry, Rz]` | Euler rotation angles (degrees) orienting the mesh in global coordinates. |
| `BodyMeshSymmetry` | `xz plane`, `None` | Symmetry plane used to expand the half-mesh. `xz plane` mirrors in the x–z plane; `None` uses the full mesh as-is. L03/L04/L05 multi-body models set `None`. |
| `BodyMeshDipolePanels` | (list, may be empty) | Identifies panels that act as dipole (thin-body) elements. |
| `BodyMeshPen` | pen spec | Display pen for the mesh wireframe. Used on non-root bodies in L04/L05. |

### Interior Surface

| Parameter | Observed Values | Description |
|-----------|----------------|-------------|
| `BodyAddInteriorSurfacePanels` | `Yes`, `No` | Adds an internal lid on open-bottomed bodies to suppress irregular frequencies. |
| `BodyInteriorSurfacePanelMethod` | `Triangulation method`, `Radial method` | Algorithm used to generate the interior lid mesh. L01/L02/L03 use triangulation; L04/L05 use radial method. |

### Control Surface

| Parameter | Observed Values | Description |
|-----------|----------------|-------------|
| `BodyControlSurfaceType` | `Defined by mesh file` | How the control surface is specified. Only present when a control surface mesh is provided (L01). |
| `BodyControlSurfaceMeshFileName` | string | Path to the control surface mesh file. L01 only. |
| `BodyControlSurfaceMeshFormat` | string | Format of the control surface mesh file. L01 only. |
| `BodyControlSurfaceMeshLengthUnits` | `m` | Length units of the control surface mesh. L01 only. |

### OrcaFlex Import

| Parameter | Observed Values | Description |
|-----------|----------------|-------------|
| `BodyOrcaFlexImportSymmetry` | `Use global mesh symmetry` | Symmetry rule applied when importing mesh from OrcaFlex. |
| `BodyOrcaFlexImportLength` | `103` | Reference length (m) used when importing mesh geometry from OrcaFlex. L01 only. |

### Hydrostatics

| Parameter | Observed Values | Description |
|-----------|----------------|-------------|
| `BodyHydrostaticIntegralMethod` | `Standard`, `Analytic` | Method for computing hydrostatic integrals. `Standard` uses numerical panel integration; `Analytic` uses closed-form sectional integration. L04/L05 use analytic. |
| `BodyHydrostaticStiffnessMethod` | `Displacement`, `Sectional` | Method for computing the hydrostatic stiffness matrix. `Sectional` is required for sectional-body models (L04/L05). |

### Inertia

| Parameter | Observed Values | Description |
|-----------|----------------|-------------|
| `BodyInertiaSpecifiedBy` | `Matrix (for a general body)` | How inertia properties are input — as a full 6×6 inertia matrix for general bodies. |
| `BodyCentreOfMass` | `[x, y, z]` | Position of the centre of mass relative to the body origin (m). |
| `BodyMass` | float | Body mass in tonnes. |
| `BodyInertiaTensorRx` | `[Ixx, Ixy, Ixz]` | Row of the 3×3 inertia tensor about the centre of mass (t·m²). |
| `BodyInertiaTensorRy` | `[Iyx, Iyy, Iyz]` | Row of the 3×3 inertia tensor (t·m²). |
| `BodyInertiaTensorRz` | `[Izx, Izy, Izz]` | Row of the 3×3 inertia tensor (t·m²). |
| `BodyInertiaTensorOriginType` | `Centre of mass` | Reference point for the inertia tensor. |

### External Stiffness and Damping

| Parameter | Observed Values | Description |
|-----------|----------------|-------------|
| `BodyExternalStiffnessMatrix...` | 6×6 matrix rows | Full 6×6 external stiffness matrix (e.g., mooring stiffness). Non-zero values on the moored Centre column in L03. Key suffix varies by row (e.g., `BodyExternalStiffnessMatrixRx`). |
| `BodyExternalStiffnessMatrixOriginType` | `Body origin` | Reference point for the external stiffness matrix. |
| `BodyExternalDampingMatrix...` | 6×6 matrix rows | Full 6×6 external linear damping matrix. Key suffix varies by row. |
| `BodyExternalDampingMatrixOriginType` | `Body origin` | Reference point for the external damping matrix. |

### Constraints and Connection

| Parameter | Observed Values | Description |
|-----------|----------------|-------------|
| `BodyConnectionParent` | `Free`, body name string | Parent body in the multi-body hierarchy. `Free` means the body has no parent (it is a root body). Named parent bodies form a rigid connection (L03: 3 columns connected to Centre column; L04: pontoons and columns connected to Keystone). |
| `BodyFixedDOFx` | `No` | Fix translational DOF in x. All examples set `No`. |
| `BodyFixedDOFy` | `No` | Fix translational DOF in y. |
| `BodyFixedDOFz` | `No` | Fix translational DOF in z. |
| `BodyFixedDOFRx` | `No` | Fix rotational DOF about x (roll). |
| `BodyFixedDOFRy` | `No` | Fix rotational DOF about y (pitch). |
| `BodyFixedDOFRz` | `No` | Fix rotational DOF about z (yaw). |
| `BodyIncreaseRollDampingToTarget` | `No` | Activates the optional roll-damping augmentation feature. All examples set `No`. |

### Morison Elements (per-body)

| Parameter | Observed Values | Description |
|-----------|----------------|-------------|
| `MorisonElements` | list | Per-body list of Morison element instances. Each entry references a `MorisonElementType` by name and specifies position, attitude, length, and segment count. L02/L03 only. |

---

## Morison Element Parameters

Morison elements are defined at model level as `MorisonElementTypes` (type
definitions) and referenced per-body in `MorisonElements`. Present only in L02
and L03.

### MorisonElementTypes (type definitions)

| Parameter | Description |
|-----------|-------------|
| `MorisonElementTypeName` | Identifier string for this type, referenced by `MorisonElementType` in per-body elements. |
| `MorisonElementTypeNormalDragDiameter` | Effective diameter (m) for normal (transverse) drag force computation. |
| `MorisonElementTypeAxialDragDiameter` | Effective diameter (m) for axial drag force computation. |
| `MorisonElementTypeDragCoefficients` | Drag coefficient(s) `[Cd_normal, Cd_axial]`. |
| `MorisonElementTypeNormalHydrodynamicDiameter` | Effective diameter (m) for the normal added-mass and inertia force. |
| `MorisonElementTypeAxialHydrodynamicDiameter` | Effective diameter (m) for the axial added-mass force. |
| `MorisonElementTypeCaCoefficients` | Added-mass coefficient(s) `[Ca_normal, Ca_axial]`. |
| `MorisonElementTypeCmCoefficients` | Inertia coefficient(s) `[Cm_normal, Cm_axial]`. |
| `MorisonElementTypePen` | Display pen for rendering this element type. |

L02 defines four types: `UC morison` (upper column), `BC morison` (base column),
`MC morison` (main column), and `Pontoon morison`.

### MorisonElements (per-body instances)

| Parameter | Description |
|-----------|-------------|
| `MorisonElementType` | Name of the `MorisonElementType` this element uses. |
| `MorisonElementPosition` | Position `[x, y, z]` of the element's start point relative to the body origin (m). |
| `MorisonElementAttitude` | Euler angles `[Rx, Ry, Rz]` orienting the element's axis (degrees). |
| `MorisonElementLength` | Length of the Morison element (m). |
| `MorisonElementNumberOfSegments` | Number of integration segments along the element length. |

---

## Field Point Parameters

| Parameter | Observed Values | Description |
|-----------|----------------|-------------|
| `DetectAndSkipFieldPointsInsideBodies` | `Yes` | When computing field-point responses, automatically skips points that lie inside a body mesh. Present in all examples. |

---

## Drawing Parameters

Keys that affect the visual rendering and default view in the OrcaWave GUI.

| Parameter | Observed Values | Description |
|-----------|----------------|-------------|
| `InteriorSurfacePanelsPen` | pen spec | Display pen for interior surface (lid) panels. Present in all examples. |
| `FreeSurfaceMeshPen` | pen spec | Display pen for the free-surface mesh elements. Present in all examples. |
| `DefaultViewParameterSpecification` | `Specified` | Activates explicit control over the default 3-D view. L04/L05 only. |
| `DefaultViewSize` | `150` | Size parameter for the default view (m, controls zoom level). L04/L05 only. |
| `DefaultViewElevation` | `30` | Elevation angle (degrees) of the default 3-D view camera. L04/L05 only. |

---

## Cross-Example Comparison Table

The table below marks which solver/configuration choices differ across examples.
An asterisk (*) indicates the non-default or noteworthy value.

| Parameter | L01 | L02 | L03 | L04 | L05 | L06A | L06B |
|-----------|-----|-----|-----|-----|-----|------|------|
| `SolveType` | Pot+Src | Pot+Src | Pot+Src | Pot only* | Pot only* | Full QTF* | Full QTF* |
| `RestartingFrom` | — | — | — | — | — | yes* | yes* |
| `LinearSolverMethod` | Direct LU | Iterative AGS* | Iterative AGS* | Direct LU | Direct LU | — | — |
| `DivideNonPlanarPanels` | Yes | No* | No* | — | — | — | — |
| `ValidatePanelArrangement` | No | Yes* | Yes* | Yes* | Yes* | — | — |
| `WaterDepth` | 400 | 200 | 200 | 200 | 200 | — | — |
| `WaveHeading count` | 9 | 9 | 16* | 16* | 16* | — | — |
| `PeriodOrFrequency count` | 24 | 32 | 32 | 31 | 31 | — | — |
| Max period (s) | 22 | 30 | 30 | 600* | 600* | — | — |
| `HasWaveSpectrumForDragLinearisation` | No | Yes* | Yes* | No | No | — | — |
| `QuadraticLoadControlSurface` | Yes | No* | No* | — | — | — | — |
| `PreferredQuadraticLoadCalculationMethod` | Ctrl surf | Press int* | Press int* | — | — | — | — |
| `BodyMeshSymmetry` | xz plane | xz plane | None* | None* | None* | — | — |
| `BodyHydrostaticIntegralMethod` | Standard | Standard | Standard | Analytic* | Analytic* | — | — |
| `BodyHydrostaticStiffnessMethod` | Displacement | Displacement | Displacement | Sectional* | Sectional* | — | — |
| `BodyInteriorSurfacePanelMethod` | Triangulation | Triangulation | Triangulation | Radial* | Radial* | — | — |
| Morison elements | — | Yes* | Yes* | — | — | — | — |
| Multi-body hierarchy | — | — | Yes* | Yes* | Yes* | — | — |
| `OutputPanelPressures` | No | No | No | No | Yes* | — | — |
| `OutputIntermediateResults` | No | No | No | Yes* | Yes* | — | — |
| Control surface mesh | Yes* | — | — | — | — | — | — |
| `QTFFrequencyTypes` | — | — | — | — | — | Sum | Sum |
| `IncludeMeanDriftFullQTFs` | — | — | — | — | — | No | Yes* |
| `FreeSurfacePanelledZoneType` | — | — | — | — | — | Auto* | Mesh file |

Legend: `—` = parameter not applicable or not present; `*` = noteworthy or
non-default value.

---

*Generated from audit of L01–L06 YAML files. See `ORCAFLEX_MODELS.md` for model
descriptions and file paths.*
