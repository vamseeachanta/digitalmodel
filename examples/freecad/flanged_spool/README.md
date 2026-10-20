# Parametric flanged spool pilot

Synthetic public geometry for [#2804](https://github.com/vamseeachanta/digitalmodel/issues/2804),
under [#2272](https://github.com/vamseeachanta/digitalmodel/issues/2272).
Comparator class: **closed-form analytical geometry**. This is a generic steel
spool with two weld-neck flanges, a straight pipe and a continuous cylindrical
bore. Dimensions and density are chosen inputs, not published benchmark values
or a flange-standard designation. No external data was downloaded.

## Revision history

| Revision | Date | Change and reason |
|---|---|---|
| 1 | 2026-10-18 | Replaced the ineffective linear-tetrahedron quality metric, separated native dependencies and added hole-spacing checks to detect angular displacement. |

The factory is [flanged_spool.py](../../../src/digitalmodel/visualization/design_tools/flanged_spool.py)
in the existing FreeCAD design-tools package. Parameters, hand calculation,
judge and STEP meshing adapter are in
[spool_pilot.py](../../../src/digitalmodel/solvers/gmsh_meshing/spool_pilot.py).
Changing `SpoolParameters` and rebuilding regenerates the solid. The simplified
geometry has conical necks and through bolt holes; weld beads, bevels, raised
faces, gaskets and fillets are outside this slice.

## Inputs and hand calculation

All lengths are mm. Both flanges have the same nominal dimensions, but their
thickness parameters are independent so one flange can be deliberately wrong.

| Input | Value |
|---|---:|
| Straight pipe length L | 300 |
| Pipe outside diameter 2r | 90 |
| Continuous bore diameter 2a | 78 |
| Flange outside diameter 2F | 160 |
| Left and right flange thickness tL, tR | 18 each |
| Each conical neck length h | 30 |
| Neck base diameter 2b (tip diameter = 2r) | 120 |
| Bolt circle diameter | 140 |
| Bolt hole diameter 2q | 12 |
| Equally spaced bolt holes N per flange | 4 |
| Steel density rho, kg/m³ | 7850 |

Regions meet at faces and have no overlapping volume. A flange is an annular
disk; a neck is a conical frustum minus a cylindrical bore. With T = tL + tR:

```text
Vpipe    = pi (r² - a²) L
Vnecks   = 2 pi h [(b² + br + r²)/3 - a²]
Vflanges = pi (F² - a²) T
Vholes   = N pi q² T
Vsteel   = Vpipe + Vnecks + Vflanges - Vholes
mass_kg  = Vsteel_mm³ × 10⁻⁹ × rho_kg/m³
length   = L + 2h + T
```

| Component | Exact volume, mm³ | Decimal volume, mm³ |
|---|---:|---:|
| Straight pipe | 151200 pi | 475008.8 |
| Both necks | 75240 pi | 236373.4 |
| Both flange disks | 175644 pi | 551801.9 |
| Bolt holes (subtract) | 5184 pi | 16286.0 |
| Steel total | **396900 pi** | **1246898.1** |

Nominal mass is **9.8 kg** and overall length is **396 mm**.
Changing only tL to 19.8 mm adds `8523 pi mm³`, giving
**1273673.9 mm³**, **10.0 kg**, and **397.8 mm** overall length.
That is a **2.1%** volume error and **1.8 mm** thickness error.
The defective solid is always judged against the original nominal parameters.
Mass is derived from CAD volume and the specified density; it is not an
independent material-density measurement.

## Automatic judgment and execution

1. CAD volume and volume-derived mass must each agree within **0.5%**.
2. All geometric inputs are measured from B-rep cylinder/cone surfaces and
   their axial extents, without passing parameters into the measurement code.
   Both flange diameters/thicknesses, both neck lengths/base diameters, bore,
   pipe diameter/length, overall length, and every hole diameter/bolt circle
   and adjacent hole-centre chord spacing (`2R sin(pi/N)`)
   must agree within **0.01 mm**. Hole counts must match exactly. The solid
   must be valid, closed and singular; unexpected topology raises an error.
3. The actual solid is exported to STEP and passed to the existing
   `GMSHMeshGenerator.generate_mesh_from_step`, with **5 mm** element size.
   After standard gmsh optimization and an explicit `optimize("Netgen")` pass,
   only linear 3D tetrahedra are accepted: **1000–80000** inclusive, each signed
   `minDetJac > 0`, and **minSICN >= 0.2**. The signed inverse condition number
   measures shape; `minSJ` measures curvature distortion and is one for any
   straight-sided tetrahedron, including a sliver. Surface/line elements are
   excluded from the count. Nonfinite or missing observations fail.

The **0.2** threshold is retained with the shape-sensitive metric. The added
Netgen pass attempts to improve shape rather than relaxing that threshold.
Native acceptance after this change remains unverified: the required gmsh
version has no compatible Arm Linux wheel and FreeCAD/Part are unavailable.
The maintainers must confirm the optimizer and threshold against the native
nominal and changed-parameter tests before accepting the mesh result. A
remaining element below the threshold is a failure, including on the nominal
geometry. Mass verdicts explicitly label the mass as derived from volume and
the specified density; mass and volume errors are not independent.

Bolt-hole cutting tools extend **1 mm** beyond each flange face to avoid
coplanar boolean faces. B-rep hole measurements still use the actual flange
extents.

With the repository dependencies installed, use a Python interpreter able to
import FreeCAD/Part and gmsh (including their native libraries):

```bash
PYTHONPATH=src python -m pytest tests/solvers/gmsh_meshing/test_spool_pilot.py -q -rs -s
```

For a dependency-light check of the hand calculation and judge on this host:

```bash
uv run --no-project --with 'pytest>=7.4.3,<9' --with 'pytest-asyncio==0.21.1' \
  --with pyyaml --with 'numpy<2' --with scipy --with loguru \
  --with 'pandas<3' --with 'plotly==5.17.0' --with 'pydantic>=2.7,<3' \
  --with 'shapely>=2' \
  python -m pytest tests/solvers/gmsh_meshing/test_spool_pilot.py -q -rs
```

For an interactive Python invocation in the native environment:

```python
from pathlib import Path
from tempfile import TemporaryDirectory
from digitalmodel.solvers.gmsh_meshing.spool_pilot import SpoolParameters, run_pilot
with TemporaryDirectory() as folder:
    solid, mesh = run_pilot(SpoolParameters(), Path(folder))
    solid.assert_passed()
    mesh.assert_passed()
```

The nominal integration test exercises all three checks. A separate test
builds a valid second solid with a 10% left flange thickness defect and asserts
that volume and dimension checks reject it. Another integration test changes
all geometric inputs to exercise regeneration. A displaced-hole test holds
volume and bolt-circle diameter constant while checking chord-spacing failure.
These B-rep tests require FreeCAD/Part; the combined pipeline also needs gmsh.

Mesh tests require only gmsh: an independent OCC primitive construction exports
nominal and changed-parameter STEP solids through the production adapter, while
a discrete-mesh test plants one sliver among 1000 positively oriented linear
tetrahedra. The sliver must fail on shape alone, despite `minSJ` remaining one.
A regular-tetrahedron control must pass. Dependency fixtures keep gmsh tests
available when FreeCAD is absent and B-rep metrology available without gmsh.
Only dependency import errors skip native tests; CAD construction, metrology,
optimization and quality failures stay red. Synthetic judge and mocked surface
observations are explicitly distinguished from native CAD evidence.

Local focused results are **85 passed, 8 skipped**, compared with **72 passed,
3 skipped** before this change. All eight skips require unavailable native
libraries: four mesh checks and four CAD or combined checks. None constitutes
native acceptance evidence.

## References and later work

The annulus and frustum equations above are elementary geometry; no paper
benchmark is claimed for this synthetic item. For the mesher: Christophe
Geuzaine and Jean-François Remacle (2009), “Gmsh: A 3-D finite element mesh
generator with built-in pre- and post-processing facilities,” *International
Journal for Numerical Methods in Engineering*, 79(11), 1309–1331,
[doi:10.1002/nme.2579](https://doi.org/10.1002/nme.2579).
The [gmsh API manual](https://gmsh.info/doc/texinfo/#gmsh_002fmodel_002fmesh_002fgetElementQualities)
documents the element-quality measures. This pilot reads `minSICN` and
`minDetJac` directly:
the existing general quality analyzer's mean-ratio metric is a different
quantity and uses absolute volume, so it cannot establish this criterion.

Later work under #2272 includes drawing ingestion and scale recovery,
assembled structures, a shared twin record, downstream engineering workflows
and a browser viewer. The next pilot step is to run these native tests in a
FreeCAD/gmsh environment and retain the measured quality evidence. Any failing
check must remain failing until its cause is fixed; tolerances stay unchanged.
