# Parametric flanged spool pilot

Synthetic public geometry for [#2804](https://github.com/vamseeachanta/digitalmodel/issues/2804),
under [#2272](https://github.com/vamseeachanta/digitalmodel/issues/2272).
Comparator class: **closed-form analytical geometry**. This is a generic steel
spool with two weld-neck flanges, a straight pipe and a continuous cylindrical
bore. Dimensions and density are chosen inputs, not published benchmark values
or a flange-standard designation. No external data was downloaded.

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
| Straight pipe | 151200 pi | 475008.809223 |
| Both necks | 75240 pi | 236373.431256 |
| Both flange disks | 175644 pi | 551801.900047 |
| Bolt holes (subtract) | 5184 pi | 16286.016316 |
| Steel total | **396900 pi** | **1246898.124210** |

Nominal mass is **9.788150275 kg** and overall length is **396 mm**.
Changing only tL to 19.8 mm adds `8523 pi mm³`, giving
**1273673.918396 mm³**, **9.998340259 kg**, and **397.8 mm** overall length.
That is a **2.147392290%** volume error and **1.8 mm** thickness error.
The defective solid is always judged against the original nominal parameters.
Mass is derived from CAD volume and the specified density; it is not an
independent material-density measurement.

## Automatic judgment and execution

1. CAD volume and volume-derived mass must each agree within **0.5%**.
2. All geometric inputs are measured from B-rep cylinder/cone surfaces and
   their axial extents, without passing parameters into the measurement code.
   Both flange diameters/thicknesses, both neck lengths/base diameters, bore,
   pipe diameter/length, overall length, and every hole diameter/bolt circle
   must agree within **0.01 mm**. Hole counts must match exactly. The solid
   must be valid, closed and singular; unexpected topology raises an error.
3. The actual solid is exported to STEP and passed to the existing
   `GMSHMeshGenerator.generate_mesh_from_step`, with **5 mm** element size.
   After standard gmsh optimization, only linear 3D tetrahedra are accepted:
   **1000–80000** inclusive, each signed `minDetJac > 0`, and **minSJ >= 0.2**.
   Surface/line elements are excluded from the count. Nonfinite or missing
   observations fail. These fixed limits are not adjusted to fit a result.

With the repository dependencies installed, use a Python interpreter able to
import FreeCAD/Part and gmsh (including their native libraries):

```bash
PYTHONPATH=src python -m pytest tests/solvers/gmsh_meshing/test_spool_pilot.py -q -rs -s
```

For a dependency-light check of the hand calculation and judge on this host:

```bash
uv run --no-project --with 'pytest>=7.4.3,<9' --with 'pytest-asyncio==0.21.1' \
  --with pyyaml --with numpy --with scipy --with loguru \
  python -m pytest tests/solvers/gmsh_meshing/test_spool_pilot.py -q -rs
```

For an interactive Python invocation in the native environment:

```python
from pathlib import Path
from digitalmodel.solvers.gmsh_meshing.spool_pilot import SpoolParameters, run_pilot
solid, mesh = run_pilot(SpoolParameters(), Path("/tmp/flanged-spool"))
solid.assert_passed()
mesh.assert_passed()
```

The nominal integration test exercises all three checks. A separate test
builds a valid second solid with a 10% left flange thickness defect and asserts
that volume and dimension checks reject it. Another integration test changes
all geometric inputs to exercise regeneration. Only dependency import errors
skip native tests; CAD construction, metrology and quality failures stay red.
On the implementation host FreeCAD/Part and gmsh are unavailable, so native
acceptance is **unverified**, not a recorded CAD/mesh pass. Synthetic judge
observations are explicitly distinguished from CAD evidence in the tests.

## References and later work

The annulus and frustum equations above are elementary geometry; no paper
benchmark is claimed for this synthetic item. For the mesher: Christophe
Geuzaine and Jean-François Remacle (2009), “Gmsh: A 3-D finite element mesh
generator with built-in pre- and post-processing facilities,” *International
Journal for Numerical Methods in Engineering*, 79(11), 1309–1331,
[doi:10.1002/nme.2579](https://doi.org/10.1002/nme.2579).
The [gmsh API manual](https://gmsh.info/doc/texinfo/#gmsh_002fmodel_002fmesh_002fgetElementQualities)
defines `minSJ` and `minDetJac`. This pilot uses those measures directly:
the existing general quality analyzer's mean-ratio metric is a different
quantity and uses absolute volume, so it cannot establish this criterion.

Later work under #2272 includes drawing ingestion and scale recovery,
assembled structures, a shared twin record, downstream engineering workflows
and a browser viewer. The next pilot step is to run these native tests in a
FreeCAD/gmsh environment and retain the measured quality evidence. Any failing
check must remain failing until its cause is fixed; tolerances stay unchanged.
