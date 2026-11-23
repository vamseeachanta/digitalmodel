# HD T1 Suzuka Aero CFD Feasibility

## Verdict

**CFD-readiness verdict: can-do-with-additional-data.** The provided package contains useful wind-tunnel/downforce data, photos, and CAD for small mounting hardware, but it does **not** contain a watertight 3D surface or solid for either the Nissan GT-R (R35) body or the wing aerofoil. Without a watertight full-car/wing geometry package, we cannot start a credible external-aero OpenFOAM run. A scan/CAD capture effort can close the gap if it captures the full aerodynamic surfaces at suitable fidelity.

This is a geometry-readiness limitation, not a basic solver impossibility. Our current `src/digitalmodel/solvers/openfoam/` stack is oriented around marine current/wave/seakeeping case builders (`marine_solvers.py`, `wave_models.py`, ITTC-style domain sizing), so an automotive aero project would need a separate external-aero workflow: watertight STL/STEP input, `snappyHexMesh`, steady incompressible RANS (`simpleFoam`), moving-ground boundary condition, rotating-wheel/MRF treatment if wheels are included, and force/moment post-processing.

## Data Inventory

Source inspected: `.scratch/hd-t1-suzuka/extracted/` on 2026-05-28. This directory is gitignored client data and should remain uncommitted.

| Category | Count | Notes |
|---|---:|---|
| Total files | 114 | Extracted T1 Suzuka Aero package |
| Images | 95 | 85 `.jpg`/`.JPG`, 10 `.png`/`.PNG`; photo filenames indicate model/wind-tunnel tests, wing mockups, completed wing, chassis/bulkhead/load tests, splitter/strake experiments, windshield angle, and setup/test-note photos |
| CAD geometry | 11 | IGES files for brackets, pedestals, spacers, clevis, top posts, and lower mount location |
| Spreadsheets | 5 | Four `.xls`, one `.xlsx`; aero/downforce tables and simulation/comparison data |
| Documents/PDF | 3 | Two `.doc`, one `T1-R35-V2WING.pdf` |

## IGES Geometry Findings

The IGES files are component/mounting CAD, not full aero geometry. Entity parsing found NURBS/trimmed surface entities in several files, but only for small hardware. No IGES file contained type `144` trimmed surfaces or type `186` manifold solid B-rep entities, and no file name/content indicated a full car body, underbody, wheel/tire envelope, or wing aerofoil surface.

| IGES file | Entity evidence | Interpretation |
|---|---|---|
| `Mount drawings/Gas Bottom  Brkt.igs` | 579 entities: 436 type 126 curves, 46 type 128 NURBS surfaces, 53 type 141 boundaries, 42 type 143 bounded surfaces | Gas-spring bottom bracket; small mounting part |
| `Mount drawings/Gas Top Blkt .igs` | 409 entities: 304 type 126, 37 type 128, 35 type 141, 31 type 143 | Gas-spring top bracket; small mounting part |
| `Mount drawings/Pedestal closed L.igs` | 870 entities: 640 type 126, 82 type 128, 78 type 141, 68 type 143 | Left pedestal/post component |
| `Mount drawings/Pedestal closed R.igs` | 870 entities: same as left pedestal | Right pedestal/post component |
| `V2 Wing/V2 Wing Component Drawings/Angle spacer 14mm .igs` | 424 entities: 31 type 128, 35 type 141, 31 type 143 | Angle spacer |
| `V2 Wing/V2 Wing Component Drawings/Gas SP bottom brkt.igs` | 985 entities: 90 type 128, 81 type 141, 76 type 143 | Gas-spring bottom bracket |
| `V2 Wing/V2 Wing Component Drawings/Gas SP top brkt .igs` | 222 entities: 23 type 128, 20 type 141, 17 type 143 | Gas-spring top bracket |
| `V2 Wing/V2 Wing Component Drawings/Lower clevis .igs` | 759 entities: 59 type 128, 64 type 141, 54 type 143 | Clevis/mounting part |
| `V2 Wing/V2 Wing Component Drawings/Steel 12mm spacer.igs` | 20 entities: 4 type 128, 4 type 141, 2 type 143 | Spacer |
| `V2 Wing/V2 Wing Component Drawings/Top main post R&L.igs` | 1,449 entities: 166 type 128, 112 type 141, 88 type 143 | Top main posts, left/right |
| `V2 Wing/Ver2 Wing lower mount location.igs` | 2,235 entities: 1,117 type 110 lines, 70 type 100 arcs, 915 type 126 curves, text/annotation entities | 2D/annotation-heavy lower mount location drawing, not surface/solid aero geometry |

**Critical result:** no usable watertight car-body or wing-aerofoil surface was found. The existing CAD may help define mounting locations and support structure, but it is not a CFD-ready aerodynamic boundary.

## Spreadsheet Aero Data

| File | Contents | Useful values |
|---|---|---|
| `Speed vs DF.xls` | Downforce calculation table using `DF = 1/2 rho V^2 Cl S`, with front reference area `S = 2.268 m^2`; compares `CLr = 0.53` at 15 deg and `CLr = 0.274` at 0 deg | At 200 kph: 231.9 kg / 511.2 lb for `CLr=0.53`; 119.9 kg / 264.3 lb for `CLr=0.274`. At 242 kph: 339.5 kg / 748.4 lb for `CLr=0.53`; 175.5 kg / 386.9 lb for `CLr=0.274` |
| `Stock and drag wing aero details.xlsx` | Wind-tunnel/test coefficient comparison by configuration, pitch, front/rear lift coefficient and drag coefficient | Stock wing / no wing / T1 spoiler cases. `CD` ranges roughly 0.250 to 0.282; `CLR` ranges from -0.126 to 0.002 in the listed stock/T1 spoiler rows |
| `V2 Wing/V2 wing with downforce in lb added.xls` | Long-chord Siko comparison; original vs longer-chord wing, angle sweep and downforce projections | At 250 kph and alpha 16 deg: original `CLR=0.378`, rear downforce 258.4 kgf, `CD=0.364`; longer chord `CLR=0.702`, rear downforce 479.8 kgf, `CD=0.410`. Projection table gives rear downforce for `CLr=0.53` and `0.70`; at 242 kph: 1,140.7 lb and 2,509.6 lb respectively |
| `V2 Wing/new wing 説明.xls` | Similar long-chord Siko comparison without the added lb projection columns | Same angle/coefficient comparison table and kg downforce projections |
| `Testing email 3/3 codes comparison..xls` | Quarter-mile simulation comparison across Nissan Simulator, Performance Trends, and Car Test 2000 | Uses base `Cd=0.28`, base `CL=0.1`, frontal area `2.35 m^2`; compares weight sensitivity and terminal speed/ET outputs |

These data are useful for target correlation and sanity checks, but they do not replace CFD geometry.

## Geometry Needed

Minimum CFD input for a credible external-aero study:

1. **Full closed car exterior:** front bumper, grille openings as modeled/blocked for the test condition, hood, windshield, greenhouse, roof, rear glass, trunk, rear bumper, wheel arches, side skirts, splitter, diffuser, underbody treatment if it is part of the aero package, and any vents/duct exits that materially affect flow.
2. **Full wing assembly:** aerofoil main element with true cross-section, span, endplates, gurney flap if used, angle-of-attack settings, strakes, supports/posts, brackets that are in the flow, and relative position to body datum.
3. **Wheels/tires and ride height:** tire geometry, wheelbase/track, static ride height, rake, steering/yaw condition if applicable. Rotating wheels and moving ground are material for track/drag external aero.
4. **Watertight CFD surface:** STL or STEP suitable for cleanup and `snappyHexMesh`; no self-intersections, open seams, duplicate surfaces, or ambiguous thin gaps.

### Does a 3D Scanner Close the Gap?

Yes, if used deliberately. HD should scan the **full car in final aero configuration** and the **full wing assembly mounted on the car**, not just the brackets. Scanning only the wing supports will not close the CFD gap.

Recommended scan scope:

- Full exterior at final ride height/rake, with splitter/diffuser/underbody boundaries represented as far as practical.
- Full wing aerofoil, endplates, strakes, gurney features, posts, and trunk/rear-body interface.
- Wheel/tire outer surfaces and wheel openings; if rotating-wheel CFD is planned, capture tire envelope cleanly.
- Target surface fidelity: approximately 1-2 mm point spacing on wing, endplates, splitter leading edges, and sharp aero features; approximately 3-5 mm is acceptable on broad body panels. Preserve sharp edges intentionally instead of over-smoothing.
- Deliverables: registered watertight mesh plus raw scan/point cloud; include scale/datum references, ride height, wing angle, and mounting coordinates.

## Proposed CFD Approach

If the geometry is supplied or scanned:

- **Geometry prep:** clean scan/CAD, close holes, define patches (`body`, `wing`, `wheels`, `ground`, `inlet`, `outlet`, `sides`, `top`), verify watertight STL/STEP and normals.
- **Domain:** external wind-tunnel/open-road box sized around car length with adequate upstream, downstream, lateral, and top clearance; use symmetry only if yaw/geometry supports it.
- **Mesh:** `blockMesh` background domain plus `snappyHexMesh` surface refinement on car/wing, local refinement boxes for wing wake, underbody, splitter/diffuser, wheel wakes; prism/inflation layers if wall-function quality is targeted.
- **Solver:** steady incompressible RANS with `simpleFoam` for baseline; move to transient `pimpleFoam` only if separated wake/dragstrip transient behavior needs it.
- **Turbulence:** k-omega SST.
- **Boundary conditions:** inlet velocity for requested speeds, pressure outlet, slip/symmetry far-field sides/top as appropriate, moving ground at vehicle speed, no-slip vehicle surfaces, rotating wheels via MRF or rotating-wall simplification depending on fidelity requirement.
- **Runs:** speed sweep, wing angle sweep, yaw sweep if requested, and configuration deltas against available wind-tunnel/test coefficient data.
- **Outputs:** total `Cd`, drag force, total downforce, front/rear balance, wing-only and body-only force breakdown, surface `Cp`, wall shear/skin friction where meaningful, wake/Q-criterion/streamline visualizations, pressure slices through wing/endplate/underbody zones.

## Effort Estimate

| Work package | Engineer-hours | Offshore under NDA? |
|---|---:|---|
| Data intake, coordinate system, scan/CAD quality review | 4-8 | Yes |
| Geometry cleanup and watertight CFD surface preparation | 24-60 | Yes, if raw client data is covered by NDA and secure transfer is approved |
| Automotive OpenFOAM case template adaptation | 16-32 | Partly; senior review needed for BCs and force accounting |
| Mesh development and grid sensitivity | 24-60 | Yes for meshing iterations; senior engineer should set acceptance criteria |
| Baseline solve and convergence tuning | 12-32 | Yes for batch execution/monitoring |
| Speed/angle/yaw/configuration sweep | 16-48 | Yes |
| Post-processing, plots, and client memo | 12-24 | Partly; final interpretation should remain senior-reviewed |
| **Total rough order** | **108-264 hr** | **Large parts can be offshored under NDA after geometry/security setup** |

The lower end assumes clean watertight geometry and a narrow baseline/speed sweep. The upper end assumes scan cleanup, rotating wheels, yaw/angle sweeps, and presentation-grade flow visualization.

## Clarifying Questions For HD

1. What decision do you need CFD to support: absolute drag/downforce prediction, wing-angle optimization, front/rear aero balance, comparison against wind-tunnel model data, or dragstrip ET impact?
2. What speed range, yaw range, ride height/rake, and wing angles should be treated as the design envelope?
3. Is the body/wing/splitter/diffuser geometry final, or are parts still changing?
4. If scanning proceeds, can HD provide the car in final ride-height/wing configuration and approve NDA-covered transfer of raw scan/CAD data for offshore cleanup support?
