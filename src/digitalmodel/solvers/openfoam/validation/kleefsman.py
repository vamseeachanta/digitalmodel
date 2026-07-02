#!/usr/bin/env python3
"""
ABOUTME: Marine CFD validation case #1172 — dam break against an obstacle
(green-water / wave-impact), validated against the Kleefsman et al. (2005)
MARIN experiment via the official SPHERIC Test 2 data. A 0.55 m water column
collapses down a 3.22 m tank onto a container-scale box carrying pressure
sensors; impact pressure and water heights are gated against the measured
traces. The canonical violent-free-surface-impact benchmark.

Source / citation
-----------------
- Kleefsman, K.M.T., Fekken, G., Veldman, A.E.P., Iwanowski, B. & Buchner, B.
  (2005). "A Volume-of-Fluid based simulation method for wave impact
  problems." J. Comput. Phys. 206(1), 363-393. doi:10.1016/j.jcp.2004.12.007.
- Official experimental data + geometry: SPHERIC/ERCOFTAC Test-case 2
  (Issa & Violeau, EDF-LNHE, 2006), https://www.spheric-sph.org/tests/test-02
  — tank 3.22x1x1 m, water column x in [1.992, 3.22] x 0.55 m deep, box
  0.161x0.403x0.161 m centred at x = 0.744 (front face x = 0.8245 carrying
  P1-P4 at y = 0.471; P5-P8 on the top), height gauges H1-H4 at
  x = 0.496/0.992/1.488/2.638. Data archived at
  docs/api/cfd/cases/kleefsman_impact/kleefsman_exp_data.csv.

Validation gates (#1172): P2 peak impact pressure and arrival time within
20% of the measured trace; H2/H4 water-height normalized MAE <= 10%; mass
conserved (< 1%) through the impact.
"""

from __future__ import annotations

import json
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Dict, Tuple

# Issue #1172 gates (impact problems are noisy; loose-but-real).
PRESSURE_PEAK_TOLERANCE = 0.20
PRESSURE_ARRIVAL_TOLERANCE = 0.20
HEIGHT_MAE_TOLERANCE = 0.10
IMPACT_MASS_TOLERANCE = 0.01

# Sensor geometry (m) from the SPHERIC Test 2 description.
BOX = ((0.6635, 0.2955, 0.0), (0.8245, 0.6985, 0.161))
PRESSURE_SENSORS = {
    "P1": (0.8245, 0.471, 0.021), "P2": (0.8245, 0.471, 0.061),
    "P3": (0.8245, 0.471, 0.101), "P4": (0.8245, 0.471, 0.141),
    "P5": (0.8035, 0.471, 0.161), "P6": (0.7635, 0.471, 0.161),
    "P7": (0.7235, 0.471, 0.161), "P8": (0.6835, 0.471, 0.161),
}
HEIGHT_GAUGES = {"H1": 0.496, "H2": 0.992, "H3": 1.488, "H4": 2.638}


def experiment_data_path() -> Path:
    """Path of the archived official experimental data CSV (repo checkout)."""
    here = Path(__file__).resolve()
    for parent in here.parents:
        cand = (parent / "docs" / "api" / "cfd" / "cases" /
                "kleefsman_impact" / "kleefsman_exp_data.csv")
        if cand.is_file():
            return cand
    raise FileNotFoundError(
        "kleefsman_exp_data.csv not found — requires a repo checkout with "
        "docs/api/cfd/cases/kleefsman_impact/"
    )


def load_experiment() -> Dict[str, Any]:
    """Official MARIN traces as a dict of numpy arrays (Time, P1..P8, H1..H4)."""
    import numpy as np

    raw = np.genfromtxt(experiment_data_path(), delimiter=",", names=True)
    return {name: np.asarray(raw[name]) for name in raw.dtype.names}


@dataclass
class KleefsmanConfig:
    """Config for the Kleefsman dam-break-with-obstacle case.

    Geometry is fixed by the experiment; the mesh density and duration are
    configurable. The default (81x25x25, 1.5 s) is the *fast* regression
    variant covering the primary impact; the reference report uses
    ``nx, ny, nz = 161, 50, 50`` and 6 s (half the Kleefsman grid).

    Attributes:
        nx, ny, nz: Mesh cells (tank is 3.22 x 1 x 1 m).
        end_time: Physical end time (s).
        write_interval: Field output interval (s).
        name: Case directory name.
    """

    nx: int = 81
    ny: int = 25
    nz: int = 25
    end_time: float = 1.5
    write_interval: float = 0.1
    name: str = "validation_kleefsman"

    @property
    def cell_size(self) -> Tuple[float, float, float]:
        """(dx, dy, dz) in metres."""
        return (3.22 / self.nx, 1.0 / self.ny, 1.0 / self.nz)


_HDR = "FoamFile {{ version 2.0; format ascii; class {c}; object {o}; }}\n"

_BLOCKMESHDICT = """
// SPHERIC Test 2 / Kleefsman et al. (2005) MARIN dam-break-with-obstacle.
scale 1;
vertices
(
    (0 0 0) (3.22 0 0) (3.22 1 0) (0 1 0)
    (0 0 1) (3.22 0 1) (3.22 1 1) (0 1 1)
);
blocks ( hex (0 1 2 3 4 5 6 7) (@NX@ @NY@ @NZ@) simpleGrading (1 1 1) );
edges ();
boundary
(
    stationaryWalls
    {
        type wall;
        faces ( (0 3 2 1) (2 6 5 1) (1 5 4 0) (3 7 6 2) (0 4 7 3) );
    }
    atmosphere { type patch; faces ( (4 5 6 7) ); }
    obstacle { type wall; faces (); }
);
mergePatchPairs ();
"""

_TOPOSETDICT = """
actions
(
    {
        name    c0;
        type    cellSet;
        action  new;
        source  boxToCell;
        // Kleefsman box: front face x = 0.8245 carries P1-P4 at y = 0.471
        box     (0.6635 0.2955 0) (0.8245 0.6985 0.161);
    }
    { name c0; type cellSet; action invert; }
);
"""

_SETFIELDSDICT = """
defaultFieldValues ( volScalarFieldValue alpha.water 0 );
regions
(
    boxToCell
    {
        // reservoir: right 1.228 m of the tank, 0.55 m deep
        box (1.992 -1 -1) (4 2 0.55);
        fieldValues ( volScalarFieldValue alpha.water 1 );
    }
);
"""

_CONTROLDICT_HEAD = """
application     interFoam;
startFrom       startTime;
startTime       0;
stopAt          endTime;
endTime         @ENDTIME@;
deltaT          0.001;
writeControl    adjustable;
writeInterval   @WRITEINTERVAL@;
purgeWrite      0;
writeFormat     ascii;
writePrecision  6;
writeCompression off;
timeFormat      general;
timePrecision   6;
runTimeModifiable yes;
adjustTimeStep  yes;
maxCo           0.75;
maxAlphaCo      0.75;
maxDeltaT       0.01;
functions
{
    // Kleefsman sensors; pressure probes sit half a cell off the faces so
    // they sample fluid cells at any mesh density.
    pressureProbes
    {
        type            probes;
        libs            (sampling);
        writeControl    timeStep;
        writeInterval   5;
        fields          (p p_rgh);
        probeLocations
        (
@PROBES@
        );
    }
    heightGauges
    {
        type            interfaceHeight;
        libs            (fieldFunctionObjects);
        alpha           alpha.water;
        writeControl    timeStep;
        writeInterval   5;
        locations
        (
            (0.496 0.5 0)
            (0.992 0.5 0)
            (1.488 0.5 0)
            (2.638 0.5 0)
        );
    }
    waterVolume
    {
        type            volFieldValue;
        libs            (fieldFunctionObjects);
        fields          (alpha.water);
        operation       volIntegrate;
        regionType      all;
        writeControl    timeStep;
        writeInterval   20;
        writeFields     false;
        log             false;
    }
}
"""

_FVSCHEMES = """
ddtSchemes      { default Euler; }
gradSchemes     { default Gauss linear; }
divSchemes
{
    div(rhoPhi,U)   Gauss linearUpwind grad(U);
    div(phi,alpha)  Gauss vanLeer;
    div(phirb,alpha) Gauss linear;
    div(((rho*nuEff)*dev2(T(grad(U))))) Gauss linear;
}
laplacianSchemes { default Gauss linear corrected; }
interpolationSchemes { default linear; }
snGradSchemes   { default corrected; }
"""

_FVSOLUTION = """
solvers
{
    "alpha.water.*"
    {
        nAlphaCorr      2;
        nAlphaSubCycles 1;
        cAlpha          1;
        MULESCorr       yes;
        nLimiterIter    5;
        solver          smoothSolver;
        smoother        symGaussSeidel;
        tolerance       1e-8;
        relTol          0;
    }
    "pcorr.*" { solver PCG; preconditioner DIC; tolerance 1e-5; relTol 0; }
    p_rgh     { solver PCG; preconditioner DIC; tolerance 1e-07; relTol 0.05; }
    p_rghFinal { $p_rgh; relTol 0; }
    U { solver smoothSolver; smoother symGaussSeidel; tolerance 1e-06; relTol 0; }
}
PIMPLE
{
    momentumPredictor   no;
    nOuterCorrectors    1;
    nCorrectors         3;
    nNonOrthogonalCorrectors 0;
}
relaxationFactors { equations { ".*" 1; } }
"""

_TRANSPORT = """
phases          (water air);
water { transportModel Newtonian; nu 1e-06; rho 1000; }
air   { transportModel Newtonian; nu 1.48e-05; rho 1; }
sigma           0.07;
"""

_GRAVITY_DICT = """
dimensions      [0 1 -2 0 0 0 0];
value           (0 0 -9.81);
"""

_TURBULENCE = """
simulationType  laminar;
"""

_FIELDS = {
    ("volScalarField", "alpha.water"): """
dimensions      [0 0 0 0 0 0 0];
internalField   uniform 0;
boundaryField
{
    stationaryWalls { type zeroGradient; }
    obstacle { type zeroGradient; }
    atmosphere { type inletOutlet; inletValue uniform 0; value uniform 0; }
}
""",
    ("volScalarField", "p_rgh"): """
dimensions      [1 -1 -2 0 0 0 0];
internalField   uniform 0;
boundaryField
{
    stationaryWalls { type fixedFluxPressure; value uniform 0; }
    obstacle { type fixedFluxPressure; value uniform 0; }
    atmosphere { type totalPressure; p0 uniform 0; }
}
""",
    ("volVectorField", "U"): """
dimensions      [0 1 -1 0 0 0 0];
internalField   uniform (0 0 0);
boundaryField
{
    stationaryWalls { type noSlip; }
    obstacle { type noSlip; }
    atmosphere { type pressureInletOutletVelocity; value uniform (0 0 0); }
}
""",
}


def build_kleefsman_case(
    config: KleefsmanConfig | None = None,
    parent_dir: Path | str = ".",
) -> Path:
    """Generate the Kleefsman dam-break-with-obstacle case.

    Run sequence: ``blockMesh -> topoSet -> subsetMesh -overwrite c0 -patch
    obstacle -> setFields -> interFoam``, i.e.
    ``OpenFOAMRunConfig(run_topo_set=True, subset_mesh_set="c0",
    subset_mesh_patch="obstacle", run_set_fields=True)``.
    """
    config = config or KleefsmanConfig()
    case_dir = Path(parent_dir) / config.name
    for sub in ("system", "constant", "0"):
        (case_dir / sub).mkdir(parents=True, exist_ok=True)

    fmt = "{:.6g}".format
    dx = config.cell_size[0]
    probes = []
    for name, (x, y, z) in PRESSURE_SENSORS.items():
        if name in ("P1", "P2", "P3", "P4"):     # front face: offset +x
            probes.append(f"            ({fmt(x + dx / 2)} {fmt(y)} {fmt(z)})")
        else:                                     # top face: offset +z
            probes.append(f"            ({fmt(x)} {fmt(y)} {fmt(z + config.cell_size[2] / 2)})")
    control = (
        _CONTROLDICT_HEAD
        .replace("@ENDTIME@", fmt(config.end_time))
        .replace("@WRITEINTERVAL@", fmt(config.write_interval))
        .replace("@PROBES@", "\n".join(probes))
    )
    blockmesh = (
        _BLOCKMESHDICT
        .replace("@NX@", str(config.nx))
        .replace("@NY@", str(config.ny))
        .replace("@NZ@", str(config.nz))
    )

    sysd, constd, zerod = case_dir / "system", case_dir / "constant", case_dir / "0"
    (sysd / "blockMeshDict").write_text(_HDR.format(c="dictionary", o="blockMeshDict") + blockmesh)
    (sysd / "topoSetDict").write_text(_HDR.format(c="dictionary", o="topoSetDict") + _TOPOSETDICT)
    (sysd / "setFieldsDict").write_text(_HDR.format(c="dictionary", o="setFieldsDict") + _SETFIELDSDICT)
    (sysd / "controlDict").write_text(_HDR.format(c="dictionary", o="controlDict") + control)
    (sysd / "fvSchemes").write_text(_HDR.format(c="dictionary", o="fvSchemes") + _FVSCHEMES)
    (sysd / "fvSolution").write_text(_HDR.format(c="dictionary", o="fvSolution") + _FVSOLUTION)
    (constd / "transportProperties").write_text(_HDR.format(c="dictionary", o="transportProperties") + _TRANSPORT)
    (constd / "g").write_text(_HDR.format(c="uniformDimensionedVectorField", o="g") + _GRAVITY_DICT)
    (constd / "turbulenceProperties").write_text(_HDR.format(c="dictionary", o="turbulenceProperties") + _TURBULENCE)
    for (c, o), body in _FIELDS.items():
        (zerod / o).write_text(_HDR.format(c=c, o=o) + body)

    (case_dir / "provenance.json").write_text(json.dumps({
        "validation_case": "kleefsman_impact",
        "issue": "#1172",
        "reference": "Kleefsman et al. (2005) MARIN experiment, "
                     "doi:10.1016/j.jcp.2004.12.007",
        "citations": [
            "Kleefsman et al. (2005), J. Comput. Phys. 206(1) 363-393",
            "SPHERIC/ERCOFTAC Test-case 2 (official geometry + data), "
            "https://www.spheric-sph.org/tests/test-02",
        ],
        "geometry": {"tank_m": [3.22, 1.0, 1.0], "box": BOX,
                     "water_column_x_m": [1.992, 3.22], "water_depth_m": 0.55},
        "sensors": {"pressure": PRESSURE_SENSORS, "height_x": HEIGHT_GAUGES},
        "mesh": [config.nx, config.ny, config.nz],
        "end_time_s": config.end_time,
        "tolerances": {
            "p2_peak": PRESSURE_PEAK_TOLERANCE,
            "p2_arrival": PRESSURE_ARRIVAL_TOLERANCE,
            "height_mae": HEIGHT_MAE_TOLERANCE,
            "mass": IMPACT_MASS_TOLERANCE,
        },
    }, indent=2) + "\n")
    return case_dir


# ---------------------------------------------------------------------------
# Solved-case extraction
# ---------------------------------------------------------------------------


def extract_impact_metrics(
    case_dir: Path | str,
    window: Tuple[float, float] = (0.3, 1.2),
    height_window: Tuple[float, float] | None = None,
) -> Dict[str, Any]:
    """Impact metrics vs the official experiment.

    Returns P1/P2 peak + arrival comparisons over ``window``, H2/H4
    normalized MAE over ``height_window`` (default: up to the solved end
    time), and the water-volume drift.
    """
    import numpy as np

    case_dir = Path(case_dir)
    exp = load_experiment()

    files = sorted(case_dir.glob("postProcessing/pressureProbes/*/p"),
                   key=lambda q: float(q.parent.name))
    rows = []
    for f in files:
        for line in f.read_text().splitlines():
            if line.startswith("#") or not line.strip():
                continue
            rows.append([float(v) for v in line.split()])
    pr = np.array(rows)
    t_p = pr[:, 0]

    def baseline(v):
        return np.median(v[t_p < 0.2])

    def peak_metrics(v, v_exp):
        m = (t_p >= window[0]) & (t_p <= window[1])
        me = (exp["Time"] >= window[0]) & (exp["Time"] <= window[1])
        pk, pke = float(v[m].max()), float(v_exp[me].max())
        thr = 0.25 * pke
        ta = float(t_p[m][np.argmax(v[m] > thr)])
        tae = float(exp["Time"][me][np.argmax(v_exp[me] > thr)])
        return {"peak": pk, "peak_exp": pke,
                "peak_err": (pk - pke) / pke,
                "t_arrival": ta, "t_arrival_exp": tae,
                "arrival_err": (ta - tae) / tae}

    res: Dict[str, Any] = {
        "P1": peak_metrics(pr[:, 1] - baseline(pr[:, 1]), exp["P1"]),
        "P2": peak_metrics(pr[:, 2] - baseline(pr[:, 2]), exp["P2"]),
    }

    hfiles = sorted(case_dir.glob("postProcessing/heightGauges/*/height.dat"),
                    key=lambda q: float(q.parent.name))
    hg = np.vstack([np.loadtxt(f, comments="#") for f in hfiles])
    t_h = hg[:, 0]
    cols = hg[:, 1::2] if hg.shape[1] - 1 == 8 else hg[:, 1:]
    hw = height_window or (0.0, float(t_h[-1]))

    def mae(col, name):
        m = (exp["Time"] >= hw[0]) & (exp["Time"] <= hw[1])
        vi = np.interp(exp["Time"][m], t_h, cols[:, col])
        return float(np.mean(np.abs(vi - exp[name][m])) / np.max(exp[name][m]))

    res["H2_mae_norm"] = mae(1, "H2")
    res["H4_mae_norm"] = mae(3, "H4")

    mass = np.loadtxt(case_dir / "postProcessing/waterVolume/0/volFieldValue.dat",
                      comments="#")
    res["mass_drift"] = float(np.max(np.abs(mass[:, 1] - mass[0, 1])) / mass[0, 1])
    return res
