# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Build and smoke a public KCS body-force case using only the standard library.

The constant-KT propeller is a controller demonstrator, not an independently
validated KP505 performance model. Run this file directly to avoid optional
package dependencies. OpenFOAM v2312 must be sourced before the smoke command.
"""

from __future__ import annotations

import argparse
import csv
import hashlib
import importlib.util
import json
import math
import re
import subprocess
import sys
import time
from dataclasses import dataclass
from pathlib import Path

REFERENCE = Path(__file__).with_name("data") / "kcs_tokyo2015_self_propulsion.json"
NUMBER = r"[-+]?(?:\d+\.?\d*|\.\d+)(?:[eE][-+]?\d+)?"


def finite(value: float, name: str, *, minimum: float = 0) -> float:
    """Require finite values at or above a declared lower bound."""
    if isinstance(value, bool) or not math.isfinite(value) or value < minimum:
        raise ValueError(f"invalid {name}")
    return value


def thrust(rpm: float, *, density: float = 999.1, diameter: float = 0.25,
           kt: float = 0.17) -> float:
    """Return total disk thrust in newtons; the input speed is RPM, not rps."""
    finite(rpm, "rpm")
    for name, value in (("density", density), ("diameter", diameter), ("kt", kt)):
        finite(value, name, minimum=1e-15)
    return density * (rpm / 60) ** 2 * diameter ** 4 * kt


@dataclass
class RpmController:
    """PI speed control with conditional integration at the RPM bounds.

    Gains have units RPM/N and RPM/(N s). The towing correction is a positive
    forward assistance, so error = resistance - towing - applied thrust.
    An update is made once per completed solver segment, not per corrector.
    Gains and the 900 RPM cap are demonstration assumptions, based on previous
    experience. The cap allows feedback above the 570 RPM reference; it is not
    a mechanical limit or a validated operating bound.
    """

    kp: float = 1.0
    ki: float = 0.1
    rpm_max: float = 900.0
    integral: float = 0.0

    def update(self, rpm: float, *, resistance: float, applied_thrust: float,
               dt: float, towing: float = 0) -> float:
        """Advance the controller and return the next segment's RPM."""
        for name, value in (("rpm", rpm), ("applied thrust", applied_thrust),
                            ("towing", towing), ("kp", self.kp), ("ki", self.ki)):
            finite(value, name)
        finite(self.rpm_max, "rpm maximum", minimum=1e-15)
        finite(dt, "dt", minimum=1e-15)
        if not math.isfinite(resistance) or not math.isfinite(self.integral):
            raise ValueError("nonfinite controller state or resistance")
        if rpm > self.rpm_max:
            raise ValueError("rpm exceeds controller bound")
        error = resistance - towing - applied_thrust
        candidate = self.integral + error * dt
        raw = rpm + self.kp * error + self.ki * candidate
        if 0 <= raw <= self.rpm_max or (raw > self.rpm_max and error < 0) or (raw < 0 and error > 0):
            self.integral = candidate
        return max(0.0, min(self.rpm_max, raw))


def load_reference() -> dict:
    """Read the small, cited workshop transcription."""
    return json.loads(REFERENCE.read_text(encoding="utf-8"))


def header(name: str, kind: str = "dictionary") -> str:
    """Render a minimal OpenFOAM ASCII header without runtime identifiers."""
    return ("FoamFile\n{\n    version 2.0;\n    format ascii;\n"
            f"    class {kind};\n    object {name};\n}}\n\n")


def source_dictionary(force_n: float) -> str:
    """Apply total force to water through interFoam's density-weighted U equation.

    The absolute volume mode divides by the selected mesh volume internally.
    The source is in N, not acceleration: no division by water density occurs.
    Fluid momentum is downstream (+x); the propeller reaction is forward (-x).
    """
    finite(force_n, "thrust")
    return header("fvOptions") + f"""propeller
{{
    type vectorSemiImplicitSource;
    active yes;
    selectionMode cellZone;
    cellZone propeller;
    volumeMode absolute;
    injectionRateSuSp {{ U (({force_n:.12g} 0 0) 0); }}
}}
"""


def control_dictionary(end: float, dt: float) -> str:
    """Write transient controls and physical full-hull forces for each segment."""
    return header("controlDict") + f"""application interFoam;
startFrom latestTime;
startTime 0;
stopAt endTime;
endTime {end:.12g};
deltaT {dt:.12g};
adjustTimeStep no;
maxCo 0.5;
maxAlphaCo 0.5;
maxDeltaT {dt:.12g};
writeControl timeStep;
writeInterval 1;
purgeWrite 0;
writeFormat ascii;
writePrecision 12;
writeCompression off;
timeFormat general;
timePrecision 12;
runTimeModifiable false;
functions
{{
    hullForces
    {{
        type forces;
        libs ("libforces.so");
        patches (hull);
        p p;
        U U;
        rho rho;
        CofR (0 0 0);
        writeControl timeStep;
        writeInterval 1;
        log yes;
    }}
}}
"""


def _geometry_module():
    path = Path(__file__).resolve().parents[2] / "naval_architecture/kcs_geometry.py"
    spec = importlib.util.spec_from_file_location("_public_kcs_geometry", path)
    if spec is None or spec.loader is None:
        raise ValueError("public geometry module could not be loaded")
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


def _write(case: Path, name: str, text: str) -> None:
    path = case / name
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(text, encoding="utf-8", newline="\n")


def build_case(destination: Path) -> Path:
    """Build a new coarse fixed-hull VOF case; refuse to overwrite any case."""
    case = Path(destination)
    case.mkdir(parents=True, exist_ok=False)
    ref = load_reference()
    geometry = _geometry_module()
    surface = geometry.build_kcs_model_surface()
    (case / "constant/triSurface").mkdir(parents=True)
    geometry.write_stl(surface.triangles, case / "constant/triSurface/hull.stl")
    _write(case, "system/blockMeshDict", header("blockMeshDict") + """scale 1;
vertices
(
    (-7.5 -2 -1) (12 -2 -1) (12 2 -1) (-7.5 2 -1)
    (-7.5 -2 0.5) (12 -2 0.5) (12 2 0.5) (-7.5 2 0.5)
);
blocks (hex (0 1 2 3 4 5 6 7) (78 24 18) simpleGrading (1 1 1));
edges ();
boundary
(
    inlet { type patch; faces ((0 4 7 3)); }
    outlet { type patch; faces ((1 2 6 5)); }
    sides { type patch; faces ((0 1 5 4) (3 7 6 2)); }
    bottom { type patch; faces ((0 3 2 1)); }
    atmosphere { type patch; faces ((4 5 6 7)); }
);
mergePatchPairs ();
""")
    x = (ref["propeller"]["x_from_fp_over_lpp"] - 0.5) * 7.2786
    z = -ref["propeller"]["depth_over_lpp"] * 7.2786
    _write(case, "system/snappyHexMeshDict", header("snappyHexMeshDict") + f"""castellatedMesh true;
snap true;
addLayers false;
geometry
{{
    hull.stl {{ type triSurfaceMesh; name hull; }}
    diskRegion
    {{
        type searchableCylinder;
        point1 ({x-0.3:.12g} 0 {z:.12g});
        point2 ({x+0.3:.12g} 0 {z:.12g});
        radius 0.25;
    }}
}}
castellatedMeshControls
{{
    maxLocalCells 150000;
    maxGlobalCells 150000;
    minRefinementCells 0;
    nCellsBetweenLevels 2;
    features ();
    refinementSurfaces {{ hull {{ level (1 1); patchInfo {{ type wall; }} }} }}
    resolveFeatureAngle 30;
    refinementRegions {{ diskRegion {{ mode inside; levels ((1e15 2)); }} }}
    locationInMesh (-7 -1 -0.8);
    allowFreeStandingZoneFaces true;
}}
snapControls
{{
    nSmoothPatch 3;
    tolerance 1;
    nSolveIter 30;
    nRelaxIter 5;
    nFeatureSnapIter 0;
}}
addLayersControls {{}}
meshQualityControls
{{
    #includeEtc "caseDicts/mesh/generation/meshQualityDict.cfg"
}}
mergeTolerance 1e-6;
""")
    _write(case, "system/topoSetDict", header("topoSetDict") + f"""actions
(
    {{ name diskCells; type cellSet; action new; source cylinderToCell;
       point1 ({x-0.05:.12g} 0 {z:.12g});
       point2 ({x+0.05:.12g} 0 {z:.12g}); radius 0.125; }}
    {{ name propeller; type cellZoneSet; action new;
       source setToCellZone; set diskCells; }}
);
""")
    _write(case, "system/setFieldsDict", header("setFieldsDict") + """defaultFieldValues (volScalarFieldValue alpha.water 0);
regions
(
    boxToCell
    {
        box (-100 -100 -100) (100 100 0);
        fieldValues (volScalarFieldValue alpha.water 1);
    }
    // v2312 cell assignment does not modify fixedValue patches.
    // Select submerged inlet face centres explicitly; dry faces retain zero.
    boxToFace
    {
        box (-7.500001 -2 -1) (-7.499999 2 0);
        fieldValues (volScalarFieldValue alpha.water 1);
    }
);
""")
    _write(case, "system/fvSchemes", header("fvSchemes") + """ddtSchemes { default Euler; }
gradSchemes { default Gauss linear; }
divSchemes
{
    default none;
    div(rhoPhi,U) Gauss upwind;
    div(phi,alpha) Gauss vanLeer;
    div(phirb,alpha) Gauss linear;
    div(((rho*nuEff)*dev2(T(grad(U))))) Gauss linear;
}
laplacianSchemes { default Gauss linear corrected; }
interpolationSchemes { default linear; }
snGradSchemes { default corrected; }
fluxRequired { default no; p_rgh; alpha.water; }
""")
    _write(case, "system/fvSolution", header("fvSolution") + """solvers
{
    "alpha.water.*"
    {
        nAlphaCorr 1; nAlphaSubCycles 1; cAlpha 1;
        MULESCorr yes; nLimiterIter 3;
        solver smoothSolver; smoother symGaussSeidel;
        tolerance 1e-8; relTol 0;
    }
    "pcorr.*" { solver PCG; preconditioner DIC; tolerance 1e-8; relTol 0; }
    p_rgh { solver GAMG; smoother DIC; tolerance 1e-7; relTol 0.01; }
    p_rghFinal { $p_rgh; relTol 0; }
    "U.*" { solver smoothSolver; smoother symGaussSeidel; tolerance 1e-8; relTol 0; }
}
PIMPLE
{
    momentumPredictor yes;
    nOuterCorrectors 1;
    nCorrectors 2;
    nNonOrthogonalCorrectors 1;
    pRefCell 0;
    pRefValue 0;
}
""")
    _write(case, "constant/transportProperties", header("transportProperties") + """phases (water air);
water { transportModel Newtonian; nu 1.139e-6; rho 999.1; }
// Air rho [kg/m3], nu [m2/s] and sigma [N/m] are demonstration assumptions.
// Basis: https://doc.cfd.direct/openfoam/user-guide-v11/dambreak
// Tutorial temperature and pressure are unspecified, not matched test conditions.
air { transportModel Newtonian; nu 1.48e-5; rho 1; }
sigma 0.07;
""")
    _write(case, "constant/turbulenceProperties", header("turbulenceProperties") + "simulationType laminar;\n")
    _write(case, "constant/g", header("g", "uniformDimensionedVectorField") + "dimensions [0 1 -2 0 0 0 0];\nvalue (0 0 -9.81);\n")
    boundaries = """inlet { type fixedValue; value uniform (2.196 0 0); }
    outlet { type zeroGradient; }
    sides { type slip; }
    bottom { type slip; }
    atmosphere { type pressureInletOutletVelocity; value uniform (0 0 0); }
    hull { type fixedValue; value uniform (0 0 0); }"""
    _write(case, "0/U", header("U", "volVectorField") + "dimensions [0 1 -1 0 0 0 0];\ninternalField uniform (2.196 0 0);\nboundaryField\n{\n" + boundaries + "\n}\n")
    _write(case, "0/alpha.water", header("alpha.water", "volScalarField") + """dimensions [0 0 0 0 0 0 0];
internalField uniform 0;
boundaryField
{
    inlet { type fixedValue; value uniform 0; }
    outlet { type zeroGradient; }
    "(sides|bottom|hull)" { type zeroGradient; }
    atmosphere { type inletOutlet; inletValue uniform 0; value uniform 0; }
}
""")
    _write(case, "0/p_rgh", header("p_rgh", "volScalarField") + """dimensions [1 -1 -2 0 0 0 0];
internalField uniform 0;
boundaryField
{
    "(inlet|sides|bottom|hull)" { type fixedFluxPressure; value uniform 0; }
    outlet { type fixedValue; value uniform 0; }
    atmosphere { type totalPressure; p0 uniform 0; value uniform 0; }
}
""")
    _write(case, "system/controlDict", control_dictionary(0.003, 0.0005))
    _write(case, "constant/fvOptions", source_dictionary(thrust(570)))
    manifest = {
        "schema_version": 1,
        "geometry": {"kind": "public_workshop_surface_grid", "stand_in": False,
                     "vintage": "Tokyo 2005, not the later Tokyo 2015 re-fairing",
                     "source": "docs/api/cfd/cases/kcs_resistance/geometry",
                     "coordinates": "midship origin, x downstream, z up",
                     "source_sha256": {p.name: hashlib.sha256(p.read_bytes()).hexdigest()
                                       for p in sorted(geometry.workshop_grid_dir().glob("*.dat"))}},
        "reference_sha256": hashlib.sha256(REFERENCE.read_bytes()).hexdigest(),
        "solver": "interFoam", "body": "fixed even keel", "full_hull": True,
        "model": "uniform axial body force, constant KT=0.17 from EFD",
        "turbulence": "laminar startup demonstration only",
        "controller": {"kp_rpm_per_n": 1, "ki_rpm_per_n_s": 0.1, "rpm_max": 900},
        "validation_status": "not_established",
        "files": {str(p.relative_to(case)): hashlib.sha256(p.read_bytes()).hexdigest()
                  for p in sorted(case.rglob("*")) if p.is_file()},
    }
    _write(case, "manifest.json", json.dumps(manifest, indent=2) + "\n")
    return case


def read_resistance(path: Path) -> list[tuple[float, float]]:
    """Read v2312 force.dat: time, total vector, pressure vector, viscous vector.

    Full-domain hull force is downstream-positive resistance in N. No factor
    of two is applied. Unsupported legacy forces.dat layouts are rejected.
    """
    rows = []
    for line in path.read_text(encoding="utf-8").splitlines():
        if not line.strip() or line.lstrip().startswith("#"):
            continue
        numbers = [float(v) for v in re.findall(NUMBER, line)]
        if len(numbers) != 10 or not all(math.isfinite(v) for v in numbers):
            raise ValueError("invalid v2312 force.dat row")
        rows.append((numbers[0], numbers[1]))
    if not rows or any(b[0] <= a[0] for a, b in zip(rows, rows[1:])):
        raise ValueError("force history is empty or unordered")
    return rows


def compare_point(rows: list[dict]) -> dict:
    """Compare a supplied window diagnostically, without inventing scatter."""
    if not rows:
        raise ValueError("no completed controller samples")
    keys = ("time_s", "rpm", "thrust_n", "resistance_n", "towing_n")
    if any(not math.isfinite(r[k]) for r in rows for k in keys):
        raise ValueError("nonfinite controller history")
    if any(b["time_s"] <= a["time_s"] for a, b in zip(rows, rows[1:])):
        raise ValueError("controller history must increase in time")
    if any(r[k] < 0 for r in rows for k in ("rpm", "thrust_n", "towing_n")):
        raise ValueError("invalid controller history")
    ref = load_reference()
    c, efd = ref["conditions"], ref["efd"]
    reference = {
        "rpm": 60 * efd["n_rps"],
        "thrust_n": thrust(60 * efd["n_rps"], density=c["density_kg_m3"],
                           diameter=ref["propeller"]["diameter_m"], kt=efd["kt"]),
        "resistance_n": 0.5 * c["density_kg_m3"] * c["speed_m_s"]**2 * c["wetted_area_m2"] * efd["ct"],
    }
    quantities = {}
    for key, expected in reference.items():
        value = sum(r[key] for r in rows) / len(rows)
        quantities[key] = {"computed": round(value, 1), "reference": round(expected, 1),
                           "difference_percent": round(100 * (value - expected) / expected, 1)}
    return {"quantities": quantities, "samples": len(rows),
            "force_imbalance_n": round(sum(r["resistance_n"] - r["towing_n"] - r["thrust_n"] for r in rows) / len(rows), 1),
            "validation_status": "not_established",
            "reason": "Published scatter is unavailable; constant KT is not an independent propeller prediction. Geometry vintage, fixed attitude and laminar startup also differ.",
            "reference_sha256": hashlib.sha256(REFERENCE.read_bytes()).hexdigest()}


def _run_tool(case: Path, tool: str, log_name: str) -> None:
    with (case / log_name).open("xb") as output:
        subprocess.run(["nice", "-n", "10", tool, "-case", str(case)],
                       stdout=output, stderr=subprocess.STDOUT, check=True, timeout=600)


def smoke(case: Path) -> dict:
    """Run serial coarse meshing and three two-step feedback segments once.

    Per-stage receipts allow finished preparation steps to resume. A solver
    segment with a log but no receipt is left for inspection, never rerun.
    Each completed segment records the inputs, history and controller state.
    """
    case = Path(case).resolve()
    manifest = json.loads((case / "manifest.json").read_text())
    if hashlib.sha256(REFERENCE.read_bytes()).hexdigest() != manifest.get("reference_sha256"):
        raise ValueError("smoke reference digest mismatch")
    state_path = case / "smoke_state.json"
    binding = hashlib.sha256((case / "manifest.json").read_bytes()).hexdigest()
    if state_path.exists():
        state = json.loads(state_path.read_text())
        if state["manifest_sha256"] != binding:
            raise ValueError("smoke input digest mismatch")
    else:
        for name, digest in manifest["files"].items():
            if hashlib.sha256((case / name).read_bytes()).hexdigest() != digest:
                raise ValueError("case input digest mismatch")
        state = {"manifest_sha256": binding, "prepared": [], "rows": [],
                 "rpm": 570.0, "integral": 0.0, "tools": {}}
    for name, digest in state.get("checkpoint_sha256", {}).items():
        if hashlib.sha256((case / name).read_bytes()).hexdigest() != digest:
            raise ValueError("smoke checkpoint digest mismatch")
    for name, digest in state["tools"].items():
        if hashlib.sha256((case / f"log.{name}").read_bytes()).hexdigest() != digest:
            raise ValueError("completed tool log digest mismatch")

    def save():
        checkpoint = [p for folder in ("0", "constant", "system")
                      for p in (case / folder).rglob("*") if p.is_file()]
        if state["rows"]:
            folder = case / f"{state['rows'][-1]['time_s']:.12g}"
            checkpoint.extend(p for p in folder.rglob("*") if p.is_file())
        state["checkpoint_sha256"] = {
            str(p.relative_to(case)): hashlib.sha256(p.read_bytes()).hexdigest()
            for p in sorted(checkpoint)
        }
        _write(case, "smoke_state.next.json", json.dumps(state, indent=2) + "\n")
        (case / "smoke_state.next.json").replace(state_path)
    for tool in ("blockMesh", "snappyHexMesh", "topoSet", "setFields", "checkMesh"):
        if tool in state["prepared"]:
            continue
        log_name = f"log.{tool}"
        if tool == "snappyHexMesh":
            with (case / log_name).open("xb") as output:
                subprocess.run(["nice", "-n", "10", tool, "-overwrite", "-case", str(case)],
                               stdout=output, stderr=subprocess.STDOUT, check=True, timeout=600)
        else:
            _run_tool(case, tool, log_name)
        log = (case / log_name).read_text()
        if tool == "checkMesh" and "Mesh OK." not in log:
            raise ValueError("coarse checkMesh did not report Mesh OK")
        if tool == "topoSet":
            matches = re.findall(r"cellZoneSet propeller now size (\d+)", log)
            if not matches or int(matches[-1]) == 0:
                raise ValueError("propeller cell zone is empty")
        state["prepared"].append(tool)
        state["tools"][tool] = hashlib.sha256((case / log_name).read_bytes()).hexdigest()
        save()
    controller = RpmController(integral=state["integral"])
    for index in range(len(state["rows"]), 3):
        rpm, start, end = state["rpm"], index * 0.001, (index + 1) * 0.001
        applied = thrust(rpm)
        _write(case, "constant/fvOptions", source_dictionary(applied))
        _write(case, "system/controlDict", control_dictionary(end, 0.0005))
        log_name = f"log.interFoam.{index+1}"
        _run_tool(case, "interFoam", log_name)
        samples = []
        for path in (case / "postProcessing/hullForces").glob("*/force.dat"):
            samples.extend((t, r) for t, r in read_resistance(path) if start < t <= end + 1e-12)
        samples = sorted(set(samples))
        if len(samples) != 2 or not math.isclose(samples[-1][0], end, abs_tol=1e-12):
            raise ValueError("smoke segment did not advance exactly two steps")
        resistance = sum(r for _, r in samples) / len(samples)
        row = {"time_s": end, "rpm": rpm, "thrust_n": applied,
               "resistance_n": resistance, "towing_n": 30.3}
        state["rows"].append(row)
        state["rpm"] = controller.update(rpm, resistance=resistance, applied_thrust=applied, dt=0.001, towing=30.3)
        state["integral"] = controller.integral
        state["tools"][f"interFoam.{index+1}"] = hashlib.sha256((case / log_name).read_bytes()).hexdigest()
        save()
    with (case / "controller.csv").open("w", encoding="utf-8", newline="") as output:
        writer = csv.DictWriter(output, fieldnames=list(state["rows"][0]), lineterminator="\n")
        writer.writeheader()
        writer.writerows(state["rows"])
    mesh_log = (case / "log.checkMesh").read_text()
    cell_match = re.search(r"^\s*cells:\s*(\d+)", mesh_log, re.MULTILINE)
    if cell_match is None:
        raise ValueError("mesh log does not contain a cell count")
    cells = int(cell_match[1])
    zone_log = (case / "log.topoSet").read_text()
    zone_cells = int(re.findall(r"cellZoneSet propeller now size (\d+)", zone_log)[-1])
    steps = sum(len(re.findall(r"^Time = ", (case / f"log.interFoam.{i}").read_text(), re.MULTILINE)) for i in range(1, 4))
    if steps != 6:
        raise ValueError("solver logs do not contain six advanced steps")
    return {"solver": "interFoam", "ranks": 1, "steps": steps, "end_time_s": 0.003,
            "mesh_cells": cells, "disk_cells": zone_cells, "mesh_check": "Mesh OK",
            "rpm_by_segment": [round(row["rpm"], 1) for row in state["rows"]],
            "manifest_sha256": binding, "comparison": compare_point(state["rows"]),
            "logs": state["tools"], "purpose": "startup and feedback wiring only"}


def main(argv: list[str] | None = None) -> int:
    """Expose build, smoke and diagnostic reference readback commands."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("command", choices=("build", "smoke", "check"))
    parser.add_argument("path", type=Path, help="new case, existing case, or controller CSV")
    parser.add_argument("--output", type=Path, help="write a portable JSON result")
    args = parser.parse_args(argv)
    started = time.perf_counter()
    if args.command == "build":
        build_case(args.path)
        result = {"built": True, "wall_time_s": round(time.perf_counter() - started, 3)}
    elif args.command == "smoke":
        result = smoke(args.path)
        result["wall_time_s"] = round(time.perf_counter() - started, 1)
    else:
        with args.path.open(encoding="utf-8", newline="") as source:
            rows = [{k: float(v) for k, v in r.items()} for r in csv.DictReader(source)]
        result = compare_point(rows)
    text = json.dumps(result, indent=2, allow_nan=False) + "\n"
    if args.output:
        args.output.write_text(text, encoding="utf-8", newline="\n")
    print(text, end="")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
