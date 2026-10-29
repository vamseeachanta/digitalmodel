"""Generic berth preparation and stored-result checks; no solver execution.

Axes: x along the berth, y toward the passing track, z up. Moments follow
x cross y = z. Supplied velocities point *toward* the force direction, unlike
meteorological 'from' directions. All calculations use SI; native review models
use metres, tonnes and kN. Native load wiring and equilibrium are NOT_EVALUATED.

Equation sources:
* NASA, Drag Equation, public web edition retrieved 2026-10-20: F=rho*Cd*A*U²/2.
* Orcina, Links: Data, public web edition retrieved 2026-10-20: tether and
  piecewise-linear spring laws; negative spring tension denotes compression.
* docs/domains/hydrodynamics/wang-passing-ship-forces.md: 1975 slender-body
  formulation and finite-depth image sums. Reuses the qualified legacy port.
No environmental coefficient or acceptance limit is supplied by this module.
"""

from __future__ import annotations

import csv
import hashlib
from importlib.metadata import version
from itertools import product
import json
import math
from pathlib import Path
import platform
import re
from typing import Callable

import yaml

from digitalmodel.hydrodynamics.wang_passing_ship import WangVessel, wang_passing_forces


SOURCE_URLS = {
    "drag": "https://www1.grc.nasa.gov/beginners-guide-to-aeronautics/drag-equation/",
    "links": "https://www.orcina.com/webhelp/OrcaFlex/Content/html/Links,Data.htm",
    "passing": "docs/domains/hydrodynamics/wang-passing-ship-forces.md",
}


def _number(value, *, positive=False):
    if isinstance(value, bool) or not isinstance(value, (int, float)):
        raise ValueError("finite numerical input required")
    if not math.isfinite(value) or (positive and value <= 0):
        raise ValueError("finite input required, positive where specified")
    return value


def _vector(values, length):
    if not isinstance(values, list) or len(values) != length:
        raise ValueError("vector has incorrect length")
    return [_number(v) for v in values]


def drag_load(load: dict) -> list[float]:
    """Return [Fx N, Fy N, Mz N m] for an isotropic projected-area screen.

    NASA Drag Equation, retrieved 2026-10-20. The factor 1/2 is part of
    dynamic pressure. A single supplied Cd and area apply to the resultant;
    this is not a heading-dependent marine coefficient model.
    Mz = x*Fy-y*Fx follows the definition of the cross product.
    """
    rho, area, cd = [
        _number(load[k], positive=True)
        for k in ("density_kg_m3", "area_m2", "coefficient")
    ]
    vx, vy = _vector(load["velocity_m_s"], 2)
    x, y = _vector(load["application_m"], 2)
    factor = 0.5 * rho * area * cd * math.hypot(vx, vy)
    fx, fy = factor * vx, factor * vy
    result = [fx, fy, x * fy - y * fx]
    for value in result:
        _number(value)
    return result


def load_spec(path: Path) -> dict:
    """Load the generic preparation input and validate before any calculation."""
    spec = yaml.safe_load(Path(path).read_text())
    validate_spec(spec)
    return spec


def validate_spec(spec: dict) -> None:
    """Reject ambiguous geometry, missing bases and nonfinite numerical inputs."""
    try:
        for group in (
            "moored",
            "passing",
            "depth_m",
            "water_density_kg_m3",
            "wind",
            "current",
            "lines",
            "fenders",
            "passages",
            "sampling",
            "numerics",
        ):
            if (
                not isinstance(spec["basis"][group], str)
                or not spec["basis"][group].strip()
            ):
                raise ValueError(
                    "every input group requires a source or assumption basis"
                )
        for kind in ("moored", "passing"):
            hull = spec[kind]
            for key in ("length_m", "beam_m", "draft_m", "midship_area_m2"):
                _number(hull[key], positive=True)
            if hull["midship_area_m2"] > hull["beam_m"] * hull["draft_m"]:
                raise ValueError("sectional area exceeds immersed rectangular section")
        for key in ("mass_t", "yaw_inertia_t_m2"):
            _number(spec["moored"][key], positive=True)
        depth = _number(spec["depth_m"], positive=True)
        if depth <= max(spec[k]["draft_m"] for k in ("moored", "passing")):
            raise ValueError("depth must exceed both drafts")
        _number(spec["water_density_kg_m3"], positive=True)
        for key in ("wind", "current"):
            drag_load(spec[key])
        times = spec["sampling"]["times_s"]
        _vector(times, len(times))
        if (
            len(times) < 2
            or times[0] != 0
            or any(b <= a for a, b in zip(times, times[1:]))
        ):
            raise ValueError("time samples must start at zero and strictly increase")
        _number(spec["sampling"]["initial_stagger_m"])
        _number(spec["sampling"]["time_step_s"], positive=True)
        for key, minimum in (("quadrature_nodes", 2), ("image_pairs", 0)):
            value = spec["numerics"][key]
            # Mathematical quadrature/image counts, not physical acceptance limits.
            if isinstance(value, bool) or not isinstance(value, int) or value < minimum:
                raise ValueError("invalid numerical integration count")
        if not spec["passages"]:
            raise ValueError("at least one passage required")
        seen = set()
        for passage in spec["passages"]:
            speed = _number(passage["speed_m_s"], positive=True)
            separation = _number(passage["separation_m"], positive=True)
            if separation <= (spec["moored"]["beam_m"] + spec["passing"]["beam_m"]) / 2:
                raise ValueError("parallel hull footprints overlap")
            if (speed, separation) in seen:
                raise ValueError("duplicate passage")
            seen.add((speed, separation))
        objects = spec["lines"] + spec["fenders"]
        names = [o["name"] for o in objects]
        if not spec["lines"] or not spec["fenders"] or len(names) != len(set(names)):
            raise ValueError("lines and fenders need unique populated names")
        if any(not re.fullmatch(r"[a-z][a-z0-9_]*", n) for n in names):
            raise ValueError("object names must be safe identifiers")
        for item in objects:
            fairlead = _vector(item["fairlead_m"], 3)
            anchor = _vector(item["anchor_m"], 3)
            if math.dist(fairlead, anchor) <= 0:
                raise ValueError("link endpoints must be distinct")
            if fairlead[1] > 0 or anchor[1] > 0:
                raise ValueError(
                    "generic berth links must stay on the quay side, y <= 0"
                )
        for item in spec["lines"]:
            _number(item["unstretched_length_m"], positive=True)
            _number(item["ea_kn"], positive=True)
        for item in spec["fenders"]:
            table = item["length_force_kn"]
            if len(table) < 2:
                raise ValueError("fender length-force table requires at least two rows")
            for row in table:
                length, force = _vector(row, 2)
                if length < 0 or force > 0:
                    raise ValueError(
                        "fender length must be nonnegative and tension nonpositive"
                    )
            if any(b[0] <= a[0] or b[1] < a[1] for a, b in zip(table, table[1:])):
                raise ValueError(
                    "fender lengths strictly increase and compression must decrease"
                )
            if table[-1][1] != 0 or table[-2][1] != 0:
                raise ValueError(
                    "fender must finish with two zero-force rows to prevent tensile extrapolation"
                )
            reach = math.dist(item["fairlead_m"], item["anchor_m"])
            if reach > table[-1][0]:
                raise ValueError("initial fender length exceeds table domain")
        if set(spec["criteria"]) - set(names):
            raise ValueError("criterion supplied for unknown object")
        for criterion in spec["criteria"].values():
            _number(criterion["allowable_kn"], positive=True)
            if (
                not isinstance(criterion["source"], str)
                or not criterion["source"].strip()
            ):
                raise ValueError("acceptance criterion requires a source")
    except (KeyError, TypeError, AttributeError) as exc:
        raise ValueError("missing or malformed berth preparation input") from exc


def build_campaign(spec: dict) -> list[dict]:
    """Explicit passages crossed with calm, wind, current and combined loads."""
    validate_spec(spec)
    cases = []
    for index, passage in enumerate(spec["passages"], start=1):
        for wind, current in product((False, True), repeat=2):
            cases.append(
                dict(
                    name=f"case_{index}_w{int(wind)}_c{int(current)}",
                    **passage,
                    wind=wind,
                    current=current,
                )
            )
    return cases


def prepare_history(
    spec: dict, case: dict, *, force_function: Callable | None = None
) -> list[dict]:
    """Sample x=x0+U*t, add explicit steady loads; no response integration.

    Straight-line constant-speed kinematics and linear force superposition;
    passing forces use the repository's documented 1975 image-sum method.
    Current drag is independent; passage speed is through still water, and
    current is not subtracted along the passing track. Channel/bank effects,
    current-passage interaction and moving-hull feedback are excluded.
    """
    validate_spec(spec)
    speed = _number(case["speed_m_s"], positive=True)
    separation = _number(case["separation_m"], positive=True)
    if separation <= (spec["moored"]["beam_m"] + spec["passing"]["beam_m"]) / 2:
        raise ValueError("parallel hull footprints overlap")
    # Preserve requested endpoints while bounding the interval by time_step_s.
    # Faster passages use the same nominal stagger increment as the slowest
    # declared passage; aligned endpoints then preserve sampled U squared peaks.
    step = spec["sampling"]["time_step_s"] * min(
        p["speed_m_s"] for p in spec["passages"]
    ) / speed
    requested = spec["sampling"]["times_s"]
    times = [requested[0]]
    for start, finish in zip(requested, requested[1:]):
        count = math.ceil((finish - start) / min(step, spec["sampling"]["time_step_s"]))
        times.extend(start + (finish - start) * i / count for i in range(1, count))
        times.append(finish)
    staggers = [spec["sampling"]["initial_stagger_m"] + speed * t for t in times]
    if force_function is None:
        vessels = [
            WangVessel(spec[k]["length_m"], spec[k]["midship_area_m2"])
            for k in ("moored", "passing")
        ]
        result = wang_passing_forces(
            staggers,
            separation,
            *vessels,
            speed,
            spec["water_density_kg_m3"],
            depth=spec["depth_m"],
            n_images=spec["numerics"]["image_pairs"],
            n_nodes=spec["numerics"]["quadrature_nodes"],
        )
        forces = [list(row) for row in zip(result.surge, result.sway, result.yaw)]
    else:
        forces = force_function(staggers, separation, speed)
    if len(forces) != len(times):
        raise ValueError("passing force row count differs from time samples")
    steady = [0.0, 0.0, 0.0]
    for key in ("wind", "current"):
        if case[key]:
            steady = [a + b for a, b in zip(steady, drag_load(spec[key]))]
    rows = []
    for t, x, force in zip(times, staggers, forces):
        if len(force) != 3:
            raise ValueError("three passing force components required")
        combined = [_number(float(a + b)) for a, b in zip(force, steady)]
        rows.append(
            dict(
                time_s=t,
                stagger_m=x,
                surge_n=combined[0],
                sway_n=combined[1],
                yaw_nm=combined[2],
            )
        )
    return rows


def native_model(spec: dict) -> dict:
    """Build a generic native review model, without claiming solver loadability.

    Object/property spelling follows the existing library, not its values.
    Links implement Orcina's tension-only tether and compression-only table.
    Environmental loads are in the accompanying CSV, not yet wired to vessel
    applied loads. Native wind/current loads are disabled to avoid double count.
    The review model requires load wiring, equilibrium and hydrodynamic data
    qualification before any approved solve; structural YAML checks alone do
    not establish these. No template with unrecorded provenance is imported.
    """
    validate_spec(spec)
    connections = (
        "Connection, ConnectionX, ConnectionY, ConnectionZ, ConnectionzRelativeTo"
    )
    links = []
    for item in spec["lines"] + spec["fenders"]:
        link = dict(Name=item["name"], ReleaseStage=None)
        link[connections] = [
            ["berth_body", *item["fairlead_m"], None],
            ["Fixed", *item["anchor_m"], None],
        ]
        if item in spec["lines"]:
            link.update(
                LinkType="Tether",
                UnstretchedLength=item["unstretched_length_m"],
                Stiffness=item["ea_kn"],
            )
        else:
            link.update(
                LinkType="Spring/damper",
                LinearSpring="No",
                LinearDamper="Yes",
                DampingConstant=0,
            )
            link["SpringLength, SpringTension"] = item["length_force_kn"]
        links.append(link)
    hull = spec["moored"]
    times = spec["sampling"]["times_s"]
    time_step = spec["sampling"]["time_step_s"]
    return {
        "General": {
            "UnitsSystem": "SI",
            "DynamicsSolutionMethod": "Implicit time domain",
            "ImplicitConstantTimeStep": time_step,
            "StartTime": times[0],
            "FirstStage": 1,
            "StageDuration": [times[-1] - times[0]],
            # Orcina General data: Analysis: an already completed custom ramp
            # preserves the CSV loads; no build-up is simulated in this model.
            "RampStartTime": times[0] - 2 * time_step,
            "RampFinishTime": times[0] - time_step,
        },
        # BIPM SI Brochure, 9th ed. (2019), SI prefixes: kilo=10^3.
        # OrcaFlex SI density is tonnes/m³, unlike the force calculation in kg/m³.
        "Environment": {
            "WaterDepth": spec["depth_m"],
            "Density": spec["water_density_kg_m3"] / 1000,
            "RefCurrentSpeed": 0,
            "WindSpeed": 0,
            "WaveTrains": [
                {
                    "Name": "still_water",
                    "WaveType": "Airy",
                    "WaveHeight": 0,
                    "WavePeriod": 1,
                }
            ],
        },
        "VesselTypes": [
            {
                "Name": "generic_type",
                "Length": hull["length_m"],
                "Draughts": [
                    {
                        "Name": "generic_draft",
                        "Mass": hull["mass_t"],
                        "MomentOfInertiaTensorX, MomentOfInertiaTensorY, MomentOfInertiaTensorZ": [
                            [None, 0, 0],
                            [0, None, 0],
                            [0, 0, hull["yaw_inertia_t_m2"]],
                        ],
                    }
                ],
            }
        ],
        "Vessels": [
            {
                "Name": "berth_body",
                "VesselType": "generic_type",
                "Draught": "generic_draft",
                "Connection": "Free",
                "InitialPosition": [0, 0, 0],
                "Orientation": [0, 0, 0],
                "IncludedInStatics": "3 DOF",
                "PrimaryMotion": "Calculated (3 DOF)",
                "SuperimposedMotion": "None",
                "IncludeAppliedLoads": "No",
                **{
                    key: "No"
                    for key in (
                        "IncludeWindLoad",
                        "IncludeCurrentLoad",
                        "IncludeWaveLoad1stOrder",
                        "IncludeWaveDriftLoad2ndOrder",
                        "IncludeAddedMassAndDamping",
                        "IncludeOtherDamping",
                    )
                },
            }
        ],
        "Links": links,
    }


def input_digest(spec: dict) -> str:
    """Identify the exact input including its assumptions and criteria."""
    return hashlib.sha256(
        json.dumps(spec, sort_keys=True, allow_nan=False).encode()
    ).hexdigest()


def evaluate_results(spec: dict, stored: dict | None) -> list[dict]:
    """Reduce already exported tensile/compressive magnitudes in kN.

    Acceptance is peak <= supplied allowable, by definition of an allowable.
    Utilisation is peak/allowable, a dimensionless ratio. No design factor is
    inferred. Stored loads must be nonnegative magnitudes, not signed native
    spring tensions. A single nonempty solver version covers all cases.
    Missing criteria or results remain NOT_EVALUATED, never PASS.
    """
    cases = build_campaign(spec)
    names = [item["name"] for item in spec["lines"] + spec["fenders"]]
    if stored is not None:
        if stored.get("input_sha256") != input_digest(spec):
            raise ValueError("stored input digest does not match preparation input")
        if (
            not isinstance(stored.get("solver_version"), str)
            or not stored["solver_version"].strip()
        ):
            raise ValueError("recorded solver version required")
        if set(stored.get("cases", {})) != {c["name"] for c in cases}:
            raise ValueError("stored case set must exactly match campaign")
        for values in stored["cases"].values():
            if set(values) != set(names):
                raise ValueError("stored object set must exactly match model")
            for series in values.values():
                if not isinstance(series, list) or not series:
                    raise ValueError("nonempty stored load samples required")
                if any(_number(v) < 0 for v in series):
                    raise ValueError(
                        "stored loads must be nonnegative magnitudes in kN"
                    )
    records = []
    for case in cases:
        for name in names:
            peak = None if stored is None else max(stored["cases"][case["name"]][name])
            criterion = spec["criteria"].get(name)
            allowable = None if criterion is None else criterion["allowable_kn"]
            status = (
                "NOT_EVALUATED"
                if peak is None or allowable is None
                else "PASS"
                if peak <= allowable
                else "FAIL"
            )
            records.append(
                dict(
                    case=case["name"],
                    object=name,
                    peak_kn=peak,
                    allowable_kn=allowable,
                    utilisation=None
                    if peak is None or allowable is None
                    else peak / allowable,
                    status=status,
                    criterion_source=None if criterion is None else criterion["source"],
                    solver_version=None if stored is None else stored["solver_version"],
                )
            )
    return records


def write_campaign(
    spec: dict, destination: Path, *, stored: dict | None = None
) -> dict:
    """Write native review YAML, force CSVs and a digest-bearing case manifest."""
    cases = build_campaign(spec)
    response = evaluate_results(spec, stored)
    destination = Path(destination)
    destination.mkdir(parents=True, exist_ok=True)
    records = []
    model = native_model(spec)
    for case in cases:
        model_name, history_name = case["name"] + ".yml", case["name"] + ".csv"
        rows = prepare_history(spec, case)
        model_text = (
            "# Generic review model; all values assumed, based on previous experience.\n"
            "# Not solve-ready: applied load wiring and equilibrium NOT_EVALUATED.\n"
            "# Zero damping/waves are exclusions; unit wave period is inactive.\n"
            "# Force laws: Orcina Links: Data, public web edition, 2026-10-20.\n"
            f"# Case: {case['name']}\n"
            f"# Force history: {history_name}\n"
            "# Roll/pitch inertia: NOT_EVALUATED; null entries require supplied data before use.\n"
        )
        (destination / model_name).write_text(
            model_text + yaml.safe_dump(model, sort_keys=False)
        )
        with (destination / history_name).open("w", newline="") as stream:
            writer = csv.DictWriter(
                stream, fieldnames=list(rows[0]), lineterminator="\n"
            )
            writer.writeheader()
            writer.writerows(rows)
        records.append(
            {
                **case,
                "model": model_name,
                "history": history_name,
                "coupled_response": "NOT_EVALUATED",
                "applied_load_wiring": "NOT_EVALUATED",
                "peak_force_n": {
                    k: max(abs(r[k]) for r in rows) for k in ("surge_n", "sway_n")
                },
                "peak_yaw_nm": max(abs(r["yaw_nm"]) for r in rows),
            }
        )
    files = [
        name for record in records for name in (record["model"], record["history"])
    ]
    manifest = dict(
        cases=records,
        sources=SOURCE_URLS,
        basis=spec["basis"],
        numerical_runtime={
            "python": platform.python_version(),
            **{
                package: version(package)
                for package in (
                    "numpy", "scipy", "pyyaml", "pydantic", "pandas", "plotly", "jinja2"
                )
            },
        },
        input_sha256=input_digest(spec),
        artifacts={
            name: hashlib.sha256((destination / name).read_bytes()).hexdigest()
            for name in files
        },
        solver_version=None if stored is None else stored["solver_version"],
        response=response,
    )
    (destination / "manifest.json").write_text(
        json.dumps(manifest, indent=2, allow_nan=False) + "\n"
    )
    return manifest
