"""Bind arbitrary-hull case construction to matched resistance evidence.

Sources read for this implementation:
* OpenFOAM v2312 (2023), forces.C, writeIntegratedDataFileHeader and
  writeIntegratedDataFile, lines 429-499:
  https://api.openfoam.com/2312/forces_8C_source.html
* NASA (2021), Examining Spatial (Grid) Convergence, sections 'Order of Grid
  Convergence', 'Richardson Extrapolation' and 'Grid Convergence Index':
  https://www.grc.nasa.gov/www/wind/valid/tutorial/spatconv.html
* Repository calm-water resistance study, equations (1)-(3), for signed
  half-body force, coefficient normalisation, Reynolds and Froude numbers.

This workflow provides numerical diagnostics, never experimental validation.
It does not change the benchmark scorer or inherit its acceptance limits.
"""

from __future__ import annotations

import hashlib
import json
import math
from dataclasses import asdict, dataclass
from pathlib import Path
from typing import Any, Sequence

from .hull_case import HullCaseConfig, build_hull_case
from .runner import OpenFOAMRunConfig, OpenFOAMRunner, OpenFOAMRunResult


def _positive(value: float, name: str) -> None:
    if isinstance(value, bool) or not math.isfinite(value) or value <= 0:
        raise ValueError(f"{name} must be finite and positive")


@dataclass(frozen=True)
class CampaignPolicy:
    """Required assessment settings and their declared engineering basis.

    ``window`` has the force file's time-coordinate units (iterations for an
    LTS case). ``maximum_relative_span`` bounds (max(Ct)-min(Ct))/abs(mean(Ct));
    this is a stationarity diagnostic, not an uncertainty estimate. No default
    tolerance, sampling requirement or acceptance recommendation is supplied.
    """

    window: float
    minimum_samples: int
    maximum_relative_span: float
    force_balance_relative_tolerance: float
    refinement_ratio_relative_tolerance: float
    source: str

    def __post_init__(self) -> None:
        for name in (
            "window",
            "maximum_relative_span",
            "force_balance_relative_tolerance",
            "refinement_ratio_relative_tolerance",
        ):
            _positive(getattr(self, name), name)
        # A range needs at least two observations: a mathematical requirement.
        if type(self.minimum_samples) is not int or self.minimum_samples < 2:
            raise ValueError("minimum_samples must be an integer of at least two")
        if not self.source.strip():
            raise ValueError("source must state the basis of the assessment settings")


@dataclass(frozen=True)
class ResistanceEvidence:
    """One case, one explicitly selected force history and measured grid data.

    ``spacing_m`` is a representative grid spacing chosen consistently across
    the study, not inferred from total cells in a locally refined mesh.
    """

    case_dir: Path
    force_file: Path
    cells: int
    spacing_m: float

    def __post_init__(self) -> None:
        _positive(self.spacing_m, "spacing_m")
        if type(self.cells) is not int or self.cells < 1:
            raise ValueError("cells must be a positive integer")


def _digest(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def _input_files(case: Path) -> dict[str, str]:
    return {
        path.relative_to(case).as_posix(): _digest(path)
        for folder in ("system", "constant", "0.orig")
        for path in sorted((case / folder).rglob("*"))
        if path.is_file()
    }


def prepare_resistance_case(config: HullCaseConfig, parent_dir: Path | str) -> Path:
    """Emit a case and retain the physical condition and original input digests.

    Consume the existing ingestion contract: metres, x forward, y port, z up,
    aft-perpendicular keel origin and closed surface. Ingestion and union-area
    checks remain the existing builder's responsibility. Refuse overwrites
    and full-domain requests: the builder emits a symmetry half-domain.
    """
    parent = Path(parent_dir)
    if Path(config.name).name != config.name or config.name in ("", ".", ".."):
        raise ValueError("case name must be a single directory name")
    if (parent / config.name).exists():
        raise FileExistsError("case already exists; use a new case directory")
    for name, value in (
        ("density", config.density),
        ("velocity", config.velocity),
        ("viscosity", config.nu),
    ):
        _positive(value, name)
    if type(config.half_domain) is not bool:
        raise ValueError("half_domain must be boolean")
    if not config.half_domain:
        raise ValueError(
            "only half-domain preparation is supported by the case builder"
        )
    case = build_hull_case(config, parent)
    condition = {
        "geometry": config.manifest.to_provenance(),
        "surface_digests": {
            region.stl_path.name: _digest(region.stl_path)
            for region in config.surface_regions
        },
        "velocity": config.velocity,
        "density": config.density,
        "kinematic_viscosity": config.nu,
        "area_m2": config.manifest.reference_wetted_surface_m2,
        "half_domain": config.half_domain,
        "attitude": "fixed_even_keel",
        "turbulence_intensity": config.turbulence_intensity,
        "turbulence_method": config.turbulence_method.value,
        "eddy_viscosity_ratio": config.eddy_viscosity_ratio,
        "length_scale_factor": config.length_scale_factor,
        "numerical_method": {
            name: _digest(case / "system" / name)
            for name in ("fvSchemes", "fvSolution")
        },
        "domain_factors": {
            key: getattr(config, key)
            for key in (
                "upstream_lpp",
                "downstream_lpp",
                "lateral_lpp",
                "depth_lpp",
                "freeboard_lpp",
                "keel_clearance_drafts",
            )
        },
    }
    receipt = {"condition": condition, "input_digests": _input_files(case)}
    (case / "resistance_input.json").write_text(
        json.dumps(receipt, indent=2, sort_keys=True, allow_nan=False) + "\n"
    )
    return case


def run_resistance_case(
    config: HullCaseConfig,
    parent_dir: Path | str,
    *,
    timeout_seconds: int,
    dry_run: bool = True,
) -> OpenFOAMRunResult:
    """Prepare and run the complete existing meshing/towing stage sequence.

    Dry run is the default. The runner requires an explicit positive per-stage
    timeout even on dry runs. Completion alone makes no resistance claim.
    """
    _positive(timeout_seconds, "timeout_seconds")
    case = prepare_resistance_case(config, parent_dir)
    return OpenFOAMRunner(
        OpenFOAMRunConfig(
            solver="interFoam",
            ranks=config.ranks,
            dtchull_pipeline=True,
            dry_run=dry_run,
            timeout_seconds=timeout_seconds,
            to_vtk=False,
        )
    ).run(case)


def _force_rows(path: Path) -> list[list[float]]:
    """Strict v2312 nonporous global-axis total/pressure/viscous force layout.

    OpenFOAM v2312 forces.C, lines 443-450 and 489-495: time followed by three
    Cartesian force vectors. Porous and older pressure/viscous layouts require
    a separate adapter; neither is silently interpreted as this format.
    """
    rows = []
    expected = ["Time"] + [
        f"{component}_{axis}"
        for component in ("total", "pressure", "viscous")
        for axis in ("x", "y", "z")
    ]
    header_seen = False
    for number, line in enumerate(path.read_text().splitlines(), 1):
        text = line.strip()
        if not text:
            continue
        if text.startswith("#"):
            header = text[1:].split()
            if header and header[0] == "Time":
                if header != expected:
                    raise ValueError(f"unsupported force header at line {number}")
                header_seen = True
            continue
        if not header_seen:
            raise ValueError(
                "force history requires the v2312 total/pressure/viscous header"
            )
        try:
            values = [
                float(x) for x in text.replace("(", " ").replace(")", " ").split()
            ]
        except ValueError as exc:
            raise ValueError(f"invalid force row at line {number}") from exc
        # Ten fields are the cited time plus three 3D vectors, not a tolerance.
        if len(values) != 10 or not all(math.isfinite(x) for x in values):
            raise ValueError(f"force row must have ten finite values at line {number}")
        if rows and values[0] <= rows[-1][0]:
            raise ValueError(f"force times must increase strictly at line {number}")
        rows.append(values)
    if not rows:
        raise ValueError("force history contains no samples")
    return rows


def reduce_resistance_history(
    force_file: Path | str,
    *,
    density: float,
    area: float,
    speed: float,
    half_domain: bool,
    policy: CampaignPolicy,
) -> dict[str, Any]:
    """Reduce the final time span without inferring or correcting drag sign.

    Negative x is the emitted case's flow/drag direction. R=-F_x for a full
    body and R=-2 F_x for a reflected half body. Ct=R/(rho*S*U**2/2), from
    repository calm-water resistance study equations (1)-(2). The same factor
    and full-body union area apply to every component exactly once.
    """
    for name, value in (("density", density), ("area", area), ("speed", speed)):
        _positive(value, name)
    if type(half_domain) is not bool:
        raise ValueError("half_domain must be boolean")
    rows = _force_rows(Path(force_file))
    rows = [row for row in rows if row[0] > rows[-1][0] - policy.window]
    factor = -2 if half_domain else -1  # Reflection symmetry and negative-x drag.
    denominator = density * area * speed**2 / 2  # Dynamic pressure definition.
    forces = [[factor * row[index] for index in (1, 4, 7)] for row in rows]
    means = [math.fsum(row[i] for row in forces) / len(forces) for i in range(3)]
    coefficients = [value / denominator for value in means]
    ct_values = [row[0] / denominator for row in forces]
    span = (
        (max(ct_values) - min(ct_values)) / abs(coefficients[0])
        if coefficients[0]
        else None
    )
    balanced = all(
        abs(total - pressure - viscous)
        <= policy.force_balance_relative_tolerance
        * max(abs(total), abs(pressure) + abs(viscous))
        for total, pressure, viscous in forces
    )
    enough = len(rows) >= policy.minimum_samples
    stationary = (
        "NOT_EVALUATED"
        if not enough or span is None
        else "PASS"
        if span <= policy.maximum_relative_span
        else "FAIL"
    )
    return {
        "resistance_n": means[0],
        "ct": coefficients[0],
        "cp": coefficients[1],
        "cv": coefficients[2],
        "samples": len(rows),
        "first_time": rows[0][0],
        "last_time": rows[-1][0],
        "relative_span": span,
        "checks": {
            "resistance_sign": "PASS" if all(row[0] > 0 for row in forces) else "FAIL",
            "force_balance": "PASS" if balanced else "FAIL",
            "stationarity": stationary,
        },
        "force_sha256": _digest(Path(force_file)),
    }


def _grid_result(
    levels: list[dict[str, Any]], policy: CampaignPolicy
) -> dict[str, Any]:
    result: dict[str, Any] = {"status": "NOT_EVALUATED", "absolute_gci": None}
    # Three levels are required to measure the order (NASA 2021, cited above).
    if len(levels) != 3:
        return dict(result, reason="exactly three matched grid levels are required")
    fine, medium, coarse = levels
    r21 = medium["spacing_m"] / fine["spacing_m"]
    r32 = coarse["spacing_m"] / medium["spacing_m"]
    # NASA 2021 'Grid Considerations': r >= 1.1 separates discretisation errors.
    if min(r21, r32) < 1.1 or not math.isclose(
        r21,
        r32,
        rel_tol=policy.refinement_ratio_relative_tolerance,
        abs_tol=0,
    ):
        return dict(result, reason="spacing ratios must be equal and at least 1.1")
    if not fine["cells"] > medium["cells"] > coarse["cells"]:
        return dict(result, reason="cell counts must increase as spacing decreases")
    d21 = medium["ct"] - fine["ct"]
    d32 = coarse["ct"] - medium["ct"]
    # A positive, decreasing difference is required by the single-term model.
    if d21 == 0 or d32 == 0 or not 0 < d21 / d32 < 1:
        return dict(
            result, reason="sequence is not nondegenerate monotonic convergence"
        )
    order = math.log(d32 / d21) / math.log(r21)
    error = d21 / (r21**order - 1)
    # NASA 2021 'Grid Convergence Index': safety factor 1.25 for three grids.
    return {
        "status": "EVALUATED",
        "observed_order": order,
        "refinement_ratio": r21,
        "extrapolated_ct": fine["ct"] - error,
        "absolute_gci": 1.25 * abs(error),
        "source": "NASA (2021), Examining Spatial (Grid) Convergence, Grid Convergence Index",
        "scope": "Spatial discretisation estimate only; asymptotic model assumed",
    }


def assess_resistance_campaign(
    evidence: Sequence[ResistanceEvidence],
    policy: CampaignPolicy,
) -> dict[str, Any]:
    """Assess matched force histories; missing evidence never becomes a pass.

    Input digests detect stale dictionaries or replaced geometry. Case receipts
    are reproducibility records, not authenticated attestations. Grid spacing,
    cell counts and force-to-case association are supplied by the caller.
    """
    if not evidence:
        raise ValueError("at least one case is required")
    cases = [Path(item.case_dir).resolve() for item in evidence]
    if len(set(cases)) != len(cases):
        raise ValueError("grid levels must use distinct case directories")
    condition = None
    levels = []
    for item in evidence:
        case = Path(item.case_dir)
        receipt = json.loads((case / "resistance_input.json").read_text())
        for relative, expected in receipt["input_digests"].items():
            path = case / relative
            if not path.is_file() or _digest(path) != expected:
                raise ValueError("case inputs changed since preparation")
        current = receipt["condition"]
        if condition is not None and current != condition:
            raise ValueError("physical conditions do not match across grid levels")
        condition = current
        reduced = reduce_resistance_history(
            item.force_file,
            density=current["density"],
            area=current["area_m2"],
            speed=current["velocity"],
            half_domain=current["half_domain"],
            policy=policy,
        )
        levels.append(dict(reduced, cells=item.cells, spacing_m=item.spacing_m))
    levels.sort(key=lambda level: level["spacing_m"])
    if len({level["last_time"] for level in levels}) != 1:
        raise ValueError("final averaging times do not match across grid levels")
    checks = [value for level in levels for value in level["checks"].values()]
    if len({level["first_time"] for level in levels}) != 1:
        grid = {
            "status": "NOT_EVALUATED",
            "absolute_gci": None,
            "reason": "actual averaging coverage must match across grid levels",
        }
    elif all(value == "PASS" for value in checks):
        grid = _grid_result(levels, policy)
    else:
        grid = {
            "status": "NOT_EVALUATED",
            "absolute_gci": None,
            "reason": "all force histories must satisfy the declared reduction policy",
        }
    status = (
        "FAIL"
        if "FAIL" in checks
        else "NOT_EVALUATED"
        if "NOT_EVALUATED" in checks or grid["status"] != "EVALUATED"
        else "PASS"
    )
    return {
        "mode": "referent_free_prediction",
        "condition": condition,
        "policy": asdict(policy),
        "levels": levels,
        "grid": grid,
        "checks": {"numerical_evidence": status},
        "validation": {
            "status": "NOT_EVALUATED",
            "reason": "No matched experiment supplied",
        },
        "qualification": {
            "status": "NOT_EVALUATED",
            "reason": (
                "Temporal and domain sensitivity, near-wall resolution, modelling error "
                "and transfer of a passing benchmark are not established by this assessment"
            ),
        },
        "empirical_screening": {
            "status": "NOT_EVALUATED",
            "reason": (
                "No sourced hull-specific empirical input supplied; no form-factor band assumed"
            ),
        },
    }
