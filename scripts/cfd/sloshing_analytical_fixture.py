# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Generate synthetic rigid-tank analytical references with explicit evidence gaps.

Only the standard library is required. SI inputs are synthetic assumptions;
publication metadata is descriptive and never supplies calculation inputs.
"""

from __future__ import annotations

import argparse
import copy
from html import escape
import json
import math
from pathlib import Path

REVISION = "analytical-v1"
UNITS = {
    "length_m": "m", "height_m": "m", "depth_m": "m",
    "displacement_m": "m", "gravity_m_s2": "m/s^2", "period_ratio": "1",
}
RESULT_UNITS = {
    "omega1_rad_s": "rad/s", "t1_s": "s", "excitation_omega_rad_s": "rad/s",
    "excitation_period_s": "s", "acceleration_amplitude_m_s2": "m/s^2",
    "quasi_static_end_to_end_m": "m", "period_ratio": "1",
}
MISSING = [
    {"id": "numerical-sway-reference", "status": "BLOCKED",
     "needed": "Rights-cleared rigid-tank sway samples with units and probe coordinates."},
    {"id": "sway-probe-boundary-equivalence", "status": "UNVERIFIED",
     "needed": "Probe definitions, boundary conditions and rigid-wall equivalence evidence."},
    {"id": "calibrated-pitch-forcing", "status": "BLOCKED",
     "needed": "Linkage dimensions and calibrated pitch forcing; not replaced by sway."},
    {"id": "native-input-output-evidence", "status": "UNVERIFIED",
     "needed": "Native inputs, histories, digests and reduction criteria for matched cases."},
    {"id": "source-specific-public-numerical-reuse", "status": "UNVERIFIED",
     "needed": "Documented source-specific permission for public numerical reuse."},
]
SOURCES = [
    {"id": "supplied-evidence-summary",
     "sha256": "c24795c00c6c56df19a953fff558adf60620be0c3e1549bf69e732659feb54b1"},
    {"id": "historical-research-summary",
     "sha256": "6d68bca3184e0023db9ec912323135254f3a57122d216bba5370d84ed694c9af"},
    {"id": "cited-publication",
     "sha256": "a80411c1054c6f877c2974f195af1d125c24ce45164b9fbde50eaee1d474e4fc"},
]
CITED_METADATA = {
    "status": "UNVERIFIED", "numerical_reuse": "UNVERIFIED",
    "used_in_calculation": False, "source_id": "cited-publication",
    "basis": "Supplied historical descriptors; publication payload was not inspected.",
    "citation": "Applied Ocean Research; publication date 2026-09-13; full title not supplied.",
    "rights": "Citation only; CC BY-NC-ND 4.0 does not establish numerical reuse here.",
    "sway": {
        "length_m": 0.57, "height_m": 0.30, "depth_m": 0.15,
        "displacement_mm": 0.4, "excitation_omega_rad_s": 5.028,
        "reported_frequency_ratio": 0.83,
        "acceleration_expression": "a*omega^2*sin(omega*t); a is displacement",
        "wall_condition": "Flexible-wall figure; rigid-limit equivalence UNVERIFIED",
    },
    "historical_proposal": {"omega1_rad_s": 6.058, "status": "UNADOPTED",
                            "use": "Historical metadata only; no numerical comparator."},
    "pitch": {
        "length_m": 0.25, "span_m": 0.16, "height_m": 0.15,
        "fill_fractions": [0.3, 0.5], "period_s": 6.0,
        "reported_excitation_hz": 0.167, "reported_first_frequency_hz": 1.47,
        "probe_from_right_wall_mm": 25.0, "acquisition_frames_s": 50.0,
        "numerical_samples": "ABSENT", "forcing": "BLOCKED: linkage dimensions absent",
        "viscosity": "UNVERIFIED exponent; excluded from calculation",
    },
    "rotational_geometry": {
        "breadth_m": 0.9, "length_m": 1.35, "height_m": 0.9, "depth_m": 0.45,
        "use": "Descriptive only; roll/pitch summaries are not pure-sway references.",
    },
    "unadopted_proposals": {
        "period_ratio_threshold": 8.0, "planar_spatial_difference_percent": 3.0,
        "quasi_static_difference_percent": 5.0,
        "use": "Not physical acceptance authority; no solver comparison is available.",
    },
}
DERIVATION = [
    "Incompressible irrotational flow gives Laplace(phi)=0 in a rigid rectangular tank.",
    "For x in [0,L], z in [-h,0], impermeable boundaries admit "
    "phi_n proportional to cos(k_n*x)*cosh(k_n*(z+h)), k_n=n*pi/L.",
    "Linear kinematic and dynamic free-surface conditions give "
    "omega_n^2=g*k_n*tanh(k_n*h), for positive integer n.",
    "Odd and even modes are eigenmodes; their forcing coupling is not calculated.",
    "T1=2*pi/omega1; T=period_ratio*T1; omega=omega1/period_ratio; "
    "acceleration amplitude=displacement*omega^2.",
    "Hydrostatic slope d(eta)/dx=-acceleration/g gives the spatial end-to-end "
    "magnitude L*displacement*omega^2/g; this is not temporal peak-to-peak elevation.",
]


def _positive(value: object, name: str) -> float:
    if isinstance(value, bool) or not isinstance(value, (int, float)):
        raise ValueError(f"{name} must be a finite positive number")
    try:
        number = float(value)
    except (OverflowError, ValueError) as exc:
        raise ValueError(f"{name} is not representable") from exc
    if not math.isfinite(number) or number <= 0:
        raise ValueError(f"{name} must be a finite positive number")
    return number


def length_to_m(value: float, unit: str) -> float:
    """Convert a positive length in m or mm to SI, rejecting other units.

    Args:
        value: Positive length magnitude.
        unit: Exact unit symbol, either m or mm.

    Returns:
        Length in metres.

    Raises:
        ValueError: The unit or magnitude is invalid or unrepresentable.
    """
    if unit not in ("m", "mm"):
        raise ValueError("length unit must be m or mm")
    return _positive(_positive(value, "length") * (0.001 if unit == "mm" else 1), "length_m")


def natural_frequency(length_m: float, depth_m: float, gravity_m_s2: float,
                      mode: int = 1) -> float:
    """Return a rigid rectangular tank eigenfrequency in rad/s.

    Args:
        length_m: Tank length along the mode direction, in metres.
        depth_m: Liquid depth, in metres.
        gravity_m_s2: Gravitational acceleration, in metres per second squared.
        mode: Positive integer eigenmode number.

    Returns:
        Angular eigenfrequency in radians per second.

    Raises:
        ValueError: An input or derived magnitude is invalid.
    """
    if type(mode) is not int or mode < 1:
        raise ValueError("mode must be a positive integer")
    length = _positive(length_m, "length_m")
    depth = _positive(depth_m, "depth_m")
    gravity = _positive(gravity_m_s2, "gravity_m_s2")
    try:
        k = _positive(mode * math.pi / length, "wave_number_rad_m")
        x = _positive(k * depth, "relative_depth")
        squared = _positive(gravity * k * math.tanh(x), "omega_squared")
        return _positive(math.sqrt(squared), "omega_rad_s")
    except OverflowError as exc:
        raise ValueError("eigenfrequency is not representable") from exc


def _point(inputs: dict, omega1: float, ratio: float) -> dict:
    try:
        omega = _positive(omega1 / ratio, "excitation_omega_rad_s")
        acceleration = _positive(inputs["displacement_m"] * omega**2, "acceleration_amplitude_m_s2")
        excursion = _positive(inputs["length_m"] * acceleration / inputs["gravity_m_s2"], "quasi_static_end_to_end_m")
        return {
            "period_ratio": ratio, "omega1_rad_s": omega1,
            "t1_s": _positive(2 * math.pi / omega1, "t1_s"),
            "excitation_omega_rad_s": omega,
            "excitation_period_s": _positive(2 * math.pi / omega, "excitation_period_s"),
            "acceleration_amplitude_m_s2": acceleration,
            "quasi_static_end_to_end_m": excursion,
            "classification": "QUASI_STATIC_ANCHOR" if ratio >= 8 else "DYNAMIC_REFERENCE",
            "excursion_interpretation": (
                "Hydrostatic spatial amplitude approximation; physical accuracy NOT_ESTABLISHED."
                if ratio >= 8 else
                "Hydrostatic slope proxy only; NOT a dynamic or near-resonant response amplitude."
            ),
            "status": "SYNTHETIC-ANALYTICAL", "units": RESULT_UNITS.copy(),
        }
    except OverflowError as exc:
        raise ValueError("forced quantities are not representable") from exc


def build_fixture(inputs: dict) -> dict:
    """Calculate a synthetic reference and attach the governing evidence limits.

    Args:
        inputs: Six positive SI magnitudes and an exact units mapping.

    Returns:
        JSON-compatible records; physical validation remains blocked.

    Raises:
        ValueError: Fields, units or values violate the analytical contract.
    """
    if not isinstance(inputs, dict) or set(inputs) != {*UNITS, "units"}:
        raise ValueError("provide only the six SI inputs and units; acceleration is derived")
    if inputs["units"] != UNITS:
        raise ValueError("input units must match the explicit SI fields")
    values = {name: _positive(inputs[name], name) for name in UNITS}
    if values["depth_m"] > values["height_m"]:
        raise ValueError("depth_m exceeds height_m")
    omega = natural_frequency(values["length_m"], values["depth_m"], values["gravity_m_s2"])
    points = [_point(values, omega, ratio) for ratio in (1.1, 8.0)]
    modes = [{"mode": n, "wave_number_rad_m": n * math.pi / values["length_m"],
              "omega_rad_s": natural_frequency(values["length_m"], values["depth_m"], values["gravity_m_s2"], n),
              "units": {"wave_number_rad_m": "rad/m", "omega_rad_s": "rad/s"}}
             for n in range(1, 5)]
    return _records(values, omega, points, modes)


def _records(values: dict, omega: float, points: list, modes: list) -> dict:
    return {
        "model_revision": REVISION, "criteria_revision": REVISION,
        "dataset_id": "sloshing-analytical-fixture", "owner": "digitalmodel",
        "intended_use": "Synthetic analytical software regression only; no physical validation.",
        "inputs": {**values, "units": UNITS.copy()},
        "assumptions": {**dict.fromkeys(UNITS, "SYNTHETIC-ASSUMPTION"),
                        "model": "Rigid walls, incompressible irrotational flow, linear small displacement; "
                                 "viscosity, damping, wall flexibility and nonlinear response excluded.",
                        "reference_points": "Synthetic period ratios 1.1 and 8; frequencies only, no response histories."},
        "derivation": DERIVATION.copy(), "modes": modes,
        "results": _point(values, omega, values["period_ratio"]),
        "reference_points": [{**point, "purpose": purpose} for point, purpose in zip(points, (
            "Synthetic near-first-mode frequency descriptor; no dynamic response predicted.",
            "Synthetic off-resonance sway anchor; no pitch experiment reproduced.",
        ))],
        "criteria": {"software_identity_relative_allowance": 1e-12,
                     "allowance_scope": "Software/asymptotic identities only; no physical tolerance.",
                     "quasi_static_period_ratio_minimum": 8.0,
                     "threshold_basis": "Proposed synthetic anchor convention; no accuracy demonstrated.",
                     "physical_acceptance": "NOT_ESTABLISHED"},
        "evidence_status": {"synthetic_model_regression": "ANALYTICAL_REFERENCE",
                            "physical_validation": "BLOCKED", "solver_2d_3d_agreement": "NOT_ESTABLISHED"},
        "missing": copy.deepcopy(MISSING), "cited_metadata": copy.deepcopy(CITED_METADATA),
        "sources": [{**source, "verification": "SUPPLIED-HISTORICAL-ASSERTION",
                     "observed_at": "2026-10-25 UTC", "freshness": "NOT_REVERIFIED",
                     "use": "Provenance only; no source-derived arrays."} for source in SOURCES],
        "counts": {"synthetic_reference_points": len(points), "eigenmodes": len(modes),
                   "qualified_physical_results": 0, "missing_evidence_items": len(MISSING)},
        "next_evidence_step": "Obtain a rights-cleared rigid-tank sway reference package "
                              "with probe definitions, forcing, native histories and input digests.",
        "revision_history": [{"revision": "1", "date": "2026-10-25",
                              "change": "Synthetic rigid-tank references added to expose physical-evidence gaps."}],
    }


def _view(value: object, field: str = "") -> str:
    if isinstance(value, dict):
        units = value.get("units", {})
        rows = "".join(f"<tr><th>{escape(str(key))}"
                       f'{" [" + escape(units[key]) + "]" if key in units else ""}'
                       f"</th><td>{_view(item, key)}</td></tr>"
                       for key, item in value.items())
        return f"<table><tbody>{rows}</tbody></table>"
    if isinstance(value, list):
        return "<ol>" + "".join(f"<li>{_view(item)}</li>" for item in value) + "</ol>"
    if isinstance(value, float):
        text = (f"{value:.1f}" if {"ratio", "percent"} & set(field.split("_")) else
                f"{value:.3e}" if 0 < abs(value) < 0.001 else f"{value:.3f}")
        return escape(text)
    return escape(str(value))


def _html(fixture: dict, serialized: str) -> str:
    safe_json = serialized.replace("&", "\\u0026").replace("<", "\\u003c").replace(">", "\\u003e")
    counts = fixture["counts"]
    sections = "\n".join(
        f'<section><h2>{escape(key.replace("_", " ").capitalize())}</h2>{_view(value)}'
        f'<p class="caption">{escape(key)}: rounded display; exact records are in the companion JSON.</p></section>'
        for key, value in fixture.items() if key != "revision_history"
    )
    return (
        '<!doctype html>\n<html lang="en">\n<head><meta charset="utf-8">\n'
        '<meta name="viewport" content="width=device-width, initial-scale=1">\n'
        '<title>Rigid-tank sloshing analytical analysis</title>\n'
        '<style>body{font:16px system-ui;max-width:1100px;margin:auto;padding:24px;color:#182432}'
        'table{border-collapse:collapse;width:100%;margin:8px 0}th,td{border:1px solid #bcc8d2;'
        'padding:8px;text-align:left;vertical-align:top;overflow-wrap:anywhere}'
        'th{width:30%}h1,h2{color:#123c59}.caption{font-size:0.9em}li{margin:8px 0}</style></head>\n<body>\n'
        '<h1>Rigid-tank sloshing analytical analysis</h1>\n<section id="summary"><h2>Summary</h2>\n'
        f'<p>{counts["synthetic_reference_points"]} synthetic frequency descriptors and '
        f'{counts["eigenmodes"]} eigenmodes are calculated under analytical-v1; '
        f'{counts["qualified_physical_results"]} qualified physical results are supplied; '
        'physical acceptance is NOT_ESTABLISHED. '
        'Physical validation is BLOCKED because reference samples, probe/boundary equivalence, '
        'calibrated pitch forcing, native evidence and numerical-reuse permission are missing. '
        'The hydrostatic proxy at the near-mode frequency is not a response amplitude.</p>\n</section>\n'
        '<section id="revision-history"><h2>Revision history</h2>\n'
        f'{_view(fixture["revision_history"])}<p class="caption">Revision history: changes and their purpose.</p></section>\n'
        f'{sections}\n<script type="application/json" id="fixture-data">{safe_json}</script>\n'
        '</body>\n</html>\n'
    )


def write_fixture(fixture: dict, out_dir: Path) -> None:
    """Write deterministic JSON and HTML to an explicit directory.

    Args:
        fixture: Records produced by build_fixture.
        out_dir: Caller-selected output directory.
    """
    serialized = json.dumps(fixture, indent=2, ensure_ascii=False, allow_nan=False) + "\n"
    rendered = _html(fixture, serialized)
    out_dir = Path(out_dir)
    out_dir.mkdir(parents=True, exist_ok=True)
    (out_dir / "sloshing-analytical-fixture.json").write_text(serialized, encoding="utf-8", newline="\n")
    (out_dir / "sloshing-analytical-fixture.html").write_text(rendered, encoding="utf-8", newline="\n")


def main(argv: list[str] | None = None) -> int:
    """Parse SI inputs and generate fixtures, reporting invalid input as exit 2.

    Args:
        argv: Optional explicit CLI arguments.

    Returns:
        Zero after both output files have been written.
    """
    parser = argparse.ArgumentParser(description=__doc__)
    for name in UNITS:
        parser.add_argument("--" + name.replace("_", "-"), type=float, required=True,
                            help=f"Synthetic positive input in {UNITS[name]}")
    parser.add_argument("--out-dir", type=Path, required=True)
    args = vars(parser.parse_args(argv))
    out_dir = args.pop("out_dir")
    try:
        fixture = build_fixture({**args, "units": UNITS.copy()})
        write_fixture(fixture, out_dir)
    except (ValueError, OSError) as exc:
        parser.error(str(exc))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
