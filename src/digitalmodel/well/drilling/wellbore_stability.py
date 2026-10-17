# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Elastic wellbore stability screening; every general path is NOT_VALIDATED.

Compression-positive total stresses in psi; effective stress = total - Pp I
(Biot coefficient 1). Circular, impermeable, isotropic, linear-elastic wall;
no thermal or poroelastic correction. See the workflow README for equations,
published example web-page citations, numerical resolution and exclusions.

Run independently of the engine and licensed integrations::

    python -m digitalmodel.well.drilling.wellbore_stability input.yml
"""

from __future__ import annotations

import argparse
import json
import math
from dataclasses import asdict, dataclass, replace
from pathlib import Path

import numpy as np

NOT_VALIDATED = "NOT_VALIDATED"
CRITERIA = ("mohr_coulomb", "mogi_coulomb")
NOT_COVERED = [
    "single-plane-of-weakness and laminated-shale anisotropy",
    "chemical and time-dependent effects",
    "thermal stress, poroelastic diffusion and non-unit Biot coefficient",
    "plastic redistribution, post-failure geometry and breakout growth",
    "natural fractures, fracture propagation and operational loss pressure",
    "ECD, surge/swab, offshore pressure datum and operational margins",
    "field calibration and correlated input uncertainty",
]


@dataclass(frozen=True)
class StabilityInput:
    """Point inputs. Azimuth clockwise from north; inclination from down vertical."""

    sv_psi: float
    shmax_psi: float
    shmin_psi: float
    shmax_azimuth_deg: float
    pore_pressure_psi: float
    ucs_psi: float
    friction_angle_deg: float
    poisson_ratio: float
    inclination_deg: float
    azimuth_deg: float
    tensile_strength_psi: float = 0.0

    def __post_init__(self):
        if not all(math.isfinite(v) for v in asdict(self).values()):
            raise ValueError("inputs must be finite")
        if min(self.sv_psi, self.shmax_psi, self.shmin_psi, self.ucs_psi) <= 0:
            raise ValueError("total principal stresses and UCS must be positive")
        if self.shmax_psi < self.shmin_psi:
            raise ValueError("SHmax must be >= Shmin")
        if self.pore_pressure_psi < 0 or self.tensile_strength_psi < 0:
            raise ValueError("pore pressure and tensile strength must be nonnegative")
        if not 0 <= self.poisson_ratio < 0.5:
            raise ValueError("poisson_ratio must be in [0, 0.5)")
        if not 0 <= self.friction_angle_deg < 90:
            raise ValueError("friction_angle_deg must be in [0, 90)")
        if math.sin(math.radians(self.friction_angle_deg)) == 1.0:
            raise ValueError(
                "friction_angle_deg is numerically singular near 90 degrees"
            )
        if not 0 <= self.inclination_deg <= 180:
            raise ValueError("inclination_deg must be in [0, 180]")


def stress_in_well_axes(case: StabilityInput) -> np.ndarray:
    """Rotate total stresses from North/East/down to right-handed well x/y/z.

    z follows the well, x points to the high side, y is horizontal to the right.
    At vertical, x follows well azimuth. Wall theta increases from x toward y.
    """
    h, a, i = np.radians(
        [case.shmax_azimuth_deg, case.azimuth_deg, case.inclination_deg]
    )
    principal_axes = np.array(
        [[np.cos(h), -np.sin(h), 0], [np.sin(h), np.cos(h), 0], [0, 0, 1]]
    )
    geographic = (
        principal_axes
        @ np.diag([case.shmax_psi, case.shmin_psi, case.sv_psi])
        @ principal_axes.T
    )
    well = np.array(
        [
            [np.cos(i) * np.cos(a), np.cos(i) * np.sin(a), -np.sin(i)],
            [-np.sin(a), np.cos(a), 0],
            [np.sin(i) * np.cos(a), np.sin(i) * np.sin(a), np.cos(i)],
        ]
    )
    return well @ geographic @ well.T


def _wall_terms(case, theta_deg):
    s = stress_in_well_axes(case) - case.pore_pressure_psi * np.eye(3)
    theta = np.radians(theta_deg)
    anisotropy = (s[0, 0] - s[1, 1]) * np.cos(2 * theta) + 2 * s[0, 1] * np.sin(
        2 * theta
    )
    hoop_at_balance = s[0, 0] + s[1, 1] - 2 * anisotropy
    axial = s[2, 2] - 2 * case.poisson_ratio * anisotropy
    shear = 2 * (s[1, 2] * np.cos(theta) - s[0, 2] * np.sin(theta))
    return hoop_at_balance, axial, shear


def _principal(hoop, axial, shear, radial):
    radius = np.hypot((hoop - axial) / 2, shear)
    mean = (hoop + axial) / 2
    radial = np.broadcast_to(radial, np.shape(hoop))
    return np.sort(np.stack([mean + radius, mean - radius, radial], axis=-1), axis=-1)[
        ..., ::-1
    ]


def kirsch_wall(case: StabilityInput, mud_pressure_psi: float, theta_deg) -> dict:
    """Effective radial, hoop, axial, hoop-axial shear and descending principals.

    Surface tractions rr = Pmud - Pp, r-theta = r-z = 0. Hoop-axial shear is
    retained for deviated wells; it changes both wall principal stresses.
    """
    theta = np.atleast_1d(np.asarray(theta_deg, dtype=float))
    if (
        not np.all(np.isfinite(theta))
        or not math.isfinite(mud_pressure_psi)
        or mud_pressure_psi < 0
    ):
        raise ValueError("wall angles must be finite and mud pressure nonnegative")
    hoop0, axial, shear = _wall_terms(case, theta)
    radial = np.full_like(theta, mud_pressure_psi - case.pore_pressure_psi)
    hoop = hoop0 - radial
    return {
        "radial_psi": radial,
        "hoop_psi": hoop,
        "axial_psi": axial,
        "hoop_axial_shear_psi": shear,
        "principal_psi": _principal(hoop, axial, shear, radial),
    }


def failure_margin(case: StabilityInput, principal_psi, criterion="mohr_coulomb"):
    """Strength minus demand, psi; negative means shear failure.

    Mogi-Coulomb uses tau_oct = a + b*(s1+s3)/2 with a,b inferred from
    UCS/friction (conventional triaxial equivalence, no true-triaxial fit).
    """
    if criterion not in CRITERIA:
        raise ValueError(f"criterion must be one of {CRITERIA}")
    p = np.asarray(principal_psi, dtype=float)
    if (
        p.shape[-1:] != (3,)
        or not np.all(np.isfinite(p))
        or np.any(np.diff(p, axis=-1) > 0)
    ):
        raise ValueError("principal stresses must be finite, descending triples")
    s1, s2, s3 = np.moveaxis(p, -1, 0)
    sinphi = math.sin(math.radians(case.friction_angle_deg))
    if criterion == "mohr_coulomb":
        q = (1 + sinphi) / (1 - sinphi)
        return case.ucs_psi + q * s3 - s1
    a = math.sqrt(2) / 3 * case.ucs_psi * (1 - sinphi)
    b = 2 * math.sqrt(2) / 3 * sinphi
    octahedral = np.sqrt((s1 - s2) ** 2 + (s2 - s3) ** 2 + (s3 - s1) ** 2) / 3
    return a + b * (s1 + s3) / 2 - octahedral


def _bisect(function, left, right, right_positive):
    # Return the feasible side of a bracket, with < 1e-6 psi pressure error.
    for _ in range(64):
        if right - left < 1e-6:
            break
        mid = (left + right) / 2
        if (function(mid) >= 0) == right_positive:
            right = mid
        else:
            left = mid
    return right if right_positive else left


def _bound(pressure, status):
    return {
        "pressure_psi": pressure,
        "numerical_status": status,
        "validation_status": NOT_VALIDATED,
    }


def pressure_bounds(
    case: StabilityInput,
    *,
    criterion="mohr_coulomb",
    allowed_breakout_width_deg=0.0,
    angular_samples=360,
    pressure_samples=129,
) -> dict:
    """Find sampled stable intervals between pore and tensile initiation.

    angular_samples covers 180 degrees (wall principal stresses repeat after
    180). Allowed width is aggregate failed angular extent per half-circle,
    equal to one breakout lobe for a vertical well. Limited to 0..60 degrees.
    Collapse is the pressure relieving initial shear failure, floored at Pp.
    A separate upper shear bound prevents reporting recurrent high-pressure
    shear failure as stable, including when it occurs before collapse is
    relieved elsewhere on the wall. Unresolved narrow intervals may be missed; no-pass
    results say NO_FEASIBLE_SAMPLED_PRESSURE rather than proving impossibility.
    """
    if criterion not in CRITERIA:
        raise ValueError(f"criterion must be one of {CRITERIA}")
    if (
        not math.isfinite(allowed_breakout_width_deg)
        or not 0 <= allowed_breakout_width_deg <= 60
    ):
        raise ValueError("allowed breakout width must be in [0, 60] degrees")
    if not isinstance(angular_samples, int) or angular_samples < 36:
        raise ValueError("angular_samples must be an integer >= 36")
    if not isinstance(pressure_samples, int) or pressure_samples < 17:
        raise ValueError("pressure_samples must be an integer >= 17")
    theta = np.arange(angular_samples) * 180 / angular_samples
    hoop0, axial, shear = _wall_terms(case, theta)
    ts = case.tensile_strength_psi
    # At tensile initiation det([[hoop+Ts, shear],[shear, axial+Ts]])=0.
    # Axial compression must remain above -Ts for this Schur complement.
    if np.any(axial + ts <= 0):
        fracture = 0.0
    else:
        fracture = max(
            0.0,
            float(
                np.min(case.pore_pressure_psi + hoop0 + ts - shear**2 / (axial + ts))
            ),
        )
    frac_status = "ALREADY_TENSILE_AT_ZERO" if fracture == 0 else "ROOT_ON_ANGULAR_GRID"
    allowed_nodes = int(
        math.floor(allowed_breakout_width_deg / 180 * angular_samples + 1e-12)
    )

    def wall_margins(pressure):
        dp = np.asarray(pressure)[..., None] - case.pore_pressure_psi
        principal = _principal(hoop0 - dp, axial, shear, dp)
        return failure_margin(case, principal, criterion)

    def margin(pressure):
        return np.partition(wall_margins(pressure), allowed_nodes, axis=-1)[
            ..., allowed_nodes
        ]

    def local_roots(left, right, right_positive):
        # One independent root per wall angle, refined together. Entries which
        # never bracket a root are masked by the caller.
        for _ in range(48):
            if np.max(right - left) < 1e-6:
                break
            mid = (left + right) / 2
            dp = mid - case.pore_pressure_psi
            positive = (
                failure_margin(
                    case, _principal(hoop0 - dp, axial, shear, dp), criterion
                )
                >= 0
            )
            move_right = positive == right_positive
            right = np.where(move_right, mid, right)
            left = np.where(move_right, left, mid)
        return right if right_positive else left

    intervals = []
    has_recurrence = False
    collapse, shear_ceiling = None, None
    if fracture > case.pore_pressure_psi:
        grid = np.linspace(case.pore_pressure_psi, fracture, pressure_samples)
        local_stable = wall_margins(grid) >= 0
        first = np.argmax(local_stable, axis=0)
        has_entry = np.any(local_stable, axis=0)
        entries = local_roots(grid[np.maximum(first - 1, 0)], grid[first], True)
        entries = np.where(has_entry, entries, np.inf)
        candidate = float(np.sort(entries)[angular_samples - 1 - allowed_nodes])
        collapse = candidate if math.isfinite(candidate) else None
        exits = local_stable[:-1] & ~local_stable[1:]
        has_recurrence = bool(np.any(exits))
        first_exit = np.argmax(exits, axis=0)
        exit_roots = local_roots(grid[first_exit], grid[first_exit + 1], False)
        # Conservatively permit no recurrent shear, even for nonzero initial
        # breakout allowance. This ceiling is additional to tensile initiation.
        shear_ceiling = float(
            np.min(np.where(np.any(exits, axis=0), exit_roots, fracture))
        )
        stable = (
            np.partition(wall_margins(grid), allowed_nodes, axis=-1)[:, allowed_nodes]
            >= 0
        )
        start = float(grid[0]) if stable[0] else None
        for j in range(1, pressure_samples):
            if stable[j] and not stable[j - 1]:
                start = _bisect(margin, float(grid[j - 1]), float(grid[j]), True)
            elif stable[j - 1] and not stable[j]:
                end = _bisect(margin, float(grid[j - 1]), float(grid[j]), False)
                intervals.append([start, end])
                start = None
        if start is not None:
            intervals.append([start, fracture])
    status = "NO_FEASIBLE_SAMPLED_PRESSURE"
    if collapse is not None:
        status = (
            "FLOOR_AT_PORE_PRESSURE"
            if collapse == case.pore_pressure_psi
            else "ROOT_ON_ANGULAR_GRID"
        )
    elif fracture <= case.pore_pressure_psi:
        status = "FRACTURE_AT_OR_BELOW_PORE_PRESSURE"
    ceiling = (
        min(fracture, case.shmin_psi, shear_ceiling)
        if shear_ceiling is not None
        else None
    )
    upper_status = status
    if shear_ceiling is not None:
        upper_status = (
            "FIRST_LOCAL_SHEAR_RECURRENCE"
            if has_recurrence
            else "NO_SHEAR_RECURRENCE_BEFORE_TENSILE_CEILING"
        )
    return {
        "validation_status": NOT_VALIDATED,
        "criterion": criterion,
        "collapse": _bound(collapse, status),
        "fracture_initiation": _bound(fracture, frac_status),
        "shear_upper": _bound(shear_ceiling, upper_status),
        "stable_pressure_intervals_psi": intervals,
        "conservative_ceiling_psi": ceiling,
        "feasible": collapse is not None and ceiling is not None and ceiling > collapse,
        "resolution": {
            "angular_step_deg": 180 / angular_samples,
            "pressure_samples": pressure_samples,
            "pressure_search_range_psi": [case.pore_pressure_psi, fracture],
            "root_tolerance_psi": 1e-6,
            "allowed_breakout_width_deg": allowed_breakout_width_deg,
        },
    }


def _band(values, tvd):
    # Never condition on successful draws: unresolved draws invalidate bands.
    quantiles = (
        [None] * 3
        if any(v is None for v in values)
        else np.quantile(values, [0.1, 0.5, 0.9]).tolist()
    )
    result = {"validation_status": NOT_VALIDATED}
    for name, value in zip(("p10", "p50", "p90"), quantiles):
        result[name + "_psi"] = value
        result[name + "_emw_ppg"] = None if value is None else value / (0.052 * tvd)
    return result


def run_workflow(config: dict, output_dir: str | Path) -> dict:
    """Write summary/window/polar JSON from a profile and seeded input ensemble.

    Sigma values are independent normal standard deviations in the input's own
    units. The same standardized draws are reused at every depth; all curves
    within a draw use the same input realization. Invalid draws raise, without
    clipping, rejection sampling or silently changing the prior distribution.
    """
    ensemble = config.get("ensemble", {})
    n = ensemble.get("samples", 128)
    if not isinstance(n, int) or n < 1:
        raise ValueError("ensemble samples must be an integer >= 1")
    sigma = ensemble.get("sigma", {})
    fields = set(StabilityInput.__dataclass_fields__)
    if set(sigma) - fields or any(
        not math.isfinite(s) or s < 0 for s in sigma.values()
    ):
        raise ValueError(
            "sigma must name valid input fields with finite nonnegative standard deviations"
        )
    rng = np.random.default_rng(ensemble.get("seed", 2669))
    draw_fields = sorted(sigma)
    standardized = rng.standard_normal((n, len(draw_fields)))
    controls = config.get("screening", {})
    profile = config["profile"]
    if not profile:
        raise ValueError("profile must contain at least one depth")
    window = []
    nominal_cases = []
    for point in profile:
        tvd = point["tvd_ft"]
        if not math.isfinite(tvd) or tvd <= 0:
            raise ValueError("tvd_ft must be finite and positive")
        case = StabilityInput(
            **(
                config.get("defaults", {})
                | {k: v for k, v in point.items() if k != "tvd_ft"}
            )
        )
        nominal_cases.append(case)
        draws, bounds = [], []
        for z in standardized:
            draw = replace(
                case,
                **{
                    key: getattr(case, key) + sigma[key] * value
                    for key, value in zip(draw_fields, z)
                },
            )
            draws.append(draw)
            bounds.append(pressure_bounds(draw, **controls))
        curves = {
            "pore": _band([c.pore_pressure_psi for c in draws], tvd),
            "collapse": _band([b["collapse"]["pressure_psi"] for b in bounds], tvd),
            "fracture_initiation": _band(
                [b["fracture_initiation"]["pressure_psi"] for b in bounds], tvd
            ),
            "minimum_horizontal_stress": _band([c.shmin_psi for c in draws], tvd),
        }
        failed = sum(b["collapse"]["pressure_psi"] is None for b in bounds)
        low = (
            None
            if failed
            else max(curves["pore"]["p90_psi"], curves["collapse"]["p90_psi"])
        )
        # Include the first shear interval's upper bound as well as the four
        # requested curves. Joint draw ceilings preserve competing upper limits.
        upper = _band([b["conservative_ceiling_psi"] for b in bounds], tvd)
        high = upper["p10_psi"]
        window.append(
            {
                "validation_status": NOT_VALIDATED,
                "tvd_ft": tvd,
                "inputs": asdict(case),
                "nominal": pressure_bounds(case, **controls),
                "curves": curves,
                "failed_draws": failed,
                "infeasible_draws": sum(not b["feasible"] for b in bounds),
                "conservative_window": {
                    "validation_status": NOT_VALIDATED,
                    "low_psi": low,
                    "high_psi": high,
                    "low_emw_ppg": None if low is None else low / (0.052 * tvd),
                    "high_emw_ppg": None if high is None else high / (0.052 * tvd),
                    "feasible": low is not None and high is not None and high > low,
                },
            }
        )
    polar_cfg = config.get("polar", {})
    index = polar_cfg.get("profile_index", 0)
    if not isinstance(index, int) or not 0 <= index < len(profile):
        raise ValueError("polar profile_index must identify a profile row")
    polar = []
    for inc in polar_cfg.get("inclinations_deg", [0, 30, 60, 90]):
        if not math.isfinite(inc) or not 0 <= inc < 180:
            raise ValueError("polar inclinations must be finite and in [0, 180)")
        for azi in polar_cfg.get("azimuths_deg", list(range(0, 360, 30))):
            case = replace(nominal_cases[index], inclination_deg=inc, azimuth_deg=azi)
            bound = pressure_bounds(case, **controls)
            polar.append(
                {
                    "validation_status": NOT_VALIDATED,
                    "inclination_deg": inc,
                    "azimuth_deg": azi,
                    "radius": math.tan(math.radians(inc) / 2),
                    "tvd_ft": profile[index]["tvd_ft"],
                    "bounds": bound,
                }
            )
    result = {
        "schema_version": 1,
        "screening_only": True,
        "validation_status": NOT_VALIDATED,
        "validation_scope": "Two vertical Mohr-Coulomb textbook checks are in tests; no general path is qualified.",
        "model": {
            "validation_status": NOT_VALIDATED,
            "stress_sign": "compression positive",
            "units": {"pressure": "psi", "tvd": "ft", "emw": "ppg", "angle": "deg"},
            "pressure_datum": "surface gauge, onshore single fluid column",
            "geographical_axes": "North/East/down; vertical is a principal stress",
            "well_axes": "right handed; x high side, z along trajectory",
            "effective_stress": "total stress minus pore pressure times identity",
            "biot_coefficient": 1.0,
            "wall": "circular, impermeable, isotropic, linear elastic",
            "tensile_test": "least hoop/axial eigenvalue; excludes radial traction",
            "mogi_parameters": "inferred conventional triaxial equivalence, uncalibrated",
            "breakout_width": "aggregate failed extent per half circle, limited to 0..60 deg",
            "emw_conversion": "pressure_psi / (0.052 * tvd_ft)",
        },
        "not_covered": NOT_COVERED,
        "uncertainty": {
            "validation_status": NOT_VALIDATED,
            "samples": n,
            "seed": ensemble.get("seed", 2669),
            "sigma": sigma,
            "method": "empirical normal-input Monte Carlo; P10 low / P50 median / P90 high",
            "dependence": "independent fields; identical standardized draws across depth",
            "failed_draw_policy": "null bands if any bound is unresolved; invalid physical draws raise",
        },
        "polar_projection": "equal-angle stereographic: radius=tan(inclination/2), azimuth clockwise from north; nominal only",
        "window": window,
        "polar": polar,
    }
    # Serialize strictly before writing any artifact.
    summary = json.dumps(result, indent=2, allow_nan=False) + "\n"
    directory = Path(output_dir)
    directory.mkdir(parents=True, exist_ok=True)
    (directory / "summary.json").write_text(summary, encoding="utf-8")
    for name, data in (("window", window), ("polar", polar)):
        (directory / f"{name}.json").write_text(
            json.dumps(data, indent=2, allow_nan=False) + "\n", encoding="utf-8"
        )
    return result


def main():
    """Run the standalone screening workflow from a YAML input file."""
    import yaml

    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("input", type=Path)
    parser.add_argument(
        "--output-dir", type=Path, default=Path("results/wellbore-stability")
    )
    args = parser.parse_args()
    run_workflow(
        yaml.safe_load(args.input.read_text(encoding="utf-8")), args.output_dir
    )
    print("Screening outputs written; NOT_VALIDATED")


if __name__ == "__main__":
    main()
