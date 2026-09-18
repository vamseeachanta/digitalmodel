"""Deterministic assumed vessel grids; no API 579 acceptance calculation."""
from __future__ import annotations

import csv
import hashlib
import io
import json
import math
import re
from dataclasses import asdict, dataclass
from pathlib import Path

NOMINAL_MM = 16.0
UNCERTAINTY_MM = 0.2
FUTURE_LOSS_MM = 0.5
RADIUS_MM = 1000.0


@dataclass(frozen=True, kw_only=True)
class Area:
    area_id: str
    centre_x_mm: float
    theta_deg: float
    axial_extent_mm: float
    circumferential_extent_mm: float
    minimum_mm: float
    plateau_fraction: float = 0.0


AREAS = (
    Area(area_id="A", centre_x_mm=1100, theta_deg=90, axial_extent_mm=200,
         circumferential_extent_mm=160, minimum_mm=14),
    Area(area_id="B", centre_x_mm=2200, theta_deg=270, axial_extent_mm=400,
         circumferential_extent_mm=240, minimum_mm=8),
    Area(area_id="C", centre_x_mm=3600, theta_deg=90, axial_extent_mm=350,
         circumferential_extent_mm=300, minimum_mm=6.5, plateau_fraction=0.6),
    Area(area_id="D", centre_x_mm=4600, theta_deg=270, axial_extent_mm=800,
         circumferential_extent_mm=700, minimum_mm=5, plateau_fraction=0.6),
)


def validate(area: Area, pitch: float) -> None:
    values = [v for v in asdict(area).values() if not isinstance(v, str)] + [pitch]
    if not all(math.isfinite(v) for v in values):
        raise ValueError("All geometric values must be finite")
    if area.area_id not in "ABCD" or len(area.area_id) != 1:
        raise ValueError("Area ID must be A, B, C or D")
    if min(pitch, area.axial_extent_mm, area.circumferential_extent_mm) <= 0:
        raise ValueError("Pitch and extents must be positive")
    if not 0.7 < area.minimum_mm <= NOMINAL_MM:
        raise ValueError("Assessed thickness must be positive and below nominal")
    if not 0 <= area.plateau_fraction < 1 or not 0 <= area.theta_deg < 360:
        raise ValueError("Invalid plateau fraction or azimuth")
    half = area.axial_extent_mm / 2
    if not half <= area.centre_x_mm <= 6000 - half:
        raise ValueError("Patch must lie inside the tangent-to-tangent shell")


def _shape(coordinate: float, half_extent: float, plateau: float) -> float:
    fraction = abs(coordinate) / half_extent
    if fraction >= 1:
        return 0.0
    if fraction <= plateau:
        return 1.0
    return (1 + math.cos(math.pi * (fraction - plateau) / (1 - plateau))) / 2


def thickness(area: Area, x: float, s: float) -> float:
    """Example current thickness, with compact C1 transitions."""
    a, b = area.axial_extent_mm / 2, area.circumferential_extent_mm / 2
    broad = _shape(x, a, area.plateau_fraction) * _shape(s, b, area.plateau_fraction)
    depth = NOMINAL_MM - area.minimum_mm
    if area.area_id == "B":
        narrow = _shape(x, a / 4, 0) * _shape(s, b / 4, 0)
        return NOMINAL_MM - depth * (0.25 * broad + 0.75 * narrow)
    return NOMINAL_MM - depth * broad


def _axis(extent: float, target_pitch: float) -> tuple[list[float], float]:
    count = math.ceil(extent / 2 / target_pitch)
    spacing = extent / 2 / count
    outer = count + math.ceil(50 / spacing)
    if outer > 2000:
        raise ValueError("Requested sampling exceeds example resource limit")
    return [i * spacing for i in range(-outer, outer + 1)], spacing


def sample(area: Area, pitch: float) -> dict:
    validate(area, pitch)
    xs, dx = _axis(area.axial_extent_mm, pitch)
    ss, ds = _axis(area.circumferential_extent_mm, pitch)
    if len(xs) * len(ss) > 250000:
        raise ValueError("Requested grid exceeds example resource limit")
    rows = []
    for x in xs:
        for s in ss:
            current = thickness(area, x, s)
            rows.append(dict(
                example_data=True, area_id=area.area_id, local_x_mm=x,
                local_s_mm=s, global_x_mm=area.centre_x_mm + x,
                theta_deg=(area.theta_deg + math.degrees(s / RADIUS_MM)) % 360,
                current_mm=current, uncertainty_mm=UNCERTAINTY_MM,
                future_loss_mm=FUTURE_LOSS_MM,
                assessed_mm=current - UNCERTAINTY_MM - FUTURE_LOSS_MM,
            ))
    return dict(x_mm=xs, s_mm=ss, actual_pitch_x_mm=dx,
                actual_pitch_s_mm=ds, target_pitch_mm=pitch, rows=rows)


def critical_profiles(rows: list[dict]) -> dict:
    result = {}
    for name, coordinate in (("axial", "local_x_mm"), ("circumferential", "local_s_mm")):
        profile = {}
        for row in rows:
            at, wall = row[coordinate], row["assessed_mm"]
            profile[at] = min(wall, profile.get(at, wall))
        result[name] = [[at, wall] for at, wall in sorted(profile.items())]
    return result


def _integral(points: list[list[float]]) -> float:
    return sum((b[0] - a[0]) * (a[1] + b[1]) / 2 for a, b in zip(points, points[1:]))


def analytic_volume(area: Area) -> float:
    """Closed-form developed-surface loss integral at bore radius, not physical volume."""
    coefficient = (1 + area.plateau_fraction) ** 2
    if area.area_id == "B":
        coefficient = 0.25 * coefficient + 0.75 / 16
    return ((NOMINAL_MM - area.minimum_mm) * area.axial_extent_mm
            * area.circumferential_extent_mm * coefficient / 4)


def _metrics(area: Area, pitch: float) -> dict:
    grid = sample(area, pitch)
    profiles = critical_profiles(grid["rows"])
    metrics = {"pitch_mm": pitch, "points": len(grid["rows"])}
    for name, profile in profiles.items():
        loss = [[at, NOMINAL_MM - UNCERTAINTY_MM - FUTURE_LOSS_MM - t] for at, t in profile]
        metrics[name + "_loss_area_mm2"] = _integral(loss)
    by_x = {}
    for row in grid["rows"]:
        by_x.setdefault(row["local_x_mm"], []).append(
            [row["local_s_mm"], NOMINAL_MM - row["current_mm"]])
    volume = _integral([[x, _integral(sorted(cells))] for x, cells in sorted(by_x.items())])
    metrics["developed_loss_mm3"] = volume
    exact = analytic_volume(area)
    metrics["analytic_developed_loss_mm3"] = exact
    metrics["developed_loss_error_fraction"] = abs(volume - exact) / exact if exact else 0
    return metrics


def sampling_study(area: Area, pitches=(25, 12.5, 6.25)) -> dict:
    if len(pitches) != 3 or not pitches[0] > pitches[1] > pitches[2] > 0:
        raise ValueError("Three decreasing positive pitches are required")
    resolutions = [_metrics(area, p) for p in pitches]
    metrics = ("axial_loss_area_mm2", "circumferential_loss_area_mm2", "developed_loss_mm3")
    prev, fine = resolutions[-2:]
    changes = {k: abs(fine[k] - prev[k]) / abs(fine[k]) if fine[k] else 0 for k in metrics}
    meets = max(*changes.values(), fine["developed_loss_error_fraction"]) <= 0.01
    return dict(resolutions=resolutions, final_relative_changes=changes,
                criterion_fraction=0.01, status="SAMPLING CRITERION MET" if meets else "PROVISIONAL",
                scope="Developed-surface integrals at bore radius; not physical volume or FE convergence",
                fea_mesh_convergence="NOT EVALUATED")


def _precision(value):
    if isinstance(value, float):
        if not math.isfinite(value):
            raise ValueError("Nonfinite value cannot be serialized")
        rounded = round(value, 6)
        return rounded if rounded else 0.0
    if isinstance(value, dict):
        return {k: _precision(v) for k, v in value.items()}
    if isinstance(value, (list, tuple)):
        return [_precision(v) for v in value]
    return value


def json_bytes(value) -> bytes:
    return (json.dumps(_precision(value), sort_keys=True, indent=2, allow_nan=False) + "\n").encode("utf-8")


def _csv_bytes(rows: list[dict]) -> bytes:
    stream = io.StringIO(newline="")
    writer = csv.DictWriter(stream, fieldnames=list(rows[0]), lineterminator="\n")
    writer.writeheader()
    for row in rows:
        writer.writerow({k: f"{v:.6f}" if isinstance(v, float) else v for k, v in row.items()})
    return stream.getvalue().encode("utf-8")


def _material_basis() -> dict:
    return dict(material_id="example-carbon-steel-100c", source_type="user-authorized example assumption",
                code_qualified=False, specification="generic carbon steel; no grade qualification",
                property_temperature_c=100, elastic_modulus_mpa=195000.0, poisson_ratio=0.30,
                yield_mpa=240.0, tensile_mpa=450.0, screening_stress_mpa=120.0,
                screening_stress_use="independently assumed; not a code-qualified allowable",
                tensile_use="information only; not a constitutive point or failure criterion",
                density_kg_m3=7850.0, expansion_per_k=0.000012,
                expansion_range_c=[20, 100], stress_free_temperature_c=20,
                homogeneity="homogeneous isotropic; weld and HAZ properties not differentiated",
                constitutive_model="von Mises elastic-perfectly-plastic; associated flow; no hardening",
                constitutive_use="preliminary isothermal response and limit-load exploration only",
                qualification_limits=["no temperature interpolation for strength or modulus",
                                      "thermal restraints not established",
                                      "no local-failure strain, toughness, fracture or fatigue data",
                                      "no code-qualified material or asset acceptance"])


def _basis(areas, version="v1") -> dict:
    return dict(example_data=True, dataset_id="pressure-vessel-four-area-example", version=version,
                material=_material_basis(),
                calibration_status="PROVISIONAL; target assessment routes not established",
                nominal_mm=NOMINAL_MM, uncertainty_mm=UNCERTAINTY_MM, future_loss_mm=FUTURE_LOSS_MM,
                inside_radius_mm=RADIUS_MM, shell_tangent_length_mm=6000,
                head_type="2:1 ellipsoidal", target_pressure_mpa_g=1.5,
                future_horizon_years=5, future_corrosion_rate_mm_per_year=FUTURE_LOSS_MM / 5,
                reduced_pressure_trial_mpa_g=0.8, assessment_temperature_c=100,
                theta_convention="crown=0; clockwise looking along +x; bore radius used for arc conversion",
                source="original assumed example, not client inspection data",
                non_interaction="user-imposed: areas far apart, independent",
                thickness_lineage="assessed=current-uncertainty-future_loss; both CTPs use assessed",
                profiles_scope="min-envelope across the orthogonal coordinate; no spatial sorting",
                supplemental_loads="NOT EVALUATED", areas=[asdict(a) for a in areas])


def _payloads(areas, version="v1") -> dict[str, bytes]:
    from .example_vessel_report import render_report
    grids = {a.area_id: sample(a, 6.25) for a in areas}
    studies = {a.area_id: sampling_study(a) for a in areas}
    basis = _basis(areas, version)
    payloads = {"assumptions.json": json_bytes(basis), "sampling.json": json_bytes(studies)}
    for area in areas:
        grid = grids[area.area_id]
        stem = "area-" + area.area_id.lower()
        payloads[stem + "-grid.csv"] = _csv_bytes(grid["rows"])
        payloads[stem + "-profiles.json"] = json_bytes(dict(
            example_data=True, area_id=area.area_id, units={"coordinate": "mm", "thickness": "mm"},
            profiles=critical_profiles(grid["rows"]), target_pitch_mm=6.25,
            actual_pitch_x_mm=grid["actual_pitch_x_mm"], actual_pitch_s_mm=grid["actual_pitch_s_mm"]))
    payloads["report.html"] = render_report(basis, grids, studies).encode("utf-8")
    return payloads


def _check_footprints(areas) -> None:
    """Reject geometric overlap; this does not establish code non-interaction."""
    for i, first in enumerate(areas):
        validate(first, 6.25)
        for second in areas[i + 1:]:
            axial_gap = abs(first.centre_x_mm - second.centre_x_mm)
            angle = abs((first.theta_deg - second.theta_deg + 180) % 360 - 180)
            arc_gap = math.radians(angle) * RADIUS_MM
            if (axial_gap < (first.axial_extent_mm + second.axial_extent_mm) / 2
                    and arc_gap < (first.circumferential_extent_mm + second.circumferential_extent_mm) / 2):
                raise ValueError("Example footprints overlap; independence cannot be imposed")


def write_package(parent: Path, name: str, *, areas=AREAS, version="v1") -> Path:
    """Write a new explicit output directory, never overwrite an existing one."""
    if not re.fullmatch(r"[a-z0-9][a-z0-9-]*", name):
        raise ValueError("Output name must be one lowercase path segment")
    if not isinstance(version, str) or not re.fullmatch(r"v[1-9][0-9]*", version):
        raise ValueError("Dataset version must have the form v1, v2, ...")
    parent = Path(parent).absolute()
    for ancestor in (parent, *parent.parents):
        if ancestor.is_symlink() or getattr(ancestor, "is_junction", lambda: False)():
            raise ValueError("Output ancestry cannot contain links or junctions")
    parent = parent.resolve(strict=True)
    target = parent / name
    if target.exists():
        raise FileExistsError(target)
    if len(areas) != 4 or {a.area_id for a in areas} != set("ABCD"):
        raise ValueError("Exactly four unique areas A through D required")
    _check_footprints(areas)
    payloads = _payloads(areas, version)
    sources = [Path(__file__), Path(__file__).with_name("example_vessel_report.py")]
    manifest = dict(example_data=True, dataset_id="pressure-vessel-four-area-example", version=version,
                    files={n: hashlib.sha256(b).hexdigest() for n, b in sorted(payloads.items())},
                    generator_sha256={p.name: hashlib.sha256(p.read_bytes()).hexdigest() for p in sources},
                    assumptions_sha256=hashlib.sha256(payloads["assumptions.json"]).hexdigest(),
                    numeric_precision_decimal_places=6, encoding="utf-8", newlines="LF",
                    qualification="example geometry only; all code/FEA/repair NOT EVALUATED")
    payloads["manifest.json"] = json_bytes(manifest)
    target.mkdir()  # exclusive: an existing/racing destination is never reused
    for name, content in sorted(payloads.items()):
        with (target / name).open("xb") as stream:
            stream.write(content)
    return target
