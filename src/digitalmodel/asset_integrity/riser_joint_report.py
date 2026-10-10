"""Report-only composition of the existing riser-joint engines (issue 2183)."""

from __future__ import annotations

import hashlib
import html
import io
import json
from dataclasses import asdict
from pathlib import Path

import numpy as np
import pandas as pd

from .assessment.ffs_report import FFSReport
from .corroded_pipe import _B31G_MAX_DT
from .dnv_rp_f101 import _DNV_F101_MAX_DT
from .riser_joint_ffs import (
    COLLAPSE_DESIGN_FACTOR,
    DEFAULT_WELD_CVN_JOULES,
    DEPTH_CAP_WELD,
    MM_PER_IN,
    SEAWATER_PSI_PER_FT,
    ZONE_FATIGUE_MARGIN,
    fleet_rollup,
    level1_flaw_envelope,
    place_joint,
)

RECORDS = (
    "docs/domains/asset-integrity/b31g-validation-2026-06-27.md",
    "docs/domains/asset-integrity/ffs-validation-record-2026-06-27.md",
    "tests/asset_integrity/test_crack_fad.py",
    "docs/domains/asset-integrity/riser-joint-ffs-validation-2026-10-10.md",
)
LIMITS = (
    "Screening composition only; no independent qualification of the measured fleet. "
    "Base-metal envelopes use nominal wall and idealized longitudinal metal loss, "
    "not pit morphology extracted from the grids. Weld envelopes assume Barlow hoop "
    "membrane stress, zero bending and the engine's default Charpy correlation; "
    "measured weld toughness and fatigue flaw growth are not established. "
    "Placement uses measured minimum wall at the time of inspection and register "
    "minimum Main life across all scans of each joint with practice zone margins; "
    "it is not a fatigue-life calculation. Envelopes use the matching scan's rate. "
    "Unmeasured cells are excluded, so minimum wall is limited to measured stations. "
    "Axially downsampled grids do not establish full-resolution minima; the joint "
    "governing collapse result is the lowest limit among supplied grids only. "
    "Collapse uses the specified differential-head fraction. Fleet roll-up is life-based, not a fleet-wide collapse assessment."
)


def _read_source(raw_path, base, sources):
    path = (base / raw_path).resolve()
    data = path.read_bytes()
    repo = Path(__file__).resolve().parents[3]
    label = (
        path.relative_to(repo).as_posix() if path.is_relative_to(repo) else path.name
    )
    sources.append({"path": label, "sha256": hashlib.sha256(data).hexdigest()})
    return data


def _read_grid(raw_path, base, sources):
    data = _read_source(raw_path, base, sources)
    try:
        grid = pd.read_csv(io.BytesIO(data), header=None).astype(float)
    except (ValueError, pd.errors.EmptyDataError) as exc:
        raise ValueError("grid must contain numeric wall thickness in mm") from exc
    values = grid.to_numpy()
    measured = values[~np.isnan(values)]
    if not measured.size or not np.isfinite(measured).all() or (measured <= 0).any():
        raise ValueError("grid requires finite positive measured wall thickness")
    return grid / MM_PER_IN


def _envelopes(spec, rate):
    basis = {k: spec[k] for k in ("od_in", "wt_in", "grade", "design_pressure_psi")}
    envelopes = {}
    for method in ("b31g", "modified_b31g", "dnv_f101", "bs7910_option1_fad"):
        args = (
            {"region": "weld"} if method == "bs7910_option1_fad" else {"method": method}
        )
        envelopes[method] = {
            "start": level1_flaw_envelope(**basis, **args),
            "campaign_end": level1_flaw_envelope(
                **basis,
                **args,
                corrosion_rate_in_per_yr=rate,
                campaign_years=spec["campaign_years"],
            ),
        }
    return envelopes


def _scan_result(scan, spec, grid, register):
    rows = register[
        (register["component"] == "Main")
        & (register["joint_id"] == scan["joint_id"])
        & (register["scan_location"] == scan["scan_location"])
    ]
    if len(rows) != 1:
        raise ValueError("scan requires exactly one matching Main register row")
    row = rows.iloc[0]
    rate = float(row["corrosion_rate_mm_per_year"]) / MM_PER_IN
    scan_life = float(row["min_life_years"])
    life = float(
        register[
            (register["component"] == "Main")
            & (register["joint_id"] == scan["joint_id"])
        ]["min_life_years"].min()
    )
    if not np.isfinite([rate, scan_life]).all() or rate < 0 or scan_life < 0:
        raise ValueError(
            "register life and corrosion rate must be finite and nonnegative"
        )
    envelopes = _envelopes(spec, rate)
    minimum = float(grid.min().min())
    placement = asdict(
        place_joint(
            scan["joint_id"],
            life,
            minimum,
            **{
                k: spec[k]
                for k in ("od_in", "grade", "campaign_water_depth_ft", "campaign_years")
            },
            differential_head_fraction=spec["differential_head_fraction"],
        )
    )
    placement["scan"] = Path(scan["grid_csv"]).name
    return {
        "joint_id": scan["joint_id"],
        "scan": placement["scan"],
        "measured_min_wt_in": minimum,
        "scan_register_life_years": scan_life,
        "joint_governing_life_years": life,
        "unmeasured_cells": int(grid.isna().sum().sum()),
        "envelopes": envelopes,
    }, placement


def _table(headers, rows):
    head = "".join(f"<th>{html.escape(str(x))}</th>" for x in headers)
    body = "".join(
        "<tr>" + "".join(f"<td>{html.escape(str(x))}</td>" for x in row) + "</tr>"
        for row in rows
    )
    return f"<table><thead><tr>{head}</tr></thead><tbody>{body}</tbody></table>"


def _reported_lengths(envelope):
    limits = dict.fromkeys(("b31g", "modified_b31g"), _B31G_MAX_DT)
    limits["dnv_f101"] = _DNV_F101_MAX_DT
    limit = limits.get(envelope["method"], envelope["depth_cap_frac"])
    growth = envelope["corrosion_rate_in_per_yr"] * envelope["campaign_years"]
    growth /= envelope["wt_in"]
    for fraction, length in zip(
        envelope["depth_frac"], envelope["max_acceptable_length_in"]
    ):
        outside = fraction + growth > limit + 1e-12  # construction roundoff only
        yield "Not evaluated: OUT OF APPLICABILITY" if outside else length


def _envelope_sections(result):
    sections = []
    for scan in result["scans"]:
        sections += [
            f"<h2>Acceptance envelopes: {html.escape(scan['scan'])}</h2>",
            f"<p>Measured minimum wall: {scan['measured_min_wt_in']:.3f} in; "
            f"unmeasured cells: {scan['unmeasured_cells']}.</p>",
        ]
        for method, pair in scan["envelopes"].items():
            start, end = pair["start"], pair["campaign_end"]
            sections += [
                f"<h3>{html.escape(method)}</h3>",
                f"<p>Depth limits: B31G/Modified B31G d/t ≤ {_B31G_MAX_DT:.2f}; "
                f"DNV-RP-F101 ≤ {_DNV_F101_MAX_DT:.2f}; weld practice cap ≤ {DEPTH_CAP_WELD:.2f}. "
                "Campaign-end depth includes growth. Out-of-limit lengths are not evaluated for report acceptance; unchanged raw JSON lengths are unqualified.</p>",
                _table(
                    [
                        "Depth (in)",
                        "Start max length (in)",
                        "Campaign-end max length (in)",
                    ],
                    zip(
                        start["depth_in"],
                        _reported_lengths(start),
                        _reported_lengths(end),
                    ),
                ),
            ]
    return sections


def _placement_sections(placements, title):
    return [
        f"<h2>{title}</h2>",
        _table(
            [
                "Scan",
                "Life (yr)",
                "Depth limit (ft)",
                "Zones",
                "Verdict",
                "Criterion",
            ],
            [
                (
                    p["scan"],
                    p["min_life_years"],
                    p["acceptable_depth_ft"],
                    ", ".join(p["eligible_zones"]),
                    p["verdict"],
                    p["reason"] or "Life covers zone margin; depth covers campaign",
                )
                for p in placements
            ],
        ),
    ]


def _rollup_sections(result):
    return [
        "<h2>Fleet roll-up</h2>",
        f"<p>{result['rollup']['n_joints']} unique Main joints. "
        "Two RJ-101 grids are separate scan assessments, not additional inventory.</p>",
        _table(
            [
                "Campaign",
                "Horizon (yr)",
                "Fit",
                "Repair",
                "High fatigue",
                "Shortfall",
            ],
            [
                (
                    c["campaign"],
                    c["horizon_years"],
                    c["fit_joints"],
                    c["repair_joints"],
                    c["high_fatigue_qualified"],
                    c["high_fatigue_shortfall"],
                )
                for c in result["rollup"]["campaigns"]
            ],
        ),
        "<h2>Validation references</h2>",
    ]


class RiserJointReport(FFSReport):
    """Reuse the FFS report shell without its API-579 metal-loss conclusions."""

    @staticmethod
    def generate(result, spec):
        sections = [
            FFSReport._html_head(
                "Riser fleet", str(result["provenance"]["inspection_epoch"])
            ),
            "<h2>Basis and limitations</h2>",
            f"<p>{LIMITS}</p>",
            _table(["Input", "Value"], spec.items()),
            "<h2>Provenance</h2>",
            "<p>Anonymization statement supplied by the example: "
            + html.escape(str(result["provenance"]["anonymization_statement"]))
            + "</p>",
            _table(
                ["Source", "SHA-256"],
                [(x["path"], x["sha256"]) for x in result["provenance"]["sources"]],
            ),
        ]
        sections += _envelope_sections(result)
        sections += _placement_sections(
            result["joint_governing"], "Joint governing placement"
        )
        sections += _placement_sections(
            result["placements"],
            "String placement and collapse-limited water depth by scan",
        )
        sections += _rollup_sections(result)
        # Repository URLs remain valid when the report is saved outside this checkout.
        sections += [
            f'<p><a href="https://github.com/vamseeachanta/digitalmodel/blob/main/{p}">{p}</a></p>'
            for p in RECORDS
        ]
        return "\n".join(sections) + "\n</body></html>"


def _calculate(spec, base):
    sources = []
    register = pd.read_csv(
        io.BytesIO(_read_source(spec["register_csv"], base, sources))
    )
    _validate_register(register)
    _read_source(spec["provenance_readme"], base, sources)
    scans, placements = [], []
    for scan in spec["scans"]:
        grid = _read_grid(scan["grid_csv"], base, sources)
        result, placement = _scan_result(scan, spec, grid, register)
        scans.append(result)
        placements.append(placement)
    if not scans:
        raise ValueError("report requires at least one grid")
    rollup = fleet_rollup(
        register,
        **{
            k: spec[k]
            for k in ("campaign_years", "n_campaigns", "min_high_fatigue_joints")
        },
    )
    result = {
        "domain": "riser_joint_ffs",
        "scans": scans,
        "placements": placements,
        "joint_governing": _governing_placements(placements),
        "rollup": rollup,
        "provenance": {
            "sources": sources,
            "input_units": "mm",
            "calculation_units": "in/psi/ft/yr",
            **spec["provenance"],
        },
    }
    return result


def run_report(spec, analysis):
    _validate_basis(spec)
    result = _calculate(spec, Path(analysis["analysis_root_folder"]))
    report = RiserJointReport.generate(
        result,
        {
            "zone_life_margin_multipliers": ZONE_FATIGUE_MARGIN,
            "weld_default_cvn_joules": DEFAULT_WELD_CVN_JOULES,
            "weld_bending_stress_mpa": 0.0,
            "collapse_design_factor": COLLAPSE_DESIGN_FACTOR,
            "seawater_psi_per_ft": SEAWATER_PSI_PER_FT,
            **{
                k: v
                for k, v in spec.items()
                if k not in ("scans", "register_csv", "provenance_readme", "provenance")
            },
        },
    )
    output = Path(analysis["result_folder"])
    output.mkdir(parents=True, exist_ok=True)
    result["report_html"] = str(output / "riser-joint-ffs.html")
    result["result_json"] = str(output / "riser-joint-ffs.json")
    encoded = json.dumps(result, indent=2, allow_nan=False)
    Path(result["report_html"]).write_text(report, encoding="utf-8")
    Path(result["result_json"]).write_text(encoded, encoding="utf-8")
    return result


def _validate_register(register):
    main = register[register["component"] == "Main"]
    life = pd.to_numeric(main["min_life_years"], errors="coerce")
    ids = main["joint_id"]
    if (
        main.empty
        or not np.isfinite(life).all()
        or (life < 0).any()
        or ids.isna().any()
        or ids.astype(str).str.strip().eq("").any()
    ):
        raise ValueError(
            "Main register requires joint IDs and finite nonnegative life for every row"
        )


def _governing_placements(placements):
    governing = {}
    for placement in placements:
        key = placement["joint_id"]
        if (
            key not in governing
            or placement["acceptable_depth_ft"] < governing[key]["acceptable_depth_ft"]
        ):
            governing[key] = placement
    return list(governing.values())


def _validate_basis(spec):
    for key in (
        "od_in",
        "wt_in",
        "design_pressure_psi",
        "campaign_years",
        "campaign_water_depth_ft",
        "differential_head_fraction",
    ):
        if not np.isfinite(spec[key]) or spec[key] <= 0:
            raise ValueError(f"basis requires finite positive {key}")
    if spec["wt_in"] >= spec["od_in"] / 2 or spec["differential_head_fraction"] > 1:
        raise ValueError("basis requires wall < OD/2 and differential head <= 1")
    for key, minimum in (("n_campaigns", 1), ("min_high_fatigue_joints", 0)):
        if type(spec[key]) is not int or spec[key] < minimum:
            raise ValueError(f"basis requires integer {key} >= {minimum}")
