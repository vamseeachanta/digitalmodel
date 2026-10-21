"""Offline generic-vessel case inventory, analytical screens and metric review.

No solver integration is imported. API RP 2SK and DNV-OS-E301 limits are
intentionally absent: the caller supplies a condition-specific criterion.
"""

from __future__ import annotations

import copy
import hashlib
import json
import math
from pathlib import Path

import yaml

# Enumeration prescribed by issue 2426, not standard-required directional coverage.
HEADINGS_DEG = tuple(range(0, 360, 45))
TEMPLATE_ROOT = Path("docs/domains/orcaflex/templates/mooring_systems")
CATENARY_SOURCE = (
    "MoorPy Catenary.py, ProfileType 2, inextensible frictionless limit; "
    "https://github.com/NREL/MoorPy/blob/main/moorpy/Catenary.py"
)
LINEAR_SOURCE = (
    "OpenStax University Physics Volume 1 (2016), section 15.1, "
    "linear restoring force; "
    "https://openstax.org/books/university-physics-volume-1/pages/15-1-simple-harmonic-motion"
)


def _finite(value, name, *, positive=False):
    if isinstance(value, bool) or not isinstance(value, (int, float)):
        raise ValueError(f"{name} must be numeric")
    if not math.isfinite(value) or (positive and value <= 0):
        raise ValueError(
            f"{name} must be finite" + (" and positive" if positive else "")
        )
    return value


def build_case_table(repo_root: Path, damaged_lines: dict | None = None) -> list[dict]:
    """Read generic templates; return planning cases, never solver-ready models.

    Headings are proposed co-directional wave/current headings in the template
    global convention. Original environmental values remain unchanged as evidence.
    The digest binds template bytes and case selection, not a native solver input.
    """
    cases = []
    damaged_lines = damaged_lines or {}
    if set(damaged_lines) - {"spread", "turret"}:
        raise ValueError("unknown damaged line system")
    for system in ("spread", "turret"):
        path = TEMPLATE_ROOT / f"{system}_mooring_hybrid/base/{system}_mooring_base.yml"
        raw = (Path(repo_root) / path).read_bytes()
        template = yaml.safe_load(raw)
        names = [line["Name"] for line in template["Lines"]]
        removed = damaged_lines.get(system)
        if removed is not None and removed not in names:
            raise ValueError(f"damaged line is absent from {system} template")
        for condition in ("intact", "damaged"):
            for heading in HEADINGS_DEG:
                selected = removed if condition == "damaged" else None
                selection = [system, condition, heading, selected]
                digest = hashlib.sha256(
                    raw + json.dumps(selection).encode()
                ).hexdigest()
                cases.append(
                    {
                        "case_id": f"{system}-{condition}-{heading:03d}",
                        "system": system,
                        "condition": condition,
                        "heading_deg": heading,
                        "removed_line": selected,
                        "active_lines": [n for n in names if n != selected],
                        "basis_sha256": digest,
                        "environment": {
                            "source": path.as_posix() + "#Environment",
                            "units": "template SI units; angles in degrees",
                            "values": template["Environment"],
                        },
                        "model_status": "NOT_EVALUATED",
                        "limitations": (
                            "damaged line"
                            if condition == "damaged" and selected is None
                            else "physical input readiness and native model validation pending"
                        ),
                    }
                )
    return cases


def catenary_screen(horizontal_kn, weight_kn_m, height_m, length_m) -> dict:
    """Uniform inextensible line with frictionless horizontal touchdown.

    Same grounded-line regime as orcaflex.mooring_design.solve_catenary,
    expressed directly in submerged force/length without a gravity assumption.
    H and h prescribe the anchor radius; this does not solve fixed-end geometry.
    """
    for name, value in locals().copy().items():
        _finite(value, name, positive=True)
    # MoorPy Catenary.py ProfileType 2: h=(sqrt(H²+V²)-H)/w,
    # V=w*s. Hence s²=h²+2*(H/w)*h; T=H+w*h (inelastic limit).
    a = horizontal_kn / weight_kn_m
    suspended = math.sqrt(height_m * (height_m + 2 * a))
    if suspended > length_m:
        raise ValueError(
            "fully suspended regime requires a different geometry solution"
        )
    horizontal = a * math.acosh(1 + height_m / a)
    return {
        "top_tension_kn": horizontal_kn + weight_kn_m * height_m,
        "suspended_length_m": suspended,
        "grounded_length_m": length_m - suspended,
        "anchor_radius_m": horizontal + length_m - suspended,
        "source": CATENARY_SOURCE,
    }


def linear_offset(force_xy_kn, stiffness_kn_m) -> tuple[float, float]:
    """Solve K q = F for a supplied symmetric positive-definite horizontal K.

    OpenStax (2016) section 15.1: linear restoring force F=-k*x;
    this is its two-dimensional static equilibrium extension. K must be
    independently established for this arrangement AND damage condition.
    Yaw, slack-line changes and nonlinear load redistribution are excluded.
    """
    if len(force_xy_kn) != 2 or len(stiffness_kn_m) != 2:
        raise ValueError("force and stiffness must have two horizontal dimensions")
    if any(len(row) != 2 for row in stiffness_kn_m):
        raise ValueError("stiffness must be two by two")
    fx, fy = (_finite(v, "force") for v in force_xy_kn)
    (a, b), (c, d) = stiffness_kn_m
    for value in (a, b, c, d):
        _finite(value, "stiffness")
    det = a * d - b * c
    if b != c or a <= 0 or d <= 0 or det <= 0 or not math.isfinite(det):
        raise ValueError("stiffness must be finite symmetric positive definite")
    return ((d * fx - b * fy) / det, (a * fy - c * fx) / det)


def _validated_screen(screen, active_lines):
    """Snapshot a sourced single-line comparison and its physical input basis."""
    if not isinstance(screen, dict):
        raise ValueError("screen must be a record")
    line_id = screen.get("line_id")
    if not isinstance(line_id, str) or line_id not in active_lines:
        raise ValueError("comparison requires an active screened line")
    basis = {"line_id": line_id}
    for key in ("offset_m", "top_tension_kn"):
        value = _finite(screen.get(key), f"screen {key}")
        if value < 0:
            raise ValueError("screen values cannot be negative")
        basis[key] = value
    source = screen.get("source")
    if not isinstance(source, str) or not source.strip():
        raise ValueError("screen source is required")
    basis["source"] = source
    if "inputs" not in screen and "input_manifest" not in screen:
        raise ValueError(
            "screen requires physical inputs or an input manifest reference"
        )
    if "inputs" in screen:
        inputs = screen["inputs"]
        if not isinstance(inputs, dict) or not inputs:
            raise ValueError("screen inputs must be a nonempty physical input record")
        try:
            basis["inputs"] = json.loads(json.dumps(inputs, allow_nan=False))
        except (TypeError, ValueError) as error:
            raise ValueError("screen inputs must be finite JSON data") from error
    if "input_manifest" in screen:
        reference = screen["input_manifest"]
        if not isinstance(reference, str) or not reference.strip():
            raise ValueError("screen input manifest reference must be nonempty")
        basis["input_manifest"] = reference
    return basis


def assess_case(case, result=None, *, capacities_kn=None, limit=None, screen=None):
    """Review supplied peak metrics; PASS refers only to the supplied ratio limit.

    Capacities are line-specific required inputs. Limit schema: ratio, source.
    No standard default, dynamic amplification or damaged multiplier is assumed.
    A screen requires line_id, offset_m, top_tension_kn, source, and physical
    inputs or an input_manifest reference. Its tension is matched to line_id.
    Supplied extracted metrics do not establish native-output qualification.
    """
    out = {
        "case_id": case["case_id"],
        "status": "NOT_EVALUATED",
        "qualified": False,
        "basis_sha256": case["basis_sha256"],
        "utilisation": None,
        "offset_m": None,
        "offset_difference_pct": None,
        "tension_difference_pct": None,
        "reason": "stored results absent",
        "criterion": copy.deepcopy(limit),
    }
    if screen is not None:
        out["screen"] = _validated_screen(screen, case["active_lines"])
    if result is None:
        return out
    if result["basis_sha256"] != case["basis_sha256"]:
        raise ValueError("case basis digest mismatch")
    for field in ("solver_version", "source"):
        if not isinstance(result.get(field), str) or not result[field].strip():
            raise ValueError(f"{field} is required")
    tensions = result["tensions_kn"]
    if not tensions or set(tensions) != set(case["active_lines"]):
        raise ValueError("tensions must cover exactly the active lines")
    for value in tensions.values():
        if _finite(value, "tension") < 0:
            raise ValueError("tension cannot be negative")
    xy = result["offset_xy_m"]
    if len(xy) != 2:
        raise ValueError("offset requires two horizontal components")
    out["offset_m"] = math.hypot(*[_finite(v, "offset") for v in xy])
    out["max_tension_kn"] = max(tensions.values())
    out["solver_version"] = result["solver_version"]
    out["source"] = result["source"]
    out["reason"] = "condition-specific capacity or criterion absent"
    if case["condition"] == "damaged" and case["removed_line"] is None:
        out["reason"] = "damaged line unspecified"
        return out
    if capacities_kn is not None and limit is not None:
        if set(capacities_kn) != set(tensions):
            raise ValueError("capacities must cover exactly the active lines")
        ratio = _finite(limit["ratio"], "limit", positive=True)
        if (
            ratio > 1
            or not isinstance(limit.get("source"), str)
            or not limit["source"].strip()
        ):
            raise ValueError("ratio limit requires a source and cannot exceed unity")
        utils = {
            name: tension
            / _finite(capacities_kn[name], "capacity", positive=True)
            / ratio
            for name, tension in tensions.items()
        }
        out.update(
            utilisation=max(utils.values()),
            governing_line=max(utils, key=utils.get),
            peak_mbl_ratio=max(utils.values()) * ratio,
            limit_ratio=ratio,
            capacities_kn=dict(capacities_kn),
            tensions_kn=dict(tensions),
            status="PASS" if max(utils.values()) <= 1 else "FAIL",
            reason="supplied peak/MBL ratio compared with supplied limit",
        )
    if screen is not None:
        basis = out["screen"]
        # Percent difference definition: 100*(stored-screen)/screen.
        for key, actual, expected in (
            ("offset", out["offset_m"], basis["offset_m"]),
            ("tension", tensions[basis["line_id"]], basis["top_tension_kn"]),
        ):
            out[f"{key}_difference_pct"] = (
                100 * (actual - expected) / expected if expected else None
            )
    return out


def _presentation_echo(value, key=""):
    """Round report display only; assessment calculations remain unrounded."""
    if isinstance(value, dict):
        return {name: _presentation_echo(item, name) for name, item in value.items()}
    if isinstance(value, list):
        return [_presentation_echo(item) for item in value]
    if isinstance(value, float):
        return round(value, 1 if key.endswith("_pct") else 3)
    return value


def render_report(cases, assessments, *, revision, date) -> str:
    """Build standard HTML; absent cases remain visible and unqualified.

    Assessments are keyed by case ID and contain assess_case outputs. A report
    cannot assert full native result acceptance; native readback is a later gate.
    """
    from digitalmodel.reporting.engine import render_html
    from digitalmodel.reporting.provenance import Provenance
    from digitalmodel.reporting.spec import (
        DocumentMeta,
        ReportSpec,
        Section,
        TableBlock,
        TextBlock,
    )

    ids = [case["case_id"] for case in cases]
    if not ids or len(ids) != len(set(ids)):
        raise ValueError("case inventory must be nonempty and unique")
    if set(assessments) - set(ids):
        raise ValueError("assessment references an unknown case")
    versions = {
        v["solver_version"] for v in assessments.values() if "solver_version" in v
    }
    if len(versions) > 1:
        raise ValueError("all stored results must use one solver version")
    rows = []
    provenance = Provenance()
    for case in cases:
        item = assessments.get(case["case_id"], assess_case(case))
        if item["case_id"] != case["case_id"]:
            raise ValueError("assessment case identity mismatch")
        if item.get("basis_sha256") != case["basis_sha256"]:
            raise ValueError("assessment basis digest mismatch")

        def fmt(value, places=1):
            return "Not established" if value is None else f"{value:.{places}f}"

        rows.append(
            [
                case["case_id"],
                item["status"],
                fmt(item.get("peak_mbl_ratio"), 3),
                fmt(item.get("limit_ratio"), 3),
                fmt(item.get("utilisation"), 3),
                fmt(item.get("offset_m")),
                fmt(item.get("offset_difference_pct")),
                fmt(item.get("tension_difference_pct")),
                item["reason"],
                (item.get("criterion") or {}).get("source", "Not established"),
            ]
        )
        provenance.add(
            "template",
            case["environment"]["source"],
            digest=case["basis_sha256"],
            description="Template plus case-selection basis; not native input",
        )
        if "source" in item:
            provenance.add(
                "extracted metrics",
                item["source"],
                description="Supplied metrics; native readback not established",
            )
        if "screen" in item:
            screen = item["screen"]
            provenance.add(
                "analytical screen",
                screen["source"],
                description=(
                    f"Analytical screen for {case['case_id']}, line {screen['line_id']}; "
                    "physical input basis retained in the input echo"
                ),
            )
    supplied = sum("solver_version" in item for item in assessments.values())
    spec = ReportSpec(
        document=DocumentMeta(
            number=f"M2426-RPT-001-{revision}",
            revision=revision,
            title="Mooring analysis",
            project="Generic mooring workflow",
            client="Not applicable",
            date=date,
        ),
        provenance=provenance,
        input_echo=_presentation_echo({"cases": cases, "assessments": assessments}),
        sections=[
            Section(
                key="summary",
                title="Summary",
                blocks=[
                    TextBlock(
                        markdown=(
                            f"Stored metrics supplied: {supplied} / {len(cases)}. "
                            "Native results qualified: 0. PASS applies only to a supplied "
                            "tension criterion. Vessel offset acceptance is NOT_EVALUATED; "
                            "an allowable offset and governing reference are not established."
                        )
                    )
                ],
            ),
            Section(
                key="revision-history",
                title="Revision history",
                blocks=[
                    TableBlock(
                        title="Issue record",
                        columns=["Revision", "Date", "Change and reason"],
                        rows=[
                            [
                                revision,
                                date,
                                "Offline case assessment issued to expose missing evidence.",
                            ]
                        ],
                    )
                ],
            ),
            Section(
                key="results",
                title="Case results",
                blocks=[
                    TableBlock(
                        title="Supplied metrics and analytical comparisons",
                        columns=[
                            "Case",
                            "Tension criterion",
                            "Peak / MBL [1]",
                            "Limit [1]",
                            "Utilisation [1]",
                            "Offset [m]",
                            "Offset difference [%]",
                            "Tension difference [%]",
                            "Basis / gap",
                            "Criterion source",
                        ],
                        rows=rows,
                    )
                ],
            ),
            Section(
                key="method",
                title="Method and limitations",
                blocks=[
                    TextBlock(
                        markdown=(
                            "Case headings enumerate the issue scope; they are not code-required coverage. "
                            "Template environments are different between arrangements and cannot establish "
                            "a matched benchmark. Catenary screening assumes uniform submerged weight, "
                            "inextensible line and frictionless horizontal touchdown. Linear Kq=F offsets "
                            "exclude yaw and nonlinear redistribution. Percent differences use the screen "
                            "as denominator; tension comparisons use the identified active line. "
                            "The screen source and physical inputs or manifest reference are retained "
                            "in the input echo. A zero screen leaves the difference not established.\n\n"
                            + CATENARY_SOURCE
                            + "\n\n"
                            + LINEAR_SOURCE
                        )
                    )
                ],
            ),
            Section(
                key="next",
                title="Way forward",
                blocks=[
                    TextBlock(
                        markdown=(
                            "Establish common sourced environmental and response inputs, damaged-line "
                            "selection and edition-specific criteria. Resolve platform/component readiness "
                            "under the linked library work before model generation. Generate and validate "
                            "native models, then obtain separately authorized statics/dynamics runs and "
                            "native-output readback on one recorded solver version. Differences require "
                            "matched inputs and an engineering explanation; no benchmark is established."
                        )
                    )
                ],
            ),
        ],
    )
    return render_html(spec)
