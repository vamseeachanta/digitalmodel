# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Solver-free flexible riser comparison and report construction.

Sources: issue 2429 defines the symbolic case product. Orcina online help
(accessed 2026-10-20), 'Line types: Limits data' defines allowable tension and
MBR; 'Line results: Contact' defines seabed clearance. No code capacities,
pipe/site properties, mesh tolerances or physical constants are assumed.

PASS means only that a supplied scalar meets its supplied scalar criterion.
Native solver readback, source rights and design-code compliance are not attested.
"""

from __future__ import annotations

import argparse
from dataclasses import dataclass
from hashlib import sha256
from html import escape
from itertools import product
import math
from pathlib import Path
import re
from typing import Mapping, Sequence


CONFIGURATIONS = ("catenary", "lazy_wave", "steep_wave", "pliant_wave")
OFFSETS = ("near", "far", "cross")
SEA_STATES = ("sea_1", "sea_2")
# Symbolic labels/counts above are from issue 2429, Scope and licensed-solver work.
ISSUE = "https://github.com/vamseeachanta/digitalmodel/issues/2429"
HELP = "https://www.orcina.com/webhelp/OrcaFlex/Content/html/"
SOURCES = {
    "limits": HELP + "Linetypes,Limitsdata.htm",
    "clearance": HELP + "Lineresults,Contact.htm",
    "mesh": HELP + "Linedata,Structure.htm",
}
METRICS = {
    "tension": ("kN", "maximum"),
    "curvature": ("1/m", "maximum"),
    "clearance": ("m", "minimum"),
}


def _text(value: str, label: str) -> None:
    if not isinstance(value, str) or not value.strip():
        raise ValueError(f"{label} requires nonempty text")


def _number(value: float) -> None:
    if (
        isinstance(value, bool)
        or not isinstance(value, (int, float))
        or not math.isfinite(value)
    ):
        raise ValueError("a finite numeric value is required")


@dataclass(frozen=True)
class Case:
    """A symbolic case; offset magnitudes and sea-state data remain required."""

    configuration: str
    offset: str
    sea_state: str

    def __post_init__(self):
        if (
            self.configuration not in CONFIGURATIONS
            or self.offset not in OFFSETS
            or self.sea_state not in SEA_STATES
        ):
            raise ValueError("unknown configuration, offset or sea state")

    @property
    def key(self) -> str:
        return f"{self.configuration}/{self.offset}/{self.sea_state}"


def case_table() -> list[Case]:
    """Return the issue's complete symbolic product; no launch is authorised."""
    return [Case(*parts) for parts in product(CONFIGURATIONS, OFFSETS, SEA_STATES)]


def inventory(root: Path) -> list[dict[str, str]]:
    """Hash existing source specifications without copying vendor model content."""
    records = []
    library = Path("docs/domains/orcaflex/library")
    for group, configuration in product(
        ("tier2_fast", "model_library"), CONFIGURATIONS
    ):
        relative = library / group / f"a01_{configuration}_riser" / "spec.yml"
        payload = (root / relative).read_bytes()  # Missing evidence fails closed.
        records.append(
            {
                "configuration": configuration,
                "group": group,
                "path": relative.as_posix(),
                "sha256": sha256(payload).hexdigest(),
            }
        )
    return records


@dataclass(frozen=True)
class Measurement:
    """An explicitly sourced scalar in canonical units; no implicit conversion."""

    value: float
    unit: str
    source: str

    def __post_init__(self):
        _number(self.value)
        _text(self.unit, "unit")
        _text(self.source, "source")


@dataclass(frozen=True)
class Criterion(Measurement):
    """A project-supplied scalar limit, not a built-in standards factor."""

    direction: str

    def __post_init__(self):
        super().__post_init__()
        if self.direction not in ("maximum", "minimum"):
            raise ValueError("direction must be maximum or minimum")


@dataclass(frozen=True)
class Evidence:
    """Externally extracted whole-period extrema over a declared comparison region.

    The common basis digest identifies pipe, site, offset/sea-state definitions,
    extraction region/time window and mesh-independent configuration definitions.
    Input/output digests identify run-specific files. Digests are recorded claims;
    this class does not open private native files or authenticate their contents.
    """

    case: Case
    solver_version: str
    basis_digest: str
    input_digest: str
    output_digest: str
    criteria_revision: str
    kind: str
    converged: bool
    tension: Measurement | None
    curvature: Measurement | None
    clearance: Measurement | None

    def __post_init__(self):
        if not isinstance(self.case, Case) or type(self.converged) is not bool:
            raise ValueError("case and boolean convergence are required")
        for field in ("solver_version", "criteria_revision"):
            _text(getattr(self, field), field)
        for field in ("basis_digest", "input_digest", "output_digest"):
            if not re.fullmatch(r"[a-f0-9]{64}", getattr(self, field)):
                raise ValueError(f"{field} requires a SHA-256 digest")
        if self.kind not in ("synthetic", "stored"):
            raise ValueError("kind must be synthetic or stored")
        for name, (unit, _) in METRICS.items():
            result = getattr(self, name)
            if result is not None:
                if not isinstance(result, Measurement) or result.unit != unit:
                    raise ValueError(f"{name} requires unit {unit}")
                if name == "curvature" and result.value < 0:
                    raise ValueError("curvature magnitude cannot be negative")


def compare(evidence: Evidence, criteria: Mapping[str, Criterion]) -> list[dict]:
    """Compare max tension/curvature and min clearance using inclusive bounds.

    Method: direct scalar inequality, not an implementation of API RP 2RD or
    DNV-OS-F201. Constant MBR-derived curvature limits must be supplied by the
    caller; tension-dependent/anisotropic MBR is outside this scalar screen.
    """
    if set(criteria) - METRICS.keys():
        raise ValueError("unknown criterion")
    rows = []
    for name, (unit, direction) in METRICS.items():
        result, limit = getattr(evidence, name), criteria.get(name)
        if limit is not None and (limit.unit != unit or limit.direction != direction):
            raise ValueError(f"{name} criterion unit or direction mismatch")
        if limit is not None and (name != "clearance" and limit.value <= 0):
            raise ValueError(f"{name} criterion must be positive")
        reason = ""
        status = "NOT_EVALUATED"
        if not evidence.converged:
            reason = "Run convergence is not established"
        elif result is None or limit is None:
            reason = "Response or sourced criterion is missing"
        else:
            meets = (
                result.value <= limit.value
                if direction == "maximum"
                else result.value >= limit.value
            )
            status = "PASS" if meets else "FAIL"
        rows.append(
            {
                "metric": name,
                "unit": unit,
                "direction": direction,
                "value": result.value if result else None,
                "limit": limit.value if limit else None,
                "source": limit.source if limit else None,
                "response_source": result.source if result else None,
                "status": status,
                "reason": reason,
            }
        )
    return rows


def mesh_change(coarse: Measurement, fine: Measurement) -> float | None:
    """Report a descriptive relative change, with no assumed convergence limit.

    Definition (independent arithmetic): |coarse-fine|/|fine| * 100 percent.
    The 100 is the definition of percent, not an engineering coefficient.
    Zero fine reference gives an undefined relative change (None), even at 0/0.
    Orcina 'Line data: Structure' describes the segmentation being varied; this
    two-level statistic alone cannot establish mesh convergence.
    """
    if coarse.unit != fine.unit:
        raise ValueError("mesh comparison unit mismatch")
    if fine.value == 0:
        return None
    change = abs(coarse.value - fine.value) / abs(fine.value) * 100
    _number(change)
    return change


def _common(records: Sequence[Evidence]) -> None:
    if len({r.case for r in records}) != len(records):
        raise ValueError("duplicate case evidence")
    for field in ("solver_version", "basis_digest", "criteria_revision", "kind"):
        if len({getattr(r, field) for r in records}) > 1:
            raise ValueError(f"a common {field} is required")


def _cell(value) -> str:
    if value is None:
        return "NOT_EVALUATED"
    if isinstance(value, (int, float)):
        return f"{value:.3g}"
    return escape(str(value))


@dataclass(frozen=True)
class MeshPair:
    """Two externally extracted runs, with sourced target lengths per section.

    Each corresponding section shall be unchanged except segmentation. Lengths
    record intended refinement, not proof of actual generated node coordinates.
    """

    coarse: Evidence
    fine: Evidence
    coarse_mesh: tuple[Measurement, ...]
    fine_mesh: tuple[Measurement, ...]

    def __post_init__(self):
        for field in (
            "case",
            "solver_version",
            "basis_digest",
            "criteria_revision",
            "kind",
        ):
            if getattr(self.coarse, field) != getattr(self.fine, field):
                raise ValueError(f"mesh pair requires common {field}")
        if self.coarse.input_digest == self.fine.input_digest:
            raise ValueError("refinement requires distinct input digests")
        if self.coarse.output_digest == self.fine.output_digest:
            raise ValueError("refinement requires distinct output digests")
        if not self.coarse_mesh or len(self.coarse_mesh) != len(self.fine_mesh):
            raise ValueError("corresponding section meshes are required")
        for coarse, fine in zip(self.coarse_mesh, self.fine_mesh):
            if coarse.unit != "m" or fine.unit != "m":
                raise ValueError("mesh length unit must be m")
            if not 0 < fine.value <= coarse.value:
                raise ValueError(
                    "section target lengths must refine and remain positive"
                )
        if not any(f.value < c.value for c, f in zip(self.coarse_mesh, self.fine_mesh)):
            raise ValueError("at least one section must refine")


def _mesh_rows(meshes: Sequence[MeshPair], lookup: Mapping[Case, Evidence]) -> str:
    rows = []
    for pair in meshes:
        if lookup.get(pair.coarse.case) != pair.coarse:
            raise ValueError("mesh coarse evidence must match supplied baseline")
        for name, (unit, _) in METRICS.items():
            coarse, fine = getattr(pair.coarse, name), getattr(pair.fine, name)
            change = "NOT_EVALUATED"
            absolute = "NOT_EVALUATED"
            if coarse and fine and pair.coarse.converged and pair.fine.converged:
                relative = mesh_change(coarse, fine)
                change = (
                    f"{relative:.1f} %"
                    if relative is not None
                    else "undefined (zero reference)"
                )
                absolute = f"{abs(coarse.value - fine.value):.1f}"
            rows.append(
                f"<tr><td>{pair.coarse.case.key}</td><td>{name} ({unit})</td>"
                f"<td>{absolute}</td><td>{change}</td></tr>"
            )
    return "".join(rows)


def _evidence_entry(record: Evidence) -> str:
    metadata = (
        f"{record.case.key}; kind={record.kind}; solver={record.solver_version}; "
        f"converged={record.converged}; criteria revision={record.criteria_revision}; "
        f"basis SHA-256={record.basis_digest}; input SHA-256={record.input_digest}; "
        f"output SHA-256={record.output_digest}"
    )
    responses = []
    for name in METRICS:
        result = getattr(record, name)
        if result:
            responses.append(
                f"{name}={_cell(result.value)} {escape(result.unit)}; "
                f"source={escape(result.source)}"
            )
    return "<li>" + escape(metadata) + "; " + "; ".join(responses) + "</li>"


def render_report(
    records: Sequence[Evidence],
    criteria: Mapping[str, Criterion],
    *,
    revision: str,
    date: str,
    sources: Sequence[dict] = (),
    meshes: Sequence[MeshPair] = (),
    history: Sequence[tuple[str, str, str]] = (),
) -> str:
    """Render a results-and-method report; never infer readiness from coverage."""
    _text(revision, "revision")
    _text(date, "date")
    _common(records)
    lookup = {r.case: r for r in records}
    mesh_rows = _mesh_rows(meshes, lookup)
    history = history or [
        (
            revision,
            date,
            "Initial offline comparison; missing inputs and solver evidence remain explicit.",
        )
    ]
    if history[-1][:2] != (revision, date) or len({r[0] for r in history}) != len(
        history
    ):
        raise ValueError(
            "revision history must end at the current revision/date and have unique revisions"
        )
    revision_rows = "".join(
        "<tr>" + "".join(f"<td>{escape(v)}</td>" for v in row) + "</tr>"
        for row in history
    )
    rows = []
    for case in case_table():
        evidence = lookup.get(case)
        checks = compare(evidence, criteria) if evidence else []
        cells = []
        for check in checks:
            cells.append(
                "<td>"
                + "; ".join(
                    _cell(check[k])
                    for k in (
                        "value",
                        "limit",
                        "status",
                        "reason",
                        "source",
                        "response_source",
                    )
                )
                + "</td>"
            )
        if not checks:
            cells = ["<td>NOT_EVALUATED: result missing</td>" for _ in METRICS]
        rows.append(
            f'<tr data-case="{case.key}"><td>{case.key}</td>' + "".join(cells) + "</tr>"
        )
    provenance = "".join("<li>" + escape(str(record)) + "</li>" for record in sources)
    evidence_list = "".join(_evidence_entry(record) for record in records)
    for pair in meshes:
        evidence_list += _evidence_entry(pair.fine)
        for label, lengths in (("coarse", pair.coarse_mesh), ("fine", pair.fine_mesh)):
            evidence_list += (
                "<li>"
                + label
                + " section targets: "
                + "; ".join(f"{_cell(m.value)} m ({escape(m.source)})" for m in lengths)
                + "</li>"
            )
    source_links = "".join(
        f'<li><a href="{url}">{name}</a></li>' for name, url in SOURCES.items()
    )
    return f"""<!doctype html>
<html lang="en"><meta charset="utf-8"><title>Flexible riser configuration analysis</title>
<style>body{{font:16px system-ui;max-width:1200px;margin:2em auto;padding:1em}}
table{{border-collapse:collapse;width:100%}}td,th{{border:1px solid #bbb;padding:.5em}}
li{{overflow-wrap:anywhere}}caption{{caption-side:bottom;text-align:left}}</style>
<h1>Flexible riser configuration analysis</h1>
<h2>Summary</h2><p>{len(records)} / {len(case_table())} cases have supplied summaries.
Engineering acceptance is NOT_EVALUATED: native readback is not established.
Supplied scalar PASS/FAIL checks do not establish design-code compliance.</p>
<h2>Revision history</h2><table><tr><th>Revision</th><th>Date</th><th>Change and reason</th></tr>
{revision_rows}
<caption>Revision history. Re-issues shall preserve earlier rows.</caption></table>
<h2>Case table</h2><p>Each result cell lists value; limit; status; reason; criterion source; response source.
Max effective tension (kN), max curvature magnitude (1/m), min seabed clearance (m).
Limits are inclusive. Signed clearance includes contact penetration.
Scalar displays use three significant figures; comparisons use unrounded inputs.</p>
<table><tr><th>Configuration / offset / sea state</th><th>Tension (kN)</th>
<th>Curvature (1/m)</th><th>Clearance (m)</th></tr>{"".join(rows)}
<caption>Symbolic cases from <a href="{ISSUE}">the comparison scope</a>.
No offset magnitude or sea-state number is inferred.</caption></table>
<h2>Input basis and limitations</h2><p>Required before a common-site comparison:
a sourced common pipe section (including buoyancy variants), site/seabed/current,
near/far/cross offset vectors, two sea-state definitions, extraction region and
period, tension/curvature/clearance criteria and criteria revision.
Existing library specimens are references, not an approved common basis.
Whole-line clearance may be governed by intended touchdown; define a common
suspended region before applying a sag-bend clearance criterion.</p>
<h2>Mesh sensitivity</h2><p>{"Paired evidence supplied; two-level change only." if meshes else "NOT_EVALUATED: no paired mesh evidence is supplied."}
Use mesh_change for sourced coarse/fine extrema on identical cases, common basis,
solver version and extraction scope. Report absolute changes alongside relative
changes; a zero fine reference has no relative percentage. More than two levels
and a project-supplied tolerance are needed to support a convergence conclusion.</p>
<table><tr><th>Case</th><th>Metric (unit)</th><th>Absolute change (metric unit)</th>
<th>Relative change (%)</th></tr>{mesh_rows}
<caption>Descriptive mesh changes; convergence acceptance remains NOT_EVALUATED.</caption></table>
<h2>Way forward</h2><p>Resolve the common input basis and criteria, retain native
outputs and version evidence for every case, then compare extracted extrema and
mesh refinements. Solver execution requires separate authorisation. Vendor model
redistribution rights remain unverified; this report carries method and metadata.</p>
<h2>Sources</h2><p>Orcina online help, accessed 2026-10-20; no licensed standard
values are reproduced. API RP 2RD and DNV-OS-F201 checks are NOT_EVALUATED.</p>
<ul>{source_links}</ul><h2>Source inventory</h2><ul>{provenance}</ul>
<h2>Supplied evidence (synthetic or stored; not native attestation)</h2>
<ul>{evidence_list}</ul></html>"""


def main() -> None:
    """Build the initial unissued report without a solver or copied model data."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--revision", required=True)
    parser.add_argument("--date", required=True)
    args = parser.parse_args()
    root = Path(__file__).resolve().parents[4]
    report = render_report(
        [], {}, revision=args.revision, date=args.date, sources=inventory(root)
    )
    args.output.write_text(report, encoding="utf-8")
    if args.output.read_text(encoding="utf-8") != report:
        raise OSError("report readback differs from generated content")


if __name__ == "__main__":
    main()
