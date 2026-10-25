# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Audit bounded knowledge-pack claims; never grant engineering qualification.

Observations in the declared JSON are regeneration input. Generated findings are
derived separately, so a prior summary cannot supply its own acceptance evidence.
No engineering module is imported, and no command from a pack or observation is
executed. Probe receipts are retained manual evidence, not authenticated runs.
"""

from __future__ import annotations

import argparse
import ast
import copy
import hashlib
import html
import json
import math
import os
from pathlib import Path, PurePosixPath
import re
import tempfile
from typing import Any

FAMILIES = (
    "asset_integrity",
    "code_checks",
    "riser_fatigue",
    "mooring_fatigue",
    "viv_screening",
    "vessel_stability_screening",
    "hull_girder_screening",
)
JSON_NAME = "knowledge-pack-acceptance.json"
HTML_NAME = "knowledge-pack-acceptance.html"
DISPOSITIONS = ("SUPPORTED", "CONTRADICTED", "UNESTABLISHED")
KINDS = (
    "source_presence",
    "citation_integrity",
    "numerical_report",
    "behavioral",
    "gate_coverage",
)
CRITERIA = {
    "source_presence": "Current, hashed, repository-local source and test references must resolve within the declared scope; presence does not establish calculation validity.",
    "citation_integrity": "Local pack citations must resolve to existing files and valid line ranges. Circular audit citations cannot establish acceptance; external sources remain unreviewed.",
    "numerical_report": "Each numerical-report claim requires at least three distinct reported quantities, independent hand-worked literal anchors, working beside the assertions, finite comparisons and an explicit reference basis.",
    "behavioral": "Inspected screening inputs must reject or escalate missing, None, non-finite, out-of-range or non-applicable data with a reason. Aggregation must inspect a worst member after the first and reject non-finite margins before selection.",
    "gate_coverage": "A current pull-request execution must run that pull request's own tests. Source presence, configured test folders and historical test counts do not establish execution.",
}
REFERENCE = re.compile(r"^(.*?)(?::L?(\d+)(?:-L?(\d+))?)?$")
PACK_REFERENCE = re.compile(
    r"[\[(]([\w./-]+\.(?:py|md|rst|ya?ml|json|html|txt|toml)"
    r"(?::[^\s\[\]()]+)?)[\])]"
)


def sha256(data: bytes) -> str:
    """Return a content digest without interpreting the source."""
    return hashlib.sha256(data).hexdigest()


def source_path(root: Path, name: str) -> Path:
    """Constrain evidence to local, non-generated, non-symlinked files."""
    if not isinstance(name, str) or not name or "\\" in name:
        raise ValueError("invalid source path")
    pure = PurePosixPath(name)
    if pure.is_absolute() or ".." in pure.parts or ":" in name:
        raise ValueError("source path escapes repository")
    if pure.parts[0] == ".git" or name.startswith("docs/audits/"):
        raise ValueError("generated or circular audit source")
    if pure.name in (JSON_NAME, HTML_NAME):
        raise ValueError("generated audit source")
    path = root / pure
    if any(parent.is_symlink() for parent in (path, *path.parents) if parent != root):
        raise ValueError("symlink source is outside the inspected scope")
    if not path.resolve().is_relative_to(root.resolve()) or not path.is_file():
        raise ValueError("missing or external source")
    return path


def reference_info(root: Path, reference: str) -> tuple[str, bool, str]:
    """Resolve file and optional line bounds without following external links."""
    if not isinstance(reference, str):
        return "", False, "invalid reference"
    match = REFERENCE.fullmatch(reference)
    if match is None:
        return "", False, "invalid reference"
    name, start, end = match.groups()
    try:
        path = source_path(root, name)
        if start:
            first, last = int(start), int(end or start)
            length = len(path.read_text(encoding="utf-8").splitlines())
            if not 1 <= first <= last <= length:
                return name, False, "line range does not resolve"
    except (ValueError, OSError, UnicodeError) as exc:
        return name, False, str(exc)
    return name, True, "resolved"


def inventory(root: Path) -> list[dict]:
    """Enumerate exactly the seven requested families, not every module."""
    rows = []
    for family in FAMILIES:
        package = root / "src/digitalmodel" / family
        pack = package / "docs/agent-pack.md"
        code = sorted(
            p.relative_to(root).as_posix()
            for p in package.rglob("*.py")
            if not p.is_symlink()
        )
        delegated = root / f"src/digitalmodel/naval_architecture/{family}.py"
        if delegated.is_file():
            code.append(delegated.relative_to(root).as_posix())
        tests = sorted(
            p.relative_to(root).as_posix()
            for p in (root / "tests").rglob("test_*.py")
            if family in p.as_posix()
        )
        citations = []
        if pack.is_file() and not pack.is_symlink():
            for ref in sorted(
                set(PACK_REFERENCE.findall(pack.read_text(encoding="utf-8")))
            ):
                name, valid, reason = reference_info(root, ref)
                citations.append(
                    {
                        "reference": ref,
                        "path": name,
                        "resolved": valid,
                        "reason": reason,
                    }
                )
        rows.append(
            {
                "family": family,
                "pack_path": pack.relative_to(root).as_posix()
                if pack.is_file()
                else None,
                "implementation_paths": code,
                "test_paths": tests,
                "citations": citations,
            }
        )
    return rows


def validate_document(document: dict) -> None:
    """Reject ambiguous family identities and unknown acceptance vocabulary."""
    if not isinstance(document, dict) or document.get("schema_version") != 1:
        raise ValueError("unsupported observations schema")
    bundles = document.get("observations")
    if not isinstance(bundles, list):
        raise ValueError("observations must be a family list")
    families = []
    for bundle in bundles:
        if not isinstance(bundle, dict):
            raise ValueError("invalid family bundle")
        family = bundle.get("family")
        if family not in FAMILIES:
            raise ValueError("unknown family")
        if family in families:
            raise ValueError("duplicate family record")
        families.append(family)
        ids = []
        if not isinstance(bundle.get("findings"), list) or not bundle["findings"]:
            raise ValueError("family must contain findings")
        for row in bundle["findings"]:
            if not isinstance(row, dict):
                raise ValueError("invalid family finding")
            if not row.get("id") or row["id"] in ids:
                raise ValueError("missing or duplicate claim id")
            ids.append(row["id"])
            if row.get("family") != family:
                raise ValueError("finding family differs from bundle")
            if row.get("disposition") not in DISPOSITIONS:
                raise ValueError("unknown disposition")
            if row.get("claim_kind") not in KINDS:
                raise ValueError("unknown claim kind")


def verify_digests(root: Path, observations: list[dict]) -> None:
    """Refuse stale input before generating either output."""
    for bundle in observations:
        for row in bundle["findings"]:
            digests = row.get("source_digests", {})
            if not isinstance(digests, dict):
                raise ValueError("source digests must be a mapping")
            for name, expected in digests.items():
                try:
                    actual = sha256(source_path(root, name).read_bytes())
                except (ValueError, OSError) as exc:
                    raise ValueError(f"missing or stale source: {name}") from exc
                if actual != expected:
                    raise ValueError(f"stale source digest: {name}")


def reference_gaps(root: Path, row: dict, item: dict) -> list[str]:
    """Require current hashes for every cited acceptance source."""
    gaps = []
    refs = [row.get("claim_location", "")]
    for key in ("implementation_refs", "test_refs"):
        values = row.get(key)
        if not isinstance(values, list) or not values:
            gaps.append(f"missing {key}")
        else:
            refs.extend(values)
            prefix = "tests/" if key == "test_refs" else "src/"
            if any(
                not isinstance(ref, str)
                or not ref.startswith(prefix)
                or not ref.partition(":")[0].endswith(".py")
                for ref in values
            ):
                gaps.append(f"{key} must cite its own source role")
    pack = row.get("pack_path")
    if not item["pack_path"] or pack != item["pack_path"]:
        gaps.append("pack missing or different from discovered family pack")
    elif pack not in row.get("source_digests", {}):
        gaps.append("pack digest missing")
    for ref in refs:
        name, valid, reason = reference_info(root, ref)
        if not valid:
            gaps.append(f"unresolved reference {ref}: {reason}")
        if name not in row.get("source_digests", {}):
            gaps.append(f"source digest missing: {name}")
    return gaps


def probe_state(row: dict) -> tuple[str, list[str]]:
    """Check a retained receipt, not a PASS label or a successful shell exit."""
    result = row.get("probe_result") or {}
    if not isinstance(result, dict):
        return "UNESTABLISHED", ["NOT_RUN: malformed probe receipt"]
    if result.get("status") != "RUN":
        return "UNESTABLISHED", [
            "NOT_RUN: " + result.get("reason", "probe evidence absent")
        ]
    required = (
        "output",
        "output_sha256",
        "program",
        "program_sha256",
        "interface",
        "cases",
    )
    if any(not result.get(key) for key in required):
        return "UNESTABLISHED", ["probe output or execution provenance missing"]
    if not isinstance(result["output"], str) or not isinstance(result["program"], str):
        return "UNESTABLISHED", ["probe output and program must be retained text"]
    valid = (
        result.get("completed") is True
        and result.get("exit_status") == 0
        and result.get("synthetic") is True
        and bool(row.get("probe_command"))
        and result.get("command") == row["probe_command"]
        and result.get("source_digests") == row.get("source_digests")
        and sha256(result["output"].encode()) == result["output_sha256"]
        and sha256(result["program"].encode()) == result["program_sha256"]
    )
    try:
        valid = valid and json.loads(result["output"]) == result["cases"]
    except (ValueError, TypeError):
        valid = False
    if not valid or not isinstance(result["cases"], list):
        return "UNESTABLISHED", [
            "probe receipt does not match current sources, command or output"
        ]
    cases = result["cases"]
    if any(
        not isinstance(case, dict)
        or not all(
            key in case for key in ("arguments", "expected", "observed", "detail")
        )
        or not case["detail"]
        for case in cases
    ):
        return "UNESTABLISHED", ["probe cases are incomplete"]
    if any(case["expected"] != case["observed"] for case in cases):
        return "CONTRADICTED", []
    return "SUPPORTED", []


def finite_number(value: Any) -> bool:
    """Accept finite numeric literals, excluding booleans."""
    return type(value) in (int, float) and math.isfinite(value)


def literal_quantity_comparison(node: ast.AST, anchor: dict) -> bool:
    """Require a quantity reference equated directly to its numeric literal."""
    if (
        not isinstance(node, ast.Compare)
        or len(node.ops) != 1
        or not isinstance(node.ops[0], ast.Eq)
    ):
        return False
    left, right = node.left, node.comparators[0]
    for reported, literal in ((left, right), (right, left)):
        try:
            value = ast.literal_eval(literal)
        except (ValueError, TypeError):
            continue
        if not finite_number(value) or value != anchor.get("expected"):
            continue
        quantity = anchor.get("quantity")
        if (
            isinstance(reported, ast.Name)
            and reported.id == quantity
            or isinstance(reported, ast.Attribute)
            and reported.attr == quantity
            or isinstance(reported, ast.Subscript)
            and isinstance(reported.slice, ast.Constant)
            and reported.slice.value == quantity
        ):
            return True
    return False


def anchor_source_valid(root: Path, anchor: dict) -> bool:
    """Match a real quantity assertion and its working within the cited span.

    Quantity references are names, attributes or literal subscript keys. This
    static check does not authenticate execution or the hand-worked basis.
    """
    ref = anchor.get("test_ref", "")
    name, valid, _ = reference_info(root, ref)
    if not valid:
        return False
    match = REFERENCE.fullmatch(ref)
    if match is None:
        return False
    _, start, end = match.groups()
    if not start:
        return False
    lines = source_path(root, name).read_text(encoding="utf-8").splitlines()
    first, last = int(start), int(end or start)
    span = "\n".join(lines[first - 1 : last])
    assertion = anchor.get("assertion", "")
    working = anchor.get("working", "")
    if (
        not isinstance(assertion, str)
        or not assertion
        or not isinstance(working, str)
        or not working
        or working not in span
    ):
        return False
    try:
        source = ast.parse("\n".join(lines))
        declared = ast.parse(assertion)
    except (SyntaxError, ValueError, TypeError):
        return False
    if (
        len(declared.body) != 1
        or not isinstance(declared.body[0], ast.Assert)
        or not literal_quantity_comparison(declared.body[0].test, anchor)
    ):
        return False
    return any(
        isinstance(node, ast.Assert)
        and node.end_lineno is not None
        and first <= node.lineno <= node.end_lineno <= last
        and ast.dump(node) == ast.dump(declared.body[0])
        for node in ast.walk(source)
    )


def anchor_gaps(root: Path, row: dict) -> list[str]:
    """Check three distinct scoped anchors without inventing a reference basis."""
    anchors = row.get("anchors", [])
    if (
        not isinstance(anchors, list)
        or len(anchors) < 3
        or not row.get("reference_basis")
    ):
        return [
            "three independent literal anchors and reference basis are not established"
        ]
    quantities = [anchor.get("quantity") for anchor in anchors]
    if not all(quantities) or len(set(quantities)) != len(quantities):
        return ["anchors must address distinct reported quantities"]
    gaps = []
    for anchor in anchors:
        numbers = [anchor.get(key) for key in ("expected", "actual", "tolerance")]
        if not all(finite_number(value) for value in numbers) or numbers[2] < 0:
            gaps.append("anchor values and tolerance must be finite literals")
            continue
        ref = anchor.get("test_ref", "")
        name, valid, _ = reference_info(root, ref)
        cited_tests = {
            reference_info(root, value)[0] for value in row.get("test_refs", [])
        }
        if (
            not valid
            or name not in cited_tests
            or name not in row.get("source_digests", {})
        ):
            gaps.append("anchor test source is unresolved or outside claim evidence")
        if anchor.get("independence") != "hand-worked" or not anchor.get("working"):
            gaps.append("independent hand working is absent")
        elif not anchor_source_valid(root, anchor):
            gaps.append(
                "hand working and literal assertion are not retained in the cited span"
            )
        cases = row.get("probe_result", {}).get("cases", [])
        if not any(
            case.get("quantity") == anchor.get("quantity")
            and case.get("observed") == numbers[1]
            for case in cases
        ):
            gaps.append("anchor is not linked to a reported quantity in probe output")
        if abs(numbers[0] - numbers[1]) > numbers[2]:
            gaps.append("reported number differs from the literal comparator")
    return gaps


def assess(root: Path, row: dict, item: dict) -> dict:
    """Derive disposition while preserving the original human judgment."""
    result = copy.deepcopy(row)
    gaps = list(row.get("missing_evidence", []))
    for key in (
        "claim",
        "criterion",
        "claim_location",
        "observation_time",
        "observation_basis",
    ):
        if not isinstance(row.get(key), str) or not row[key].strip():
            gaps.append(f"missing {key}")
    gaps.extend(reference_gaps(root, row, item))
    state = "SUPPORTED"
    kind = row["claim_kind"]
    if kind in ("behavioral", "numerical_report"):
        state, probe_gaps = probe_state(row)
        gaps.extend(probe_gaps)
    if kind == "numerical_report":
        gaps.extend(anchor_gaps(root, row))
    if kind == "citation_integrity":
        if not item["citations"]:
            gaps.append("no local pack citations discovered")
        elif any(not citation["resolved"] for citation in item["citations"]):
            state = "CONTRADICTED"
        for citation in item["citations"]:
            if citation["path"].endswith("agent-pack.md"):
                gaps.append("pack-to-pack citations are not independent evidence")
            if citation["resolved"] and citation["path"] not in row.get(
                "source_digests", {}
            ):
                gaps.append("citation target digest missing: " + citation["path"])
    if kind == "gate_coverage":
        gaps.append(
            "NOT_RUN: current pull-request test execution is not established by this local audit"
        )
    if state != "CONTRADICTED":
        state = "UNESTABLISHED" if gaps or row["disposition"] != "SUPPORTED" else state
    result["disposition"] = state
    result["missing_evidence"] = sorted(set(gaps))
    result["declared_disposition"] = row["disposition"]
    return summarize_evidence(root, result, row)


def summarize_evidence(root: Path, result: dict, observation: dict) -> dict:
    """Keep full receipts in observations and bounded evidence in summaries."""
    result["evidence_input"] = {
        "family": observation["family"],
        "claim_id": observation["id"],
    }
    excluded = {"program", "stdout", "output", "source_digests"}
    receipt = observation.get("probe_result")
    if isinstance(receipt, dict):
        result["probe_result"] = {
            key: value for key, value in receipt.items() if key not in excluded
        }
    digests = observation.get("source_digests", {})
    result["source_manifest_sha256"] = sha256(
        json.dumps(digests, sort_keys=True).encode()
    )
    direct = {observation.get("pack_path")}
    for field in ("implementation_refs", "test_refs"):
        for ref in observation.get(field, []):
            direct.add(reference_info(root, ref)[0])
    result["source_digests"] = {
        key: value for key, value in digests.items() if key in direct
    }
    return result


def gap_finding(item: dict) -> dict:
    """Keep an unobserved family visible rather than silently omitting it."""
    gaps = ["no current manual observation retained"]
    if not item["pack_path"]:
        gaps.append("pack missing")
    if not item["implementation_paths"]:
        gaps.append("family code missing")
    return {
        "id": "scope_gap",
        "family": item["family"],
        "pack_path": item["pack_path"],
        "claim_kind": "source_presence",
        "claim": "Family acceptance is not established.",
        "criterion": CRITERIA["source_presence"],
        "claim_location": None,
        "implementation_refs": [],
        "test_refs": [],
        "source_digests": {},
        "probe_command": None,
        "probe_result": {
            "status": "NOT_RUN",
            "reason": "No manual observation supplied.",
        },
        "disposition": "UNESTABLISHED",
        "missing_evidence": gaps,
        "observation_time": None,
        "observation_basis": "Discovery only.",
    }


def build_audit(root: Path, document: dict | None = None) -> dict:
    """Build deterministic summaries separately from explicit regeneration input."""
    root = Path(root).resolve()
    document = (
        copy.deepcopy(document)
        if document is not None
        else {"schema_version": 1, "observations": []}
    )
    validate_document(document)
    verify_digests(root, document["observations"])
    items = inventory(root)
    bundles = {bundle["family"]: bundle for bundle in document["observations"]}
    findings: list[dict] = []
    for item in items:
        bundle = bundles.get(item["family"])
        findings.extend(
            assess(root, row, item) for row in bundle["findings"]
        ) if bundle else findings.append(gap_finding(item))
    summary = {
        state: sum(row["disposition"] == state for row in findings)
        for state in DISPOSITIONS
    }
    summary.update(
        qualified_engineering_results=0,
        families=len(FAMILIES),
        packs=sum(bool(item["pack_path"]) for item in items),
    )
    return {
        "schema_version": 1,
        "scope": {
            "families": list(FAMILIES),
            "basis": "Only the retained observation references were inspected; discovery inventories are not reviewed-code coverage.",
        },
        "summary": summary,
        "criteria": CRITERIA,
        "inventory": summarize_inventory(items),
        "findings": findings,
        "observations": document["observations"],
        "revision_history": document.get("revision_history", []),
        "context": document.get("context", {}),
        "limitations": [
            "No blanket engineering PASS or new numerical qualification is granted.",
            "Retained probe receipts are manual observations, not authenticated execution attestations.",
            "Historical finding totals and fix drafts are not evidence of current defects or acceptance.",
            "Licensed documents, solvers, private inputs and other repositories are outside scope.",
            "Independent second-provider review of the frozen report and its sources remains required before merge.",
        ],
    }


def summarize_inventory(items: list[dict]) -> list[dict]:
    """Retain each citation once and full diagnostics for unresolved targets."""
    rows = []
    for item in items:
        row = {key: value for key, value in item.items() if key != "citations"}
        row["citations"] = [citation["reference"] for citation in item["citations"]]
        row["unresolved_citations"] = [
            citation for citation in item["citations"] if not citation["resolved"]
        ]
        rows.append(row)
    return rows


def escaped(value: object) -> str:
    """Escape all observation content for HTML text contexts."""
    return html.escape(str(value), quote=True)


def render_html(report: dict) -> str:
    """Render escaped evidence with the summary and revision history first."""
    escape = escaped
    summary = report["summary"]
    parts = [
        '<!doctype html><html lang="en"><head><meta charset="utf-8">',
        '<meta name="viewport" content="width=device-width,initial-scale=1">',
        "<title>Knowledge-pack acceptance audit</title><style>",
        "body{font:16px/1.5 system-ui,sans-serif;max-width:1100px;margin:2rem auto;padding:0 1rem;color:#172b3a}",
        "table{border-collapse:collapse;width:100%}td,th{border:1px solid #b9c6cd;padding:.5rem;text-align:left;vertical-align:top}",
        "pre{white-space:pre-wrap;overflow-wrap:anywhere;background:#f1f5f7;padding:1rem}h2{margin-top:2rem}",
        "</style></head><body><h1>Knowledge-pack acceptance audit</h1><h2>Summary</h2>",
        "<p>"
        + escape(
            f"{summary['families']} families; {summary['packs']} local packs; "
            + "; ".join(f"{state}: {summary[state]}" for state in DISPOSITIONS)
        )
        + ".</p>",
        "<p>Qualified engineering results: 0. SUPPORTED applies only to the stated criterion and inspected scope.</p>",
        "<h2>Revision history</h2><table><tr><th>Revision</th><th>Date</th><th>Change and reason</th></tr>",
    ]
    for row in report["revision_history"]:
        parts.append(
            "<tr>"
            + "".join(
                "<td>" + escape(row.get(key, "")) + "</td>"
                for key in ("revision", "date", "change")
            )
            + "</tr>"
        )
    parts.extend(["</table><h2>Criteria</h2>"])
    for kind, criterion in report["criteria"].items():
        parts.append(
            "<p><strong>" + escape(kind) + "</strong>: " + escape(criterion) + "</p>"
        )
    parts.append("<h2>Findings</h2>")
    for row in report["findings"]:
        parts.append(
            "<article><h3>"
            + escape(f"{row['family']}: {row['id']} — {row['disposition']}")
            + "</h3>"
        )
        parts.append(
            "<pre>"
            + escape(json.dumps(row, indent=2, sort_keys=True, allow_nan=False))
            + "</pre></article>"
        )
    parts.append("<h2>Unresolved engineering basis</h2><ul>")
    parts.extend("<li>" + escape(limit) + "</li>" for limit in report["limitations"])
    parts.append(
        "</ul><h2>Inspected scope and context</h2><pre>"
        + escape(
            json.dumps(
                {"scope": report["scope"], "context": report["context"]},
                indent=2,
                sort_keys=True,
            )
        )
        + "</pre></body></html>\n"
    )
    return "\n".join(parts)


def validate_output(root: Path, out_dir: Path, document: dict) -> None:
    """Reject output aliases and source-tree destinations before any write."""
    absolute = out_dir.absolute()
    if any(path.is_symlink() for path in (absolute, *absolute.parents)):
        raise ValueError("output path must not contain symlinks")
    resolved = absolute.resolve()
    if resolved == root or any(
        resolved.is_relative_to(root / directory)
        for directory in ("src", "tests", "scripts", ".git")
    ):
        raise ValueError("output directory overlaps source scope")
    for name in (JSON_NAME, HTML_NAME):
        target = resolved / name
        if target.is_symlink() or (target.exists() and not target.is_file()):
            raise ValueError("output target is not a regular file")
    for bundle in document.get("observations", []):
        for row in bundle["findings"]:
            if any(
                (root / name).resolve().is_relative_to(resolved)
                for name in row.get("source_digests", {})
            ):
                raise ValueError("output directory contains observation sources")


def generate(root: Path, out_dir: Path, document: dict | None = None) -> dict:
    """Stage and read back both outputs before replacing either saved artifact."""
    root, out_dir = Path(root).resolve(), Path(out_dir)
    document = (
        document if document is not None else {"schema_version": 1, "observations": []}
    )
    report = build_audit(root, document)
    validate_output(root, out_dir, document)
    encoded = json.dumps(report, indent=2, sort_keys=True, allow_nan=False) + "\n"
    page = render_html(report)
    out_dir.mkdir(parents=True, exist_ok=True)
    originals = {
        name: (out_dir / name).read_bytes() if (out_dir / name).exists() else None
        for name in (JSON_NAME, HTML_NAME)
    }
    with tempfile.TemporaryDirectory(prefix=".pack-audit-", dir=out_dir) as staging:
        stage = Path(staging)
        (stage / JSON_NAME).write_text(encoded, encoding="utf-8")
        (stage / HTML_NAME).write_text(page, encoding="utf-8")
        if (
            json.loads((stage / JSON_NAME).read_text()) != report
            or (stage / HTML_NAME).read_text() != page
        ):
            raise ValueError("output readback differs")
        replaced = []
        try:
            for name in (JSON_NAME, HTML_NAME):
                os.replace(stage / name, out_dir / name)
                replaced.append(name)
        except OSError:
            for name in replaced:
                original = originals[name]
                if original is not None:
                    (out_dir / name).write_bytes(original)
                else:
                    (out_dir / name).unlink()
            raise
    return report


def main(argv: list[str] | None = None) -> int:
    """Read declared observations before writing to the requested directory."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path, default=Path("."))
    parser.add_argument("--out-dir", type=Path, required=True)
    parser.add_argument("--observations", type=Path)
    args = parser.parse_args(argv)
    try:
        document = (
            json.loads(args.observations.read_text(encoding="utf-8"))
            if args.observations
            else None
        )
        report = generate(args.root, args.out_dir, document)
    except (ValueError, OSError, TypeError) as exc:
        parser.exit(2, f"Audit refused: {exc}\n")
    print(json.dumps(report["summary"], sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
