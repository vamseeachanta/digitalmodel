# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Build fixture hygiene evidence from pytest receipts and a static test audit.

No solver interfaces are loaded. Full receipts remain outside the repository;
only test counts, relative locators and sanitized reasons are retained.
"""

from __future__ import annotations

import argparse
import hashlib
import html
import importlib.util
import json
import re
import xml.etree.ElementTree as ET
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
DESTINATION = ROOT / "docs/reports/modules/diffraction/fixture-paths"
SPEC = importlib.util.spec_from_file_location(
    "fixture_audit", ROOT / "scripts/testing/audit_fixture_paths.py",
)
AUDIT = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(AUDIT)
CHANGES = [{"revision": "A", "date": "2026-10-22", "change":
            "Record repository-relative fixture rules, measured coverage and remaining limits."}]


def sanitize(value: str) -> str:
    """Remove location strings from public test reasons, retaining no identities."""
    value = re.sub(r"[A-Za-z]:[\\/][^\s'\"]*", "[path removed]", value)
    return re.sub(r"/(?:home|mnt|Users)/[^\s'\"]*", "[path removed]", value)


def receipt(path: Path) -> dict:
    """Derive counts from individual JUnit cases, including expected failures."""
    cases = ET.parse(path).getroot().findall(".//testcase")
    counts = dict.fromkeys(("passed", "failed", "errors", "skipped", "xfailed"), 0)
    skips, failures = [], []
    for case in cases:
        skipped = case.find("skipped")
        if skipped is not None:
            expected = skipped.get("type") == "pytest.xfail"
            counts["xfailed" if expected else "skipped"] += 1
            if not expected:
                skips.append({"test": case.get("classname") + "::" + case.get("name"),
                              "reason": sanitize(skipped.get("message", ""))})
        elif case.find("failure") is not None:
            counts["failed"] += 1
            failures.append(case.get("classname") + "::" + case.get("name"))
        elif case.find("error") is not None:
            counts["errors"] += 1
        else:
            counts["passed"] += 1
    return {"counts": counts, "not_evaluated": skips, "failed_tests": failures,
            "sha256": hashlib.sha256(path.read_bytes()).hexdigest()}


def table(rows: list[dict], fields: tuple[str, ...]) -> str:
    """Render a compact HTML table without trusting source strings as markup."""
    header = "".join(f"<th>{html.escape(field)}</th>" for field in fields)
    body = "".join("<tr>" + "".join(
        f"<td>{html.escape(str(row[field]))}</td>" for field in fields
    ) + "</tr>" for row in rows)
    return f"<table><thead><tr>{header}</tr></thead><tbody>{body}</tbody></table>"


def render(evidence: dict) -> str:
    """Present measured counts, the scan table and explicit unevaluated cases."""
    counts = [{"run": label, **evidence[label]["counts"]} for label in ("before", "after")]
    tracked_guards = [row for row in evidence["audit_before"]["skip_guards"]
                      if row["tracked"] == "yes"]
    external = [row for row in evidence["audit_after"]["skip_guards"]
                if row["tracked"] == "no"]
    metrics = table([{"metric": key, "count": value}
                     for key, value in evidence["metrics"].items()], ("metric", "count"))
    revisions = table(CHANGES, ("revision", "date", "change"))
    coverage = table(counts, ("run", "passed", "failed", "errors", "skipped", "xfailed"))
    scan = table(tracked_guards, ("file", "line", "path", "tracked"))
    external_table = table(external, ("file", "line", "path", "tracked", "reason"))
    omitted = table(evidence["after"]["not_evaluated"], ("test", "reason"))
    return f'''<!doctype html>
<html lang="en"><meta charset="utf-8"><title>Diffraction fixture path analysis</title>
<style>body{{font-family:system-ui;max-width:90rem;margin:2rem auto;padding:1rem}}
table{{border-collapse:collapse;width:100%;font-size:.85rem}}th,td{{border:1px solid #bbb;
padding:.4rem;text-align:left;overflow-wrap:anywhere}}h2{{margin-top:2rem}}</style>
<h1>Diffraction fixture path analysis</h1>
<h2>Summary</h2>
<p>Tracked fixtures resolve relative to the repository. Their absence is a test
failure. Optional dependencies and external inputs retain explicit qualifications.
This is a text-parser and test-path analysis; physical solver results are NOT_EVALUATED.</p>
<h2>Revision history</h2>{revisions}
<h2>Rule and worked example</h2>
<p>The reference test resolves from its own file and changes to an empty or decoy
working directory. The YAML is parsed as text. The unmodified partial-mesh symmetry
is refused; a temporary copy with only that declaration changed checks properties.
No mesh qualification is inferred. The test docstring cites YAML lines for depth,
density, mass, body name, mesh reference and all periods and headings. Angular
frequency is independently calculated as one cycle of radians divided by period.</p>
<h2>Measured folder runs</h2>{coverage}
<p>The before run uses the available main reference, where the original lookup
repair already exists. These counts must not be interpreted as a newly eliminated
missing-fixture skip. The run receipts are identified by digest in evidence.json;
full logs remain external. Expected failures are shown separately from skips.
The command-line entry point is absent and existing Polars assertions assume an
older fixture grid. The before run also lacks source imports in a child interpreter;
the after run explicitly supplies them. Counts are descriptive, not a claim that
all folder tests pass. Failed test identifiers are retained in evidence.json.</p>
<h2>Fixture acceptance counts</h2>{metrics}
<h2>Scan table</h2>
<p>Each row below is an existence-based skip on a tracked input found before the
change. All such statically resolved guards are removed. Existing assertions are
retained. The three stale CSV checkout locations are also replaced by unique
repository-relative suffix discovery. The complete sanitized absolute-literal
inventory, including untracked synthetic and external locations, is in evidence.json.</p>
{scan}
<h2>NOT_EVALUATED</h2>{omitted}
<p>The following remaining guards refer to untracked generated or external inputs.
Their precise existing reasons are retained in the tests; those suites are outside
the diffraction folder measurement. They are follow-up inputs, never substituted.</p>
{external_table}
<h2>Known limits and way forward</h2>
<p>The audit resolves simple pathlib assignments and existence conditions without
executing tests. Dynamic fixtures, helper-returned paths and arbitrary expression
syntax require manual review; absence from the table is not proof of coverage.
Absolute literals are matched by path suffix, not by content identity. Synthetic
configuration strings are distinguished from fixture reads by review. The diffraction
guard has only exact, commented configuration-value exceptions. Review the broader
unmatched inventory before extending that guard to other modules. Optional mesh
packages and the command-line entry point must be supplied to evaluate those cases.
The legacy automation file listed under syntax_errors cannot be parsed by this audit.</p>
<h2>Rebuild</h2>
<p>Run scripts/reporting/build_diffraction_fixture_report.py with --before and --after
JUnit XML receipts, --base set to the tested reference and --output set to this
report directory. Use --check for a read-only rebuild check. Counts come exclusively
from the receipts; no result number is transcribed into this builder.</p>
</html>
'''


def build(before: Path, after: Path, base: str) -> dict:
    """Bind measurements to their reference and statically audited test sources."""
    evidence = {"base": base, "before": receipt(before), "after": receipt(after),
                "audit_before": AUDIT.inventory(ROOT, base), "audit_after": AUDIT.inventory(ROOT)}
    evidence["metrics"] = {
        "remaining tracked fixture path mismatches": sum(
            row["tracked"] == "yes" for row in evidence["audit_after"]["absolute_literals"]
        ),
        "remaining statically resolved tracked fixture skip guards": sum(
            row["tracked"] == "yes" for row in evidence["audit_after"]["skip_guards"]
        ),
        "diffraction missing-fixture skips": sum(
            bool(re.search(r"fixture|reference|test data|committed", row["reason"], re.I))
            for row in evidence["after"]["not_evaluated"]
        ),
    }
    return evidence


def write_report(evidence: dict, output: Path, check: bool = False) -> None:
    """Write or compare the generated report, evidence and revision record."""
    files = {"index.html": render(evidence),
             "evidence.json": json.dumps(evidence, indent=2) + "\n",
             "changes.json": json.dumps(CHANGES, indent=2) + "\n"}
    if check:
        for name, content in files.items():
            if (output / name).read_text(encoding="utf-8") != content:
                raise ValueError(f"Stale generated fixture report: {name}")
    else:
        output.mkdir(parents=True, exist_ok=True)
        for name, content in files.items():
            (output / name).write_text(content, encoding="utf-8")


def main() -> None:
    """Generate evidence only from explicit pytest receipts and a tested base."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--before", type=Path, required=True)
    parser.add_argument("--after", type=Path, required=True)
    parser.add_argument("--base", required=True)
    parser.add_argument("--output", type=Path, default=DESTINATION)
    parser.add_argument("--check", action="store_true")
    args = parser.parse_args()
    write_report(build(args.before, args.after, args.base), args.output, args.check)


if __name__ == "__main__":
    main()
