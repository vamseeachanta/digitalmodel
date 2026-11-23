#!/usr/bin/env python3
"""Report provenance ratchet (#1019), using only the standard library.

Usage: python scripts/enforcement/check-report-provenance.py [--root REPO]
Exit 0: clean; exit 1: uncontracted report modules; exit 2: tool/input error.
Scans tracked src/digitalmodel Python modules whose stems contain "report",
never tests or scripts. Adoption means importing an approved symbol; use of
that symbol is not established. Modules named render.py or writer.py are outside
this naming heuristic. Grandfathered modules live in report-provenance-allowlist.txt
and must belong to the frozen legacy roster below; stale exemptions fail. A trailing
``# report-lint: allowed`` exempts only the annotated HTML-producing evidence.
This heuristic checks adoption, not literal-result correctness. Runtime read
contracts are enforced by assemble_report, not by static literal detection.
"""

import argparse
import ast
import io
import os
import subprocess
import sys
import tokenize
from pathlib import Path

APPROVED = {"assemble_report", "ReportSkeleton", "Provenance"}
ALLOWLIST = Path("scripts/enforcement/report-provenance-allowlist.txt")
ESCAPE = "# report-lint: allowed"
# Initial 37 violations at the reviewed #1019 head. This roster is a ceiling,
# not an exemption: the active allowlist may only shrink as modules migrate.
# Keep it independent of the editable allowlist so adding an exemption fails
# the checker itself, including in shallow or history-free fixture checkouts.
FROZEN_LEGACY = frozenset(
    """
src/digitalmodel/ansys/report_generator.py
src/digitalmodel/asset_integrity/assessment/ffs_report.py
src/digitalmodel/asset_integrity/assessment/screening_report.py
src/digitalmodel/asset_integrity/riser_joint_report.py
src/digitalmodel/cathodic_protection/cp_reporting.py
src/digitalmodel/cathodic_protection/ecda_reporting.py
src/digitalmodel/floating_wind/economics_report.py
src/digitalmodel/floating_wind/report.py
src/digitalmodel/hydrodynamics/diffraction/report_builders_header.py
src/digitalmodel/hydrodynamics/diffraction/report_builders_hydrostatics.py
src/digitalmodel/hydrodynamics/diffraction/report_builders_responses.py
src/digitalmodel/hydrodynamics/passing_ship/benchmark_report.py
src/digitalmodel/hydrodynamics/passing_ship/preparation_report.py
src/digitalmodel/infrastructure/base_solvers/benchmarks/report_generator.py
src/digitalmodel/marine_ops/artificial_lift/dynacard/report_sections.py
src/digitalmodel/marine_ops/fowt_quasi_static/report.py
src/digitalmodel/marine_ops/installation/suitability_report.py
src/digitalmodel/marine_ops/marine_analysis/profiling/optimization_report.py
src/digitalmodel/marine_ops/marine_analysis/rao_quality_report.py
src/digitalmodel/marine_ops/marine_analysis/reporting/ocimf_interactive_report.py
src/digitalmodel/solvers/benchmark/report.py
src/digitalmodel/solvers/orcaflex/modular_input_validation/reporters/html_reporter.py
src/digitalmodel/solvers/orcaflex/mooring_analysis/comprehensive_analysis/report_generator.py
src/digitalmodel/solvers/orcaflex/reporting/report_generator.py
src/digitalmodel/solvers/orcaflex/universal/status_reporter.py
src/digitalmodel/structural/analysis/wall_thickness_mt_report.py
src/digitalmodel/structural/fatigue/design_code_report.py
src/digitalmodel/structural/fatigue/sn_comparison_report.py
src/digitalmodel/structural/parametric_report.py
src/digitalmodel/subsea/catenary_riser/report.py
src/digitalmodel/visualization/reporting/report_generator.py
src/digitalmodel/workflows/installation_forecast_report.py
src/digitalmodel/workflows/installation_full_report.py
src/digitalmodel/workflows/installation_partial_report.py
src/digitalmodel/workflows/installation_seastate_report.py
src/digitalmodel/workflows/vessel_capability_report.py
src/digitalmodel/workflows/workflow_automation/reporter.py
""".split()
)


def exemptions(source):
    """Recognize actual trailing comments, never text inside string literals."""
    return {
        token.start[0]
        for token in tokenize.generate_tokens(io.StringIO(source).readline)
        if token.type == tokenize.COMMENT and token.string.rstrip().endswith(ESCAPE)
    }


def html_evidence(tree, source):
    lines = source.splitlines()
    for node in ast.walk(tree):
        if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef)):
            if node.name.endswith("_html") or (
                node.name.startswith("generate_") and "report" in node.name
            ):
                yield node.lineno
        if not isinstance(node, ast.Call):
            continue
        name = getattr(node.func, "attr", getattr(node.func, "id", ""))
        if name not in {
            "write",
            "open",
            "write_text",
            "write_html",
            "to_html",
        }:
            continue
        # Include nearby assignments such as path = out / "report.html".
        nearby = "\n".join(lines[max(0, node.lineno - 6) : node.end_lineno + 5])
        try:
            tokens = tokenize.generate_tokens(io.StringIO(nearby).readline)
            has_html = any(
                token.type == tokenize.STRING and ".html" in token.string.lower()
                for token in tokens
            )
        except (IndentationError, tokenize.TokenError):
            has_html = any(
                isinstance(item, ast.Constant)
                and isinstance(item.value, str)
                and ".html" in item.value.lower()
                for item in ast.walk(node)
            )
        if has_html:
            yield node.lineno


def compliant(tree):
    return any(
        isinstance(node, ast.ImportFrom)
        and node.module is not None
        and (
            node.module == "digitalmodel.reporting"
            or node.module.startswith("digitalmodel.reporting.")
        )
        and any(alias.name in APPROVED for alias in node.names)
        for node in ast.walk(tree)
    )


def check(root):
    allowlist = root / ALLOWLIST
    allowed = (
        {
            entry
            for line in allowlist.read_text(encoding="utf-8").splitlines()
            if (entry := line.split("#", 1)[0].strip())
        }
        if allowlist.exists()
        else set()
    )
    paths = subprocess.run(
        [
            "git",
            "ls-files",
            "-z",
            "src/digitalmodel/**/*.py",
            "src/digitalmodel/*.py",
        ],
        cwd=root,
        check=True,
        capture_output=True,
        text=True,
        env={
            key: value
            for key, value in os.environ.items()
            if key
            not in {"GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR", "GIT_INDEX_FILE"}
        },
    ).stdout.split("\0")
    uncontracted = {}
    for name in sorted(set(filter(None, paths))):
        path = Path(name)
        if "report" not in path.stem.lower():
            continue
        source = (root / path).read_text(encoding="utf-8")
        try:
            tree = ast.parse(source)
            if not compliant(tree):
                escaped = exemptions(source)
                evidence = sorted(set(html_evidence(tree, source)) - escaped)
                if evidence:
                    uncontracted[name] = evidence[0]
        except (SyntaxError, tokenize.TokenError) as exc:
            raise ValueError(f"Cannot parse tracked report module {name}") from exc
    violations = [
        (name, line, "HTML reporter must import shared provenance (#1019)")
        for name, line in sorted(uncontracted.items())
        if name not in allowed
    ]
    for name in sorted(allowed):
        if name not in FROZEN_LEGACY:
            violations.append(
                (name, 1, "allowlist entry not in frozen legacy roster (#1019)")
            )
        elif name not in uncontracted:
            violations.append(
                (name, 1, "stale allowlist entry; remove exemption (#1019)")
            )
    return violations


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--root", type=Path, default=Path(__file__).resolve().parents[2]
    )
    args = parser.parse_args()
    try:
        violations = check(args.root)
    except (OSError, ValueError, subprocess.CalledProcessError) as exc:
        print(
            f"check-report-provenance: {type(exc).__name__}: scan failed",
            file=sys.stderr,
        )
        return 2
    for name, line, reason in violations:
        print(f"{name}:{line}: {reason}")
    if violations:
        print(
            f"check-report-provenance: {len(violations)} violation(s)",
            file=sys.stderr,
        )
    return int(bool(violations))


if __name__ == "__main__":
    sys.exit(main())
