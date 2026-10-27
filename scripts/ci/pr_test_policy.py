# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Select CI harness checks and trace touched-domain PR jobs."""

from __future__ import annotations

import argparse
import json
from pathlib import Path
import subprocess

from detect_touched_domains import (
    git_changed_files, git_pinned_domains, parse_domains, touched_domains,
)


ROOT = Path(__file__).resolve().parents[2]
HARNESS_PREFIXES = (".github/workflows/", "scripts/ci/", "tests/scripts/",
                    "tests/workflows/automation/", "scripts/enforcement/")
HARNESS_FILES = {
    ".claude/quality-gates.yaml", "pyproject.toml", "uv.lock", "pytest.ini",
    "conftest.py", "tests/conftest.py", "tests/DOMAINS.md",
    "src/digitalmodel/workflows/automation/quality_gates.py",
    "tests/workflows/test_ci_triggers.py", "tests/workflows/test_durable_workflows.py",
    "tests/contracts/test_flaky_register_schema.py",
    "tests/docs/test_implementation_map.py", "tests/docs/test_implementation_map_search.py",
    "scripts/generate_implementation_map.py", "scripts/implementation_map_search.py",
    ".github/ci/flaky-register.yml",
    "tests/marine_ops/marine_engineering/visualization/test_no_regression_traces.py",
}


def needs_harness(paths: list[str]) -> bool:
    """Run the CI harness when its code, checks or shared configuration changes."""
    return any(path in HARNESS_FILES or path.startswith(HARNESS_PREFIXES) for path in paths)


def trace(paths: list[str], *, base: str = "HEAD", head: str = "HEAD",
          pinned_domains: dict[str, set[str]] | None = None) -> dict:
    """Describe PR jobs for supplied paths using the production domain routes.

    Explicit routes include known cross-domain dependents. Shared source and
    configuration without narrower ownership retain the detector's full-matrix
    fallback. Record pins from both revisions use the production detector.
    """
    domains = parse_domains(ROOT / "tests/DOMAINS.md")
    pins_available = True
    if pinned_domains is None:
        pinned_domains = {}
        try:
            for ref in dict.fromkeys((base, head)):
                for path, names in git_pinned_domains(ref, domains).items():
                    pinned_domains.setdefault(path, set()).update(names)
        except (subprocess.CalledProcessError, OSError):
            pins_available = False
    selected = touched_domains(paths, domains, base=base, head=head,
                               pinned_domains=pinned_domains) if pins_available else domains
    harness = needs_harness(paths)
    jobs = ["Detect touched test domains", "Domain test aggregate", "Repository structure contract"]
    if harness:
        jobs.append("CI harness and compliance tests")
    jobs.extend(f"tests-{domain.name}" for domain in selected)
    return {"paths": paths, "domains": [domain.name for domain in selected],
            "harness": harness, "jobs": jobs, "record_pins_checked": pins_available,
            "required": ["Domain test aggregate", "Repository structure contract"]}


def main() -> None:
    """Print a harness decision or a dry trace for a supplied diff."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--base")
    parser.add_argument("--head")
    parser.add_argument("--paths", nargs="+")
    parser.add_argument("--dry-trace", action="store_true")
    args = parser.parse_args()
    if args.paths is None and not (args.base and args.head):
        parser.error("provide --paths or both --base and --head")
    paths = args.paths if args.paths is not None else git_changed_files(args.base, args.head)
    print(json.dumps(trace(paths, base=args.base or "HEAD", head=args.head or "HEAD"))
          if args.dry_trace else str(needs_harness(paths)).lower())


if __name__ == "__main__":
    main()
