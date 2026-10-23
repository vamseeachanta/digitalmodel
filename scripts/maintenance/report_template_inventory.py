# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Inventory committed HTML copies of the shared demo report template.

Run from the repository root:
    python scripts/maintenance/report_template_inventory.py
    python scripts/maintenance/report_template_inventory.py --check

After inspecting intentional template or consumer edits, refresh the snapshot:
    python scripts/maintenance/report_template_inventory.py > config/report_template_consumers.json

The first command prints JSON for review; the second compares it with the
committed manifest. Neither command writes files or executes report builders.
The census uses every tracked HTML path, including manually maintained pages.
Hashes record working-tree bytes with CRLF normalized to LF, so Git checkout
line-ending conversion does not cause drift. Other edits, including unstaged
edits to the template or any matching HTML copy, intentionally fail --check
and the repository snapshot test until the inspected snapshot is refreshed.
Hashes do not prove derivation or regeneration.

Scope is deliberately one template family. Matching literal CSS markers is
discovery evidence, not proof that all other template families or heavily
rewritten copies have been found. A marker change requires signature review.
Freshness remains NOT_EVALUATED until reproducible rendering is available.
Later slices can register other families and producer/copy relationships.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import os
import subprocess
import sys
from pathlib import Path

if __package__:
    from scripts.generated_html_ownership import MANUAL_PAGES
else:
    # Direct execution starts with this directory on the import path.
    sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
    from generated_html_ownership import MANUAL_PAGES


REPO = Path(__file__).resolve().parents[2]
TEMPLATE = "examples/demos/gtm/report_template.py"
MANIFEST = "config/report_template_consumers.json"
# Evidence source: TEMPLATE, _CSS definition (repository revision).
# These selectors survive the promotional-block retirement; no CSS is executed.
MARKERS = ("--clr-primary:", ".report-header .case-badge", ".report-footer")


def tracked_html_paths(repo: Path) -> list[str]:
    """Return tracked HTML names, independent of the working directory."""
    env = {key: value for key, value in os.environ.items()
           if key not in {"GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR", "GIT_INDEX_FILE"}}
    result = subprocess.run(
        ["git", "ls-files", "-z"], cwd=repo, env=env, check=True, capture_output=True,
    )
    names = (os.fsdecode(name) for name in result.stdout.split(b"\0") if name)
    return sorted(name for name in names if Path(name).suffix.casefold() == ".html")


def build_inventory(repo: Path) -> dict:
    """Record one template and matching tracked consumers without rendering."""
    source = (repo / TEMPLATE).read_bytes().replace(b"\r\n", b"\n")
    if not all(marker in source.decode("utf-8") for marker in MARKERS):
        raise ValueError("template evidence markers changed; review the signature")
    consumers = []
    for relative in sorted(tracked_html_paths(repo)):
        content = (repo / relative).read_bytes().replace(b"\r\n", b"\n")
        text = content.decode("utf-8", errors="replace")
        if all(marker in text for marker in MARKERS):
            consumers.append({
                "path": relative,
                "sha256": hashlib.sha256(content).hexdigest(),
                "page_classification": (
                    "manual" if relative in MANUAL_PAGES else "not_classified_as_manual"
                ),
            })
    return {
        "template": TEMPLATE,
        "template_sha256": hashlib.sha256(source).hexdigest(),
        "evidence_markers": list(MARKERS),
        "classification_source": "scripts/generated_html_ownership.py:MANUAL_PAGES",
        "freshness": "NOT_EVALUATED",
        "consumers": consumers,
    }


def main(argv: list[str] | None = None) -> int:
    """Print the census or check the committed snapshot without modifying it."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--repo", type=Path, default=REPO)
    parser.add_argument("--check", action="store_true")
    args = parser.parse_args(argv)
    try:
        current = build_inventory(args.repo)
        if not args.check:
            print(json.dumps(current, indent=2))
            return 0
        recorded = json.loads((args.repo / MANIFEST).read_text(encoding="utf-8"))
    except (OSError, ValueError, subprocess.CalledProcessError) as error:
        print(f"inventory check failed: {error}", file=sys.stderr)
        return 1
    if recorded != current:
        print("inventory differs; inspect template and consumers, then review a new snapshot",
              file=sys.stderr)
        return 1
    print("inventory matches; rendering freshness is NOT_EVALUATED")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
