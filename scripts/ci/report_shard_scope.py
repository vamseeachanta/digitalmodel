#!/usr/bin/env python3
"""Report the shard scope of a Quality Gates by Domain run (issue #1863).

Domain gating means an untouched shard is never dispatched, so a green run can
mean "the shards we ran passed" rather than "all shards passed". This script
makes that distinction explicit: it compares the matrix the run will execute
against the full matrix and reports the difference.

Inputs (environment variables):
  SELECTED_MATRIX  JSON emitted by detect_touched_domains.py for this run.
  FULL_MATRIX      JSON emitted by detect_touched_domains.py --mode full.
  SCOPE_EVENT      Optional event name, used only in the human-readable text.

Outputs:
  GITHUB_OUTPUT        selected-count, total-count, skipped-count,
                       skipped-domains, scope-line
  GITHUB_STEP_SUMMARY  a markdown block stating the scope of the run

This is reporting only and never gates: it exits 0 unless its own inputs are
unparseable, and it deliberately makes no judgement about whether skipping is
acceptable.
"""

from __future__ import annotations

import json
import os
import sys


def domain_names(raw: str) -> list[str]:
    """Extract ordered domain names from a detect_touched_domains.py matrix."""
    payload = json.loads(raw)
    include = payload.get("include", [])
    return [str(entry["domain"]) for entry in include if "domain" in entry]


def scope_line(selected: list[str], total: list[str], event: str) -> str:
    skipped = [name for name in total if name not in set(selected)]
    if not total:
        return "No test domains are defined; shard scope is empty."
    if not skipped:
        return f"Shard scope: all {len(total)} shards dispatched."
    return (
        f"Shard scope: {len(selected)} of {len(total)} shards dispatched"
        f" ({len(skipped)} not run) for event '{event}'."
    )


def summary_markdown(selected: list[str], total: list[str], event: str) -> str:
    skipped = [name for name in total if name not in set(selected)]
    lines = [
        "## Domain shard scope",
        "",
        scope_line(selected, total, event),
        "",
    ]
    if skipped:
        lines += [
            "A green result on this run means **the dispatched shards passed**,",
            "not that every shard passed. The shards below were not executed",
            "here; they run in full on every push to `main` and nightly via",
            "`.github/workflows/full-matrix-sweep.yml`.",
            "",
            f"<details><summary>Shards not run ({len(skipped)})</summary>",
            "",
        ]
        lines += [f"- `tests-{name}`" for name in skipped]
        lines += ["", "</details>", ""]
    if selected:
        lines += [
            f"<details><summary>Shards dispatched ({len(selected)})</summary>",
            "",
        ]
        lines += [f"- `tests-{name}`" for name in selected]
        lines += ["", "</details>", ""]
    return "\n".join(lines)


def write_outputs(path: str | None, pairs: dict[str, str]) -> None:
    if not path:
        for key, value in pairs.items():
            print(f"{key}={value}")
        return
    with open(path, "a", encoding="utf-8") as handle:
        for key, value in pairs.items():
            handle.write(f"{key}={value}\n")


def append_summary(path: str | None, text: str) -> None:
    if not path:
        print(text)
        return
    with open(path, "a", encoding="utf-8") as handle:
        handle.write(text + "\n")


def main() -> int:
    try:
        selected = domain_names(os.environ.get("SELECTED_MATRIX", '{"include":[]}'))
        total = domain_names(os.environ.get("FULL_MATRIX", '{"include":[]}'))
    except (ValueError, TypeError, KeyError) as exc:
        print(
            f"report_shard_scope.py: unparseable matrix input: {exc}", file=sys.stderr
        )
        return 1

    event = os.environ.get("SCOPE_EVENT", "unknown")
    skipped = [name for name in total if name not in set(selected)]
    line = scope_line(selected, total, event)

    write_outputs(
        os.environ.get("GITHUB_OUTPUT"),
        {
            "selected-count": str(len(selected)),
            "total-count": str(len(total)),
            "skipped-count": str(len(skipped)),
            "skipped-domains": ",".join(skipped),
            "scope-line": line,
        },
    )
    append_summary(
        os.environ.get("GITHUB_STEP_SUMMARY"),
        summary_markdown(selected, total, event),
    )
    print(line)
    if skipped:
        print("Shards not run: " + ", ".join(f"tests-{name}" for name in skipped))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
