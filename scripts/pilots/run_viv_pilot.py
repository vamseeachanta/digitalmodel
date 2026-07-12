#!/usr/bin/env python
# ABOUTME: Runnable dry-run of the digitalmodel#1505 synthetic VIV -> Hugging Face pilot.
# ABOUTME: InMemoryHfPort only (NO network, NO real publish). Exits 0 on a green run.
"""Run the FULL #1505 VIV publication pipeline as a dry-run (InMemoryHfPort).

Usage::

    uv run --python 3.11 --with huggingface_hub python scripts/pilots/run_viv_pilot.py [ECHO_DIR] [--emit-report]

Publishes NOTHING to Hugging Face. Drives >=3 synthetic ``viv-parametric-screening``
variations + 1 exact replay through the assetutilities publication promotion machine
against an in-memory port + a source-repo ledger, writes the rolling HTML report + the
publications.jsonl ledger under ECHO_DIR (default: a temp dir), then prints a summary
and exits 0. By default a dry-run mutates NOTHING tracked; pass ``--emit-report`` to
also refresh the committed report at
``docs/reports/viv-parametric-screening/index.html``. The real ``HuggingFaceHubHfPort``
swap is a one-line change left for a gated live run.
"""

from __future__ import annotations

import sys
import tempfile
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))

import viv_pilot  # noqa: E402

REPO_ROOT = Path(__file__).resolve().parents[2]
COMMITTED_REPORT = REPO_ROOT / "docs" / "reports" / "viv-parametric-screening" / "index.html"


def main(argv: list[str]) -> int:
    args = argv[1:]
    emit_report = "--emit-report" in args
    positional = [a for a in args if not a.startswith("--")]
    echo_dir = Path(positional[0]) if positional else Path(tempfile.mkdtemp(prefix="viv_pilot_"))
    summary = viv_pilot.run_pilot(echo_dir=echo_dir)

    # A plain dry-run must NOT mutate the tracked report; only --emit-report refreshes
    # the committed rolling report (deterministic HTML).
    if emit_report:
        COMMITTED_REPORT.parent.mkdir(parents=True, exist_ok=True)
        COMMITTED_REPORT.write_text(summary["report_html"], encoding="utf-8")

    ok = (
        summary["replay_same_run_id"]
        and summary["replay_output_equality_ok"]
        and summary["replay_already_published"]
        and summary["accepted_count"] == len(viv_pilot.PUBLISHED_ORDER)
    )

    print("=" * 72)
    print("digitalmodel #1505 -- synthetic VIV parametric HF pilot (DRY-RUN)")
    print("=" * 72)
    print(f"dataset repo (target, NOT published) : {summary['dataset_repo']}")
    print(f"accepted count (ledger eligibility)  : {summary['accepted_count']}")
    print("run_ids + in-memory dataset revisions + A/D:")
    for name in viv_pilot.PUBLISHED_ORDER:
        print(f"  {name:<13} run_id={summary['run_ids'][name]}")
        print(f"  {'':<13} rev   ={summary['revisions'][name]}  A/D={summary['a_d'][name]}")
    print("exact replay of C-lockin:")
    print(f"  same run_id           : {summary['replay_same_run_id']}")
    print(f"  output_equality passes : {summary['replay_output_equality_ok']}")
    print(f"  already published      : {summary['replay_already_published']} (identity dedup)")
    print(f"echo dir     : {echo_dir}")
    print(f"report path  : {summary['report_path']}")
    if emit_report:
        print(f"committed report refreshed: {COMMITTED_REPORT}")
    else:
        print("committed report: UNCHANGED (pass --emit-report to refresh the tracked file)")
    print("publications.jsonl ledger (sole eligibility authority):")
    for line in summary["ledger_jsonl"].splitlines():
        print(f"  {line}")
    print("=" * 72)
    print("RESULT:", "GREEN (exit 0)" if ok else "RED (exit 1)")
    return 0 if ok else 1


if __name__ == "__main__":
    raise SystemExit(main(sys.argv))
