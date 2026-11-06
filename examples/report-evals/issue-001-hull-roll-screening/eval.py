# Copyright (c) 2026 AceEngineer. Licensed under the MIT License.
"""Eval: generic hull roll reduction with a U-tube anti-roll tank (screening estimate).

Rebuilds the peak roll reduction results quoted in newsletter issue 1 and on
the study page, and compares them with results.json. Standard library only:
the builder script and the public U-tube model are loaded from the clone.

    python examples/report-evals/issue-001-hull-roll-screening/eval.py
    python examples/report-evals/issue-001-hull-roll-screening/eval.py --compare my-results.json
"""

from __future__ import annotations

import importlib.util
import json
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parent))
import evalkit  # noqa: E402

REPO_ROOT = HERE.parents[2]
BUILDER = "scripts/capabilities/build_generic_hull_screening.py"
MODEL = "src/digitalmodel/hydrodynamics/utube_tank.py"
ARCHIVE = "docs/api/cfd/data/generic_hull_roll_screening.json"
ALPHAS = (0.02, 0.05, 0.10, 0.15, 0.20)


def add_args(ap):
    ap.add_argument("--repo-root", type=Path, default=REPO_ROOT,
                    help="digitalmodel clone that holds the builder and the model")
    ap.add_argument("--archive", type=Path, help="data file to check instead of the committed one")


def _load_builder(root: Path):
    spec = importlib.util.spec_from_file_location("_hull_screening_builder", root / BUILDER)
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


def _pct(alpha: float) -> str:
    return f"{round(alpha * 100):03d}"


# Quantities of each archived case that a fresh run must reproduce, with the
# comparison used: peak periods are stored rounded to 0.0001 s; other floats keep
# 12 significant digits (builder canonical_numbers()).
CASE_FIELDS = {"reduction_percent": "rel", "bare_peak_rao": "rel", "tank_peak_rao": "rel",
               "long_period_penalty_percent": "rel", "bare_peak_period_s": "period",
               "tank_peak_period_s": "period"}
PERIODS, DAMPINGS = (18.0, 21.0), (0.02, 0.05)


def _same(field: str, new: float, old: float) -> bool:
    if CASE_FIELDS[field] == "period":
        return abs(round(new, 4) - old) <= 1e-9
    return abs(new - old) <= 1e-9 * max(1.0, abs(old))


def archive_check(cases: list[dict], archive: dict, notes: dict) -> int:
    """Count archived cases reproduced, with one-to-one coverage of the declared matrix.

    The declared matrix is 2 roll periods x 2 damping ratios x 5 alphas = 20
    cases. A duplicate, missing or extra archived case, or any differing field
    of CASE_FIELDS, lowers the count below 20.
    """
    key = lambda c: (c["roll_period_s"], c["hull_damping_ratio"], c["alpha"])  # noqa: E731
    declared = {(p, d, a) for p in PERIODS for d in DAMPINGS for a in ALPHAS}
    fresh = {key(c): c for c in cases}
    old_keys = [key(c) for c in archive["cases"]]
    problems = []
    if len(old_keys) != len(set(old_keys)):
        problems.append("duplicate archived cases")
    if set(old_keys) != declared:
        problems.append(f"archived case set differs from the declared matrix "
                        f"(missing {len(declared - set(old_keys))}, extra {len(set(old_keys) - declared)})")
    reproduced = 0
    for old in archive["cases"]:
        new = fresh.get(key(old))
        if new is None:
            continue
        bad = [f for f in CASE_FIELDS if f not in old or not _same(f, new[f], old[f])]
        if bad:
            problems.append(f"case {key(old)} differs in {', '.join(bad)}")
        else:
            reproduced += 1
    if problems:
        reproduced = min(reproduced, len(declared) - 1)
        notes["archive_cases_reproduced"] = "; ".join(problems)[:300]
    else:
        notes["archive_cases_reproduced"] = (f"{reproduced} of {len(declared)} declared cases, "
                                             f"fields {', '.join(CASE_FIELDS)}")
    return reproduced


def compute(args, values, notes, context):
    root = args.repo_root
    context.update(repo_root=str(root), repo_head=evalkit.git_head(root),
                   sha256={p: evalkit.sha256_file(root / p) for p in (BUILDER, MODEL, ARCHIVE)})
    record = _load_builder(root).compute()
    cases = record["cases"]

    def case(period, damping, alpha):
        return next(c for c in cases if c["roll_period_s"] == period
                    and c["hull_damping_ratio"] == damping and c["alpha"] == alpha)

    for damping, tag in ((0.02, "2pct"), (0.05, "5pct")):
        for alpha in ALPHAS:
            values[f"reduction_18s_{tag}_alpha{_pct(alpha)}"] = round(
                case(18.0, damping, alpha)["reduction_percent"], 1)
    values["tank_peak_period_18s_2pct_alpha005"] = round(case(18.0, 0.02, 0.05)["tank_peak_period_s"], 1)
    values["long_period_penalty_60s_alpha005"] = round(case(18.0, 0.02, 0.05)["long_period_penalty_percent"], 1)
    values["long_period_penalty_60s_alpha020"] = round(case(18.0, 0.02, 0.20)["long_period_penalty_percent"], 1)
    # Slow-forcing limit 1/(1 - alpha), quoted on the page for alpha 0.20.
    values["slow_forcing_limit_alpha020"] = round((1 / (1 - 0.20) - 1) * 100, 1)
    archive = json.loads((args.archive or root / ARCHIVE).read_text(encoding="utf-8"))
    values["archive_cases_reproduced"] = archive_check(cases, archive, notes)


if __name__ == "__main__":
    raise SystemExit(evalkit.main(HERE / "results.json", compute, add_args))
