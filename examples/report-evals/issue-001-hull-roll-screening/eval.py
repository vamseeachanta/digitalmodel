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


def _load_builder(root: Path):
    spec = importlib.util.spec_from_file_location("_hull_screening_builder", root / BUILDER)
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


def _pct(alpha: float) -> str:
    return f"{round(alpha * 100):03d}"


def compute(args):
    root = args.repo_root
    values, notes = {}, {}
    context = {"repo_root": str(root), "repo_head": evalkit.git_head(root),
               "sha256": {p: evalkit.sha256_file(root / p) for p in (BUILDER, MODEL, ARCHIVE)}}
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

    # The committed data file must equal a fresh run, case by case.
    archive = json.loads((root / ARCHIVE).read_text(encoding="utf-8"))
    matched = 0
    for old in archive["cases"]:
        new = case(old["roll_period_s"], old["hull_damping_ratio"], old["alpha"])
        if abs(new["reduction_percent"] - old["reduction_percent"]) <= 1e-8 * max(1.0, abs(old["reduction_percent"])):
            matched += 1
    values["archive_cases_reproduced"] = matched
    notes["archive_cases_reproduced"] = f"{matched} of {len(archive['cases'])} cases in {ARCHIVE}"
    return values, notes, context


if __name__ == "__main__":
    raise SystemExit(evalkit.main(HERE / "results.json", compute, add_args))
