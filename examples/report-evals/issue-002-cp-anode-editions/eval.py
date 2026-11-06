# Copyright (c) 2026 AceEngineer. Licensed under the MIT License.
"""Eval: sacrificial anodes for six structures on each DNV-RP-B401 and DNV-RP-F103 edition.

Rebuilds the key results of the newsletter article (Tables 1, 3, 4 and 5 and
the Figure 4 counts) and compares them with results.json.

Run from a clone of digitalmodel at the commit named in results.json, with
the package installed on Python 3.11 (see PROMPT.md):

    python examples/report-evals/issue-002-cp-anode-editions/eval.py
    python examples/report-evals/issue-002-cp-anode-editions/eval.py --skip-live
    python examples/report-evals/issue-002-cp-anode-editions/eval.py --compare my-results.json

The anode counts need the digitalmodel package. The Figure 4 counts need only
the standard library: the published CSV is fetched from aceengineer.com and
today's BSEE Platform Structures file from data.bsee.gov.
"""

from __future__ import annotations

import csv
import io
import json
import statistics
import sys
import tempfile
import warnings
import zipfile
from datetime import date
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parent))
import evalkit  # noqa: E402

REPO_ROOT = HERE.parents[2]
FIXTURES = "tests/fixtures/cathodic_protection/workflow_inputs"
B401_STRUCTURES = {  # result-id prefix -> fixture name (article Table 1)
    "riser": "multi_component_riser",
    "jacket_84m": "published_jacket_84m",
    "wind_jacket_60m": "reference_jacket_60m_members",
    "monopile": "monopile",
    "manifold": "manifold",
}
B401_EDITIONS = ("2005", "2010", "2017", "2021")
F103_EDITIONS = ("2010", "2019")
LIVES = (10, 15, 20, 25, 30, 35, 40)
TEMPS = (5, 20, 30, 40, 50, 60, 70, 80)
FIG4_CSV_URL = ("https://www.aceengineer.com/assets/data/newsletter/issue-002/"
                "energy-fig4-platforms-standing.csv")
BSEE_URL = "https://www.data.bsee.gov/Platform/Files/PlatStrucRawData.zip"
BSEE_MEMBER = "PlatStrucRawData/mv_platstruc_structures.txt"


def add_args(ap):
    ap.add_argument("--repo-root", type=Path, default=REPO_ROOT,
                    help="digitalmodel clone that holds the fixture files")
    ap.add_argument("--cache", type=Path,
                    default=Path(tempfile.gettempdir()) / "report-evals-cache" / date.today().isoformat(),
                    help="folder for downloaded files (one folder per day by default)")
    ap.add_argument("--skip-live", action="store_true",
                    help="do not download today's BSEE file (live results become NOT_EVALUATED)")


def _engine():
    import yaml  # installed with digitalmodel
    warnings.filterwarnings("ignore")
    try:
        from loguru import logger
        logger.remove()
    except ImportError:
        pass
    from digitalmodel.cathodic_protection.engine_adapter import run_cathodic_protection
    return yaml, run_cathodic_protection


def _run(yaml, run_cp, fixture_dir: Path, name: str, edition: str,
         life: int | None = None, temp: float | None = None) -> dict:
    cfg = yaml.safe_load((fixture_dir / f"{name}.yml").read_text(encoding="utf-8"))
    design = cfg["inputs"].setdefault("design_data", {})
    design["edition"] = edition
    if life is not None:
        design["design_life"] = life
    if temp is not None:
        cfg["inputs"]["pipeline"]["internal_fluid_temperature_C"] = temp
    return run_cp(cfg)["results"]


def _b401_count(results: dict) -> int:
    """Recommended anodes; a multi-family structure is the sum of its families.

    status.result in the output judges the installed count of the input file,
    not this recommended count, so it is not used here.
    """
    families = results.get("anode_families")
    if families:
        return sum(int(f["recommended_anode_count"]) for f in families.values())
    return int(results["current_output_verification"]["recommended_anode_count"])


def _pipe(results: dict) -> tuple[int, float, float]:
    """F103 anode count, total net anode mass (kg) and mean current density (A/m2)."""
    a = results["anode_requirements"]
    return (int(a["anode_count"]), float(a["total_anode_mass_kg"]),
            float(results["current_densities_A_m2"]["mean_current_density_A_m2"]))


def anode_results(repo_root: Path, values: dict, notes: dict, context: dict) -> None:
    yaml, run_cp = _engine()
    import digitalmodel
    package = Path(digitalmodel.__file__).resolve()
    try:
        context["digitalmodel_file"] = package.relative_to(Path(repo_root).resolve()).as_posix()
    except ValueError:  # installed from another folder than the clone being graded
        context["digitalmodel_file"] = "outside the clone: " + package.parent.name
    fixture_dir = repo_root / FIXTURES
    run = lambda *a, **k: _run(yaml, run_cp, fixture_dir, *a, **k)  # noqa: E731

    pipe = {ed: run("pipeline", ed) for ed in F103_EDITIONS}
    count10, mass10, cd10 = _pipe(pipe["2010"])
    count19, mass19, cd19 = _pipe(pipe["2019"])
    values["pipeline_f103_2010_anodes"] = count10
    values["pipeline_f103_2019_anodes"] = count19
    values["pipeline_change_pct"] = round((count19 / count10 - 1) * 100, 1)

    for key, name in B401_STRUCTURES.items():
        counts = {ed: _b401_count(run(name, ed)) for ed in B401_EDITIONS}
        if key == "riser":
            early = {counts[ed] for ed in ("2005", "2010", "2017")}
            values["riser_b401_2005_2010_2017_anodes"] = early.pop() if len(early) == 1 else str(counts)
            values["riser_b401_2021_anodes"] = counts["2021"]
        else:
            same = set(counts.values())
            values[f"{key}_b401_all_editions_anodes"] = same.pop() if len(same) == 1 else str(counts)

    for life in LIVES:
        for ed in F103_EDITIONS:
            c, m, _ = _pipe(run("pipeline", ed, life=life))
            values[f"t3_life{life}_{ed}_anodes"] = c
            values[f"t3_life{life}_{ed}_mass_kg"] = round(m, 1)
    for temp in TEMPS:
        for ed in F103_EDITIONS:
            c, _, cd = _pipe(run("pipeline", ed, temp=temp))
            values[f"t4_temp{temp}_{ed}_anodes"] = c
            values[f"t4_temp{temp}_{ed}_current_density"] = round(cd, 4)

    # Table 5 / Figure 2 build-up at 30 yr, 60 C. The anode mass is proportional
    # to the product of the mean coating breakdown factor and the mean current
    # density (article, Figure 2 caption), so each factor's share is its ratio.
    fcm10 = float(pipe["2010"]["coating_breakdown_factors"]["mean_factor"])
    fcm19 = float(pipe["2019"]["coating_breakdown_factors"]["mean_factor"])
    values["fcm_mean_2010_30yr"] = fcm10
    values["fcm_mean_2019_30yr"] = fcm19
    values["buildup_breakdown_only_pct"] = round((fcm19 / fcm10 - 1) * 100, 1)
    values["buildup_current_density_only_pct"] = round((cd19 / cd10 - 1) * 100, 1)
    values["buildup_both_pct"] = round((fcm19 * cd19 / (fcm10 * cd10) - 1) * 100, 1)
    context["pipeline_mass_kg"] = {"2010": round(mass10, 3), "2019": round(mass19, 3)}


def _standing_counts(rows: list[dict], year_of) -> dict:
    years = [y for y in (year_of(r) for r in rows) if y is not None]
    return {
        "standing": len(rows),
        "fixed": sum(r["type"] == "FIXED" for r in rows),
        "before_1993": sum(y < 1993 for y in years),
        "before_2005": sum(y < 2005 for y in years),
        "median_year": statistics.median(years),
    }


def published_counts(cache: Path, values: dict, context: dict) -> None:
    """Figure 4 counts from the published CSV (the standing structures of the counted copy)."""
    pub = evalkit.fetch(FIG4_CSV_URL, cache / "energy-fig4-platforms-standing.csv")
    context["fig4_csv"] = pub
    rows = list(csv.DictReader(open(pub["path"], encoding="utf-8")))
    c = _standing_counts(rows, lambda r: int(r["year"]) if r["year"].strip() else None)
    for k, v in c.items():
        values[f"fig4_published_{k}"] = v


def live_counts(cache: Path, values: dict, notes: dict, context: dict) -> None:
    """Figure 4 counts from the BSEE file of the run date (live source)."""
    raw = evalkit.fetch(BSEE_URL, cache / "PlatStrucRawData.zip")
    context["bsee_platform_file"] = raw
    with zipfile.ZipFile(raw["path"]) as z:
        text = z.read(BSEE_MEMBER).decode("latin-1")
    allrows = list(csv.DictReader(io.StringIO(text)))
    # Counting rule (article, data note): no REMOVAL_DATE recorded = still standing;
    # installation year = year of INSTALL_DATE (MM/DD/YYYY); fixed jacket = STRUC_TYPE_CODE FIXED.
    standing = [{"type": r["STRUC_TYPE_CODE"].strip(), "date": r["INSTALL_DATE"].strip()}
                for r in allrows if not r["REMOVAL_DATE"].strip()]
    c = _standing_counts(standing, lambda r: int(r["date"].split("/")[-1]) if r["date"] else None)
    for k, v in c.items():
        values[f"fig4_today_{k}"] = v
    values["bsee_structures_today"] = len(allrows)
    notes["fig4_today_standing"] = f"BSEE file sha256 {raw['sha256'][:12]}..., retrieved {raw['retrieved_utc']}"


def compute(args, values, notes, context):
    ids = [r["id"] for r in json.loads((HERE / "results.json").read_text(encoding="utf-8"))["results"]]
    published = [i for i in ids if i.startswith("fig4_published_")]
    live = [i for i in ids if i.startswith("fig4_today_") or i == "bsee_structures_today"]
    anodes = [i for i in ids if i not in published and i not in live]
    context.update(repo_root=str(args.repo_root), repo_head=evalkit.git_head(args.repo_root))

    def run_anodes():
        try:
            anode_results(args.repo_root, values, notes, context)
        except ImportError as exc:
            for rid in anodes:
                notes.setdefault(rid, f"digitalmodel not importable ({exc}); install it, see PROMPT.md")
    evalkit.guard(anodes, values, notes, run_anodes)
    evalkit.guard(published, values, notes, lambda: published_counts(args.cache, values, context))
    if args.skip_live:
        for rid in live:
            notes[rid] = "--skip-live"
    else:
        evalkit.guard(live, values, notes, lambda: live_counts(args.cache, values, notes, context))


if __name__ == "__main__":
    raise SystemExit(evalkit.main(HERE / "results.json", compute, add_args))
