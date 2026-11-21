"""Render the November 18 step timing report from the retained offline evidence."""

import argparse
from collections import Counter, defaultdict
import json
from pathlib import Path

if __package__:
    from .step_timings import stats
else:
    from step_timings import stats

TEMPLATE = Path(__file__).with_name("step_timings_20261118.md.in")
METRICS = ("wall_s", "checkout", "install_s", "pytest_s", "setup_with_install_pct", "pytest_pct")
PHASES = ("install_s", "pytest_s", "collected", "passed")
STEP_LABELS = {
    "install_s": "Domain install (within gate)", "pytest_s": "Domain pytest (within gate)",
    "domain_gate": "Domain gate (includes install and pytest)", "formatter_install": "Formatter dependency install",
    "other": "Result extraction, cleanup and other", "solver": "Geotechnical solver step",
    "read_gate": "Read gate command", "uv": "Set up uv", "python": "Set up Python",
    "node": "Set up Node", "register": "Select trusted register",
}


def rows(values):
    return "\n".join("| " + " | ".join(map(str, row)) + " |" for row in values)


def fmt(value):
    return f"{value:.1f}".removesuffix(".0")


def pair(metric):
    if not metric["n"]:
        return "unknown"
    return f"{fmt(metric['median'])}/{fmt(metric['p90'])}"


def shard_table(summary, workflow):
    result = []
    for name, metrics in sorted(summary.items()):
        kind, shard = name.split("/")
        if kind == workflow and shard != "*":
            count = metrics["wall_s"]
            cells = [f"{pair(metrics[k])} ({metrics[k]['n']})" for k in METRICS]
            result.append([shard, count["n"] + count["unknown"], *cells])
    return rows(result)


def measurement_tables(report):
    summary = report["summary"]
    fields = {}
    sets = []
    for kind, label in [("sweep", "S"), ("pr", "P")]:
        jobs = [j for j in report["jobs"] if j["workflow"] == kind]
        ids = sorted({j["run"] for j in jobs})
        fields[kind + "_runs"] = ", ".join(map(str, ids))
        fields[kind + "_shards"] = shard_table(summary, kind)
        depths = Counter(j["depth"] for j in jobs)
        depth_counts = [depths[0], sum(n for k, n in depths.items() if k and k > 0), depths[None]]
        known = [sum(j.get(k) is not None for j in jobs) for k in ("install_s", "pytest_s")]
        sets.append([label, sum(r["workflow"] == kind for r in report["runs"]), len(ids), len(jobs),
                     " / ".join(map(str, depth_counts)), " / ".join(map(str, known))])
    classes = sorted({k for j in report["jobs"] for k in j["classes"]})
    step_rows = []
    for key in [*classes, "install_s", "pytest_s"]:
        label = STEP_LABELS.get(key, key.replace("_", " ").capitalize())
        distributions = [summary[kind + "/*"] for kind in ("sweep", "pr")]
        step_rows.append([label, *(pair(d[key]) if key in d else "n/a" for d in distributions)])
    return dict(fields, sets=rows(sets), steps=rows(step_rows))


def solver_fields(jobs, folder):
    solver = [j for j in jobs if j["workflow"] == "pr" and "solver_phases" in j]
    anchor = sorted((j for j in solver if j["classes"]["solver"] == 587), key=lambda j: j["job"])
    detail = [[j["run"], j["job"], 587, *(j["solver_phases"][p] for p in PHASES)] for j in anchor]
    labels = ["Install (s)", "Pytest (s)", "Collected (nodes)", "Passed (nodes)"]
    readings = []
    for label, phase in zip(labels, PHASES):
        metric = stats([j["solver_phases"][phase] for j in solver])
        readings.append([label, f"{metric['n']}/{len(solver)}", pair(metric)])
    paired = [j for j in solver if j["conclusion"] == "success" and
              any(s["class"] == "solver" and s["conclusion"] == "success" for s in j["steps"])]
    progress = stats([j.get("geo_progress_s") for j in paired])
    lists = [("Without extras", "without"), ("With extras", "with"), ("Only with extras", "only-with"),
             ("Only without extras", "only-without"), ("Both", "both")]
    nodes = rows((label, len((folder / f"{name}.nodes").read_text().splitlines())) for label, name in lists)
    return dict(solver_anchor=rows(["unknown" if v is None else v for v in line] for line in detail),
                solver_readings=rows(readings), paired_count=len(paired), progress=progress,
                progress_pair=pair(progress), nodes=nodes, local=json.loads((folder / "local-outcomes.json").read_text()))


def setup_saving(times):
    return sum(times) - max(times)


def grouping_fields(sweeps, anchor, commands):
    # Exclude shards with pinned-history consumers and the catch-all engine gate.
    excluded = {"asset-integrity", "cathodic-protection", "contracts", "hydrodynamics-diffraction",
                "hydrodynamics-other", "marine-engineering", "workflows", "naval-architecture", "subsea", "misc"}
    trial = sorted(name for name, j in anchor.items() if name not in excluded and j["pytest_s"] < 60
                   and commands[name]["signature"] == commands["capabilities"]["signature"])
    if len(trial) != 4:
        raise ValueError("expected four qualified shards for the reference sweep grouping")
    keys = ("runner_setup", "checkout", "python", "uv")
    common = {name: sum(anchor[name]["classes"][k] for k in keys) for name in trial}
    saving = setup_saving(common.values())
    cohorts = defaultdict(dict)
    for j in sweeps:
        cohort = cohorts[j["run"], j["attempt"]]
        if j["shard"] in trial:
            if j["shard"] in cohort:
                raise ValueError("ambiguous shard execution in sweep attempt")
            cohort[j["shard"]] = j
    missing = unsuccessful = unknown = 0
    values = []
    for cohort in cohorts.values():
        if len(cohort) < len(trial):
            missing += 1
        elif any(j["conclusion"] != "success" for j in cohort.values()):
            unsuccessful += 1
        elif any(j["classes"].get(k) is None for j in cohort.values() for k in keys):
            unknown += 1
        else:
            times = [sum(j["classes"][k] for k in keys) for j in cohort.values()]
            values.append(setup_saving(times))
    week = stats(values)
    return dict(candidates=rows((name, fmt(common[name]), fmt(anchor[name]["pytest_s"])) for name in trial),
                saving=saving, saving_minutes=saving / 60, weekly_pair=pair(week), week=week,
                cohort_count=len(cohorts), missing=missing, unsuccessful=unsuccessful, unknown=unknown)


def render(folder):
    report = json.loads((folder / "report.json").read_text())
    if report["pending_runs"]:
        raise ValueError("the seven-day report requires terminal runs")
    junit = json.loads((folder / "reference-junit.json").read_text())
    audit = json.loads((folder / "history-audit.json").read_text())
    commands = json.loads((folder / "gate-commands.json").read_text())
    sweeps = [j for j in report["jobs"] if j["workflow"] == "sweep"]
    reference = [j for j in sweeps if j["run"] == junit["run"]]
    anchor = {j["shard"]: j for j in reference}
    if len(anchor) != len(reference) or anchor.keys() != junit["case_seconds"].keys():
        raise ValueError("reference jobs and JUnit shards must match one-to-one")
    anchor_run, = [r for r in report["runs"] if r["id"] == junit["run"]]
    events = Counter(r["event"] for r in report["runs"] if r["workflow"] == "sweep")
    fields = dict(anchor_id=junit["run"], anchor_sha=anchor_run["head_sha"], anchor_jobs=len(anchor),
                  wall_minutes=sum(j["wall_s"] for j in reference) / 60,
                  pytest_minutes=sum(j["pytest_s"] for j in reference) / 60,
                  domain_minutes=sum(j["classes"]["domain_gate"] for j in reference) / 60,
                  under_sixty=sum(j["pytest_s"] < 60 for j in reference), junit=junit,
                  window=report["window"].replace("..", " through "), copies_removed=report["copies_removed"],
                  events=", ".join(f"{n} {event}" for event, n in sorted(events.items())),
                  geo=anchor["specialized"], audit=audit, gate_count=len(audit["shards"]))
    fields.update(measurement_tables(report))
    fields.update(solver_fields(report["jobs"], folder / "geotechnical"))
    fields.update(grouping_fields(sweeps, anchor, commands))
    return TEMPLATE.read_text().format_map(fields)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("folder", type=Path)
    print(render(parser.parse_args().folder), end="")
