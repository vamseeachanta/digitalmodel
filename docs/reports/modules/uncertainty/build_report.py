"""Build the public uncertainty report: python docs/reports/modules/uncertainty/build_report.py."""
from __future__ import annotations

import argparse
import hashlib
import json
from pathlib import Path
import re
import sys

ROOT = Path(__file__).resolve().parents[4]
HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(ROOT / "src"))

import pandas as pd
from digitalmodel.uncertainty import (
    Prior, percentile_summary, run_matrix, sample_lhs, spearman_ranks, tornado,
)


def calculate():
    """Inputs and tolerances follow tests/uncertainty/test_roundtrip.py."""
    inputs = dict(x=4.0, mean_a=3.0, std_a=0.5, low_b=1.0, high_b=2.0,
                  n_samples=2000, seed=2026)
    priors = [Prior(name="a", dist="normal", mean=inputs["mean_a"], std=inputs["std_a"]),
              Prior(name="b", dist="uniform", low=inputs["low_b"], high=inputs["high_b"])]
    samples = sample_lhs(priors, inputs["n_samples"], inputs["seed"])
    evaluate = lambda row: {"y": row["a"] * inputs["x"] + row["b"]}
    result = run_matrix(evaluate, samples)
    anchors = run_matrix(evaluate, pd.DataFrame({"a": [3.0, 3.0, 3.0], "b": [1.0, 1.5, 2.0]}))
    mean = float(result.y.mean())
    variance = float(result.y.var(ddof=0))
    reference_mean = inputs["x"] * inputs["mean_a"] + (inputs["low_b"] + inputs["high_b"]) / 2
    reference_variance = inputs["x"]**2 * inputs["std_a"]**2 + (inputs["high_b"] - inputs["low_b"])**2 / 12
    return dict(inputs=inputs, rows=result[["a", "b", "y"]].values.tolist(),
                anchors=anchors.to_dict("records"), mean=mean, variance=variance,
                reference_mean=reference_mean, reference_variance=reference_variance,
                mean_difference_pct=100 * (mean - reference_mean) / reference_mean,
                variance_difference_pct=100 * (variance - reference_variance) / reference_variance,
                mean_limit_pct=2.0, variance_limit_pct=5.0,
                percentiles=percentile_summary(result, "y"),
                tornado=tornado(result, ["a", "b"], "y").to_dict("records"),
                spearman=spearman_ranks(result, ["a", "b"], "y").to_dict("records"))


def rounded(value):
    if isinstance(value, float):
        return round(value, 6)
    if isinstance(value, dict):
        return {key: rounded(item) for key, item in value.items()}
    if isinstance(value, list):
        return [rounded(item) for item in value]
    return value


def records(data):
    rows = "".join(f'<tr><td>{r["a"]:.1f}</td><td>{data["inputs"]["x"]:.1f}</td>'
                   f'<td>{r["b"]:.1f}</td><td>{r["y"]:.1f}</td><td>Defined linear example</td></tr>'
                   for r in data["anchors"])
    percentiles = "; ".join(f'{name.upper()}: {value:.3f}' for name, value in data["percentiles"].items())
    sensitivity = "; ".join(f'{r["input"]}: {r["coefficient"]:.3f}' for r in data["tornado"])
    ranks = "; ".join(f'{r["input"]}: {r["coefficient"]:.3f}' for r in data["spearman"])
    return {"ANCHORS": rows, "PERCENTILES": percentiles,
            "SENSITIVITY": sensitivity, "RANKS": ranks}


def build(destination=HERE):
    data = calculate()
    for moment in ("mean", "variance"):
        if abs(data[moment + "_difference_pct"]) > data[moment + "_limit_pct"]:
            raise ValueError(f"{moment} exceeds the numerical comparison limit")
    template = (HERE / "report.html.in").read_text(encoding="utf-8")
    exemplar = (ROOT / "docs/reports/pipeline-ffs-assessment.html").read_text(encoding="utf-8")
    style = re.search(r"<style>.*?</style>", exemplar, re.S).group(0)
    changes = {"revision": "Initial", "changes": [
        "Distribution assumptions and numerical verification limits are stated.",
        "The linear example is evaluated and compared with analytical moments.",
        "Physical validation and application acceptance remain not evaluated."]}
    evidence = dict(data)
    sources = sorted((ROOT / "src/digitalmodel/uncertainty").glob("*.py"))
    sources += sorted((ROOT / "tests/uncertainty").glob("test_*.py"))
    evidence["sources"] = {p.relative_to(ROOT).as_posix(): hashlib.sha256(p.read_text(encoding="utf-8").encode("utf-8")).hexdigest()
                           for p in sources}
    replacements = dict(STYLE=style, EVIDENCE=json.dumps(rounded(evidence), ensure_ascii=True),
                        CHANGES=json.dumps(changes, ensure_ascii=True), **records(data))
    replacements.update({key.upper(): f'<span data-value="{key}">{value:.3f}</span>'
                         for key, value in data.items() if isinstance(value, float)})
    replacements.update({key.upper(): str(value) for key, value in data["inputs"].items()})
    for key, value in replacements.items():
        template = template.replace("@@" + key + "@@", value)
    if "@@" in template:
        raise ValueError("Unfilled report field")
    destination = Path(destination)
    destination.mkdir(parents=True, exist_ok=True)
    (destination / "index.html").write_text(template, encoding="utf-8", newline="\n")
    (destination / "changes.json").write_text(json.dumps(changes, indent=2) + "\n", encoding="utf-8", newline="\n")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", type=Path, default=HERE)
    build(parser.parse_args().output)
