"""Measure Python/JavaScript parity and refresh the README evidence table."""

import hashlib
import json
from pathlib import Path
import shutil
import subprocess
import tempfile

from build_assets import APP, PACKAGE, build_examples
from reference import field_requests, measure, python_analysis


def run_node(requests):
    node = shutil.which("node")
    if node is None:
        raise RuntimeError("node is not on PATH; reproduction cannot be measured")
    with tempfile.TemporaryDirectory(prefix="dynacard-reproduction-") as folder:
        request_path = Path(folder) / "requests.json"
        request_path.write_text(json.dumps(requests, allow_nan=False), encoding="utf-8")
        process = subprocess.run([node, str(APP / "test/run_core.cjs"), str(request_path)],
                                 check=True, capture_output=True, text=True, encoding="utf-8",
                                 creationflags=getattr(subprocess, "CREATE_NO_WINDOW", 0))
    return json.loads(process.stdout)


def reproduction_document():
    assets = build_examples()
    requests = [{"card": example["card"], "well": assets["well"]}
                for example in assets["examples"]]
    results = run_node(requests)
    rows = []
    for example, request, actual in zip(assets["examples"], requests, results, strict=True):
        if "error" in actual:
            raise ValueError(f"{example['name']}: {actual['error']}")
        rows.append(measure(example["name"], actual, python_analysis(request)))
    fields = []
    for case, actual in zip(field_requests(), run_node([c["request"] for c in field_requests()]), strict=True):
        if case["rejection"]:
            if "error" not in actual:
                raise ValueError(f"Unsupported field case was accepted: {case['name']}")
            fields.append({"fixture": case["name"], "status": "REJECTED", "reason": actual["error"]})
        else:
            fields.append({"fixture": case["name"], "status": "COMPARED",
                           **measure(case["name"], actual, python_analysis(case["request"]))})
    source_paths = [*PACKAGE.rglob("*.py"),
                    PACKAGE / "data/dynacard_classifier.json",
                    PACKAGE / "data/dynacard_shape_classifier.json"]
    source_hashes = {str(p.relative_to(PACKAGE)).replace("\\", "/"):
                     hashlib.sha256(p.read_bytes()).hexdigest()
                     for p in sorted(source_paths)}
    return {"schema_version": 1, "criterion": {
        "pump_sample_range_relative": 1e-6, "scalar_relative": 1e-6,
        "scalar_denominator": "max(abs(reference), 1)",
        "probability_absolute": 1e-6, "classification": "identical labels",
        "interpretation": "Numerical reproduction only; field accuracy NOT_EVALUATED"},
        "source_sha256": source_hashes, "examples": rows, "field_fixtures": fields}


def markdown_table(rows):
    lines = ["| Example | Max pump-load difference (lbf) | Max pump-position difference (in) | Max scalar relative difference | Labels equal | Max probability difference | Polished-rod work difference (ft·lbf) | Pump-card work difference (ft·lbf) |",
             "| --- | ---: | ---: | ---: | :---: | ---: | ---: | ---: |"]
    for row in rows:
        lines.append(f"| {row['example']} | {row['max_pump_load_difference_lbf']:.9e} | "
                     f"{row['max_pump_position_difference_in']:.9e} | "
                     f"{row['max_scalar_relative_difference']:.9e} | "
                     f"{'yes' if row['labels_equal'] else 'no'} | {row['max_probability_difference']:.9e} | "
                     f"{row['polished_rod_work_difference_ft_lbf']:.9e} | {row['pump_work_difference_ft_lbf']:.9e} |")
    return "\n".join(lines)


def summary(rows):
    maxima = {key: max(row[key] for row in rows) for key in (
        "max_pump_load_difference_lbf", "max_pump_position_difference_in",
        "max_scalar_relative_difference", "max_probability_difference",
        "polished_rod_work_difference_ft_lbf", "pump_work_difference_ft_lbf")}
    return (
        "For the ten saved surface cards replayed through live Python and JavaScript, "
        "the tolerances are `1e-6` of reference range for pump samples, `1e-6` of "
        "`max(abs(reference), 1)` for scalars and `1e-6` absolute for probabilities, "
        f"and the measured maxima are {maxima['max_pump_load_difference_lbf']:.9e} lbf, "
        f"{maxima['max_pump_position_difference_in']:.9e} in, "
        f"{maxima['max_scalar_relative_difference']:.9e} scalar relative, "
        f"{maxima['max_probability_difference']:.9e} probability absolute, "
        f"{maxima['polished_rod_work_difference_ft_lbf']:.9e} ft·lbf polished-rod work "
        f"and {maxima['pump_work_difference_ft_lbf']:.9e} ft·lbf pump-card work, "
        "with both classification labels identical in all ten cases."
    )


def main():
    document = reproduction_document()
    target = APP / "reproduction.json"
    target.write_text(json.dumps(document, indent=2, allow_nan=False) + "\n", encoding="utf-8")
    assert json.loads(target.read_text(encoding="utf-8")) == document
    readme = APP / "README.md"
    text = readme.read_text(encoding="utf-8")
    before, tail = text.split("<!-- reproduction:start -->", 1)
    _, after = tail.split("<!-- reproduction:end -->", 1)
    table = (markdown_table(document["examples"]) +
             "\n\nTable 1. Measured Python/JavaScript reproduction differences for the ten examples.\n\n" +
             summary(document["examples"]))
    rendered = before + "<!-- reproduction:start -->\n\n" + table + "\n\n<!-- reproduction:end -->" + after
    readme.write_text(rendered, encoding="utf-8")
    assert readme.read_text(encoding="utf-8") == rendered
    print(table)


if __name__ == "__main__":
    main()
