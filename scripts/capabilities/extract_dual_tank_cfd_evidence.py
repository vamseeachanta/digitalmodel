# ABOUTME: Project selected archived reductions into a public evidence record.
"""Extract an allowlisted projection; never copy archive payloads or geometry.

Usage: python scripts/capabilities/extract_dual_tank_cfd_evidence.py ARCHIVE_DIR
The documentation generator consumes the resulting record without this archive.
"""
from __future__ import annotations

import argparse
import csv
import hashlib
import io
import json
import math
from pathlib import Path
import re

OUTPUT = Path(__file__).resolve().parents[2] / "docs/api/cfd/data/dual_tank_cfd_evidence.json"


def validate_public(payload):
    """Reject private locators and identifiers, including hidden metadata."""
    text = payload if isinstance(payload, str) else json.dumps(payload, ensure_ascii=False)
    forbidden = ["/" + name + "/" for name in ("home", "mnt")]
    patterns = [r"B" + r"15\w*", r"[\w.+-]+@[\w.-]+\.[A-Za-z]{2,}",
                r"[A-Za-z]:\\+Users\\+", r"\bws" + r"-[a-f0-9]{2}\b",
                r"\bace" + r"-(?:linux|win)-\d+\b", r"\bgpu" + r"-claw\b"]
    if any(value in text for value in forbidden) or any(
        re.search(pattern, text, re.I) for pattern in patterns
    ):
        raise ValueError("Public evidence contains a prohibited identifier or locator")


class Archive:
    """Read known small reduction files and retain a digest of each source."""

    def __init__(self, root):
        self.root = Path(root).resolve()
        self.cache = {}

    def read(self, relative):
        path = Path(relative)
        if path.is_absolute() or ".." in path.parts or "\\" in relative:
            raise ValueError("Archive source must be relative")
        target = (self.root / path).resolve()
        if not target.is_relative_to(self.root):
            raise ValueError("Archive source escapes root")
        if relative not in self.cache:
            raw = target.read_bytes()
            self.cache[relative] = (raw.decode("utf-8"), hashlib.sha256(raw).hexdigest())
        return self.cache[relative]

    def json(self, relative):
        return json.loads(self.read(relative)[0])

    def text_fact(self, relative, pointer, value, unit="1", transformation=None):
        _, digest = self.read(relative)
        fact = {"value": value, "unit": unit, "provenance": {
            "path": "archived-cfd-campaign-2026-07/" + relative, "pointer": pointer, "sha256": digest}}
        if transformation:
            fact["transformation"] = transformation
        validate_public(fact)
        if isinstance(value, float) and not math.isfinite(value):
            raise ValueError("Non-finite evidence")
        return fact

    def fact(self, relative, pointer, unit="1"):
        value = self.json(relative)
        for part in pointer.strip("/").split("/"):
            part = part.replace("~1", "/").replace("~0", "~")
            value = value[int(part)] if isinstance(value, list) else value[part]
        if not isinstance(value, (str, int, float, bool)):
            raise ValueError("Only scalar facts can be exported")
        return self.text_fact(relative, pointer, value, unit)


def calibration_fill(archive, relative):
    label = archive.json(relative)["model"]["both_fitted_at"]
    matches = re.findall(r"(\d+(?:\.\d+)?)\s*%\s*fill", label)
    if len(matches) != 1:
        raise ValueError("Missing or ambiguous calibration fill")
    return archive.text_fact(relative, "/model/both_fitted_at", float(matches[0]), "%", "parse calibration fill percentage")


def prediction_groups(archive, relative):
    result = []
    for i, group in enumerate(archive.json(relative)["groups"]):
        base = f"/groups/{i}"
        name = group["group"]
        fill_match = re.fullmatch(r"fill_(\d+)pct", name)
        fill = (archive.text_fact(relative, base + "/group", int(fill_match[1]), "%", "parse group fill percentage")
                if fill_match else calibration_fill(archive, relative))
        row = {key: archive.fact(relative, base + "/" + source, unit)
               for key, source, unit in (
                   ("area", "conduit_area_m2", "m²"),
                   ("predicted_period", "predicted_Tn_s", "s"),
                   ("measured_period", "measured_Tn_s", "s"),
                   ("error", "Tn_error_pct", "%"))}
        row["fill"] = fill
        row["damping"] = [
            {key: archive.fact(relative, f"{base}/cases/{j}/{source}", unit)
             for key, source, unit in (
                 ("period", "period_s", "s"), ("roll", "roll_amplitude_deg", "°"),
                 ("measured", "zeta_measured", "1"),
                 ("linear_prediction", "zeta_predicted", "1"))}
            for j, _ in enumerate(group["cases"])
        ]
        result.append(row)
    return result


def extract(archive_dir):
    """Select scalar fields only; source schemas are intentionally not exported."""
    a = Archive(archive_dir)
    area_source = "analytical/ac_validation.json"
    area_sweep = [
        {key: a.fact(area_source, f"/rows/{i}/{source}", unit)
         for key, source, unit in (
             ("area", "A_c_m2", "m²"),
             ("historical_period", "exchange_period_s", "s"),
             ("cfd_amplitude", "cfd_exchange_amp_m3", "m³"),
             ("analytical_amplitude", "analytical_exchange_amp_m3", "m³"),
             ("cfd_ratio", "cfd_ratio_to_first", "1"),
             ("analytical_ratio", "analytical_ratio_to_first", "1"))}
        for i, _ in enumerate(a.json(area_source)["rows"])
    ]
    scan = "analytical/ac_scan_results.json"
    conditions = {"period": a.fact(scan, "/forcing_period_s", "s"),
                  "roll": a.fact(scan, "/cases/0/roll_amplitude_deg", "°")}
    groups = prediction_groups(a, "review_output/damping/prediction_checks.json")
    groups += prediction_groups(a, "review_output/damping/third_area_prediction_check.json")
    reference = "review_output/damping/exchange_damping_v2.json"
    damped = "analytical/utube_damped.json"
    calibration = {"area": a.fact(damped, "/assumptions/C7_Ac_ref/value", "m²"),
                   "fill": calibration_fill(a, "review_output/damping/prediction_checks.json"),
                   "predicted_period": a.fact(damped, "/assumptions/C6_Tn_meas/value", "s"),
                   "measured_period": a.fact(reference, "/natural_period_s", "s")}
    # Calibration is reported separately; it is not an independent zero-error check.
    chain = "analytical/design_sweep.json"
    claim = a.json(chain)["chain"][0]
    match = re.search(r"\+/-([\d.]+)\s*%", claim)
    if not match:
        raise ValueError("Archived inertia-band statement changed")
    band = a.text_fact(chain, "/chain/0", float(match[1]), "%", "parse archived +/- band; not a new acceptance verdict")
    loss_source = "analytical/loss_scaling.json"
    loss = {key: a.fact(loss_source, pointer, unit) for key, pointer, unit in (
        ("exponent", "/fitted/exponent_n", "1"),
        ("rms", "/fitted/rms_error_pct", "%"), ("cases", "/cases_used", "1"),
        ("linear_rms", "/variants/v0.3_area_exponent_1/rms_error_pct", "%"),
        ("small_area_rms", "/variants/v0.3_area_exponent_1/rms_by_group_pct/conduit_area_3.4", "%"),
        ("large_area_rms", "/variants/v0.3_area_exponent_1/rms_by_group_pct/conduit_area_13.5", "%"))}
    third = "review_output/damping/loss_scaling_third_area.json"
    loss["third"] = {key: a.fact(third, pointer, unit) for key, pointer, unit in (
        ("exponent", "/fitted/exponent_n", "1"), ("rms", "/fitted/rms_error_pct", "%"),
        ("cases", "/cases_used", "1"))}
    report = "review_output/damping/third_area_report.md"
    text = a.read(report)[0]
    match = re.search(r"for \*\*([\d.]+)% RMS\*\*", text)
    if not match:
        raise ValueError("Independent third-area error is absent")
    loss["third"]["independent_rms"] = a.text_fact(report, "text:independent-law RMS", float(match[1]), "%", "parse independent-law RMS; no refit")
    sweep_source = "review_output/period_refinement.csv"
    sweep = []
    for i, row in enumerate(csv.DictReader(io.StringIO(a.read(sweep_source)[0]))):
        if row["status"] != "completed" or row["cycle_converged"] != "True" or row["mesh"] != "medium":
            raise ValueError("Period sweep includes an unqualified case")
        sweep.append({key: a.text_fact(sweep_source, f"csv:{i + 2}:{source}", float(row[source]), unit)
                      for key, source, unit in (
                          ("period", "period_s", "s"),
                          ("exchange", "exchange_amplitude_m3", "m³"),
                          ("moment", "roll_moment_amplitude_Nm", "N m"))})
    mesh_source = "review_output/mesh_confirmation.json"
    mesh = {key: a.fact(mesh_source, pointer, unit) for key, pointer, unit in (
        ("period", "/period_s", "s"), ("exchange_error", "/exchange_change_pct", "%"),
        ("moment_error", "/roll_moment_change_pct", "%"))}
    fine = "review_output/fine_T24.json"
    mesh["max_courant"] = a.fact(fine, "/max_courant", "1")
    mesh["exchange"] = a.fact(fine, "/exchange_amplitude_m3", "m³")
    mesh["moment"] = a.fact(fine, "/roll_moment_amplitude_Nm", "N m")
    review = "review_output/README.md"
    review_text = a.read(review)[0]
    target = re.search(r"Courant ([\d.]+),\s*slightly above the nominal ([\d.]+) target", review_text)
    mesh["courant_target"] = a.text_fact(review, "text:nominal Courant target", float(target[2]), "1", "parse nominal target")
    frequency = {}
    for key, pattern, unit in (
        ("minimum_error", r"an error of\s*([\d.]+)-[\d.]+%", "%"),
        ("maximum_error", r"an error of\s*[\d.]+-([\d.]+)%", "%"),
        ("richardson_order", r"Richardson order \(([\d.]+)\)", "1"),
        ("gci", r"reported ([\d.]+)% fine-grid", "%")):
        found = re.search(pattern, review_text)
        if not found:
            raise ValueError("Free-decay review metric is absent")
        frequency[key] = a.text_fact(review, "text:" + key, float(found[1]), unit, "parse reviewed free-decay metric")
    status_source = "review_output/algorithm_status.csv"
    status = []
    for i, row in enumerate(csv.DictReader(io.StringIO(a.read(status_source)[0]))):
        if row["evidence_class"] == "Performance":
            continue  # Compute performance is outside the physical-evidence scope.
        status.append({key: a.text_fact(status_source, f"csv:{i + 2}:{key}", row[key].replace("_", " ").replace("runs", "cases"), "1", "replace underscores and use cases for attempts")
                       for key in ("evidence_class", "result_family", "status", "key_finding", "limitation")})
    for row in status:
        family = row["result_family"]["value"]
        if family not in ("screening manifest", "nine selected cases"):
            continue
        finding = row["key_finding"]
        match = re.search(r"(\d+)/(\d+)", finding["value"])
        row["case_count"] = {**finding, "value": int(match[2]),
                             "transformation": "parse denominator as attempted case count"}
    result = dict(area_sweep=area_sweep, area_conditions=conditions,
                  period_checks=groups, calibration=calibration, published_band=band,
                  loss=loss, forced_sweep=sweep, mesh=mesh, frequency=frequency,
                  evidence_classes=status)
    validate_public(result)
    return result


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("archive_dir", type=Path)
    args = parser.parse_args()
    result = extract(args.archive_dir)
    OUTPUT.parent.mkdir(parents=True, exist_ok=True)
    OUTPUT.write_text(json.dumps(result, ensure_ascii=False, indent=2, allow_nan=False) + "\n", encoding="utf-8", newline="\n")
    if json.loads(OUTPUT.read_text(encoding="utf-8")) != result:
        raise ValueError("Evidence readback failed")
    print("Public evidence projection written and read back")


if __name__ == "__main__":
    main()
