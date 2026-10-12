#!/usr/bin/env python3
"""Reproduce issue 2020 diagnostics; approximate references are not an oracle.

Run from the checkout: PYTHONPATH=src uv run python
scripts/validation/reproduce_holtrop_discrepancy.py
All inputs are the existing public fixture; no method coefficients are changed.
"""

import hashlib
import json
import math
from pathlib import Path

import yaml

from digitalmodel.naval_architecture import holtrop_mennen as hm
from digitalmodel.naval_architecture import holtrop_coefficients as hc


ROOT = Path(__file__).resolve().parents[2]
FIXTURE = ROOT / "tests/fixtures/test_vectors/naval_architecture/holtrop_mennen.yaml"


def diagnose(case, speed=None):
    """Normalize the component sum with the method's density and area."""
    inputs = dict(case["inputs"])
    if speed is not None:
        inputs["speed_ms"] = speed
    area = hm.wetted_surface_holtrop(**{
        key: inputs[key] for key in ("lwl", "beam", "draft", "cb", "cm", "cwp", "abt")
    })
    factor = hm.form_factor_k1(**{
        key: inputs[key] for key in ("lwl", "beam", "draft", "cp", "lcb_pct", "cstern")
    })
    rf = hm.frictional_resistance(inputs["lwl"], inputs["speed_ms"], area)
    rw = hm.wave_resistance(**inputs)
    ca = hm.correlation_allowance(inputs["lwl"], inputs["cb"], inputs["draft"])
    denominator = 0.5 * hc.RHO_SW * area * inputs["speed_ms"] ** 2
    rt = hm.total_resistance(**inputs)
    ct = rt / denominator
    row = {
        "id": case["id"], "speed_ms": inputs["speed_ms"], "lwl_m": inputs["lwl"],
        "froude_number": inputs["speed_ms"] / math.sqrt(hc.G * inputs["lwl"]),
        "wetted_surface_m2": area, "form_factor": factor,
        "rf_n": rf, "rw_n": rw, "ra_n": denominator * ca, "rt_n": rt,
        "viscous_ct": rf * factor / denominator, "wave_ct": rw / denominator,
        "ca": ca, "floor_ct": rf * factor / denominator + ca, "ct": ct,
    }
    if speed is None:
        row["ct_approx"] = case["outputs"]["ct_approx"]
        row["error_pct"] = 100 * (ct / row["ct_approx"] - 1)
    return row


def build_report():
    """Return a reproducible diagnostic record, without an accuracy verdict."""
    fixture = yaml.safe_load(FIXTURE.read_text())
    by_id = {case["id"]: case for case in fixture["test_cases"]}
    cases = [by_id[case_id] for case_id in ("series60_cb060", "tanker_cb080")]
    common = []
    for fn in (0.15, 0.20, 0.25, 0.30):
        rows = [diagnose(case, fn * math.sqrt(hc.G * case["inputs"]["lwl"])) for case in cases]
        ct_by_id = {row["id"]: row["ct"] for row in rows}
        common.append({"froude_number": fn, "rows": rows,
                       "tanker_relative_to_series60_pct": 100 * (
                           ct_by_id["tanker_cb080"] / ct_by_id["series60_cb060"] - 1)})
    factor_case = next(case for case in fixture["test_cases"] if case["id"] == "form_factor_validation")
    factor_inputs = factor_case["inputs"]
    factor = hm.form_factor_k1(**{key: factor_inputs[key] for key in
                                ("lwl", "beam", "draft", "cp", "lcb_pct", "cstern")})
    paths = [FIXTURE, Path(hm.__file__), Path(hc.__file__)]
    return {
        "reference_status": "unverified_approximate_fixture_values",
        "purpose": "issue-2020 reproduction; not engineering acceptance",
        "source_sha256": {str(path.relative_to(ROOT)): hashlib.sha256(path.read_bytes()).hexdigest()
                          for path in paths},
        "constants": {"rho_kg_m3": hc.RHO_SW, "nu_m2_s": hc.NU_SW, "g_m_s2": hc.G},
        "error_definition": "100 * (computed_ct / fixture_ct_approx - 1)",
        "ct_normalization": "RT / (0.5 * rho * V^2 * S); S is Holtrop-regression wetted surface, not mesh S",
        "fixed_speed": [diagnose(case) for case in cases], "common_froude": common,
        "form_factor_diagnostic": {"computed": factor, "unverified_fixture_range": factor_case["outputs"]},
    }


if __name__ == "__main__":
    print(json.dumps(build_report(), indent=2, allow_nan=False))
