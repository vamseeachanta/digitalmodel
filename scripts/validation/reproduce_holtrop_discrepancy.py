#!/usr/bin/env python3
# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
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
from digitalmodel.naval_architecture import holtrop_diagnostics as hd
from digitalmodel.naval_architecture.holtrop_diagnostics import diagnose


ROOT = Path(__file__).resolve().parents[2]
FIXTURE = ROOT / "tests/fixtures/test_vectors/naval_architecture/holtrop_mennen.yaml"


def build_report():
    """Return a reproducible diagnostic record, without an accuracy verdict."""
    fixture = yaml.safe_load(FIXTURE.read_text())
    reference_checks = hd.assess_fixture(fixture)
    by_id = {case["id"]: case for case in fixture["test_cases"]}
    cases = [
        by_id[case_id]
        for case_id in ("series60_cb060", "tanker_cb080")
        if by_id[case_id]["use_as_test"]
    ]
    common = []
    for fn in (0.15, 0.20, 0.25, 0.30):
        rows = [
            diagnose(case, fn * math.sqrt(hc.G * case["inputs"]["lwl"]))
            for case in cases
        ]
        ct_by_id = {row["id"]: row["ct"] for row in rows}
        comparison = {
            "froude_number": fn,
            "rows": rows,
            "comparison_status": "UNAVAILABLE",
            "tanker_relative_to_series60_pct": None,
        }
        if len(rows) == 2:
            comparison.update(
                comparison_status="AVAILABLE",
                tanker_relative_to_series60_pct=100
                * (ct_by_id["tanker_cb080"] / ct_by_id["series60_cb060"] - 1),
            )
        else:
            comparison["reason"] = "one or both resistance rows are disabled"
        common.append(comparison)
    factor_case = by_id["form_factor_validation"]
    if factor_case["use_as_test"]:
        factor_inputs = factor_case["inputs"]
        factor = hm.form_factor_k1(
            **{
                key: factor_inputs[key]
                for key in ("lwl", "beam", "draft", "cp", "lcb_pct", "cstern")
            }
        )
        factor_diagnostic = {
            "computed": factor,
            "unverified_fixture_range": factor_case["outputs"],
        }
    else:
        factor_diagnostic = {
            "status": "DISABLED",
            "reason": "use_as_test is false",
            "computed": None,
        }
    paths = [
        FIXTURE,
        Path(hm.__file__),
        Path(hc.__file__),
        Path(hd.__file__),
        Path(__file__),
    ]
    return {
        "reference_status": fixture["qualification"]["reference_status"],
        "qualification_status": "NOT_EVALUATED",
        "reference_checks": reference_checks,
        "purpose": "issue-2020 reproduction; not engineering acceptance",
        "source_sha256_basis": "UTF-8 text with newlines normalized to LF",
        "source_sha256": {
            path.relative_to(ROOT).as_posix(): hashlib.sha256(
                path.read_text(encoding="utf-8").encode("utf-8")
            ).hexdigest()
            for path in paths
        },
        "constants": {"rho_kg_m3": hc.RHO_SW, "nu_m2_s": hc.NU_SW, "g_m_s2": hc.G},
        "error_definition": "100 * (computed_ct / fixture_ct_approx - 1)",
        "ct_normalization": "RT / (0.5 * rho * V^2 * S); S is Holtrop-regression wetted surface, not mesh S",
        "fixed_speed": [diagnose(case) for case in cases],
        "common_froude": common,
        "form_factor_diagnostic": factor_diagnostic,
    }


if __name__ == "__main__":
    print(json.dumps(build_report(), indent=2, allow_nan=False))
