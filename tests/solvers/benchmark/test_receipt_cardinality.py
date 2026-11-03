"""Synthetic receipt qualification; no values represent a solver result.

Hand calculation: ordered samples [2, 3] have sum 5. The expected case set
contains case_a and case_b, each with variant 1 and one successful repeat.
The one-second timing and version tokens are arbitrary bookkeeping fixtures.
The synthetic timestamps establish ordering only, not measured run dates.
"""

from __future__ import annotations

from copy import deepcopy
from html.parser import HTMLParser
import json
import math

import pytest

from digitalmodel.solvers.benchmark import compare, report


EXPECTED_KEYS = {("case_a", 1), ("case_b", 1)}


def _receipt(label="fixture-left", started="2026-01-01T00:00:00+00:00"):
    return {
        "pack_version": "synthetic",
        "machine_label": label,
        "started_utc": started,
        "repeats": 1,
        "results": [
            {"case": name, "variant": 1, "n_ok": 1,
             "fingerprint": {"samples": [2, 3], "sum": 5},
             "solver_version": "synthetic-original", "input_sha256": "synthetic",
             "complete": True, "fingerprint_consistent": True,
             "baseline_eligible": True, "solve_s": {"median": 1}}
            for name in ("case_a", "case_b")
        ],
    }


@pytest.mark.parametrize("mutation", [
    "missing", "extra", "duplicate-equal", "duplicate-conflicting", "empty",
])
@pytest.mark.parametrize("side", ["new", "base"])
def test_compare_requires_nonempty_unique_complete_case_set(mutation, side):
    original = _receipt()
    assert {(e["case"], e["variant"]) for e in original["results"]} == EXPECTED_KEYS
    new, base = deepcopy(original), deepcopy(original)
    target = new if side == "new" else base
    if mutation == "missing":
        target["results"].pop()
    elif mutation == "extra":
        extra = deepcopy(target["results"][0])
        extra["case"] = "case_surplus"
        target["results"].append(extra)
    elif mutation.startswith("duplicate"):
        repeated = deepcopy(target["results"][0])
        if mutation == "duplicate-conflicting":
            repeated["fingerprint"]["samples"].reverse()
        target["results"].append(repeated)
    else:
        target["results"] = []

    if mutation.startswith("duplicate"):
        with pytest.raises(ValueError, match="duplicate"):
            compare.compare(new, base)
    elif mutation == "empty" and side == "base":
        with pytest.raises(ValueError, match="empty baseline"):
            compare.compare(new, base)
    elif ((side == "base" and mutation == "missing")
          or (side == "new" and mutation == "extra")):
        with pytest.raises(ValueError, match="membership"):
            compare.compare(new, base)
    else:
        result = compare.compare(new, base)
        assert result["ok"] is False
        assert any(r["fingerprint"] == "MISSING" for r in result["rows"])
    assert original == _receipt()


def test_compare_rejects_two_empty_receipts():
    new, base = _receipt(), _receipt()
    new["results"], base["results"] = [], []
    with pytest.raises(ValueError, match="empty baseline"):
        compare.compare(new, base)


def test_comparison_preserves_independent_expected_inputs():
    originals = [_receipt(), _receipt("fixture-right")]
    consumers = deepcopy(originals)
    comparison = compare.compare(consumers[1], consumers[0])
    assert comparison["ok"] is True
    assert {(r["case"], r["variant"]) for r in comparison["rows"]} == EXPECTED_KEYS
    model = report.build_report(consumers)
    html = report.render_html(model)
    assert "fixture-left" in html and "fixture-right" in html
    for case in model["packs"][0]["cases"]:
        assert case["variants"][0]["agreement"]["status"] == report.AGREE
    assert consumers == originals == [_receipt(), _receipt("fixture-right")]
    for receipt in consumers:
        assert {(e["case"], e["variant"]) for e in receipt["results"]} == EXPECTED_KEYS
        for entry in receipt["results"]:
            assert entry["fingerprint"] == {"samples": [2, 3], "sum": 5}
            assert sum(entry["fingerprint"]["samples"]) == 5  # 2 + 3 = 5 by hand


@pytest.mark.parametrize("value", [math.inf, -math.inf, math.nan])
@pytest.mark.parametrize("field", ["samples", "sum"])
@pytest.mark.parametrize("side", ["new", "base", "both"])
def test_nonfinite_fingerprints_cannot_qualify_even_across_versions(value, field, side):
    new, base = _receipt("fixture-right"), _receipt()
    new["results"][0]["solver_version"] = "synthetic-changed"
    targets = [new, base] if side == "both" else [new if side == "new" else base]
    for receipt in targets:
        receipt["results"][0]["fingerprint"][field] = (
            [2, value] if field == "samples" else value
        )
    result = compare.compare(deepcopy(new), deepcopy(base))
    assert result["ok"] is False
    assert result["rows"][0]["fingerprint"] == "FAILED"
    assert field in compare.differing_keys(new["results"][0], base["results"][0])
    model = report.build_report(deepcopy([base, new]))
    agreement = model["packs"][0]["cases"][0]["variants"][0]["agreement"]
    assert agreement["status"] == report.NOT_COMPARED


@pytest.mark.parametrize("samples", [[2], [3, 2], [], [[2, 3]]])
def test_ordered_fingerprint_shape_and_cardinality_must_match(samples):
    base, new = _receipt(), _receipt("fixture-right")
    new["results"][0]["fingerprint"]["samples"] = samples
    assert compare.compare(deepcopy(new), deepcopy(base))["ok"] is False
    assert compare.differing_keys(new["results"][0], base["results"][0]) == ["samples"]


def test_scalar_and_singleton_list_are_distinct_shapes():
    base, new = _receipt(), _receipt("fixture-right")
    new["results"][0]["fingerprint"]["sum"] = [5]
    assert compare.compare(new, base)["ok"] is False
    assert compare.differing_keys(new["results"][0], base["results"][0]) == ["sum"]


@pytest.mark.parametrize("via_file", [False, True])
@pytest.mark.parametrize("mutation", ["order", "length", "keys"])
def test_report_rejects_conflicting_intra_receipt_duplicates(tmp_path, via_file, mutation):
    receipt = _receipt()
    repeated = deepcopy(receipt["results"][0])
    if mutation == "order":
        repeated["fingerprint"]["samples"].reverse()
    elif mutation == "length":
        repeated["fingerprint"]["samples"].pop()
    else:
        repeated["fingerprint"].pop("sum")
    receipt["results"].append(repeated)
    untouched = deepcopy(receipt)
    with pytest.raises(ValueError, match="conflicting duplicate"):
        if via_file:
            path = tmp_path / "synthetic.json"
            path.write_text(json.dumps(receipt), encoding="utf-8", newline="\n")
            report.load_receipts([path])
        else:
            report.build_report([receipt])
    assert receipt == untouched


@pytest.mark.parametrize("via_file", [False, True])
@pytest.mark.parametrize("reorder_keys", [False, True])
@pytest.mark.parametrize("float_numbers", [False, True])
def test_report_allows_identical_intra_receipt_entries(
    tmp_path, via_file, reorder_keys, float_numbers,
):
    receipt = _receipt()
    receipt["results"][0]["metadata"] = {"flags": [True, False, {"value": 1.0}]}
    repeated = deepcopy(receipt["results"][0])
    if float_numbers:
        repeated["fingerprint"] = {"samples": [2.0, 3.0], "sum": 5.0}
    if reorder_keys:
        repeated = dict(reversed(list(repeated.items())))
        repeated["fingerprint"] = dict(reversed(list(repeated["fingerprint"].items())))
    receipt["results"].append(repeated)
    reference = _receipt("fixture-right")
    receipts = deepcopy([receipt, reference])
    if via_file:
        path = tmp_path / "synthetic.json"
        path.write_text(json.dumps(receipt), encoding="utf-8", newline="\n")
        receipts = report.load_receipts([path]) + [reference]
    model = report.build_report(receipts)
    assert {(c["case"], v["variant"]) for c in model["packs"][0]["cases"]
            for v in c["variants"]} == EXPECTED_KEYS
    assert model["packs"][0]["summary"][report.AGREE] == 2


@pytest.mark.parametrize("via_file", [False, True])
@pytest.mark.parametrize("boolean_first", [False, True])
@pytest.mark.parametrize("field", ["samples", "sum", "metadata"])
@pytest.mark.parametrize("boolean, number", [(True, 1), (False, 0), (True, 1.0)])
def test_report_rejects_boolean_numeric_duplicates_in_both_orders(
    tmp_path, via_file, boolean_first, field, boolean, number,
):
    receipt = _receipt()
    numerical = receipt["results"][0]
    boolean_entry = deepcopy(numerical)
    if field == "samples":
        # Synthetic hand sums: 1 + 3 = 4; 0 + 3 = 3.
        numerical["fingerprint"] = {"samples": [number, 3], "sum": number + 3}
        boolean_entry["fingerprint"] = {"samples": [boolean, 3], "sum": number + 3}
    elif field == "sum":
        numerical["fingerprint"]["sum"] = number
        boolean_entry["fingerprint"]["sum"] = boolean
    else:
        numerical["metadata"] = {"flags": [{"value": number}]}
        boolean_entry["metadata"] = {"flags": [{"value": boolean}]}
    entries = [boolean_entry, numerical] if boolean_first else [numerical, boolean_entry]
    receipt["results"] = entries + receipt["results"][1:]
    reference = _receipt("fixture-right")
    reference["results"][0] = deepcopy(numerical)
    with pytest.raises(ValueError, match="conflicting duplicate"):
        if via_file:
            path = tmp_path / "synthetic.json"
            path.write_text(json.dumps(receipt), encoding="utf-8", newline="\n")
            report.load_receipts([path])
        else:
            report.build_report([reference, receipt])


def test_report_keeps_newest_distinct_receipt_policy():
    old = _receipt()
    newest = _receipt(started="2026-01-02T00:00:00+00:00")
    newest["results"][0]["fingerprint"]["samples"].reverse()
    originals = deepcopy([old, newest])
    for receipts in ([old, newest], [newest, old]):
        model = report.build_report(deepcopy(receipts))
        rows = model["packs"][0]["cases"][0]["variants"][0]["rows"]
        (row,) = rows
        assert row["entry"]["fingerprint"] == {"samples": [3, 2], "sum": 5}
        assert row["supplied"] == len(receipts)
        assert "most recent of 2" in report.render_html(model)
    assert [old, newest] == originals


class _Text(HTMLParser):
    def __init__(self):
        super().__init__()
        self.parts = []

    def handle_data(self, data):
        self.parts.append(data)


@pytest.mark.parametrize("membership", ["missing", "extra", "empty"])
def test_html_readback_invalid_membership_cannot_mean_complete_agreement(membership):
    base, new = _receipt(), _receipt("fixture-right")
    if membership == "missing":
        new["results"].pop()
    elif membership == "extra":
        extra = deepcopy(new["results"][0])
        extra["case"] = "case_surplus"
        new["results"].append(extra)
    else:
        new["results"] = []
    originals = deepcopy([base, new])
    model = report.build_report(deepcopy(originals))
    parser = _Text()
    parser.feed(report.render_html(model))
    visible = " ".join(parser.parts)
    assert "fixture-left" in visible and "fixture-right" in visible
    assert report.STATUS_LABEL[report.ONE_MACHINE] in visible
    summary = model["packs"][0]["summary"]
    assert summary[report.AGREE] < summary["variants"]
    assert [base, new] == originals
