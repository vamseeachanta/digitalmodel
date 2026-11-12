# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Synthetic geometry and arithmetic; no licensed solver or real costs."""

import json
from dataclasses import asdict, replace
from decimal import localcontext
from types import SimpleNamespace

import pytest

from digitalmodel.marine_ops.marine_analysis.catenary.lazy_wave import (
    LazyWaveConfiguration,
)
from digitalmodel.riser_bom import (
    configuration_from_lazy_wave,
    cost_bom,
    load_cost_basis,
)
from tests.riser_bom.test_costing import CITATION, basis, cited_root


def test_solved_suspended_length_includes_supplied_grounded_length(tmp_path):
    """Hand sum: 20 suspended + 5 grounded = 25 installed; total = 131.5."""
    result = SimpleNamespace(total_arc_length=20)
    config = configuration_from_lazy_wave(
        result,
        grounded_length_m="5",
        joint_length_m="10",
        coated_length_m="25",
        buoyancy_module_count=4,
        ancillary_count=2,
        installation_days="1.5",
        citation=CITATION,
    )
    assert config.riser_length_m == 25
    assert (
        cost_bom(config, basis(), citation_root=cited_root(tmp_path))["total"]
        == "131.5"
    )


def lazy_wave_configuration(vertical_distance=100):
    """Invented geometry with equal bare and buoyed weight magnitudes."""
    return LazyWaveConfiguration(
        hangoff_angle=30,
        hangoff_below_msl=0,
        hog_bend_above_seabed=60,
        sag_bend_elevation=20,
        weight_without_buoyancy=2,
        weight_with_buoyancy=-2,
        vertical_distance=vertical_distance,
    )


@pytest.mark.parametrize(
    "depth,installed_length,joint_count,total",
    [(100, 414.560753, "5", "135.5"), (200, 707.458180, "8", "141.5")],
)
def test_lazy_wave_configuration_costs_external_synthetic_basis(
    tmp_path, depth, installed_length, joint_count, total
):
    """Independent lengths use S=sqrt(d*(2*R+d)) for the four bend spans.

    R is 80 or 180, the four drops are 20, 20, 30, 30, and hang-off
    length is R*sqrt(3). Five grounded metres are added. All inputs are
    invented. Existing prime rates give non-joint subtotals 75, 20, 14, 16.5.
    """
    external = tmp_path / "basis.json"
    external.write_text(
        json.dumps({"schema_version": 1, **asdict(basis())}), encoding="utf-8"
    )
    config = configuration_from_lazy_wave(
        lazy_wave_configuration(depth),
        grounded_length_m="5",
        joint_length_m="100",
        coated_length_m="25",
        buoyancy_module_count=4,
        ancillary_count=2,
        installation_days="1.5",
        citation=CITATION,
    )
    assert float(config.riser_length_m) == pytest.approx(installed_length, abs=1e-6)
    result = cost_bom(
        config, load_cost_basis(external), citation_root=cited_root(tmp_path)
    )
    assert result["total"] == total
    assert result["data_kind"] == "synthetic"
    assert result["currency"] == "TEST"
    assert [row["quantity"] for row in result["breakdown"]] == [
        joint_count,
        "25",
        "4",
        "2",
        "1.5",
    ]
    assert [row["unit_rate"] for row in result["breakdown"]] == [
        "2", "3", "5", "7", "11"
    ]
    assert result["citations"][0]["code_id"] == "SYNTHETIC-BOM"


def test_lazy_wave_configuration_rejects_negative_grounded_length():
    with pytest.raises(ValueError, match="grounded_length_m"):
        configuration_from_lazy_wave(
            lazy_wave_configuration(),
            grounded_length_m="-1",
            joint_length_m="100",
            coated_length_m="25",
            buoyancy_module_count=4,
            ancillary_count=2,
            installation_days="1.5",
            citation=CITATION,
        )


@pytest.mark.parametrize("missing", ["quantity", "rate"])
def test_lazy_wave_configuration_cost_fails_closed_without_citation(tmp_path, missing):
    config = configuration_from_lazy_wave(
        lazy_wave_configuration(),
        grounded_length_m="5",
        joint_length_m="100",
        coated_length_m="25",
        buoyancy_module_count=0,
        ancillary_count=2,
        installation_days="1.5",
        citation=None if missing == "quantity" else CITATION,
    )
    rates = basis()
    if missing == "rate":
        # Even an unused synthetic rate still requires its sidecar.
        rates = replace(
            rates,
            rates=tuple(
                replace(rate, citation=None)
                if rate.component == "buoyancy_modules"
                else rate
                for rate in rates.rates
            ),
        )
    with pytest.raises(ValueError, match="Citation"):
        cost_bom(config, rates, citation_root=cited_root(tmp_path))


@pytest.mark.parametrize("suspended,grounded", [("0", "5"), ("20", "-1"), ("NaN", "5")])
def test_invalid_adapter_lengths_rejected(suspended, grounded):
    with pytest.raises(ValueError):
        configuration_from_lazy_wave(
            SimpleNamespace(total_arc_length=suspended),
            grounded_length_m=grounded,
            joint_length_m="10",
            coated_length_m="25",
            buoyancy_module_count=4,
            ancillary_count=2,
            installation_days="1.5",
            citation=CITATION,
        )


def test_adapter_retains_length_under_low_caller_precision():
    """Hand sum: 20 suspended + 5 grounded = 25 even with one-digit context."""
    with localcontext() as context:
        context.prec = 1
        config = configuration_from_lazy_wave(
            SimpleNamespace(total_arc_length=20),
            grounded_length_m="5",
            joint_length_m="10",
            coated_length_m="25",
            buoyancy_module_count=4,
            ancillary_count=2,
            installation_days="1.5",
            citation=CITATION,
        )
    assert config.riser_length_m == 25
