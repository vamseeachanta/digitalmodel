# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""The adapter consumes solved lengths; it does not start a solver."""

from decimal import localcontext
from types import SimpleNamespace

import pytest

from digitalmodel.riser_bom import configuration_from_lazy_wave, cost_bom
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
