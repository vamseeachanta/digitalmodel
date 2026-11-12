# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Offline synthetic readback is separate from native solver qualification."""

from types import SimpleNamespace

import pytest
import yaml

from digitalmodel.solvers.smoke.model_data_readback import verify_export
from digitalmodel.solvers.smoke.model_results import numbers


@pytest.mark.parametrize("observed_m", [0.002, 0.004])
def test_elastic_bar_output_readback(tmp_path, observed_m):
    """OpenStax UP1 (2016), 12.3, eq. 12.36 rearranged: delta=PL/(EA).

    Synthetic P=2000 N, L=2 m, E=200000000 Pa, A=0.01 m² give
    delta = 4000/2000000 = 0.002 m by independent hand arithmetic.
    The second fixture doubles the result and must be rejected. SaveData
    is an offline writer of synthetic YAML, not a native solver result.
    """
    data = {"Bars": [{"Name": "synthetic", "extension_m": observed_m}]}
    model = SimpleNamespace(SaveData=lambda p: p.write_text(yaml.safe_dump(data)))
    expected = {"Bars": {"synthetic": {"extension_m": 0.002}}}
    if observed_m != 0.002:
        with pytest.raises(ValueError, match="differs"):
            verify_export(model, tmp_path, expected)
        return
    verify_export(model, tmp_path, expected)
    readback = yaml.safe_load((tmp_path / "loaded.yml").read_text())
    assert numbers([readback["Bars"][0]["extension_m"]]) == [0.002]
