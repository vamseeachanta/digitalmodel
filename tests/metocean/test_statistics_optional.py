# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""The optional engine must fail explicitly without breaking the core client."""

import builtins
import importlib

import pytest

MODULE = "digitalmodel.data_systems.data_procurement.metocean.statistics"


@pytest.mark.parametrize("function,args,kwargs", [
    ("extremes", (None, "hs", [10]), {}),
    ("joint_hs_tp", (None,), {}),
    ("environmental_contour", (None, 10), {"state_duration_hours": 1}),
    ("weather_windows", (None, {"hs": 2}, 2), {}),
])
def test_optional_engine_absence_names_package_and_extra(monkeypatch, function, args, kwargs):
    original = builtins.__import__

    def without_engine(name, *args, **kwargs):
        if name == "metocean_stats" or name.startswith("metocean_stats."):
            raise ModuleNotFoundError("simulated optional absence", name="metocean_stats")
        return original(name, *args, **kwargs)

    monkeypatch.setattr(builtins, "__import__", without_engine)
    with pytest.raises(ImportError, match=r"metocean-stats.*digitalmodel\[metocean\]"):
        bridge = importlib.import_module(MODULE)
        getattr(bridge, function)(*args, **kwargs)

    from digitalmodel.data_systems.data_procurement.metocean import MetoceanClient
    result = MetoceanClient({"apis": {}}).calculate_extremes(
        ({"hs": value} for value in [1, 2, 3]), "hs", [10]
    )
    assert result[10] > 3
