# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Current depths are bounded by the declared water bottom.

Independent synthetic basis: choose a 100 m water column and unit reference
speed and factors. Depths of 0 m and 50 m are the surface and half-depth;
100 m is the bottom. Adding 0.5 m or 2 m gives strictly deeper points,
inside and outside the former relative allowance. The next representable
float above 100 m checks that no positive allowance remains. These are
boundary arithmetic cases, not environmental design data.
"""

from copy import deepcopy
import json
from math import inf, nextafter

import pytest
from pydantic import ValidationError
import yaml

from digitalmodel.solvers.orcaflex.modular_generator.schema import ProjectInputSpec


@pytest.fixture
def synthetic_spec_data():
    """Complete root input with one synthetic object and explicit environment."""
    return {
        "metadata": {
            "name": "synthetic_current_depth",
            "description": "Synthetic current-depth boundary case",
            "structure": "generic",
            "operation": "generic",
        },
        "environment": {
            "water": {"depth": 100.0},
            "seabed": {"stiffness": {}},
            "waves": {},
            "current": {"speed": 1.0, "profile": [[0.0, 1.0], [100.0, 1.0]]},
            "wind": {},
        },
        "generic": {"buoys_3d": [{"name": "synthetic_buoy"}]},
        "equipment": {},
        "simulation": {},
    }


@pytest.fixture(params=["constructor", "mapping", "json", "yaml"])
def load_spec(request):
    """Exercise construction and serialized inputs without starting a solver."""
    def load(data):
        if request.param == "constructor":
            return ProjectInputSpec(**data)
        if request.param == "mapping":
            return ProjectInputSpec.model_validate(data)
        if request.param == "json":
            return ProjectInputSpec.model_validate_json(json.dumps(data))
        return ProjectInputSpec.model_validate(yaml.safe_load(yaml.safe_dump(data)))

    return load


@pytest.mark.parametrize("depth", [nextafter(100.0, inf), 100.5, 102.0])
def test_current_profile_below_bottom_is_rejected(synthetic_spec_data, load_spec, depth):
    """Every strictly deeper point fails with both depths and units in the error."""
    data = deepcopy(synthetic_spec_data)
    data["environment"]["current"]["profile"].append([depth, 1.0])

    with pytest.raises(ValidationError) as caught:
        load_spec(data)

    message = str(caught.value)
    assert "Current profile" in message
    assert f"{depth} m" in message
    assert "water depth 100.0 m" in message


@pytest.mark.parametrize("last_depth", [50.0, 100.0])
def test_current_profile_at_bottom_and_above_is_admitted(
    synthetic_spec_data, load_spec, last_depth
):
    """Validation and dump/reload preserve every valid depth and factor exactly."""
    data = deepcopy(synthetic_spec_data)
    profile = [[0.0, 1.0], [last_depth, 1.0]]
    data["environment"]["current"]["profile"] = deepcopy(profile)

    spec = load_spec(data)

    assert spec.environment.water.depth == 100.0
    assert spec.environment.current.profile == profile
    assert spec.model_dump()["environment"]["current"]["profile"] == profile
    reloaded = load_spec(spec.model_dump())
    assert reloaded.environment.current.profile == profile
