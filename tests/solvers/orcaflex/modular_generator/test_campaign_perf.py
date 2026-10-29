# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Bounded spec-only measurements for issue #536; no timing acceptance limit.

Scales are specified in docs/plans/2026-05-05-issue-536-per-iteration-validate-perf.md.
Synthetic axis sizes are hand factorizations: 1*1*1=1, 2*5*1=10,
4*5*5=100, and 4*10*17=680. Heights start at 1 m, periods at 6 s,
and directions at 0 degrees in 10-degree steps. These are arbitrary valid
test inputs, not measured environmental data. Base numbers follow the
synthetic example in schema/test_campaign.py::_make_base_spec_data.

Run with pytest -q -rA -p no:cacheprovider and an external --basetemp to
retain measurements in captured output and keep generated files outside git.
The timed region includes copying, overrides, validation and YAML writes;
input loading and independent output checks are outside it. Validation calls
are counted by a wrapper that delegates to the original validator.
"""

from __future__ import annotations

from importlib.metadata import version
from itertools import product
from platform import python_version
from time import perf_counter

import pytest
import yaml


@pytest.mark.benchmark(group="campaign-sweep")
@pytest.mark.parametrize(
    ("axis_sizes", "expected_count"),
    [((1, 1, 1), 1), ((2, 5, 1), 10), ((4, 5, 5), 100), ((4, 10, 17), 680)],
    ids=["1", "10", "100", "680"],
)
def test_campaign_spec_only_perf(
    axis_sizes, expected_count, tmp_path, monkeypatch, record_property
):
    """Measure one generation and verify every emitted Cartesian-product value."""
    from digitalmodel.solvers.orcaflex.modular_generator.schema import ProjectInputSpec
    from digitalmodel.solvers.orcaflex.modular_generator.schema.campaign import (
        CampaignGenerator,
    )

    height_count, period_count, direction_count = axis_sizes
    heights = tuple(float(i + 1) for i in range(height_count))
    periods = tuple(float(i + 6) for i in range(period_count))
    directions = tuple(float(i * 10) for i in range(direction_count))
    # Independent oracle: no CampaignMatrix.combinations or override helper.
    expected = set(product(heights, periods, directions))
    assert len(expected) == expected_count
    parameters = (
        "environment.waves.trains.0.height",
        "environment.waves.trains.0.period",
        "environment.waves.trains.0.direction",
    )
    aliases = ("height", "period", "direction")
    data = {
        "base": {
            "metadata": {
                "name": "synthetic",
                "description": "Synthetic sweep benchmark",
                "structure": "pipeline",
                "operation": "installation/floating",
            },
            "environment": {
                "water": {"depth": 10, "density": 1.025},
                "seabed": {"stiffness": {"normal": 1000, "shear": 100}},
                "waves": {"type": "airy", "height": 1, "period": 6, "direction": 180},
                "current": {"speed": 0.5, "direction": 270},
                "wind": {"speed": 5, "direction": 270},
            },
            "pipeline": {
                "name": "Synthetic line",
                "material": "X65",
                "dimensions": {"outer_diameter": 0.762, "wall_thickness": 0.0254},
                "coatings": {
                    "corrosion": {"thickness": 0.004, "density": 0.95},
                    "weight": [{"name": "CWC", "thickness": 0.1, "density": 3.0}],
                },
                "segments": [
                    {"type": "X65+CWC", "length": 2000, "segment_length": 5},
                    {"type": "X65+CWC", "length": 3000, "segment_length": 5},
                ],
            },
        },
        "campaign": {
            "sweeps": [
                {"parameter": parameter, "values": list(values), "alias": alias}
                for parameter, values, alias in zip(
                    parameters, (heights, periods, directions), aliases
                )
            ]
        },
        "output_naming": "{base_name}_h{height}_p{period}_d{direction}",
    }
    campaign_file = tmp_path / "campaign.yml"
    campaign_file.write_text(yaml.safe_dump(data), encoding="utf-8")
    generator = CampaignGenerator(campaign_file)
    base_before = generator.spec.base.model_dump()
    validation_calls = 0
    validate = ProjectInputSpec.model_validate.__func__

    def counted_validate(cls, *args, **kwargs):
        nonlocal validation_calls
        validation_calls += 1
        return validate(cls, *args, **kwargs)

    # Count only generation, preserving the real validation and its exceptions.
    with monkeypatch.context() as context:
        context.setattr(ProjectInputSpec, "model_validate", classmethod(counted_validate))
        started = perf_counter()
        result = generator.generate(output_dir=tmp_path / "output", spec_only=True)
        elapsed = perf_counter() - started

    assert result.errors == []
    assert result.skipped_count == 0
    assert result.run_count == expected_count
    assert len(result.run_dirs) == len(set(result.run_dirs)) == expected_count
    assert generator.spec.base.model_dump() == base_before
    # _overrides.py::apply_dotted_override performs one full validation per
    # supplied sweep value: hand count = number of axes * number of tuples.
    assert validation_calls == len(parameters) * expected_count
    assert result.manifest_path == tmp_path / "output" / "manifest.yml"
    manifest = yaml.safe_load(result.manifest_path.read_text(encoding="utf-8"))
    assert manifest["total_runs"] == expected_count
    assert len(manifest["runs"]) == expected_count
    assert {row["name"] for row in manifest["runs"]} == {
        directory.name for directory in result.run_dirs
    }

    actual = set()
    for row in manifest["runs"]:
        values = tuple(row["combo"][parameter] for parameter in parameters)
        assert row["name"] == "synthetic_h{}_p{}_d{}".format(*values)
        directory = tmp_path / "output" / row["name"]
        # spec_only emits no model master/includes and never enters a solver.
        assert {path.name for path in directory.iterdir()} == {"spec.yml"}
        emitted = yaml.safe_load((directory / "spec.yml").read_text(encoding="utf-8"))
        train = emitted["environment"]["waves"]["trains"][0]
        assert tuple(train[alias] for alias in aliases) == values
        actual.add(values)
    assert actual == expected

    # SI conversion: one second = 1000 milliseconds; no timing assertion.
    metrics = {
        "combinations": expected_count,
        "validation_calls": validation_calls,
        "generation_seconds": elapsed,
        "milliseconds_per_combination": elapsed * 1000 / expected_count,
    }
    for name, value in metrics.items():
        record_property(name, value)
    print(
        f"combinations={expected_count}; validation_calls={validation_calls}; "
        f"generation_seconds={elapsed:.1f}; "
        f"milliseconds_per_combination={elapsed * 1000 / expected_count:.1f}"
    )
    dependencies = ("pytest", "pydantic", "pydantic-core", "PyYAML")
    print(f"Python={python_version()}; " + "; ".join(
        f"{name}={version(name)}" for name in dependencies
    ))
