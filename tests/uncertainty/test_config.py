from __future__ import annotations

import pytest
import yaml
from pydantic import ValidationError

from digitalmodel.uncertainty import UncertaintyStudy, load_study


def _minimal_study_data():
    return {
        "priors": [{"name": "x", "dist": "uniform", "low": 0.0, "high": 1.0}],
        "doe": {"method": "lhs", "n_samples": 4, "seed": 1},
        "evaluator": {"builtin": "demo"},
        "outputs": {
            "primary": "y",
            "screening": {"metric": "p90", "operator": "<=", "threshold": 1.0},
        },
    }


def test_uncertainty_study_loads_yaml_front_door(tmp_path):
    path = tmp_path / "study.yml"
    path.write_text(
        """
priors:
  - name: capacity_factor
    dist: triangular
    low: 0.42
    mode: 0.50
    high: 0.58
doe:
  method: lhs
  n_samples: 25
  seed: 42
evaluator:
  builtin: floating_wind_lcoe
outputs:
  primary: lcoe_usd_per_mwh
  screening:
    metric: p90
    operator: "<="
    threshold: 220.0
""".lstrip(),
        encoding="utf-8",
    )

    study = load_study(path)

    assert isinstance(study, UncertaintyStudy)
    assert study.priors[0].name == "capacity_factor"
    assert study.doe.method == "lhs"
    assert study.outputs.screening.threshold == 220.0


def test_yaml_front_door_rejects_non_finite_prior_values(tmp_path):
    path = tmp_path / "study.yml"
    path.write_text(
        """
priors:
  - name: x
    dist: uniform
    low: 0.0
    high: .nan
doe:
  method: lhs
  n_samples: 4
  seed: 1
evaluator:
  builtin: demo
outputs:
  primary: y
""".lstrip(),
        encoding="utf-8",
    )

    with pytest.raises(ValidationError):
        load_study(path)


@pytest.mark.parametrize("threshold", [".nan", ".inf"])
def test_yaml_front_door_rejects_non_finite_screening_threshold(tmp_path, threshold):
    path = tmp_path / "study.yml"
    path.write_text(
        f"""
priors:
  - name: x
    dist: uniform
    low: 0.0
    high: 1.0
doe:
  method: lhs
  n_samples: 4
  seed: 1
evaluator:
  builtin: demo
outputs:
  primary: y
  screening:
    metric: p90
    operator: "<="
    threshold: {threshold}
""".lstrip(),
        encoding="utf-8",
    )

    with pytest.raises(ValidationError):
        load_study(path)


@pytest.mark.parametrize(
    ("path_parts", "blank_value"),
    [
        (("evaluator", "builtin"), ""),
        (("evaluator", "builtin"), "   "),
        (("evaluator", "dotted_path"), ""),
        (("evaluator", "dotted_path"), "   "),
        (("outputs", "primary"), ""),
        (("outputs", "primary"), "   "),
        (("outputs", "screening", "metric"), ""),
        (("outputs", "screening", "metric"), "   "),
    ],
)
def test_yaml_front_door_rejects_blank_config_identifiers(
    tmp_path, path_parts, blank_value
):
    data = _minimal_study_data()
    if path_parts == ("evaluator", "dotted_path"):
        data["evaluator"].pop("builtin")
    target = data
    for path_part in path_parts[:-1]:
        target = target[path_part]
    target[path_parts[-1]] = blank_value

    path = tmp_path / "study.yml"
    path.write_text(yaml.safe_dump(data), encoding="utf-8")

    with pytest.raises(ValidationError):
        load_study(path)


def test_unknown_distribution_is_rejected():
    with pytest.raises(ValidationError):
        UncertaintyStudy.model_validate(
            {
                "priors": [{"name": "x", "dist": "beta", "low": 0, "high": 1}],
                "doe": {"method": "lhs", "n_samples": 4, "seed": 1},
                "evaluator": {"builtin": "demo"},
                "outputs": {"primary": "y"},
            }
        )


@pytest.mark.parametrize(
    "evaluator",
    [
        {},
        {"builtin": "demo", "dotted_path": "pkg.mod:func"},
    ],
)
def test_evaluator_config_requires_exactly_one_dispatch_path(evaluator):
    with pytest.raises(ValidationError):
        UncertaintyStudy.model_validate(
            {
                "priors": [{"name": "x", "dist": "uniform", "low": 0.0, "high": 1.0}],
                "doe": {"method": "lhs", "n_samples": 4, "seed": 1},
                "evaluator": evaluator,
                "outputs": {"primary": "y"},
            }
        )
