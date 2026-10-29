# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Offline enumeration contract, using independent synthetic examples.

Numerical fixtures below are chosen solely for hand calculation, with no
environmental or standards basis. Cardinality is two headings times two
return-period labels times two environments = eight cases. The expected rows
are written individually rather than computed by the enumeration under test.
"""

from copy import deepcopy
from dataclasses import FrozenInstanceError

import pytest

from digitalmodel.solvers.orcaflex.dlc_matrix import enumerate_load_cases


@pytest.fixture
def axes():
    """Provide synthetic labels and values, not design requirements."""
    return {
        "headings": [0, 90],
        "return_periods": ["annual", "rare"],
        "environmental_cases": {
            "calm": {"wave": {"height": 1.0}, "profile": [[0, 0.5]]},
            "rough": {"wave": {"height": 3.0}, "profile": [[0, 1.5]]},
        },
        "basis_id": "synthetic-example",
    }


def test_cartesian_cases_match_expected(axes):
    """Check every field against the independently listed eight rows."""
    cases = enumerate_load_cases(**axes)
    expected = [
        (0.0, "annual", "calm", 1.0, 0.5),
        (0.0, "annual", "rough", 3.0, 1.5),
        (0.0, "rare", "calm", 1.0, 0.5),
        (0.0, "rare", "rough", 3.0, 1.5),
        (90.0, "annual", "calm", 1.0, 0.5),
        (90.0, "annual", "rough", 3.0, 1.5),
        (90.0, "rare", "calm", 1.0, 0.5),
        (90.0, "rare", "rough", 3.0, 1.5),
    ]
    assert isinstance(cases, tuple)
    assert len(cases) == 2 * 2 * 2 == 8  # Independent hand multiplication.
    assert len({case.case_id for case in cases}) == 8
    for case, (heading, period, label, height, speed) in zip(
        cases, expected, strict=True
    ):
        assert case.heading == heading
        assert case.return_period == period
        assert case.environmental_case == label
        assert case.environment == {
            "wave": {"height": height}, "profile": ((0, speed),)
        }
        assert case.basis_id == "synthetic-example"
        assert isinstance(case.case_id, str) and case.case_id


@pytest.mark.parametrize("heading", [0, 0.0, -0.0])
@pytest.mark.parametrize("reverse_keys", [False, True])
def test_identifier_matches_literal_canonical_payload(heading, reverse_keys):
    """Pin the public digest format independently of the implementation."""
    # Canonical UTF-8 payload (join the following lines without whitespace):
    # ["synthetic-example",0.0,"annual","calm",{"profile":[[0,0.5]],
    # "wave":{"height":1.0,"period":5.0}}]
    # SHA-256 was calculated from those literal bytes, without the enumerator.
    environment = {"wave": {"period": 5.0, "height": 1.0}, "profile": [[0, 0.5]]}
    if reverse_keys:
        environment = {
            "profile": [[0, 0.5]], "wave": {"height": 1.0, "period": 5.0}
        }
    (case,) = enumerate_load_cases(
        headings=[heading],
        return_periods=["annual"],
        environmental_cases={"calm": environment},
        basis_id="synthetic-example",
    )
    assert case.case_id == (
        "dlc-31818b95826fd498a7277885f35caa277ddc993295a831618a93055e15afee9e"
    )


def test_order_and_ids_are_stable(axes):
    """Identity follows case content, independently of position or map order."""
    original = enumerate_load_cases(**axes)
    assert enumerate_load_cases(**axes) == original
    reordered = deepcopy(axes)
    reordered["headings"].reverse()
    reordered["return_periods"].reverse()
    reordered["environmental_cases"] = {
        key: dict(reversed(list(value.items())))
        for key, value in reversed(list(axes["environmental_cases"].items()))
    }
    assert enumerate_load_cases(**reordered) == tuple(reversed(original))

    # Extending an axis leaves the existing case identifiers unchanged.
    expanded = deepcopy(axes)
    expanded["headings"].append(180)  # Synthetic third direction.
    assert enumerate_load_cases(**expanded)[:8] == original
    singleton = deepcopy(axes)
    singleton["headings"] = [0.0]
    singleton["return_periods"] = ["annual"]
    singleton["environmental_cases"] = {"calm": axes["environmental_cases"]["calm"]}
    assert enumerate_load_cases(**singleton) == original[:1]


@pytest.mark.parametrize("changed", ["basis", "heading", "period", "label", "value"])
def test_ids_include_all_case_content(axes, changed):
    """Changing any identity field changes the corresponding identifier."""
    before = enumerate_load_cases(**axes)[0].case_id
    if changed == "basis":
        axes["basis_id"] = "different-basis"
    elif changed == "heading":
        axes["headings"][0] = 45  # Synthetic intermediate direction.
    elif changed == "period":
        axes["return_periods"][0] = "other-period"
    elif changed == "label":
        axes["environmental_cases"] = {
            "other-label": axes["environmental_cases"]["calm"]
        }
    else:
        axes["environmental_cases"]["calm"]["wave"]["height"] = 2.0
    assert enumerate_load_cases(**axes)[0].case_id != before


def test_inputs_are_detached(axes):
    """Neither input mutation nor attempted output mutation changes a case."""
    before = deepcopy(axes)
    cases = enumerate_load_cases(**axes)
    assert axes == before
    axes["headings"].clear()
    axes["return_periods"].clear()
    axes["environmental_cases"]["calm"]["wave"]["height"] = 99
    axes["environmental_cases"]["calm"]["profile"][0][1] = 99
    axes["environmental_cases"].clear()
    assert cases[0].heading == 0.0
    assert cases[0].return_period == "annual"
    assert cases[0].environment == before["environmental_cases"]["calm"] | {
        "profile": ((0, 0.5),)
    }
    with pytest.raises(FrozenInstanceError):
        cases[0].heading = 99
    with pytest.raises(TypeError):
        cases[0].environment["new"] = "value"
    with pytest.raises(TypeError):
        cases[0].environment["wave"]["height"] = 99
    with pytest.raises(TypeError):
        cases[0].environment["profile"][0][1] = 99
    with pytest.raises(TypeError):
        cases[0] = cases[1]
    assert cases[2].environment["wave"]["height"] == 1.0


@pytest.mark.parametrize(
    "field,value",
    [
        ("headings", []),
        ("return_periods", []),
        ("environmental_cases", {}),
        ("headings", [float("nan")]),
        ("headings", [float("inf")]),
        ("headings", [float("-inf")]),
        ("headings", [True]),
        ("headings", ["90"]),
        ("headings", [None]),
        ("headings", [10**1000]),  # Synthetic nonrepresentable float.
        ("headings", [0, 0.0]),
        ("headings", [-0.0, 0.0]),
        ("headings", {0, 90}),
        ("headings", "90"),
        ("headings", None),
        ("return_periods", ["annual", "annual"]),
        ("return_periods", [""]),
        ("return_periods", [" "]),
        ("return_periods", [" annual"]),
        ("return_periods", ["annual\n"]),
        ("return_periods", ["an\nnual"]),
        ("return_periods", [1]),
        ("return_periods", "annual"),
        ("return_periods", {"annual"}),
        ("basis_id", ""),
        ("basis_id", " basis"),
        ("basis_id", None),
        ("environmental_cases", {"": {"height": 1}}),
        ("environmental_cases", {" calm": {"height": 1}}),
        ("environmental_cases", {1: {"height": 1}}),
        ("environmental_cases", {"calm": {}}),
        ("environmental_cases", {"calm": []}),
        ("environmental_cases", [{"height": 1}]),
        ("environmental_cases", {"a": {"height": 1}, "b": {"height": 1.0}}),
        ("environmental_cases", {"calm": {"height": float("nan")}}),
        ("environmental_cases", {"calm": {"height": float("inf")}}),
        ("environmental_cases", {"calm": {"profile": [float("-inf")]}}),
        ("environmental_cases", {"calm": {"height": object()}}),
        ("environmental_cases", {"calm": {"height": {1, 2}}}),
        ("environmental_cases", {"calm": {1: "ambiguous-key"}}),
        ("environmental_cases", {"calm": {"": "ambiguous-key"}}),
    ],
)
def test_invalid_axes(axes, field, value):
    """Reject malformed, ambiguous, nonfinite or duplicate axis values."""
    axes[field] = value
    with pytest.raises(ValueError, match=field):
        enumerate_load_cases(**axes)


def test_rejection_leaves_inputs_unchanged(axes):
    """Validation cannot partially rewrite caller data."""
    axes["return_periods"].append("annual")
    before = deepcopy(axes)
    with pytest.raises(ValueError):
        enumerate_load_cases(**axes)
    assert axes == before


def test_cyclic_environment_is_rejected(axes):
    """Self-reference is not a finite environmental specification."""
    cycle = []
    cycle.append(cycle)
    axes["environmental_cases"] = {"cyclic": {"profile": cycle}}
    with pytest.raises(ValueError, match="environmental_cases"):
        enumerate_load_cases(**axes)


def test_shared_nested_values_are_supported(axes):
    """Shared input references are allowed when there is no cycle."""
    shared = [0, 1]  # Synthetic two-point data.
    axes["environmental_cases"] = {
        "shared": {"first": shared, "second": shared, "flag": True, "note": None}
    }
    cases = enumerate_load_cases(**axes)
    shared.clear()
    assert cases[0].environment == {
        "first": (0, 1), "second": (0, 1), "flag": True, "note": None
    }


def test_case_ids_are_unambiguous_for_delimiter_labels(axes):
    """Labels containing separators cannot merge adjacent identity fields."""
    axes["return_periods"] = ["a:b", "a"]
    axes["environmental_cases"] = {"c": {"value": 1}, "b:c": {"value": 2}}
    assert len({case.case_id for case in enumerate_load_cases(**axes)}) == 8
