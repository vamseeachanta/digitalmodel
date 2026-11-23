# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent equilibrium checks for a centered load on identical sling legs.

All numerical fixtures below are synthetic hand calculations. The vertical
case splits a force of 100 equally. The inclined case uses a 3:4:5 triangle:
cos(theta) = 4/5 and sin(theta) = 3/5, so a force of 80 requires tension 50
and components (+/-30, 40) per leg. Angles are measured from vertical.
Zero and 90 degrees are the vertical and horizontal geometric boundaries.
"""

import math
import os
from pathlib import Path
import subprocess
import sys
import textwrap
from types import ModuleType

import pytest


def calculate(load, angle):
    """Import inside tests so missing implementation produces failing tests."""
    from digitalmodel.specialized.rigging.statics import calculate_two_leg_sling_forces

    return calculate_two_leg_sling_forces(load, angle)


def test_vertical_legs_share_the_applied_load():
    result = calculate(100.0, 0.0)

    assert result.tension_per_leg == pytest.approx(50.0)
    assert result.vertical_force_per_leg == pytest.approx(50.0)
    assert result.left_horizontal_force == 0.0
    assert result.right_horizontal_force == 0.0


def test_inclined_legs_match_hand_calculated_triangle():
    angle_from_vertical = math.degrees(math.atan2(3.0, 4.0))
    result = calculate(80.0, angle_from_vertical)

    assert result.tension_per_leg == pytest.approx(50.0)
    assert result.vertical_force_per_leg == pytest.approx(40.0)
    assert result.left_horizontal_force == pytest.approx(-30.0)
    assert result.right_horizontal_force == pytest.approx(30.0)


@pytest.mark.parametrize("angle", [0.0, 30.0, 60.0, 89.0, math.nextafter(90.0, 0.0)])
def test_opposing_horizontal_forces_and_vertical_equilibrium(angle):
    # Synthetic angles sample the valid domain; 80 is the applied test force.
    result = calculate(80.0, angle)

    assert result.left_horizontal_force + result.right_horizontal_force == 0.0
    assert 2.0 * result.vertical_force_per_leg == pytest.approx(80.0)
    # Resolve tension independently rather than relying only on returned V.
    assert 2.0 * result.tension_per_leg * math.cos(math.radians(angle)) == (
        pytest.approx(80.0)
    )
    assert math.hypot(
        result.left_horizontal_force, result.vertical_force_per_leg
    ) == pytest.approx(result.tension_per_leg)


@pytest.mark.parametrize("load", [0.0, -1.0, math.nan, math.inf, -math.inf])
def test_invalid_applied_load_is_rejected(load):
    with pytest.raises(ValueError, match="applied_vertical_load"):
        calculate(load, 0.0)


@pytest.mark.parametrize(
    "angle", [-1.0, 90.0, 91.0, 180.0, math.nan, math.inf, -math.inf]
)
def test_singular_or_out_of_domain_angle_is_rejected(angle):
    # Negative and beyond-horizontal samples must not be folded into the domain.
    with pytest.raises(ValueError, match="angle_from_vertical_deg"):
        calculate(80.0, angle)


def test_force_units_are_preserved_by_linear_scaling():
    # A synthetic tenfold force-unit conversion must scale every component.
    original = calculate(80.0, math.degrees(math.atan2(3.0, 4.0)))
    scaled = calculate(800.0, math.degrees(math.atan2(3.0, 4.0)))

    for field in (
        "tension_per_leg",
        "vertical_force_per_leg",
        "left_horizontal_force",
        "right_horizontal_force",
    ):
        assert getattr(scaled, field) == pytest.approx(10.0 * getattr(original, field))


def test_unrepresentable_tension_is_rejected():
    # Largest finite float divided by a near-zero cosine exceeds float range.
    with pytest.raises(ValueError, match="representable"):
        calculate(sys.float_info.max, math.nextafter(90.0, 0.0))


def test_largest_finite_vertical_load_stays_finite():
    result = calculate(sys.float_info.max, 0.0)

    assert math.isfinite(result.tension_per_leg)
    assert result.tension_per_leg == sys.float_info.max / 2.0


def test_underflowed_positive_leg_force_is_rejected():
    # Half the smallest positive float rounds to zero, violating equilibrium.
    with pytest.raises(ValueError, match="representable"):
        calculate(math.nextafter(0.0, math.inf), 0.0)


@pytest.mark.parametrize(
    ("probe", "expected_error"),
    [
        ("", None),
        (
            "import unavailable_dependency",
            "ModuleNotFoundError: Non-standard dependency requested",
        ),
        (
            "import digitalmodel.specialized.rigging.rigging",
            "AssertionError: Workbook module requested",
        ),
        (
            "import digitalmodel.specialized.rigging.rigging_components",
            "AssertionError: Workbook module requested",
        ),
        ("import digitalmodel.solvers", "AssertionError: Solver import requested"),
        (
            "open('catalogue.xlsx', encoding='utf-8')",
            "AssertionError: Workbook access requested",
        ),
        (
            "open('output.txt', 'w', encoding='utf-8', newline='\\n')",
            "AssertionError: Output file requested",
        ),
    ],
    ids=["pure", "dependency", "catalogue", "components", "solver", "workbook", "write"],
)
def test_import_and_calculation_need_only_standard_library_and_no_outputs(
    tmp_path, probe, expected_error
):
    # A fresh interpreter avoids pytest's mocks and imported optional packages.
    script = textwrap.dedent(
        """
        import importlib.abc
        import os
        import sys

        class PureImportsOnly(importlib.abc.MetaPathFinder):
            def find_spec(self, fullname, path=None, target=None):
                root = fullname.split('.')[0]
                if root not in sys.stdlib_module_names and root != 'digitalmodel':
                    raise ModuleNotFoundError(
                        'Non-standard dependency requested', name=fullname
                    )
                if fullname.rsplit('.', 1)[-1] in ('rigging_components', 'rigging'):
                    if fullname not in (
                        'digitalmodel.specialized.rigging', 'digitalmodel.rigging'
                    ):
                        raise AssertionError('Workbook module requested')
                if fullname.startswith('digitalmodel.solvers'):
                    raise AssertionError('Solver import requested')

        def no_workbooks_or_writes(event, args):
            if event == 'open':
                name, mode, flags = args
                if str(name).lower().endswith(('.xlsx', '.xls', '.xlsm')):
                    raise AssertionError('Workbook access requested')
                if flags & (os.O_WRONLY | os.O_RDWR | os.O_CREAT | os.O_TRUNC):
                    raise AssertionError('Output file requested')

        sys.meta_path.insert(0, PureImportsOnly())
        sys.addaudithook(no_workbooks_or_writes)

        # Standard-library optional probes must retain missing-module semantics.
        try:
            import unavailable_optional_dependency
        except ModuleNotFoundError as error:
            assert error.name == 'unavailable_optional_dependency'
        else:
            raise AssertionError('Optional dependency probe was not blocked')

        from digitalmodel.specialized.rigging.statics import (
            calculate_two_leg_sling_forces,
        )

        result = calculate_two_leg_sling_forces(100.0, 0.0)
        assert result.tension_per_leg == 50.0
        assert not any(
            name.endswith(('.rigging.rigging', '.rigging_components'))
            for name in sys.modules
        )
        """
    )
    script += probe
    env = dict(os.environ)
    env["PYTHONPATH"] = str(Path(__file__).resolve().parents[3] / "src")
    result = subprocess.run(
        [sys.executable, "-B", "-c", script],
        cwd=tmp_path,
        env=env,
        capture_output=True,
        text=True,
        encoding="utf-8",
        check=False,
    )

    if expected_error is None:
        assert result.returncode == 0, result.stderr
    else:
        assert result.returncode != 0
        assert expected_error in result.stderr
    assert result.stdout == ""
    assert list(tmp_path.iterdir()) == []


def test_existing_rigging_export_remains_available_without_workbook_import(monkeypatch):
    import digitalmodel.specialized.rigging as package

    # A stub tests lazy export resolution without loading catalogue code.
    module = ModuleType("digitalmodel.specialized.rigging.rigging")
    sentinel = type("Rigging", (), {})
    module.Rigging = sentinel
    monkeypatch.setitem(sys.modules, module.__name__, module)

    assert package.Rigging is sentinel
    with pytest.raises(AttributeError):
        getattr(package, "missing_attribute")
