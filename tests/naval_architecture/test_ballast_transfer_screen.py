# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Synthetic two-tank moment and capacity checks; no operational data."""

from dataclasses import FrozenInstanceError, replace
import math

import pytest

from digitalmodel.citations import CitationResolutionError
from digitalmodel.naval_architecture.ballast_transfer_screen import (
    TankState,
    screen_even_keel_transfer,
    screen_even_keel_transfer_cited,
)


@pytest.fixture
def tanks():
    """A to B has a +20 m lever and 250 t available."""
    return TankState("A", 0.0, 300.0, 500.0), TankState("B", 20.0, 50.0, 300.0)


def screen(a, b, trim=-0.4, mct=100.0):
    return screen_even_keel_transfer(
        tank_a=a, tank_b=b, initial_trim_m=trim, mct_t_m_per_cm=mct
    )


@pytest.mark.parametrize("capacity, available, feasible", [
    (300.0, 250.0, True), (249.0, 199.0, False), (250.0, 200.0, True),
])
def test_forward_capacity_boundary(tanks, capacity, available, feasible):
    a, b = tanks
    b = replace(b, capacity_t=capacity)
    before = (a, b)
    result = screen(a, b)
    # 40 cm * 100 t m/cm / 20 m = 200 t; destination spare = capacity - 50.
    assert result.required_signed_t == pytest.approx(200.0, rel=1e-12)
    assert (result.source_id, result.destination_id) == ("A", "B")
    assert result.available_transfer_t == available
    assert result.capacity_feasible is feasible
    assert (a, b) == before
    with pytest.raises(FrozenInstanceError):
        a.mass_t = 0.0


def test_reverse_transfer_uses_actual_source(tanks):
    a, b = tanks
    result = screen(a, b, trim=0.4)
    assert result.required_signed_t == pytest.approx(-200.0, rel=1e-12)
    assert (result.source_id, result.destination_id) == ("B", "A")
    assert result.available_transfer_t == 50.0
    assert result.capacity_feasible is False
    swapped = screen(b, a, trim=0.4)
    assert swapped.required_signed_t == pytest.approx(200.0, rel=1e-12)
    assert (swapped.source_id, swapped.destination_id) == ("B", "A")
    assert swapped.available_transfer_t == 50.0


def test_source_mass_limits_transfer(tanks):
    a, b = tanks
    result = screen(replace(a, mass_t=199.0), b)
    assert result.available_transfer_t == 199.0
    assert result.capacity_feasible is False


def test_zero_trim_and_empty_tanks():
    result = screen(TankState("A", 0, 0, 0), TankState("B", 20, 0, 0), trim=0)
    assert result.required_signed_t == 0.0
    assert (result.source_id, result.destination_id) == (None, None)
    assert result.available_transfer_t == 0.0
    assert result.capacity_feasible is True


@pytest.mark.parametrize("field", ["position_m", "mass_t", "capacity_t"])
@pytest.mark.parametrize("bad", [math.nan, math.inf, -math.inf, True, False, "1"])
def test_invalid_tank_numbers(field, bad):
    values = dict(identifier="A", position_m=0.0, mass_t=0.0, capacity_t=1.0)
    values[field] = bad
    with pytest.raises(ValueError, match=field):
        TankState(**values)


@pytest.mark.parametrize("identifier", ["", " ", None, True])
def test_invalid_identifier(identifier):
    with pytest.raises(ValueError, match="identifier"):
        TankState(identifier, 0.0, 0.0, 1.0)


@pytest.mark.parametrize("mass, capacity", [(-1, 1), (0, -1), (2, 1)])
def test_invalid_tank_bounds(mass, capacity):
    with pytest.raises(ValueError, match="mass_t|capacity_t"):
        TankState("A", 0.0, mass, capacity)


@pytest.mark.parametrize("trim", [0.0, -0.4])
@pytest.mark.parametrize("defect", ["identifier", "position_m"])
def test_invalid_pair_even_at_zero_trim(tanks, trim, defect):
    a, b = tanks
    b = replace(b, **{defect: getattr(a, defect)})
    with pytest.raises(ValueError, match=defect):
        screen(a, b, trim=trim)


@pytest.mark.parametrize("trim", [0.0, -0.4])
@pytest.mark.parametrize("mct", [0.0, -1.0, math.nan, math.inf, True, False])
def test_invalid_mct_even_at_zero_trim(tanks, trim, mct):
    with pytest.raises(ValueError, match="mct_t_m_per_cm"):
        screen(*tanks, trim=trim, mct=mct)


@pytest.mark.parametrize("trim", [math.nan, math.inf, -math.inf, True, False])
def test_invalid_trim(tanks, trim):
    with pytest.raises(ValueError, match="initial_trim_m"):
        screen(*tanks, trim=trim)


def test_overflow_fails_closed(tanks):
    a, b = tanks
    with pytest.raises(ValueError, match="transfer_mass_t"):
        screen(a, b, trim=1e308)
    with pytest.raises(ValueError, match="lever_arm_m"):
        screen(replace(a, position_m=-1e308), replace(b, position_m=1e308))


_RECORDS = [
    ("marine-engineering", "en400.md", "EN400", "USNA", "Summer 2020"),
    ("engineering-standards", "nist-sp-811.md", "NIST-SP-811", "NIST", "2008"),
]


@pytest.fixture
def references(tmp_path):
    """Isolated metadata fixtures do not qualify a live reference store."""
    for domain, filename, code, publisher, revision in _RECORDS:
        target = tmp_path / "wikis" / domain / "wiki" / "standards" / filename
        target.parent.mkdir(parents=True, exist_ok=True)
        target.write_text(
            f"---\ncode_id: {code}\npublisher: {publisher}\nrevision: {revision}\n---\n",
            encoding="utf-8",
        )
    return tmp_path


@pytest.mark.parametrize("trim", [-0.4, 0.0, 0.4])
def test_cited_result(tanks, references, trim):
    result = screen_even_keel_transfer_cited(
        tank_a=tanks[0], tank_b=tanks[1], initial_trim_m=trim,
        mct_t_m_per_cm=100.0, repo_root=references,
    )
    assert result["value"] == screen(*tanks, trim=trim)
    assert result["units"] == "t"
    assert [c.code_id for c in result["citations"]] == ["EN400", "NIST-SP-811"]


@pytest.mark.parametrize("record", _RECORDS)
@pytest.mark.parametrize("defect", ["missing", "code_id", "publisher", "revision"])
@pytest.mark.parametrize("trim", [0.0, -0.4])
def test_cited_references_fail_closed(tanks, references, record, defect, trim):
    domain, filename, code, _, _ = record
    target = references / "wikis" / domain / "wiki" / "standards" / filename
    if defect == "missing":
        target.rename(target.with_suffix(".unavailable"))
    else:
        lines = target.read_text(encoding="utf-8").splitlines()
        target.write_text("\n".join(
            f"{defect}: invalid" if line.startswith(f"{defect}:") else line
            for line in lines
        ) + "\n", encoding="utf-8")
    with pytest.raises(CitationResolutionError) as error:
        screen_even_keel_transfer_cited(
            tank_a=tanks[0], tank_b=tanks[1], initial_trim_m=trim,
            mct_t_m_per_cm=100.0, repo_root=references,
        )
    assert error.value.code_id == code
    assert error.value.reason.startswith(
        "page_missing" if defect == "missing" else f"frontmatter_mismatch:{defect}"
    )
