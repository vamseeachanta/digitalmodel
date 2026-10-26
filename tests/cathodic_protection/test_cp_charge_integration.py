# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Coating-to-charge integration with independent synthetic oracles.

Plan-lite
---------
Scope: this test module alone connects coating breakdown, current demand,
installed anode mass and consumed mass. Production code and inputs are unchanged.
Oracles: hand dimensional identities [D] and the independent antiderivative [C]
check synthetic charge budgets without production functions or constants.
Offline validation, from the repository root with the declared dependencies::

    PYTHONPATH=src uv run --no-project --with 'pytest>=7.4.3,<9' \
        --with 'hypothesis>=6.100,<7' --with pytest-asyncio==0.21.1 \
        --with 'pydantic>=2.7,<3' --with pyyaml --with scipy python -m pytest \
        tests/cathodic_protection/test_cp_charge_integration.py \
        tests/cathodic_protection/test_cp_cross_module_consistency.py \
        tests/cathodic_protection/test_properties.py -q -rA -p no:cacheprovider \
        --basetemp="$TMPDIR/../bt-final"

Deferred integration paths: inspection recommendations and retrofit assessment.
Synthetic charge identities do not qualify physical design inputs.

Basis and citations for the constants and expected values in this module:

[D] Dimensional identities, by hand: I [A] = area [m2] * density [A/m2] * f;
Q [Ah] = integral(I dt) with time in hours; consumed [kg] = Q / capacity
[Ah/kg]; installed [kg] = Q_design / (capacity * utilisation). Thus installed
usable charge is installed * utilisation * capacity. A non-leap test year is
365 days * 24 hours; the SI prefix milli gives 1000 mA/A. No production
conversion constant is used to construct an oracle.

[C] Independent calculus: f(t) = min(a + b*t, 1), t in years. Before the cap,
integral(f dt) = a*T + b*T**2/2. If c = (1-a)/b lies inside the life, the
integral is a*c + b*c**2/2 + (T-c). The kernel's documented design convention
in ``_kernels.coating_breakdown_mean`` is min(a + b*T/2, 1), the capped
midpoint, which is conservative rather than the exact time average when the
cap is crossed. No equality to the true mean is required in that case.

[S] Synthetic inputs, declared here rather than selected from standard tables:
single-zone area 120 m2, density 0.25 A/m2, life 8 years, capacity 1500 Ah/kg,
utilisation 0.8. Bare and intact factors are respectively 1 and 0. A linear
case uses a=0.1, b=0.05/year; a crossing case uses a=0.2, b=0.2/year.
The multi-zone test declares different areas, densities, lives, capacities
and utilisations explicitly, with a hand calculation beside each oracle.

[G] Generated domains are synthetic: area 0..10000 m2 and density 0..1 A/m2
include zero demand; a 0..1 and b either 0 or 0.001..0.5/year include intact,
bare, uncapped and capped laws without dividing by a tiny nonzero rate when
finding the cap crossing; life 0.1..50 years stays positive. Capacity 100..4000 Ah/kg
and utilisation 0.05..1 keep denominators away from zero. Partition fractions
0.1..0.9 give two positive durations. These bounds avoid overflow and tiny
denominators, not prescribe physical defaults. Each property uses 100 finite
examples with no deadline or database, following the existing property suite.

[N] Relative tolerance 1e-12 and absolute tolerance 1e-9 allow roundoff in
short products and sums of binary floating-point values; neither represents
an engineering allowance. Zero and one elsewhere are additive identities and
fraction limits; two is the analytic integration/midpoint divisor in [C].
"""

from __future__ import annotations

import math
from dataclasses import dataclass

import pytest
from hypothesis import example, given, settings
from hypothesis import strategies as st

from digitalmodel.cathodic_protection import _kernels as kernel
from digitalmodel.cathodic_protection.anode_sizing import calculate_current_demand
from digitalmodel.cathodic_protection.iso_15589_2 import anode_mass_requirement

HOURS_PER_YEAR = 365.0 * 24.0  # [D], independently of the production constant.
MA_PER_A = 1000.0  # [D].
REL = 1e-12  # [N].
ABS = 1e-9  # [N].
PROPERTY = settings(max_examples=100, deadline=None, database=None)  # [G].


@dataclass(frozen=True)
class Budget:
    """Outputs of the connected design and lifetime-consumption paths."""

    current_A: float
    installed_kg: float
    consumed_kg: float


def _chain(area, density, a, b, life, capacity, utilisation):
    """Connect coating, public demand/sizing wrappers and consumed metal."""
    mean_factor = kernel.coating_breakdown_mean(a, b, life)
    current = calculate_current_demand(area, mean_factor, density * MA_PER_A)
    installed = anode_mass_requirement(current, life, capacity, utilisation)

    # Integrate the actual demand with midpoint samples on each linear piece.
    # Splitting at the cap makes this exact for the synthetic piecewise law;
    # the independent oracle below uses an antiderivative instead of samples.
    endpoints = [0.0, life]
    if b > 0.0:
        crossing = (1.0 - a) / b
        if 0.0 < crossing < life:
            endpoints.insert(1, crossing)
    consumed = math.fsum(
        kernel.mass_consumed(
            calculate_current_demand(
                area,
                kernel.coating_breakdown_linear(a, b, (start + end) / 2.0),
                density * MA_PER_A,
            ),
            end - start,
            capacity,
        )
        for start, end in zip(endpoints, endpoints[1:])
    )
    return Budget(current, installed, consumed)


def _exposure_years(a, b, life):
    """Hand antiderivative [C], without any function or constant under test."""
    ramp_end = life if b == 0.0 else min(life, (1.0 - a) / b)
    return a * ramp_end + b * ramp_end**2 / 2.0 + (life - ramp_end)


def _assert_close(actual, expected):
    assert actual == pytest.approx(expected, rel=REL, abs=ABS)


def _assert_coverage(usable_charge, consumed_charge):
    assert usable_charge >= consumed_charge - max(ABS, REL * abs(consumed_charge))


@pytest.mark.parametrize(
    "a,b,exposure_years,design_mean",
    [
        # [S,C]: integral(1 dt) = T = 8 years.
        pytest.param(1.0, 0.0, 8.0, 1.0, id="bare-surface"),
        # [S,C]: integral(0 dt) = 0; both installed and consumed mass vanish.
        pytest.param(0.0, 0.0, 0.0, 0.0, id="intact-coating"),
        # [S,C]: 0.1*8 + 0.05*8**2/2 = 2.4; mean = 2.4/8 = 0.3.
        pytest.param(0.1, 0.05, 2.4, 0.3, id="linear-breakdown"),
        # [S,C]: cap at (1-0.2)/0.2 = 4 years. Integral =
        # 0.2*4 + 0.2*4**2/2 + (8-4) = 6.4, while design mean = 1.
        pytest.param(0.2, 0.2, 6.4, 1.0, id="crossed-clamp"),
    ],
)
def test_single_zone_chain(a, b, exposure_years, design_mean):
    """Four hand-integrated scenarios, including zero demand and the cap."""
    area, density, life, capacity, utilisation = 120.0, 0.25, 8.0, 1500.0, 0.8  # [S]
    result = _chain(area, density, a, b, life, capacity, utilisation)
    charge = area * density * HOURS_PER_YEAR * exposure_years  # [D,C].
    design_charge = area * density * design_mean * life * HOURS_PER_YEAR
    _assert_close(result.current_A, area * density * design_mean)
    _assert_close(result.installed_kg, design_charge / (capacity * utilisation))
    _assert_close(result.consumed_kg, charge / capacity)
    _assert_close(result.installed_kg * utilisation * capacity, design_charge)
    _assert_coverage(result.installed_kg * utilisation * capacity, charge)
    if a + b * life > 1.0:
        # The capped midpoint is conservative, NOT the true average 6.4/8.
        assert design_charge > charge
        assert result.installed_kg * utilisation > result.consumed_kg
    else:
        _assert_close(result.installed_kg * utilisation, result.consumed_kg)


def test_multi_zone_chain_sums_declared_budgets():
    """Sum independent zone budgets without averaging material parameters."""
    # [S,C]: zone area, density, a, b, life, capacity, utilisation, integral(f dt).
    zones = [
        # 0.1*6 + 0.1*6**2/2 = 2.4 years, uncapped, design mean 0.4.
        (75.0, 0.4, 0.1, 0.1, 6.0, 1250.0, 0.75, 2.4),
        # 0.25*2 + 0.375*2**2/2 + (5-2) = 4.25; design mean capped at 1.
        (160.0, 0.15, 0.25, 0.375, 5.0, 1750.0, 0.6, 4.25),
    ]
    results = [_chain(*zone[:-1]) for zone in zones]
    charges = [
        area * density * exposure * HOURS_PER_YEAR
        for area, density, a, b, life, capacity, utilisation, exposure in zones
    ]
    # [S,D]: zone design charges from the hand means 0.4 and 1 above.
    design_charges = [
        75.0 * 0.4 * 0.4 * 6.0 * HOURS_PER_YEAR,
        160.0 * 0.15 * 1.0 * 5.0 * HOURS_PER_YEAR,
    ]
    for result, zone, charge, design_charge in zip(
        results, zones, charges, design_charges
    ):
        area, density, a, b, life, capacity, utilisation, exposure = zone
        _assert_close(result.current_A, design_charge / (life * HOURS_PER_YEAR))
        _assert_close(result.installed_kg, design_charge / (capacity * utilisation))
        _assert_close(result.consumed_kg, charge / capacity)
        _assert_coverage(result.installed_kg * utilisation * capacity, charge)
    _assert_close(
        math.fsum(result.installed_kg for result in results),
        math.fsum(q / (zone[5] * zone[6]) for q, zone in zip(design_charges, zones)),
    )
    _assert_close(
        math.fsum(result.consumed_kg for result in results),
        math.fsum(q / zone[5] for q, zone in zip(charges, zones)),
    )
    usable_charge = math.fsum(
        result.installed_kg * zone[6] * zone[5] for result, zone in zip(results, zones)
    )
    consumed_charge = math.fsum(
        result.consumed_kg * zone[5] for result, zone in zip(results, zones)
    )
    _assert_close(consumed_charge, math.fsum(charges))
    _assert_close(usable_charge, math.fsum(design_charges))
    assert usable_charge > consumed_charge


def _finite(lo, hi):
    return st.floats(min_value=lo, max_value=hi, allow_nan=False, allow_infinity=False)


# All strategy bounds, including zero and unit endpoints, are justified in [G].
AREA = _finite(0.0, 10000.0)
DENSITY = _finite(0.0, 1.0)
INITIAL = _finite(0.0, 1.0)
RATE = st.one_of(st.just(0.0), _finite(0.001, 0.5))
LIFE = _finite(0.1, 50.0)
CAPACITY = _finite(100.0, 4000.0)
UTILISATION = _finite(0.05, 1.0)
PARTITION = _finite(0.1, 0.9)


@PROPERTY
@given(
    area=AREA,
    density=DENSITY,
    a=INITIAL,
    b=RATE,
    life=LIFE,
    capacity=CAPACITY,
    utilisation=UTILISATION,
    fraction=PARTITION,
)
def test_chain_area_partition_invariance(
    area, density, a, b, life, capacity, utilisation, fraction
):
    """Splitting one surface preserves demand, installed and consumed mass."""
    whole = _chain(area, density, a, b, life, capacity, utilisation)
    parts = [
        _chain(part_area, density, a, b, life, capacity, utilisation)
        for part_area in (area * fraction, area * (1.0 - fraction))
    ]
    _assert_close(whole.current_A, math.fsum(part.current_A for part in parts))
    _assert_close(whole.installed_kg, math.fsum(part.installed_kg for part in parts))
    _assert_close(whole.consumed_kg, math.fsum(part.consumed_kg for part in parts))
    charge = area * density * HOURS_PER_YEAR * _exposure_years(a, b, life)
    _assert_close(whole.consumed_kg * capacity, charge)
    _assert_coverage(whole.installed_kg * utilisation * capacity, charge)


@PROPERTY
@given(
    area=AREA,
    density=DENSITY,
    a=INITIAL,
    life=LIFE,
    capacity=CAPACITY,
    utilisation=UTILISATION,
    fraction=PARTITION,
)
def test_chain_constant_current_exposure_partition(
    area, density, a, life, capacity, utilisation, fraction
):
    """Equivalent constant-current time pieces preserve charge and both masses."""
    whole = _chain(area, density, a, 0.0, life, capacity, utilisation)
    parts = [
        _chain(area, density, a, 0.0, duration, capacity, utilisation)
        for duration in (life * fraction, life * (1.0 - fraction))
    ]
    charge = area * density * a * life * HOURS_PER_YEAR  # [D,C], b=0.
    for part in parts:
        _assert_close(part.current_A, area * density * a)
    _assert_close(whole.installed_kg, math.fsum(part.installed_kg for part in parts))
    _assert_close(whole.consumed_kg, math.fsum(part.consumed_kg for part in parts))
    _assert_close(whole.installed_kg * utilisation * capacity, charge)
    _assert_close(whole.consumed_kg * capacity, charge)


@PROPERTY
@given(
    area=AREA,
    density=DENSITY,
    a=INITIAL,
    b=RATE,
    life=LIFE,
    capacity=CAPACITY,
    utilisation=UTILISATION,
)
# [S,C]: explicit crossings at (1-0.1)/0.15 = 6 and (1-0.2)/0.4 = 2 years,
# respectively after and before the design midpoint at 8/2 = 4 years.
@example(
    area=120.0,
    density=0.25,
    a=0.1,
    b=0.15,
    life=8.0,
    capacity=1500.0,
    utilisation=0.8,
)
@example(
    area=120.0,
    density=0.25,
    a=0.2,
    b=0.4,
    life=8.0,
    capacity=1500.0,
    utilisation=0.8,
)
def test_chain_installed_usable_charge_covers_consumption(
    area, density, a, b, life, capacity, utilisation
):
    """Installed usable charge covers the independently integrated actual load."""
    result = _chain(area, density, a, b, life, capacity, utilisation)
    charge = area * density * HOURS_PER_YEAR * _exposure_years(a, b, life)
    consumed_charge = result.consumed_kg * capacity
    usable_charge = result.installed_kg * utilisation * capacity
    _assert_close(consumed_charge, charge)
    _assert_coverage(usable_charge, charge)
    _assert_coverage(usable_charge, consumed_charge)
    if a + b * life <= 1.0:
        _assert_close(usable_charge, charge)
