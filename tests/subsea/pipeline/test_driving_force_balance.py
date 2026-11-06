# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent hand calculation: synthetic restrained-pipe force balance.

The synthetic basis uses binary-exact inputs so cancellation can be checked
with exact equality. These are test inputs, not design data or table values:
D = 0.5 m, wall t = 0.125 m, E = 2**37 Pa, alpha = 2**-16 /K,
dT = 32 K, internal pressure p = 2**24 Pa and Poisson ratio nu = 0.25.
The required submerged weight is an arbitrary positive 1 N/m; it does not enter
this force balance. Zero loading isolates terms; boundary offsets are +/-1 N.

Hand calculation, independent of section properties and force helpers:
the bore diameter is d = D - 2t = 0.25 m. Subtracting the circular areas gives
A = pi*(D/2)**2 - pi*(d/2)**2 = 3*pi/64 m**2, while Ab = pi/64 m**2.
Restrained thermal strain alpha*dT gives axial stress E*alpha*dT [Pa], hence
thermal compression FT = E*A*alpha*dT = 3*pi*2**20 N. Pressure contributes
FP = p*Ab*(1 - 2*nu) = pi*2**17 N. Both products have units Pa*m**2 = N.
The combined compression is FT + FP = 25*pi*2**17 N. Residual tension H [N]
opposes compression: the unclipped balance is FT + FP - H, with net tension
returned as exactly zero. Setting H = FP leaves FT; setting H = FT + FP
cancels compression. Positive-force comparisons use relative tolerance 1e-12
for floating-point arithmetic, with no absolute tolerance; zero is exact.
"""

import math

import pytest

from digitalmodel.subsea.pipeline.global_buckling import (
    PipeSection,
    effective_driving_force,
)

# Synthetic inputs and independently reduced expectations: see the basis above.
OUTER_DIAMETER_M = 0.5
WALL_THICKNESS_M = 0.125
ELASTIC_MODULUS_PA = 2.0**37
THERMAL_EXPANSION_PER_K = 2.0**-16
TEMPERATURE_RISE_K = 32.0
INTERNAL_PRESSURE_PA = 2.0**24
POISSON_RATIO = 0.25
BORE_AREA_M2 = math.pi / 64.0
THERMAL_FORCE_N = 3.0 * math.pi * 2.0**20
PRESSURE_FORCE_N = math.pi * 2.0**17
COMBINED_FORCE_N = 25.0 * math.pi * 2.0**17


@pytest.fixture
def synthetic_pipe() -> PipeSection:
    """Construct the input section from the synthetic geometry and material."""
    return PipeSection.from_dimensions(
        e_modulus_pa=ELASTIC_MODULUS_PA,
        od_m=OUTER_DIAMETER_M,
        wt_m=WALL_THICKNESS_M,
        submerged_weight_N_m=1.0,
        thermal_expansion_per_K=THERMAL_EXPANSION_PER_K,
    )


@pytest.mark.parametrize(
    "temperature_rise_K, pressure_pa, residual_tension_N, expected_force_N",
    [
        pytest.param(TEMPERATURE_RISE_K, 0.0, 0.0, THERMAL_FORCE_N, id="thermal"),
        pytest.param(0.0, INTERNAL_PRESSURE_PA, 0.0, PRESSURE_FORCE_N, id="pressure"),
        pytest.param(
            TEMPERATURE_RISE_K, INTERNAL_PRESSURE_PA, 0.0,
            COMBINED_FORCE_N, id="combined",
        ),
        pytest.param(
            TEMPERATURE_RISE_K, INTERNAL_PRESSURE_PA, PRESSURE_FORCE_N,
            THERMAL_FORCE_N, id="combined-with-residual-tension",
        ),
    ],
)
def test_driving_force_terms_match_independent_force_balance(
    synthetic_pipe, temperature_rise_K, pressure_pa, residual_tension_N,
    expected_force_N,
):
    force_N = effective_driving_force(
        synthetic_pipe,
        temperature_rise_K=temperature_rise_K,
        internal_pressure_pa=pressure_pa,
        internal_area_m2=BORE_AREA_M2,
        poisson_ratio=POISSON_RATIO,
        residual_lay_tension_N=residual_tension_N,
    )
    assert force_N == pytest.approx(expected_force_N, rel=1e-12, abs=0.0)


@pytest.mark.parametrize(
    "tension_offset_N, expected_force_N",
    [
        pytest.param(-1.0, 1.0, id="compression-one-newton"),
        pytest.param(0.0, 0.0, id="exact-cancellation"),
        pytest.param(1.0, 0.0, id="net-tension-clipped"),
    ],
)
def test_residual_tension_cancels_compression_at_boundary(
    synthetic_pipe, tension_offset_N, expected_force_N,
):
    force_N = effective_driving_force(
        synthetic_pipe,
        temperature_rise_K=TEMPERATURE_RISE_K,
        internal_pressure_pa=INTERNAL_PRESSURE_PA,
        internal_area_m2=BORE_AREA_M2,
        poisson_ratio=POISSON_RATIO,
        residual_lay_tension_N=COMBINED_FORCE_N + tension_offset_N,
    )
    assert force_N == expected_force_N
