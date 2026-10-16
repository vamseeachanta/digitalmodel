"""
ABOUTME: The four hull-scale-dependent quantities that are NOT geometry (#2023):
inlet turbulence, the cell budget, the decomposition vector, and the force
coefficient reference values. Each one is a literal in the frozen KCS template
and each one is wrong for any other hull.
"""

from __future__ import annotations

import math

import pytest
import yaml

from digitalmodel.solvers.openfoam.hull_case_physics import (
    C_MU,
    KCS_CHAIN_DECOMPOSITION,
    CellBudgetError,
    DecompositionError,
    TurbulenceMethod,
    decomposition_vector,
    derive_cell_budget,
    derive_force_reference,
    derive_inlet_turbulence,
    kcs_chain_config_path,
    wall_normal_first_cell_height,
)
from digitalmodel.solvers.openfoam.hull_domain import (
    block_divisions,
    build_hull_domain,
    refinement_boxes,
)
from digitalmodel.solvers.openfoam.hull_manifest import HullManifest

from .conftest import scaled_manifest_dict

U = 2.0
LPP = 6.0
NU = 1.09e-6


@pytest.fixture
def manifest(manifest_dict) -> HullManifest:
    return HullManifest.from_dict(manifest_dict)


# --------------------------------------------------------------------------- #
#  Inlet turbulence -- 0.orig/{k,omega,nut} carry fixed DTC values
# --------------------------------------------------------------------------- #

def test_k_is_three_halves_of_the_intensity_velocity_product() -> None:
    """k = 3/2 (U I)^2 -- isotropic turbulence at a stated intensity."""
    t = derive_inlet_turbulence(U, LPP, NU, intensity=0.02)
    assert t.k == pytest.approx(1.5 * (U * 0.02) ** 2)


def test_k_scales_with_the_square_of_the_speed() -> None:
    a = derive_inlet_turbulence(U, LPP, NU)
    b = derive_inlet_turbulence(2 * U, LPP, NU)
    assert b.k == pytest.approx(4.0 * a.k)


def test_viscosity_ratio_method_sets_nut_relative_to_the_molecular_value() -> None:
    """nut_inf = R_nu * nu, then omega = k / nut_inf.

    This is the default because it controls the quantity that actually damages
    a resistance answer. A macroscopic length scale at model-scale Reynolds
    numbers puts the freestream eddy viscosity two to four orders of magnitude
    above molecular; that turbulence convects 1.5 Lpp to the hull, thickens the
    boundary layer, and lands squarely on the viscous coefficient.
    """
    t = derive_inlet_turbulence(
        U, LPP, NU, method=TurbulenceMethod.VISCOSITY_RATIO, viscosity_ratio=0.1
    )
    assert t.nut == pytest.approx(0.1 * NU)
    assert t.omega == pytest.approx(t.k / t.nut)


def test_length_scale_method_uses_lpp() -> None:
    """omega = sqrt(k) / (C_mu^{1/4} l), l = factor * Lpp."""
    t = derive_inlet_turbulence(
        U, LPP, NU, method=TurbulenceMethod.LENGTH_SCALE, length_scale_factor=0.07
    )
    assert t.length_scale == pytest.approx(0.07 * LPP)
    assert t.omega == pytest.approx(
        math.sqrt(t.k) / (C_MU**0.25 * t.length_scale)
    )
    assert t.nut == pytest.approx(t.k / t.omega)


def test_nut_is_always_k_over_omega() -> None:
    """The kOmegaSST definition, whichever method set omega."""
    for method in TurbulenceMethod:
        t = derive_inlet_turbulence(U, LPP, NU, method=method)
        assert t.nut == pytest.approx(t.k / t.omega)


def test_implied_length_scale_is_reported_for_the_viscosity_ratio_method() -> None:
    """A reviewer needs to see the length scale the ratio implies."""
    t = derive_inlet_turbulence(U, LPP, NU, method=TurbulenceMethod.VISCOSITY_RATIO)
    assert t.length_scale == pytest.approx(
        math.sqrt(t.k) / (C_MU**0.25 * t.omega)
    )
    assert t.length_scale > 0


def test_none_of_the_dtc_literals_survive() -> None:
    """0.orig/k 0.00015, omega 2, nut 5e-07 are DTC's, at DTC's condition."""
    t = derive_inlet_turbulence(U, LPP, NU)
    assert t.k != pytest.approx(0.00015)
    assert t.omega != pytest.approx(2.0)
    assert t.nut != pytest.approx(5e-07)


@pytest.mark.parametrize("bad", [0.0, -0.01, 1.5])
def test_implausible_intensity_is_refused(bad) -> None:
    with pytest.raises(ValueError, match="intensity"):
        derive_inlet_turbulence(U, LPP, NU, intensity=bad)


def test_zero_speed_is_refused() -> None:
    with pytest.raises(ValueError, match="velocity"):
        derive_inlet_turbulence(0.0, LPP, NU)


# --------------------------------------------------------------------------- #
#  The Reynolds-number requirement snappy's relative sizes do not see
# --------------------------------------------------------------------------- #

def test_first_cell_height_falls_as_reynolds_rises() -> None:
    """snappy's settings are scale-invariant; the requirement is not.

    ``relativeSizes true`` with hull level (0 0) means the near-wall mesh is a
    fixed fraction of the local background cell, so a hull ten times larger at
    the same speed gets a geometrically similar mesh -- at ten times the
    Reynolds number, where the wall layer it has to resolve is relatively
    thinner. The number below is what the mesh would have to deliver; it is
    recorded in the provenance so the gap is visible rather than assumed away.
    """
    small = wall_normal_first_cell_height(U, LPP, NU)
    big = wall_normal_first_cell_height(U, 10 * LPP, NU)
    assert big > small, "an absolute height grows with the hull"
    assert big / (10 * LPP) < small / LPP, "but the RELATIVE height shrinks"


def test_first_cell_height_is_proportional_to_the_y_plus_target() -> None:
    assert wall_normal_first_cell_height(U, LPP, NU, y_plus=100.0) == pytest.approx(
        2.0 * wall_normal_first_cell_height(U, LPP, NU, y_plus=50.0)
    )


# --------------------------------------------------------------------------- #
#  maxGlobalCells -- snappyHexMeshDict:61 ships a fixed 2000000
# --------------------------------------------------------------------------- #

@pytest.fixture
def budget_inputs(manifest):
    domain = build_hull_domain(manifest)
    return domain, refinement_boxes(manifest, domain), block_divisions(domain)


def test_background_count_is_the_exact_blockmesh_product(budget_inputs) -> None:
    domain, boxes, div = budget_inputs
    b = derive_cell_budget(domain, boxes, div, ranks=8)
    layers = 2 * div["nza"] + 2 * div["nzb"] + div["nzc"] + div["nzd"]
    assert b.background_cells == div["nx"] * div["ny"] * layers


def test_estimate_exceeds_the_background_because_six_stages_refine(budget_inputs):
    domain, boxes, div = budget_inputs
    b = derive_cell_budget(domain, boxes, div, ranks=8)
    assert b.estimated_cells > b.background_cells


def test_cap_is_carried_forward_when_it_is_large_enough(budget_inputs) -> None:
    domain, boxes, div = budget_inputs
    b = derive_cell_budget(domain, boxes, div, ranks=8)
    capped = derive_cell_budget(
        domain, boxes, div, ranks=8, cap=b.estimated_cells * 4
    )
    assert capped.max_global_cells == b.estimated_cells * 4


def test_a_cap_below_the_estimate_ERRORS_rather_than_truncating(budget_inputs):
    """snappyHexMesh stops refining at maxGlobalCells and reports success.

    The mesh it hands back is missing refinement levels it never announced. The
    solve then runs for days on a mesh nobody chose. Erroring here costs a
    millisecond.
    """
    domain, boxes, div = budget_inputs
    b = derive_cell_budget(domain, boxes, div, ranks=8)
    with pytest.raises(CellBudgetError, match="(?i)truncat"):
        derive_cell_budget(domain, boxes, div, ranks=8, cap=b.estimated_cells // 2)


def test_the_error_reports_both_numbers(budget_inputs) -> None:
    domain, boxes, div = budget_inputs
    b = derive_cell_budget(domain, boxes, div, ranks=8)
    cap = b.estimated_cells // 2
    with pytest.raises(CellBudgetError) as exc:
        derive_cell_budget(domain, boxes, div, ranks=8, cap=cap)
    assert str(cap) in str(exc.value)
    assert str(b.estimated_cells) in str(exc.value)


def test_uncapped_budget_carries_headroom_over_the_estimate(budget_inputs) -> None:
    domain, boxes, div = budget_inputs
    b = derive_cell_budget(domain, boxes, div, ranks=8, safety_factor=1.5)
    assert b.max_global_cells == math.ceil(b.estimated_cells * 1.5)


def test_max_local_cells_is_derived_from_the_rank_count(budget_inputs) -> None:
    domain, boxes, div = budget_inputs
    b8 = derive_cell_budget(domain, boxes, div, ranks=8)
    b32 = derive_cell_budget(domain, boxes, div, ranks=32)
    assert b8.max_local_cells > b32.max_local_cells
    assert b32.max_local_cells >= 1


def test_the_dtc_literal_does_not_survive(budget_inputs) -> None:
    domain, boxes, div = budget_inputs
    b = derive_cell_budget(domain, boxes, div, ranks=8)
    assert b.max_global_cells != 2000000


def test_finest_cell_size_halves_once_per_refinement_stage(budget_inputs) -> None:
    domain, boxes, div = budget_inputs
    b = derive_cell_budget(domain, boxes, div, ranks=8)
    assert b.finest_cell_size == pytest.approx(
        domain.base_cell_size / 2 ** len(boxes)
    )


def test_budget_grows_with_the_hull_when_the_cell_size_is_held(manifest_dict) -> None:
    """The scale-sanity check, stated on the budget.

    Hold the target cell size fixed and take the hull to ten times its length:
    the domain grows by 10 in each direction and the cell count follows the
    volume. A hard-coded budget is invisible on one hull and obvious here.
    """
    small = HullManifest.from_dict(manifest_dict)
    big = HullManifest.from_dict(scaled_manifest_dict(10.0))
    h = small.lpp_m / 6.3

    ds = build_hull_domain(small, base_cell_size=h)
    db = build_hull_domain(big, base_cell_size=h)
    bs = derive_cell_budget(
        ds, refinement_boxes(small, ds), block_divisions(ds), ranks=8
    )
    bb = derive_cell_budget(
        db, refinement_boxes(big, db), block_divisions(db), ranks=8
    )

    assert db.volume == pytest.approx(ds.volume * 1000.0)
    assert bb.max_global_cells > bs.max_global_cells * 100
    assert bb.background_cells > bs.background_cells * 100


# --------------------------------------------------------------------------- #
#  numberOfSubdomains -- decomposeParDict:17 ships an untokenised 8
# --------------------------------------------------------------------------- #

@pytest.mark.parametrize("ranks", [1, 2, 3, 4, 6, 8, 12, 16, 24, 32, 48, 64, 7, 11])
def test_the_vector_always_multiplies_to_the_rank_count(ranks) -> None:
    """hierarchical decomposition is fatal when prod(n) != numberOfSubdomains.

    It is fatal AFTER the mesh is built, which on a production grid is hours.
    """
    n = decomposition_vector(ranks)
    assert n[0] * n[1] * n[2] == ranks


def test_the_vector_reproduces_the_committed_chain_table() -> None:
    """Same answers as ``cfd_config.py decompose`` for the ranks it knows."""
    for ranks, expected in KCS_CHAIN_DECOMPOSITION.items():
        assert decomposition_vector(ranks) == expected


def test_the_committed_chain_table_is_the_source_of_truth() -> None:
    """Pinned against config/cfd/kcs_chain.yml so the two cannot drift apart."""
    path = kcs_chain_config_path()
    table = yaml.safe_load(path.read_text())["defaults"]["decompose_n"]
    assert {int(k): tuple(v) for k, v in table.items()} == KCS_CHAIN_DECOMPOSITION


def test_the_longest_axis_is_split_first() -> None:
    """x is the long axis of a towing domain; compact subdomains cut halo."""
    assert decomposition_vector(32) == (8, 2, 2)
    assert decomposition_vector(16)[0] >= decomposition_vector(16)[1]


@pytest.mark.parametrize("bad", [0, -4])
def test_non_positive_ranks_are_refused(bad) -> None:
    with pytest.raises(DecompositionError, match="ranks"):
        decomposition_vector(bad)


def test_an_override_that_does_not_multiply_out_is_refused() -> None:
    with pytest.raises(DecompositionError, match="multipl"):
        decomposition_vector(8, table={8: (3, 3, 3)})


def test_an_override_table_wins() -> None:
    assert decomposition_vector(8, table={8: (8, 1, 1)}) == (8, 1, 1)


# --------------------------------------------------------------------------- #
#  forceCoeffs reference values
# --------------------------------------------------------------------------- #

def test_reference_values_come_from_the_manifest(manifest) -> None:
    domain = build_hull_domain(manifest)
    ref = derive_force_reference(manifest, velocity=2.0, density=998.8, domain=domain)
    assert ref.mag_u_inf == pytest.approx(2.0)
    assert ref.l_ref == pytest.approx(manifest.lpp_m)
    assert ref.rho_inf == pytest.approx(998.8)


def test_aref_is_half_the_wetted_surface_on_a_half_domain(manifest) -> None:
    """The solve is cut at the centreplane and reports half of every force.

    forceCoeffs divides by 0.5 rho U^2 Aref and cannot double the numerator, so
    the halving has to happen in the denominator. Using the full wetted surface
    here is a clean factor of two in Ct.
    """
    domain = build_hull_domain(manifest)
    ref = derive_force_reference(manifest, velocity=2.0, density=998.8, domain=domain)
    assert ref.a_ref == pytest.approx(manifest.wetted_surface_m2 / 2.0)
    assert ref.half_domain is True

    full = derive_force_reference(
        manifest, velocity=2.0, density=998.8, domain=domain, half_domain=False
    )
    assert full.a_ref == pytest.approx(manifest.wetted_surface_m2)


def test_cofr_is_midship_at_the_free_surface(manifest) -> None:
    domain = build_hull_domain(manifest)
    ref = derive_force_reference(manifest, velocity=2.0, density=998.8, domain=domain)
    assert ref.c_of_r[0] == pytest.approx(0.0)
    assert ref.c_of_r[1] == pytest.approx(0.0)
    assert ref.c_of_r[2] == pytest.approx(manifest.draft_m)


def test_drag_direction_follows_the_flow(manifest) -> None:
    domain = build_hull_domain(manifest)
    ref = derive_force_reference(manifest, velocity=2.0, density=998.8, domain=domain)
    assert ref.drag_direction == (-1.0, 0.0, 0.0)
    assert ref.lift_direction == (0.0, 0.0, 1.0)


def test_no_kcs_constant_reaches_the_reference_values(manifest) -> None:
    domain = build_hull_domain(manifest)
    ref = derive_force_reference(manifest, velocity=2.0, density=998.8, domain=domain)
    assert ref.l_ref != pytest.approx(7.2786), "KCS Lpp"
    assert ref.a_ref != pytest.approx(9.4379), "KCS published wetted surface"
