"""Tests for the high-fidelity solver tier (issue #1025).

Offline: validates input generation (analytic GDF meshes, OrcaWave spec, OrcaFlex
config), fail-closed dispatch and the RAO result-fold. No OrcaWave/OrcaFlex
licence required -- the solve itself is the licensed step.
"""

from pathlib import Path

import pytest
import yaml

from digitalmodel.floating_wind.floaters import IEA_15MW_RNA
from digitalmodel.floating_wind.screening import LoadCase, screen_concept
from digitalmodel.floating_wind.solver_tier import (
    FrequencyGrid,
    box_gdf,
    build_orcawave_spec,
    build_panel_mesh,
    cylinder_gdf,
    dispatch_case,
    fold_rao_results,
    prepare_shortlist,
    prepare_variant,
)
from digitalmodel.orcaflex.batch_parametric import ParameterSweep

LOADS = [
    LoadCase(name="operational", hs_m=2.5, tp_s=8.0),
    LoadCase(name="extreme", hs_m=10.0, tp_s=14.0),
]


def _spar_variant():
    res = screen_concept(
        "spar",
        dict(diameter=20.0, draft=120.0, steel_mass_t=6000.0),
        sweeps=[ParameterSweep(name="draft", values=[120.0])],
        topside=IEA_15MW_RNA,
        load_cases=LOADS,
    )
    return res.variants[0]


def _barge_variant():
    res = screen_concept(
        "barge",
        dict(length=60.0, beam=60.0, draft=12.0, steel_mass_t=5000.0),
        sweeps=[ParameterSweep(name="beam", values=[60.0])],
        topside=IEA_15MW_RNA,
        load_cases=LOADS,
    )
    return res.variants[0]


def _semi_variant():
    res = screen_concept(
        "semi",
        dict(
            n_columns=3,
            column_diameter=12.5,
            column_radius=51.75,
            draft=20.0,
            pontoon_height=7.0,
            pontoon_volume=10000.0,
            steel_mass_t=4000.0,
        ),
        sweeps=[ParameterSweep(name="column_diameter", values=[12.5])],
        topside=IEA_15MW_RNA,
        load_cases=LOADS,
    )
    return res.variants[0]


def _parse_gdf_panel_count(text: str) -> int:
    lines = text.strip().splitlines()
    declared = int(lines[3])
    vertices = len(lines) - 4
    assert vertices == declared * 4  # 4 vertices per panel
    return declared


def test_box_gdf_is_well_formed():
    text = box_gdf(60.0, 60.0, 12.0, nx=6, ny=6, nz=3)
    n = _parse_gdf_panel_count(text)
    # bottom nx*ny + 2*(nx*nz) + 2*(ny*nz) = 36 + 36 + 36 = 108
    assert n == 6 * 6 + 2 * 6 * 3 + 2 * 6 * 3


def test_cylinder_gdf_is_well_formed():
    text = cylinder_gdf(20.0, 120.0, n_theta=24, nz=8)
    n = _parse_gdf_panel_count(text)
    # bottom fan n_theta + side n_theta*nz = 24 + 192 = 216
    assert n == 24 + 24 * 8


def test_build_panel_mesh_routing():
    spar_gdf, spar_status = build_panel_mesh(_spar_variant())
    assert spar_status == "prepared" and spar_gdf is not None
    barge_gdf, barge_status = build_panel_mesh(_barge_variant())
    assert barge_status == "prepared" and barge_gdf is not None
    semi_gdf, semi_status = build_panel_mesh(_semi_variant())
    assert semi_status == "needs_supplied_mesh" and semi_gdf is None


def test_orcawave_spec_inertia_from_properties():
    v = _spar_variant()
    spec = build_orcawave_spec(v, mesh_file="hull.gdf", grid=FrequencyGrid())
    inertia = spec["vessel"]["inertia"]
    p = v.properties
    # mass = displacement (kg)
    assert inertia["mass"] == pytest.approx(p.displacement_t * 1000.0, rel=1e-6)
    # CoG sits KG - draft below the still-water line (negative z).
    assert inertia["centre_of_gravity"][2] == pytest.approx(
        p.KG_m - p.draft_m, abs=1e-3
    )
    # Radii of gyration use the screen's pitch radius of gyration.
    assert inertia["radii_of_gyration"][1] == pytest.approx(
        round(p.pitch_radius_gyration_m, 3)
    )
    assert spec["analysis_type"] == "diffraction"


def test_prepare_variant_writes_mesh_and_spec(tmp_path: Path):
    case = prepare_variant(_spar_variant(), tmp_path)
    assert case.status == "prepared"
    assert case.mesh_path is not None and case.mesh_path.exists()
    assert case.orcawave_spec_path.exists()
    spec = yaml.safe_load(case.orcawave_spec_path.read_text())
    assert spec["vessel"]["geometry"]["mesh_file"] == "hull.gdf"
    assert case.orcaflex_config["basename"] == "orcaflex"


def test_prepare_semi_needs_supplied_mesh(tmp_path: Path):
    case = prepare_variant(_semi_variant(), tmp_path)
    assert case.status == "needs_supplied_mesh"
    assert case.mesh_path is None
    # Spec is still written, referencing the supplied-mesh placeholder.
    spec = yaml.safe_load(case.orcawave_spec_path.read_text())
    assert spec["vessel"]["geometry"]["mesh_file"] == "SUPPLIED_MESH.gdf"
    assert any("supply a panel mesh" in n for n in case.notes)


def test_prepare_shortlist(tmp_path: Path):
    cases = prepare_shortlist([_spar_variant(), _barge_variant()], tmp_path)
    assert len(cases) == 2
    assert all(c.status == "prepared" for c in cases)


def test_dispatch_is_fail_closed_without_license(tmp_path: Path):
    case = prepare_variant(_spar_variant(), tmp_path)
    # No licence: stays prepared, never raises.
    out = dispatch_case(case, license_available=False)
    assert out.status == "prepared"
    # With a licence: marked dispatched.
    out2 = dispatch_case(case, license_available=True)
    assert out2.status == "dispatched"


def test_dispatch_skips_supplied_mesh_case(tmp_path: Path):
    case = prepare_variant(_semi_variant(), tmp_path)
    out = dispatch_case(case, license_available=True)
    assert out.status == "needs_supplied_mesh"


def test_fold_rao_results_updates_motion_and_verdict():
    v = _spar_variant()
    assert v.passed  # passes on the closed-form proxy
    # A solved RAO peak that exceeds the limit must flip the verdict.
    folded = fold_rao_results(v, solved_motion_value=3.5, daf_limit=2.0)
    motion = [c for c in folded.checks if c.name == "motion"][0]
    assert motion.value == pytest.approx(3.5)
    assert not motion.passed
    assert not folded.passed
    assert folded.governing_check == "motion"
    assert "solved RAO peak" in motion.basis
