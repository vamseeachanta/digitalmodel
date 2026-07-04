"""Mudmat/foundation APDL template generator (digitalmodel #952)."""

from __future__ import annotations

from pathlib import Path

import pytest

from digitalmodel.ansys.mudmat import (
    MudmatGeometry,
    generate_mudmat_apdl,
    write_mudmat_inp,
)


def test_default_geometry_is_valid():
    assert MudmatGeometry().validate() == []


def test_generated_apdl_has_required_fe_blocks():
    script = generate_mudmat_apdl(MudmatGeometry())
    for token in (
        "/PREP7",
        "ET,1,SHELL181",
        "MP,EX,1,",
        "BLC4,",  # rectangular mat
        "AMESH,ALL",
        "ANTYPE,STATIC",
        "SOLVE",
        "NSORT,S,EQV",  # plate von Mises
        "*CFOPEN,mudmat_result,csv",  # numeric digest
    ):
        assert token in script, f"missing APDL token: {token}"


def test_winkler_soil_support_present():
    # Elastic foundation stiffness (EFS) real constant carries the subgrade modulus.
    geom = MudmatGeometry(subgrade_modulus_n_per_mm3=0.05)
    script = generate_mudmat_apdl(geom)
    # EFS appears as the 5th real-constant field on SHELL181's R,1 card.
    assert "R,1,60.0, , ,0.05" in script
    # bearing pressure is recovered from p = ksub * w_max (deflection sort on UZ).
    assert "NSORT,U,Z" in script
    assert "ksub = 0.05" in script
    assert "max_bearing = -uz_min * ksub" in script


def test_both_unity_checks_present():
    script = generate_mudmat_apdl(MudmatGeometry())
    assert "stress_uc = smax / allow" in script  # plate stress UC
    assert "bearing_uc = max_bearing / allow_bear" in script  # soil bearing UC


def test_allowable_uses_yield_over_design_factor():
    geom = MudmatGeometry(yield_strength_mpa=355.0, design_factor=2.0)
    script = generate_mudmat_apdl(geom)
    assert "allow = 177.5" in script  # 355 / 2.0


def test_vertical_load_and_moment_propagate():
    # Vertical load: 800 kN downward -> -800000 N total spread over nall.
    script = generate_mudmat_apdl(
        MudmatGeometry(vertical_load_kn=800.0, moment_kNm=400.0, mat_length_mm=4000.0)
    )
    assert "fz_n = -800000.0 / nall" in script
    # Moment couple edge force = M / L = 400e6 N*mm / 4000 mm = 100000 N.
    assert "edgef = 100000.0" in script


def test_allowable_bearing_propagates():
    a = generate_mudmat_apdl(MudmatGeometry(allowable_bearing_mpa=0.25))
    b = generate_mudmat_apdl(MudmatGeometry(allowable_bearing_mpa=0.50))
    assert a != b
    assert "allow_bear = 0.25" in a
    assert "allow_bear = 0.5" in b


def test_parameters_propagate_into_script():
    a = generate_mudmat_apdl(MudmatGeometry(thickness_mm=60.0))
    b = generate_mudmat_apdl(MudmatGeometry(thickness_mm=80.0))
    assert a != b
    assert "BLC4,0,0,4000.0,3000.0" in a


def test_invalid_geometry_rejected():
    with pytest.raises(ValueError, match="positive"):
        generate_mudmat_apdl(MudmatGeometry(thickness_mm=-10.0))
    with pytest.raises(ValueError, match="subgrade"):
        generate_mudmat_apdl(MudmatGeometry(subgrade_modulus_n_per_mm3=0.0))


def test_write_mudmat_inp(tmp_path: Path):
    out = write_mudmat_inp(MudmatGeometry(), tmp_path / "m" / "mudmat.inp")
    assert out.is_file()
    assert "SHELL181" in out.read_text()


def test_committed_example_inp_matches_generator():
    # The committed mudmat.inp must stay in sync with build.py's geometry.
    example = (
        Path(__file__).resolve().parents[2]
        / "examples"
        / "ansys"
        / "mudmat"
        / "mudmat.inp"
    )
    if not example.is_file():
        pytest.skip("example mudmat.inp not present")
    import importlib.util

    build = example.parent / "build.py"
    spec = importlib.util.spec_from_file_location("mudmat_build", build)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    assert example.read_text() == generate_mudmat_apdl(module.GEOM)
