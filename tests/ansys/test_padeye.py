"""Padeye/lifting-lug APDL template generator (digitalmodel #948)."""

from __future__ import annotations

from pathlib import Path

import pytest

from digitalmodel.ansys.padeye import (
    PadeyeGeometry,
    generate_padeye_apdl,
    write_padeye_inp,
)


def test_default_geometry_resolves_centred_hole():
    geom = PadeyeGeometry()
    xc, yc = geom.resolved_center()
    assert xc == pytest.approx(geom.plate_width_mm / 2.0)
    assert 0 < yc < geom.plate_height_mm
    assert geom.validate() == []


def test_generated_apdl_has_required_fe_blocks():
    script = generate_padeye_apdl(PadeyeGeometry())
    for token in (
        "/PREP7",
        "ET,1,PLANE182",
        "KEYOPT,1,3,3",  # plane stress with thickness
        "MP,EX,1,",
        "BLC4,",  # plate
        "CYL4,",  # hole
        "ASBA,1,2",  # subtract hole
        "AMESH,ALL",
        "D,ALL,ALL,0",  # fixed weld base
        "F,ALL,FX",  # sling load
        "ANTYPE,STATIC",
        "SOLVE",
        "NSORT,S,EQV",  # von Mises
        "uc = smax / allow",  # unity check
        "*CFOPEN,padeye_result,csv",  # numeric digest
    ):
        assert token in script, f"missing APDL token: {token}"


def test_load_components_follow_sling_angle():
    # 0 deg = pure vertical: all load in FY, none in FX.
    vertical = generate_padeye_apdl(
        PadeyeGeometry(sling_load_kn=500, sling_angle_deg=0)
    )
    assert "fx_n = 0.0 / nload" in vertical
    assert "fy_n = 500000.0 / nload" in vertical

    # 90 deg = pure horizontal: load in FX.
    horizontal = generate_padeye_apdl(
        PadeyeGeometry(sling_load_kn=500, sling_angle_deg=90)
    )
    assert "fx_n = 500000.0 / nload" in horizontal


def test_allowable_uses_yield_over_design_factor():
    geom = PadeyeGeometry(yield_strength_mpa=355.0, design_factor=2.0)
    script = generate_padeye_apdl(geom)
    assert "allow = 177.5" in script  # 355 / 2.0


def test_parameters_propagate_into_script():
    a = generate_padeye_apdl(PadeyeGeometry(hole_diameter_mm=80))
    b = generate_padeye_apdl(PadeyeGeometry(hole_diameter_mm=120))
    assert a != b
    assert "CYL4,200.0,180.0,60.0" in b  # radius=120/2; yc=300-120 (edge-distance rule)


def test_invalid_geometry_rejected():
    # Hole bigger than the plate -> overruns; generation raises.
    with pytest.raises(ValueError, match="overruns"):
        generate_padeye_apdl(PadeyeGeometry(hole_diameter_mm=500))


def test_write_padeye_inp(tmp_path: Path):
    out = write_padeye_inp(PadeyeGeometry(), tmp_path / "p" / "padeye.inp")
    assert out.is_file()
    assert "PLANE182" in out.read_text()


def test_committed_example_inp_matches_generator():
    # The committed padeye.inp must stay in sync with build.py's geometry.
    example = (
        Path(__file__).resolve().parents[2]
        / "examples"
        / "ansys"
        / "padeye"
        / "padeye.inp"
    )
    if not example.is_file():
        pytest.skip("example padeye.inp not present")
    import importlib.util

    build = example.parent / "build.py"
    spec = importlib.util.spec_from_file_location("padeye_build", build)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    assert example.read_text() == generate_padeye_apdl(module.GEOM)
