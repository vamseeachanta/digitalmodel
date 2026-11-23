"""Tests for the diffraction-setup mass-properties consumer (#853)."""

from __future__ import annotations

import importlib.util
import sys
from pathlib import Path

import pytest

from digitalmodel.marine_ops.vessel_db.loader import Record
from digitalmodel.marine_ops.vessel_db.mass_properties import from_record


def _rec(**fields):
    return Record(name=fields.pop("_name", "Test FPSO"), scope="floating",
                  layer="particulars", vessel_type=fields.pop("_type", "fpso"),
                  raw_fields=fields)


def test_inertia_is_mass_times_k_squared():
    # displacement 100000 te, beam 60, lbp 300, cited gyradii.
    rec = _rec(displacement_t=100000.0, beam_m=60.0, lbp_m=300.0,
               kxx_roll_m=20.0, kyy_pitch_m=75.0, kzz_yaw_m=78.0, vcg_m=18.0)
    mp = from_record(rec)
    assert mp is not None
    assert mp.inertia_te_m2["Ixx"] == pytest.approx(100000.0 * 20.0 ** 2)
    assert mp.inertia_te_m2["Iyy"] == pytest.approx(100000.0 * 75.0 ** 2)
    assert mp.inertia_te_m2["Izz"] == pytest.approx(100000.0 * 78.0 ** 2)
    assert mp.provenance["Ixx"] == "cited"


def test_estimated_gyradii_when_absent():
    rec = _rec(displacement_t=80000.0, beam_m=50.0, lbp_m=250.0)
    mp = from_record(rec)
    assert mp.gyradii_m["kxx"] == pytest.approx(0.35 * 50.0)
    assert mp.provenance["Ixx"].startswith("estimated:")
    # inertia still consistent with the estimated radius
    assert mp.inertia_te_m2["Ixx"] == pytest.approx(80000.0 * (0.35 * 50.0) ** 2)


def test_no_mass_returns_none():
    assert from_record(_rec(beam_m=40.0, lbp_m=200.0)) is None
    assert from_record(_rec(displacement_t=0.0)) is None


def test_cog_defaults_flagged_when_missing():
    mp = from_record(_rec(displacement_t=50000.0, beam_m=40.0, lbp_m=200.0))
    assert mp.cog_m == (0.0, 0.0, 0.0)
    assert mp.provenance["cog_x"].startswith("default")
    assert mp.provenance["cog_z"].startswith("default")
    assert "vcg" in mp.gaps


def test_cog_uses_cited_vcg():
    mp = from_record(_rec(displacement_t=50000.0, beam_m=40.0, lbp_m=200.0,
                          vcg_m=15.0, lcg_m=2.5))
    assert mp.cog_m[0] == pytest.approx(2.5)   # LCG
    assert mp.cog_m[2] == pytest.approx(15.0)  # VCG
    assert mp.provenance["cog_z"] == "cited"
    assert "vcg" not in mp.gaps


def test_orcawave_body_block_shape():
    mp = from_record(_rec(displacement_t=9000.0, beam_m=30.0, lbp_m=100.0,
                          kxx_roll_m=10.0, kyy_pitch_m=25.0, kzz_yaw_m=26.0))
    blk = mp.orcawave_body_block()
    for key in ("BodyMass", "BodyCentreOfMass", "BodyInertiaTensorRx",
                "BodyInertiaTensorRy", "BodyInertiaTensorRz",
                "BodyInertiaSpecifiedBy", "BodyInertiaTensorOriginType"):
        assert key in blk
    # diagonal tensor: off-diagonal terms are zero
    assert blk["BodyInertiaTensorRx"][1] == 0.0 and blk["BodyInertiaTensorRx"][2] == 0.0
    assert blk["BodyInertiaTensorRy"][0] == 0.0 and blk["BodyInertiaTensorRz"][0] == 0.0
    assert blk["BodyInertiaTensorRz"][2] > 0.0


# ---- demo smoke test ----

def _load_demo():
    path = (Path(__file__).resolve().parents[3]
            / "examples" / "demos" / "diffraction" / "diffraction_setup.py")
    spec = importlib.util.spec_from_file_location("diffraction_setup", path)
    mod = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = mod
    spec.loader.exec_module(mod)
    return mod


def test_demo_generates_setups_from_real_db():
    demo = _load_demo()
    setups = demo.setups_for_scope("floating", limit=3)
    assert setups, "no diffraction setups from the floating scope"
    for mp in setups:
        assert mp.mass_te > 0
        assert mp.orcawave_body_block()["BodyMass"] > 0
