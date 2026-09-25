"""Limit-load sensitivity deck and P_L derivation (#2157 P0b, owner card S02)."""

from __future__ import annotations

import pytest

from digitalmodel.ansys import weldolet_crack as wc
from digitalmodel.ansys import weldolet_limit as wl


def test_limit_deck_has_epp_material_and_no_cint():
    deck = wl.generate_limit_apdl(wl.LimitLoadSpec())
    for token in ("TB,BISO,1", "TBDATA,1,127.0,0.0", "NLGEOM,OFF", "NCNV,2",
                  "NSUBST,16,320,16", "*CFOPEN,weldolet_lpl_L0,txt", "PAPP = PAPP*4.0"):
        assert token in deck, token
    assert "\nCINT," not in deck
    assert "PINT = 4.7*4.0" in deck


def test_limit_deck_shares_the_elastic_model():
    elastic = wc.generate_weldolet_apdl(wc.WeldoletSpec())
    limit = wl.generate_limit_apdl(wl.LimitLoadSpec())
    nodes_e = [ln for ln in elastic.splitlines() if ln.startswith(("N,", "E,", "EMORE,"))]
    nodes_l = [ln for ln in limit.splitlines() if ln.startswith(("N,", "E,", "EMORE,"))]
    assert nodes_e == nodes_l


def test_limit_spec_requires_a_crack():
    with pytest.raises(ValueError, match="cracked model"):
        wl.generate_limit_apdl(wl.LimitLoadSpec(base=wc.WeldoletSpec(crack_depth_mm=None)))


def test_limit_deck_regenerates_from_receipt_spec():
    spec = wl.LimitLoadSpec()
    receipt = {"spec": wl.spec_dict(spec)}
    assert wl.deck_sha256_for_receipt(receipt, 0) == wc.deck_sha256(
        wl.generate_limit_apdl(spec)
    )
    assert wl.state_name(spec) == "p0b_limit_load_a2p35"


def test_only_nonconvergence_errors():
    ok = "x\n *** ERROR ***   TIME\n Solution not converged at time 0.7\n more\n"
    assert wl.only_nonconvergence_errors(ok)
    assert not wl.only_nonconvergence_errors("clean log")
    assert not wl.only_nonconvergence_errors(ok + " *** ERROR ***\n bad element\n\n\n")
    assert not wl.only_nonconvergence_errors(ok + " *** FATAL ***\n")


def test_limit_load_result_bilinear_curve():
    # elastic slope 10 MPa/mm to 8 MPa, then slope 1 MPa/mm to non-convergence;
    # the TES line p = 5 u meets p = 8 + (u - 0.8) at u = 1.8, p = 9
    pts = [(0.2, 2.0), (0.8, 8.0), (1.3, 8.5), (2.3, 9.5), (3.0, 10.2)]
    text = "# columns\n" + "".join(
        f"{i + 1}. {p / 4.7:.10E} {p:.10E} {u:.10E} 0.0\n" for i, (u, p) in enumerate(pts)
    )
    res = wl.limit_load_result(wl.LimitLoadSpec(), text)
    assert res["p_limit_mpa"] == pytest.approx(10.2)
    assert res["p_tes_mpa"] == pytest.approx(9.0)
    assert res["lr_design"] == pytest.approx(4.7 / 10.2)
    assert res["tangent_over_elastic_stiffness_last"] == pytest.approx(0.1)
    assert res["reached_nonconvergence"] is True
