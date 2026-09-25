"""Limit-load sensitivity deck and P_L derivation (#2157 P0b, owner card S02)."""

from __future__ import annotations

import pytest

from digitalmodel.ansys import weldolet_crack as wc
from digitalmodel.ansys import weldolet_limit as wl


def test_limit_deck_has_epp_material_and_no_cint():
    deck = wl.generate_limit_apdl(wl.LimitLoadSpec())
    for token in ("TB,BISO,1", "TBDATA,1,127.0,0.0", "NLGEOM,OFF", "NCNV,2",
                  "NSUBST,16,320,16", "*CFOPEN,weldolet_lpl_L0,txt", "PAPP = PAPP*4.0",
                  "*CFOPEN,weldolet_reacconv_L0,txt"):
        assert token in deck, token
    assert "\nCINT," not in deck
    assert "PINT = 4.7*4.0" in deck


def test_limit_deck_shares_the_elastic_model():
    elastic = wc.generate_weldolet_apdl(wc.WeldoletSpec())
    limit = wl.generate_limit_apdl(wl.LimitLoadSpec())
    nodes_e = [ln for ln in elastic.splitlines() if ln.startswith(("N,", "E,", "EMORE,"))]
    nodes_l = [ln for ln in limit.splitlines() if ln.startswith(("N,", "E,", "EMORE,"))]
    assert nodes_e == nodes_l


def test_limit_deck_on_the_crotch_plane():
    spec = wl.LimitLoadSpec(plane="crotch")
    deck = wl.generate_limit_apdl(spec)
    assert "! crotch-plane root flaw" in deck
    assert "TB,BISO,1" in deck and "\nCINT," not in deck
    assert "D,ALL,UY,0.0" in deck  # symmetry plane of the half model
    assert wl.state_name(spec) == "p0b_limit_load_crotch_a2p35"
    receipt = {"spec": wl.spec_dict(spec)}
    assert wl.deck_sha256_for_receipt(receipt, 0) == wc.deck_sha256(deck)
    with pytest.raises(ValueError, match="plane"):
        wl.generate_limit_apdl(wl.LimitLoadSpec(plane="other"))


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


_NCNV_DIVERGENCE = """
 *** ERROR ***                           ELAPSED TIME =    6825.345   TIME= 17:24:58
 The value of UZ at node 68450 is 9728418.3.  It is greater than the
 current limit of 1000000 (which can be reset on the NCNV command).
 This generally indicates rigid body motion as a result of an
 unconstrained model.  Verify that your model is properly constrained.

 *** ERROR ***                           ELAPSED TIME =    6825.345   TIME= 17:24:58
 *** MESSAGE CONTINUATION ---- DIAGNOSTIC INFORMATION ***
 Rigid body motion can also occur when net section yielding has
 occurred resulting in large displacements for small increments of load
"""


def test_displacement_limit_divergence_is_the_collapse_signature():
    """MAPDL ends an EPP run at collapse with the NCNV displacement-limit
    divergence (its diagnostic names net-section yielding); that counts as the
    Newton non-convergence of the stated criterion. Other errors still fail."""
    assert wl.only_nonconvergence_errors(_NCNV_DIVERGENCE)
    bad = _NCNV_DIVERGENCE + "\n *** ERROR ***\n Element 12 has a negative Jacobian.\n\n\n"
    assert not wl.only_nonconvergence_errors(bad)
    orphan = " *** ERROR ***\n *** MESSAGE CONTINUATION ---- DIAGNOSTIC INFORMATION ***\n\n\n"
    assert not wl.only_nonconvergence_errors(orphan)


def _lpl(rows) -> str:
    return "# columns\n" + "".join(
        f"{k + 1}. {lf:.10E} {lf * 4.7:.10E} {u:.10E} 0.0 {acc}.\n"
        for k, (lf, u, acc) in enumerate(rows)
    )


def test_limit_load_uses_the_last_converged_set_only():
    # three converged substeps (steps <= 1/16 of a 4.0 ramp = 0.25) and the
    # non-converged set MAPDL writes at the requested end of the ramp
    rows = [(0.25, 0.10, 1), (0.50, 0.21, 1), (0.60, 0.40, 1), (4.00, 5.0e6, 0)]
    res = wl.limit_load_result(wl.LimitLoadSpec(), _lpl(rows))
    assert res["p_limit_mpa"] == pytest.approx(0.60 * 4.7)
    assert res["n_converged_substeps"] == 3
    assert res["rejected_sets"] == [{"set": 4, "load_factor": 4.0}]
    assert res["reached_nonconvergence"] is True


def test_limit_load_result_bilinear_curve():
    # elastic slope 10 MPa/mm to 8 MPa, then slope 1 MPa/mm to non-convergence;
    # the TES line p = 5 u meets p = 8 + (u - 0.8) at u = 1.8, p = 9
    pts = [(0.2, 2.0), (0.8, 8.0), (1.3, 8.5), (2.3, 9.5), (3.0, 10.2)]
    text = "# columns\n" + "".join(
        f"{i + 1}. {p / 4.7:.10E} {p:.10E} {u:.10E} 0.0 1.\n" for i, (u, p) in enumerate(pts)
    )
    res = wl.limit_load_result(wl.LimitLoadSpec(), text)
    assert res["p_limit_mpa"] == pytest.approx(10.2)
    assert res["p_tes_mpa"] == pytest.approx(9.0)
    assert res["lr_design"] == pytest.approx(4.7 / 10.2)
    assert res["tangent_over_elastic_stiffness_last"] == pytest.approx(0.1)
    assert res["reached_nonconvergence"] is True
