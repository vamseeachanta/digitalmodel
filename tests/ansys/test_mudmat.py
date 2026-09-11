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


def test_inert_foundation_real_constant_is_absent():
    """SHELL181 does not consume an elastic foundation stiffness.

    This test previously asserted the PRESENCE of ``R,1,60.0, , ,0.05`` and its
    comment recorded, as fact, that "EFS appears as the 5th real-constant field
    on SHELL181's R,1 card". That is false. RLIST confirms the value is stored
    and the element ignores it, so the plate had no vertical stiffness and the
    model was singular at every UZ degree of freedom -- it had never solved. The
    assertion is inverted rather than deleted so the disproven construction
    cannot return (#2094).
    """
    script = generate_mudmat_apdl(MudmatGeometry(subgrade_modulus_n_per_mm3=0.05))
    directives = [
        ln.strip()
        for ln in script.splitlines()
        if ln.strip() and not ln.strip().startswith("!")
    ]
    assert not [ln for ln in directives if ln.startswith("R,1,")], (
        "inert foundation real constant re-introduced"
    )
    assert not [ln for ln in directives if "ksub" in ln], "Winkler bed re-introduced"
    assert not [ln for ln in directives if "max_bearing" in ln]


def test_soil_reaction_is_a_meyerhof_effective_area_patch():
    """The reaction is the same idealisation the bearing check uses (#2094 D1)."""
    script = generate_mudmat_apdl(MudmatGeometry())
    assert "SFE,ALL,2,PRES" in script, "no upward soil reaction applied"
    assert "ESEL,S,CENT,X" in script, "reaction not restricted to the effective area"
    assert "SFGRAD,PRES,0,X" in script, "overturning moment not applied as a gradient"


def test_plate_unity_check_present_and_bearing_delegated():
    """The FE reports plate bending only; bearing belongs to the geotech module."""
    script = generate_mudmat_apdl(MudmatGeometry())
    assert "stress_uc = smax / allow" in script
    assert "bearing_uc" not in script, (
        "the FE must not report a bearing unity check: the bearing idealisation "
        "lives in digitalmodel.geotechnical.mudmat (#2094 D1)"
    )


def test_reaction_sum_is_reported():
    """Loads balance by construction, so the reactions are the conservation check."""
    script = generate_mudmat_apdl(MudmatGeometry())
    assert "FSUM" in script
    assert "reaction_fz_n" in script


def test_applied_field_is_mesh_independent():
    """The FCUM defect cannot return.

    The previous deck distributed load with ``F,ALL,FZ`` and then overwrote the
    short-edge nodes with the couple, because FCUM defaults to REPL. The applied
    resultant was therefore 2/(L/ESIZE + 1) short of the intended value -- a
    function of mesh density. A pressure field carries no such dependence.
    """
    coarse = generate_mudmat_apdl(MudmatGeometry(element_size_mm=100.0))
    fine = generate_mudmat_apdl(MudmatGeometry(element_size_mm=50.0))

    def _pressures(script: str) -> list[str]:
        return [ln for ln in script.splitlines() if ln.startswith(("SFE,", "SFGRAD,"))]

    assert _pressures(coarse) == _pressures(fine), (
        "applied load depends on mesh density"
    )
    assert "F,ALL,FZ" not in coarse, "nodal-force load path re-introduced"


def test_eccentricity_beyond_half_length_is_rejected():
    """At e >= L/2 the effective area vanishes; the foundation has overturned.

    Without this the solver would return a plausible plate stress for a mat with
    no bearing solution at all.
    """
    geom = MudmatGeometry(vertical_load_kn=800.0, moment_kNm=1700.0)
    issues = geom.validate()
    assert any("effective area vanishes" in i for i in issues), issues


def test_allowable_uses_yield_over_design_factor():
    geom = MudmatGeometry(yield_strength_mpa=355.0, design_factor=2.0)
    script = generate_mudmat_apdl(geom)
    assert "allow = 177.5" in script  # 355 / 2.0


def test_vertical_load_and_moment_propagate():
    """Load and moment reach the deck as a pressure field, checked by hand.

    V = 800 kN, M = 400 kN*m, L = 4000 mm, B = 3000 mm:
      e       = M/V              = 500 mm
      L_eff   = L - 2e           = 3000 mm
      A_eff   = L_eff * B        = 9.0e6 mm^2
      q_soil  = V/A_eff          = 0.088889 MPa
      p(x)    = V/(L*B) + 12M(x - L/2)/(B*L^3)
              = 0.0666667 + 2.5e-5*(x - 2000)
      p(0)    = 0.0166667 MPa,  p(L) = 0.1166667 MPa
    """
    script = generate_mudmat_apdl(
        MudmatGeometry(vertical_load_kn=800.0, moment_kNm=400.0, mat_length_mm=4000.0)
    )
    assert "= 500.000 mm" in script  # eccentricity
    assert "= 3000.000 mm" in script  # effective length
    assert "0.088889 MPa" in script  # soil pressure
    assert "SFGRAD,PRES,0,X,0,0.0000250000" in script  # overturning gradient
    assert "SFE,ALL,1,PRES,,0.01666667" in script  # applied field at x=0


def test_applied_field_integrates_to_the_applied_load():
    """Force and moment balance by construction, verified against the emitted field.

    Reproduces the integration in Python rather than trusting the generator: the
    numbers in the deck must be the ones that make the reactions vanish, and that
    is the property the whole re-scope rests on.
    """
    geom = MudmatGeometry(vertical_load_kn=800.0, moment_kNm=400.0)
    script = generate_mudmat_apdl(geom)

    v_n = geom.vertical_load_kn * 1000.0
    m_nmm = geom.moment_kNm * 1.0e6
    L, B = geom.mat_length_mm, geom.mat_width_mm

    p0 = float(script.split("SFE,ALL,1,PRES,,")[1].split("\n")[0])
    slope = float(script.split("SFGRAD,PRES,0,X,0,")[1].split("\n")[0])
    q = float(script.split("SFE,ALL,2,PRES,,")[1].split("\n")[0])
    x0 = float(script.split("ESEL,S,CENT,X,")[1].split(",")[0])

    # Applied field p(x) = p0 + slope*x, integrated over the mat.
    #   force  = B * integral_0^L (p0 + slope*x) dx        = B*(p0*L + slope*L^2/2)
    #   moment = B * integral_0^L (p0 + slope*x)(x - L/2) dx
    # The uniform part contributes no moment about the centre, because
    # integral_0^L (x - L/2) dx = 0, leaving slope * L^3/12.
    applied_force = B * (p0 * L + slope * L**2 / 2.0)
    applied_moment = B * slope * L**3 / 12.0
    # Tolerance is the emitted-precision floor, not a modelling tolerance. The
    # deck carries pressures to eight decimal places, so the integrated resultant
    # lands within about 5e-8 of the applied load (0.04 N in 800 kN). A
    # formulation error -- a wrong coefficient, a missing factor, the FCUM defect
    # this replaced -- is orders of magnitude larger and still fails here.
    assert applied_force == pytest.approx(v_n, rel=1e-6)
    assert applied_moment == pytest.approx(m_nmm, rel=1e-6)

    # Soil patch: resultant and its moment about the centre, equal and opposing.
    patch_force = q * (L - x0) * B
    patch_moment = patch_force * (((x0 + L) / 2.0) - L / 2.0)
    assert patch_force == pytest.approx(v_n, rel=1e-6)
    assert patch_moment == pytest.approx(m_nmm, rel=1e-6)


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
    # Line endings normalised -- see the note in test_padeye.py (#2094).
    assert example.read_text(encoding="utf-8").replace("\r\n", "\n") == generate_mudmat_apdl(
        module.GEOM
    ).replace("\r\n", "\n")
