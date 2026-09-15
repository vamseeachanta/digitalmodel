"""Pressure-vessel screening APDL template generator (digitalmodel #951).

Verifies generate_pv_apdl emits a *complete, runnable* MAPDL deck (prep /
element / material / mesh / solve / stress + CSV digest), that the committed
example pv.inp stays in sync with build.py's geometry, and that the bugfix held
(generate_pv_apdl is defined exactly once — no shadowing).
"""

from __future__ import annotations

import importlib.util
import inspect
from pathlib import Path

from digitalmodel.ansys.pressure_vessel import (
    DesignConditions,
    PressureVesselGenerator,
    VesselGeometry,
)

EXAMPLE = Path(__file__).resolve().parents[2] / "examples" / "ansys" / "pressure-vessel"


def _gen() -> PressureVesselGenerator:
    return PressureVesselGenerator()


def test_generated_apdl_has_required_fe_blocks():
    """The deck must be a complete solve: prep, element, material, mesh,
    solve and a stress result with a numeric digest."""
    script = _gen().generate_pv_apdl(VesselGeometry(), DesignConditions())
    for token in (
        "/PREP7",  # model setup
        "ET,1,PLANE182",  # element type
        "KEYOPT,1,3,1",  # axisymmetric
        "MP,EX,1,",  # material (Young's modulus)
        "MP,PRXY,1,",  # material (Poisson)
        "RECTNG,",  # geometry (wall section)
        "AMESH,ALL",  # mesh
        "D,ALL,UY,0",  # boundary condition
        "SFL,ALL,PRES",  # internal pressure load
        "/SOLU",  # solution phase
        "ANTYPE,STATIC",  # static analysis
        "SOLVE",  # actually solve
        "/POST1",  # post-processing
        "NSORT,S,EQV",  # von Mises stress
        "*GET,smax,SORT,0,MAX",  # peak stress extraction
        "uc = smax / allow",  # unity check
        "*CFOPEN,pv_result,csv",  # numeric digest
    ):
        assert token in script, f"missing APDL token: {token}"


def test_pressure_value_propagates():
    a = _gen().generate_pv_apdl(
        VesselGeometry(), DesignConditions(design_pressure_mpa=10.0)
    )
    b = _gen().generate_pv_apdl(
        VesselGeometry(), DesignConditions(design_pressure_mpa=20.0)
    )
    assert a != b
    assert "SFL,ALL,PRES,20.0" in b


def test_hydrotest_uses_test_pressure_and_allowable():
    cond = DesignConditions(design_pressure_mpa=10.0, yield_strength_mpa=300.0)
    script = _gen().generate_pv_apdl(VesselGeometry(), cond, include_hydrotest=True)
    assert "SFL,ALL,PRES,13.0" in script  # 1.3 x 10.0
    assert "allow = 200.0" in script  # 300 / 1.5 (UG-99 test allowable)


def test_thermal_block_toggles():
    with_thermal = _gen().generate_pv_apdl(
        VesselGeometry(), DesignConditions(), include_thermal=True
    )
    without_thermal = _gen().generate_pv_apdl(
        VesselGeometry(), DesignConditions(), include_thermal=False
    )
    assert "BFUNIF,TEMP" in with_thermal
    assert "BFUNIF,TEMP" not in without_thermal


def test_generate_pv_apdl_defined_once():
    """Regression for the #951 bugfix: the method was defined twice and the
    second silently shadowed the first. Guard against re-introduction."""
    src = inspect.getsource(PressureVesselGenerator)
    assert src.count("def generate_pv_apdl(") == 1


def test_committed_example_inp_matches_generator():
    """Drift guard: the committed pv.inp must equal build.py's output.

    Line endings are normalised before comparison. Git checks this file out with
    CRLF wherever ``core.autocrlf`` is true, while the generator emits LF, so a
    byte-exact comparison fails on every Windows checkout regardless of whether
    the deck has drifted. That failure was standing on main when #2094 was
    opened: the one guard covering these decks was red on this platform and
    proving nothing on any other.
    """
    inp = EXAMPLE / "pv.inp"
    build = EXAMPLE / "build.py"
    assert inp.is_file(), "example pv.inp not present"

    spec = importlib.util.spec_from_file_location("pv_build", build)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)

    expected = _gen().generate_pv_apdl(
        module.GEOM,
        module.CONDITIONS,
        include_thermal=True,
        include_hydrotest=False,
    )
    assert inp.read_text(encoding="utf-8").replace("\r\n", "\n") == expected.replace("\r\n", "\n")


# --- physics correctness (#2094) -------------------------------------------
# The byte-match guard above proves the generator is deterministic. It proved
# nothing about the physics: the deck over-predicted peak von Mises by 3.8x for
# months while this suite was green. These assert against the model, not against
# the generator.


def test_no_spurious_radial_point_restraint():
    """An axisymmetric model has no radial rigid-body mode.

    A single node restrained in UX therefore removes no rigid-body motion; it
    resists the model's own pressure and thermal expansion and produces a
    spurious point stress concentration, which NSORT then reports as the peak.
    Measured: 981.34 MPa with the restraint, 259.54 MPa without, against a
    closed-form Lame von Mises of 260.24 MPa.
    """
    script = _gen().generate_pv_apdl(VesselGeometry(), DesignConditions())
    directives = [
        ln.strip()
        for ln in script.splitlines()
        if ln.strip() and not ln.strip().startswith("!")
    ]
    radial = [ln for ln in directives if ln.upper().replace(" ", "").startswith("D,ALL,UX")]
    assert not radial, (
        "radial restraint re-introduced: "
        f"{radial}. An axisymmetric model has no radial rigid-body mode, so this "
        "removes no rigid-body motion and instead resists the wall's own pressure "
        "and thermal expansion (#2094)."
    )


def test_deck_is_axisymmetric():
    """Binds the safety of the restraint's absence to its precondition.

    Deleting the radial restraint is correct *because* the element is
    axisymmetric. If KEYOPT(3) is ever changed to plane stress or plane strain,
    the deleted restraint becomes a live singularity with nothing guarding it.
    """
    script = _gen().generate_pv_apdl(VesselGeometry(), DesignConditions())
    assert "KEYOPT,1,3,1" in script


def test_reaction_sum_is_reported():
    """Equilibrium is the check that would have caught the defect at first run.

    A reaction sum over the constrained set makes an over-constraint
    self-announcing: the spurious restraint carried a large radial reaction that
    no committed artifact recorded.
    """
    script = _gen().generate_pv_apdl(VesselGeometry(), DesignConditions())
    assert "FSUM" in script, "deck reports no reaction sum"
    assert "rfy" in script, "reaction resultant not captured for the digest"


def test_digest_carries_reaction_and_peak_node():
    """A value without its location cannot be told from a singularity."""
    script = _gen().generate_pv_apdl(VesselGeometry(), DesignConditions())
    assert "peak_node" in script
    assert "reaction_fy_n" in script
