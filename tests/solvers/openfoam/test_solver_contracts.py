#!/usr/bin/env python3
"""
ABOUTME: Per-solver dictionary contract tests for the OpenFOAM case builder,
asserting that an emitted case carries the dictionary entries the solver named
in its own controlDict actually requires at start-up.

These are Layer 1 regression locks (issue #1959, design decision D3). Every
expected value below is a hand-written literal taken from the OpenFOAM v2312
tutorial multiphase/interFoam/laminar/damBreak, NOT read back from the
contract the emitter renders from -- a test that read its expectations from
that contract could not fail. The non-circular oracle is the real solver start
in test_case_runnable.py.
"""

import re

import pytest

from digitalmodel.solvers.openfoam.models import (
    CaseType,
    OpenFOAMCase,
    _CASE_SOLVER_MAP,
)
from digitalmodel.solvers.openfoam.case_builder import OpenFOAMCaseBuilder
from digitalmodel.solvers.openfoam.solver_contracts import SOLVER_DICTS


def _emit(tmp_path, case_type, name="contract_probe"):
    """Build a case of the given type and return its case directory."""
    case = OpenFOAMCase.for_case_type(case_type, name=name)
    return OpenFOAMCaseBuilder(case).build(tmp_path)


def _fv_solution(tmp_path, case_type):
    return (_emit(tmp_path, case_type) / "system" / "fvSolution").read_text()


def _fv_schemes(tmp_path, case_type):
    return (_emit(tmp_path, case_type) / "system" / "fvSchemes").read_text()


# ============================================================================
# D6 -- contract coverage
# ============================================================================


class TestContractCoverage:
    """Every solver the case map can select must declare its requirements."""

    def test_contract_covers_the_pinned_solver_set(self):
        """SOLVER_DICTS keys equal the pinned literal set of three solvers."""
        assert set(SOLVER_DICTS) == {"interFoam", "simpleFoam", "pimpleFoam"}

    def test_contract_covers_every_mapped_solver(self):
        """SOLVER_DICTS keys equal the solvers _CASE_SOLVER_MAP can select."""
        assert set(SOLVER_DICTS) == set(_CASE_SOLVER_MAP.values())


# ============================================================================
# interFoam fvSolution -- the headline defect
# ============================================================================


class TestInterFoamFvSolution:
    """interFoam solves alpha.water and p_rgh; it never solves a bare p."""

    def test_fvsolution_has_calpha(self, tmp_path):
        """The MULES interface-compression coefficient must be present."""
        assert "cAlpha" in _fv_solution(tmp_path, CaseType.SLOSHING)

    def test_fvsolution_calpha_is_one(self, tmp_path):
        """cAlpha is 1, the v2312 damBreak tutorial value."""
        content = _fv_solution(tmp_path, CaseType.SLOSHING)
        match = re.search(r"^\s*cAlpha\s+(\S+?);", content, re.MULTILINE)
        assert match.group(1) == "1"

    def test_fvsolution_has_dedicated_alpha_water_block(self, tmp_path):
        """alpha.water gets its own solver block, not a turbulence regex group."""
        content = _fv_solution(tmp_path, CaseType.SLOSHING)
        assert '"alpha.water.*"' in content

    def test_fvsolution_alpha_water_not_lumped_with_turbulence(self, tmp_path):
        """The old lumped regex group must be gone."""
        content = _fv_solution(tmp_path, CaseType.SLOSHING)
        assert "(k|omega|epsilon|alpha.water)" not in content

    def test_fvsolution_has_nalphasubcycles(self, tmp_path):
        """MULES sub-cycling count must be declared."""
        assert "nAlphaSubCycles" in _fv_solution(tmp_path, CaseType.SLOSHING)

    def test_fvsolution_has_p_rgh(self, tmp_path):
        """interFoam solves p_rgh."""
        content = _fv_solution(tmp_path, CaseType.SLOSHING)
        assert re.search(r"^\s{4}p_rgh$", content, re.MULTILINE) is not None

    def test_fvsolution_has_p_rgh_final(self, tmp_path):
        """The final PIMPLE corrector needs its own p_rghFinal block."""
        content = _fv_solution(tmp_path, CaseType.SLOSHING)
        assert re.search(r"^\s{4}p_rghFinal$", content, re.MULTILINE) is not None

    def test_fvsolution_drops_bare_p_solver(self, tmp_path):
        """interFoam never solves a field called p."""
        content = _fv_solution(tmp_path, CaseType.SLOSHING)
        assert re.search(r"^\s{4}p$", content, re.MULTILINE) is None

    def test_fvsolution_has_u_final(self, tmp_path):
        """PIMPLE solves U on its final corrector and looks up UFinal.

        The damBreak tutorial omits UFinal only because it sets
        momentumPredictor no, so its U equation is never solved. This builder
        leaves momentumPredictor at its default, so UFinal is required.
        """
        content = _fv_solution(tmp_path, CaseType.SLOSHING)
        assert re.search(r"^\s{4}UFinal$", content, re.MULTILINE) is not None

    def test_fvsolution_has_pcorr(self, tmp_path):
        """The pcorr.* block is required by the interFoam start-up correction."""
        assert '"pcorr.*"' in _fv_solution(tmp_path, CaseType.SLOSHING)

    def test_fvsolution_p_rgh_final_uses_no_macro_shorthand(self, tmp_path):
        """p_rghFinal must be written longhand, not as $p_rgh."""
        content = _fv_solution(tmp_path, CaseType.SLOSHING)
        assert "$p_rgh;" not in content


# ============================================================================
# interFoam fvSchemes -- the second and third fatal errors
# ============================================================================


class TestDdtSchemes:
    """The time scheme must be one the solver's own equations accept.

    Found by the issue #1959 solver-start oracle: interFoam's alpha equation
    rejects anything but Euler and CrankNicolson at run time, so the builder's
    backward scheme was a fatal error for every VOF case.
    """

    def _ddt_default(self, tmp_path, case_type, name):
        case = OpenFOAMCase.for_case_type(case_type, name=name)
        case_dir = OpenFOAMCaseBuilder(case).build(tmp_path)
        content = (case_dir / "system" / "fvSchemes").read_text()
        block = content[content.index("ddtSchemes") : content.index("gradSchemes")]
        return re.search(r"default\s+(\S+?);", block).group(1)

    def test_interfoam_uses_euler(self, tmp_path):
        """interFoam's alpha equation supports only Euler or CrankNicolson."""
        assert self._ddt_default(tmp_path, CaseType.SLOSHING, "ddt_vof") == "Euler"

    def test_pimplefoam_keeps_backward(self, tmp_path):
        """A single-phase transient case keeps the second-order scheme."""
        assert self._ddt_default(tmp_path, CaseType.VIV, "ddt_pimple") == "backward"

    def test_simplefoam_stays_steady_state(self, tmp_path):
        """A steady case keeps steadyState."""
        assert (
            self._ddt_default(tmp_path, CaseType.CURRENT_LOADING, "ddt_simple")
            == "steadyState"
        )


class TestInterFoamFvSchemes:
    """Under `default none` every div key interFoam looks up must be declared."""

    def test_fvschemes_has_rhophi_div(self, tmp_path):
        """interFoam's momentum convection term is div(rhoPhi,U)."""
        assert "div(rhoPhi,U)" in _fv_schemes(tmp_path, CaseType.SLOSHING)

    def test_fvschemes_drops_single_phase_momentum_div(self, tmp_path):
        """The single-phase div(phi,U) is not what interFoam looks up."""
        content = _fv_schemes(tmp_path, CaseType.SLOSHING)
        assert "div(phi,U)" not in content

    def test_fvschemes_has_two_phase_stress_div(self, tmp_path):
        """The two-phase viscous stress term uses rho*nuEff and dev2."""
        content = _fv_schemes(tmp_path, CaseType.SLOSHING)
        assert "div(((rho*nuEff)*dev2(T(grad(U)))))" in content

    def test_fvschemes_drops_single_phase_stress_div(self, tmp_path):
        """The single-phase nuEff/dev stress term must not remain."""
        content = _fv_schemes(tmp_path, CaseType.SLOSHING)
        assert "div((nuEff*dev(T(grad(U)))))" not in content

    def test_fvschemes_has_alpha_advection_div(self, tmp_path):
        """alphaEqn looks up div(phi,alpha), not div(phi,alpha.water)."""
        assert "div(phi,alpha)" in _fv_schemes(tmp_path, CaseType.SLOSHING)

    def test_fvschemes_has_alpha_compression_div(self, tmp_path):
        """alphaEqn looks up div(phirb,alpha), not div(phirb,alpha.water)."""
        assert "div(phirb,alpha)" in _fv_schemes(tmp_path, CaseType.SLOSHING)

    def test_fvschemes_drops_field_suffixed_alpha_div(self, tmp_path):
        """The field-suffixed alpha key is never looked up and must go."""
        content = _fv_schemes(tmp_path, CaseType.SLOSHING)
        assert "div(phi,alpha.water)" not in content


# ============================================================================
# Over-correction guard -- single-phase cases must stay single-phase
# ============================================================================


class TestSinglePhaseUnaffected:
    """The VOF fix must not leak two-phase entries into simpleFoam cases."""

    def test_simplefoam_fvsolution_keeps_p(self, tmp_path):
        """simpleFoam still solves p."""
        content = _fv_solution(tmp_path, CaseType.CURRENT_LOADING)
        assert re.search(r"^\s{4}p$", content, re.MULTILINE) is not None

    def test_simplefoam_fvsolution_has_no_calpha(self, tmp_path):
        """A single-phase case must carry no MULES entries."""
        assert "cAlpha" not in _fv_solution(tmp_path, CaseType.CURRENT_LOADING)

    def test_simplefoam_fvsolution_keeps_simple_block(self, tmp_path):
        """simpleFoam uses the SIMPLE algorithm block."""
        assert "SIMPLE" in _fv_solution(tmp_path, CaseType.CURRENT_LOADING)

    def test_simplefoam_fvschemes_keeps_single_phase_momentum_div(self, tmp_path):
        """simpleFoam's momentum convection term stays div(phi,U)."""
        assert "div(phi,U)" in _fv_schemes(tmp_path, CaseType.CURRENT_LOADING)

    def test_simplefoam_fvschemes_has_no_rhophi_div(self, tmp_path):
        """A single-phase case must not carry the two-phase momentum term."""
        assert "div(rhoPhi,U)" not in _fv_schemes(tmp_path, CaseType.CURRENT_LOADING)


# ============================================================================
# Fail-closed on an undeclared solver
# ============================================================================


class TestUnknownSolverFailsClosed:
    """An undeclared solver must raise, not silently get a default dict."""

    def test_unknown_solver_raises(self, tmp_path):
        """Emitting a case whose solver has no contract is an error."""
        case = OpenFOAMCase.for_case_type(CaseType.SLOSHING, name="bogus")
        case.solver_config.solver_name = "fooFoam"
        with pytest.raises(KeyError):
            OpenFOAMCaseBuilder(case).build(tmp_path)
