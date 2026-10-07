"""Acceptance of a comparison between two diffraction programs, by category.

A single tolerance across every quantity is the wrong shape for a code-to-code
comparison. The quantities do not carry the same uncertainty: hydrostatics is a
closed-form integral that can be matched almost exactly, excitation and added
mass are well-conditioned boundary-integral results, and radiation damping --
and the roll response at resonance, which damping sets -- is the most mesh- and
formulation-sensitive thing either program produces. Holding them to one number
either passes hydrostatic errors that should never pass or fails radiation
damping that is as good as the field achieves.

The tiers are keyed to the physical quantity, not to either program, so the
same criteria judge any pair.

One qualifier is load-bearing: agreement means little if the two programs are at
different distances from their own converged answers. A comparison inside its
threshold without demonstrated comparable convergence is PASS_CONDITIONAL, not
PASS.
"""

from __future__ import annotations

import pytest

from digitalmodel.hydrodynamics.diffraction.cross_solver_acceptance import (
    DEFAULT_CRITERIA,
    Category,
    ConvergenceEvidence,
    Verdict,
    category_for,
    convergence_from_self_study,
    evaluate,
)


class TestTheTiers:
    def test_hydrostatics_is_held_tightest(self):
        assert DEFAULT_CRITERIA[Category.HYDROSTATICS].threshold == 0.005

    def test_excitation_and_added_mass_share_the_middle_tier(self):
        assert DEFAULT_CRITERIA[Category.EXCITATION].threshold == 0.05
        assert DEFAULT_CRITERIA[Category.ADDED_MASS].threshold == 0.05

    def test_radiation_damping_and_resonant_roll_are_widest(self):
        assert DEFAULT_CRITERIA[Category.RADIATION_DAMPING].threshold == 0.10
        assert DEFAULT_CRITERIA[Category.RESONANT_ROLL_RAO].threshold == 0.10

    def test_every_category_states_its_normalisation(self):
        """A percentage without its denominator is not a criterion."""
        for crit in DEFAULT_CRITERIA.values():
            assert crit.normalisation
            assert crit.rationale

    def test_the_tiers_are_ordered_by_what_the_physics_supports(self):
        t = {c: DEFAULT_CRITERIA[c].threshold for c in Category}
        assert t[Category.HYDROSTATICS] < t[Category.EXCITATION]
        assert t[Category.EXCITATION] <= t[Category.RADIATION_DAMPING]


class TestCategoryMapping:
    @pytest.mark.parametrize("quantity,mode,expected", [
        ("restoring_stiffness", "roll", Category.HYDROSTATICS),
        ("added_mass", "heave", Category.ADDED_MASS),
        ("added_mass", "roll", Category.ADDED_MASS),
        ("damping", "pitch", Category.RADIATION_DAMPING),
        ("damping", "roll", Category.RADIATION_DAMPING),
        ("excitation", "sway", Category.EXCITATION),
        ("motion_rao", "heave", Category.MOTION_RAO),
        ("motion_rao", "roll", Category.RESONANT_ROLL_RAO),
    ])
    def test_quantity_and_mode_select_the_category(
            self, quantity, mode, expected):
        assert category_for(quantity, mode) is expected

    def test_an_unknown_quantity_is_an_error_not_a_default(self):
        """Silently judging an unrecognised quantity by some tier is the
        failure a category scheme exists to prevent."""
        with pytest.raises(ValueError):
            category_for("drift_force", "surge")


class TestVerdicts:
    CONVERGED = ConvergenceEvidence(comparable=True,
                                    basis="both converged to within 1%")
    UNKNOWN = ConvergenceEvidence(comparable=False,
                                  basis="convergence not demonstrated")

    def test_inside_threshold_with_comparable_convergence_passes(self):
        v = evaluate(Category.ADDED_MASS, 0.03, self.CONVERGED)
        assert v.verdict is Verdict.PASS

    def test_inside_threshold_without_convergence_evidence_is_conditional(self):
        v = evaluate(Category.RADIATION_DAMPING, 0.0774, self.UNKNOWN)
        assert v.verdict is Verdict.PASS_CONDITIONAL
        assert "convergence" in v.reason.lower()

    def test_outside_threshold_fails_whatever_the_convergence(self):
        assert evaluate(Category.HYDROSTATICS, 0.0049, self.CONVERGED).verdict \
            is Verdict.PASS
        assert evaluate(Category.HYDROSTATICS, 0.0051, self.CONVERGED).verdict \
            is Verdict.FAIL
        assert evaluate(Category.EXCITATION, 0.06, self.UNKNOWN).verdict \
            is Verdict.FAIL

    def test_the_threshold_itself_passes(self):
        """A stated bound is inclusive; 5.00% against 5% is inside."""
        assert evaluate(Category.EXCITATION, 0.05, self.CONVERGED).verdict \
            is Verdict.PASS

    def test_the_same_number_can_pass_one_category_and_fail_another(self):
        """7.74% is a pass for damping and a fail for added mass. That is the
        point of categorising rather than using one tolerance."""
        assert evaluate(Category.RADIATION_DAMPING, 0.0774,
                        self.CONVERGED).verdict is Verdict.PASS
        assert evaluate(Category.ADDED_MASS, 0.0774,
                        self.CONVERGED).verdict is Verdict.FAIL

    def test_a_verdict_carries_its_criterion(self):
        v = evaluate(Category.EXCITATION, 0.02, self.CONVERGED)
        assert v.threshold == 0.05
        assert v.observed == 0.02
        assert v.category is Category.EXCITATION
        assert v.margin == pytest.approx(0.03)

    @pytest.mark.parametrize("bad", [-0.01, float("nan"), float("inf")])
    def test_a_meaningless_difference_is_rejected(self, bad):
        with pytest.raises(ValueError):
            evaluate(Category.EXCITATION, bad, self.CONVERGED)

    def test_self_convergence_small_against_the_tier_is_comparable(self):
        """Each program's own residual must be small against the band being
        tested, or the band cannot resolve agreement from convergence noise."""
        ev = convergence_from_self_study(
            residual_a=0.0033, residual_b=0.0120,
            category=Category.RADIATION_DAMPING, basis="5 meshes, roll")
        assert ev.comparable is True

    def test_self_convergence_large_against_the_tier_is_not(self):
        """The same residuals judged against the 5% tier: 1.2% exceeds a
        quarter of it, so the band cannot tell them apart."""
        ev = convergence_from_self_study(
            residual_a=0.0033, residual_b=0.0140,
            category=Category.ADDED_MASS, basis="5 meshes, roll")
        assert ev.comparable is False
        assert "quarter" in ev.basis or "0.25" in ev.basis or "25%" in ev.basis

    def test_the_fraction_is_a_parameter(self):
        ev = convergence_from_self_study(
            residual_a=0.002, residual_b=0.004,
            category=Category.ADDED_MASS, basis="x", fraction=0.05)
        assert ev.comparable is False

    def test_custom_criteria_override_the_defaults(self):
        from dataclasses import replace

        tighter = dict(DEFAULT_CRITERIA)
        tighter[Category.RADIATION_DAMPING] = replace(
            DEFAULT_CRITERIA[Category.RADIATION_DAMPING], threshold=0.05)
        v = evaluate(Category.RADIATION_DAMPING, 0.0774, self.CONVERGED,
                     criteria=tighter)
        assert v.verdict is Verdict.FAIL
