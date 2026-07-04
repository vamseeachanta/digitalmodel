"""Tests for the collaboration / JIP de-risking overlay (issue #1285).

Uplift magnitudes are calibration inputs, so tests assert the mechanism:
targeted uplift, summing, the 1.0 cap, identity for an empty program, and a
conditional -> qualified requalification through the gate (#1225).
"""

import pytest
from pydantic import ValidationError

from digitalmodel.floating_wind.collaboration import (
    CollaborationProgram,
    JointIndustryProject,
    apply_collaboration,
    default_collaboration_program,
    requalify_with_collaboration,
)
from digitalmodel.floating_wind.qualification import (
    ConceptMaturity,
    QualificationVerdict,
    default_criteria,
)


def test_default_program_has_deck_jips():
    prog = default_collaboration_program()
    names = {j.name for j in prog.jips}
    assert names == {"Cability", "CALM", "SURF-IM", "SUREFLEX"}


def test_total_uplift_sums_per_criterion():
    prog = default_collaboration_program()
    tot = prog.total_uplift()
    # 3 JIPs target technology_readiness (Cability, CALM, SUREFLEX) @0.10
    assert tot["technology_readiness"] == pytest.approx(0.30)
    assert tot["track_record"] == pytest.approx(0.10)


def test_uplift_targets_only_named_criteria():
    prog = CollaborationProgram(
        jips=[JointIndustryProject(name="X", targets=["technology_readiness"], maturity_uplift=0.2)]
    )
    scores = {"technology_readiness": 0.5, "cost_confidence": 0.6}
    out = prog.uplift_scores(scores)
    assert out["technology_readiness"] == pytest.approx(0.7)
    assert out["cost_confidence"] == pytest.approx(0.6)  # untouched


def test_uplift_capped_at_one():
    prog = CollaborationProgram(
        jips=[JointIndustryProject(name="X", targets=["technology_readiness"], maturity_uplift=0.5)]
    )
    out = prog.uplift_scores({"technology_readiness": 0.8})
    assert out["technology_readiness"] == pytest.approx(1.0)  # 0.8+0.5 capped


def test_uplift_adds_absent_criterion_from_zero():
    prog = CollaborationProgram(
        jips=[JointIndustryProject(name="X", targets=["track_record"], maturity_uplift=0.15)]
    )
    out = prog.uplift_scores({"technology_readiness": 0.5})
    assert out["track_record"] == pytest.approx(0.15)


def test_empty_program_is_identity():
    prog = CollaborationProgram()
    scores = {"technology_readiness": 0.4, "track_record": 0.6}
    assert prog.uplift_scores(scores) == scores


def test_apply_collaboration_returns_new_concept():
    prog = default_collaboration_program()
    c = ConceptMaturity(name="semi", scores={"technology_readiness": 0.5})
    out = apply_collaboration(c, prog)
    assert out.name == "semi"
    assert out.scores["technology_readiness"] == pytest.approx(0.8)  # 0.5+0.30
    assert c.scores["technology_readiness"] == pytest.approx(0.5)  # original intact


def test_jips_move_conditional_to_qualified():
    """A concept weak on technology_readiness clears the gate after de-risking."""
    criteria = default_criteria()
    concept = ConceptMaturity(
        name="novel_tlp",
        scores={
            "technology_readiness": 0.50,
            "track_record": 0.70,
            "supply_chain_readiness": 0.70,
            "cost_confidence": 0.70,
        },
    )
    before, after = requalify_with_collaboration(
        concept, default_collaboration_program(), criteria
    )
    assert before.verdict is QualificationVerdict.CONDITIONAL
    assert after.verdict is QualificationVerdict.QUALIFIED
    assert after.score > before.score


def test_jip_requires_targets_and_positive_uplift():
    with pytest.raises(ValidationError):
        JointIndustryProject(name="bad", targets=[], maturity_uplift=0.1)
    with pytest.raises(ValidationError):
        JointIndustryProject(name="bad", targets=["technology_readiness"], maturity_uplift=0.0)
    with pytest.raises(ValidationError):
        JointIndustryProject(name="bad", targets=["technology_readiness"], maturity_uplift=1.5)
