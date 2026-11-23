"""Tests for the technology-qualification gate (issue #1225).

Independent of the LCOE engine: exercises weighted scoring, verdict thresholds,
ranking and the DNV-RP-A203 TRL mapping.
"""

import pytest
from pydantic import ValidationError

from digitalmodel.floating_wind.qualification import (
    DNV_TRL_MAX,
    ConceptMaturity,
    Criterion,
    QualificationCriteria,
    QualificationVerdict,
    default_criteria,
    rank_concepts,
    score_concept,
    trl_to_maturity,
)


@pytest.fixture
def criteria():
    return default_criteria()


def test_default_criteria_loads(criteria):
    keys = {c.key for c in criteria.criteria}
    assert "technology_readiness" in keys
    assert criteria.total_weight == pytest.approx(1.0)  # 0.35+0.25+0.20+0.20


def test_trl_mapping():
    assert trl_to_maturity(0) == 0.0
    assert trl_to_maturity(DNV_TRL_MAX) == 1.0
    assert trl_to_maturity(7) == pytest.approx(1.0)
    with pytest.raises(ValueError):
        trl_to_maturity(8)


def test_all_max_scores_is_qualified(criteria):
    concept = ConceptMaturity(
        name="proven_semi",
        scores={c.key: 1.0 for c in criteria.criteria},
    )
    r = score_concept(concept, criteria)
    assert r.score == pytest.approx(1.0)
    assert r.verdict is QualificationVerdict.QUALIFIED


def test_all_zero_scores_is_not_qualified(criteria):
    concept = ConceptMaturity(name="novel", scores={})
    r = score_concept(concept, criteria)
    assert r.score == pytest.approx(0.0)
    assert r.verdict is QualificationVerdict.NOT_QUALIFIED


def test_missing_criterion_counts_as_zero(criteria):
    """Omitted criteria must not inflate a score."""
    full = ConceptMaturity(name="a", scores={c.key: 0.8 for c in criteria.criteria})
    partial = ConceptMaturity(name="b", scores={"technology_readiness": 0.8})
    assert score_concept(partial, criteria).score < score_concept(full, criteria).score


def test_weighted_mean_is_correct():
    crit = QualificationCriteria(
        criteria=[
            Criterion(key="a", weight=3.0),
            Criterion(key="b", weight=1.0),
        ],
        qualified_threshold=0.9,
        conditional_threshold=0.5,
    )
    concept = ConceptMaturity(name="c", scores={"a": 1.0, "b": 0.0})
    # (3*1 + 1*0) / 4 = 0.75
    assert score_concept(concept, crit).score == pytest.approx(0.75)


def test_verdict_thresholds(criteria):
    assert criteria.verdict_for(0.70) is QualificationVerdict.QUALIFIED
    assert criteria.verdict_for(0.69) is QualificationVerdict.CONDITIONAL
    assert criteria.verdict_for(0.50) is QualificationVerdict.CONDITIONAL
    assert criteria.verdict_for(0.49) is QualificationVerdict.NOT_QUALIFIED


def test_ranking_is_best_first(criteria):
    concepts = [
        ConceptMaturity(name="low", scores={"technology_readiness": 0.2}),
        ConceptMaturity(name="high", scores={c.key: 0.9 for c in criteria.criteria}),
        ConceptMaturity(name="mid", scores={c.key: 0.6 for c in criteria.criteria}),
    ]
    ranked = rank_concepts(concepts, criteria)
    assert [r.name for r in ranked] == ["high", "mid", "low"]
    assert ranked[0].score >= ranked[1].score >= ranked[2].score


def test_score_out_of_range_rejected():
    with pytest.raises(ValidationError):
        ConceptMaturity(name="bad", scores={"technology_readiness": 1.4})


def test_thresholds_must_be_ordered():
    with pytest.raises(ValidationError):
        QualificationCriteria(
            criteria=[Criterion(key="a", weight=1.0)],
            qualified_threshold=0.4,
            conditional_threshold=0.6,
        )


def test_duplicate_keys_rejected():
    with pytest.raises(ValidationError):
        QualificationCriteria(
            criteria=[Criterion(key="a", weight=1.0), Criterion(key="a", weight=2.0)],
        )
