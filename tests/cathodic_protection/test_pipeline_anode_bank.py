"""Hand-derived tests for terminal anode banks (issue #2260)."""

from __future__ import annotations

import math
import re
from pathlib import Path

import pytest

from digitalmodel.citations import CitationResolutionError

from digitalmodel.cathodic_protection.pipeline_anode_bank import (
    AnodeBankDesignInput,
    BankAnodeInput,
    BankInput,
    PipelineSideInput,
    StructureDemandInput,
    design_anode_bank_cp,
)


def _input(*, length_m: float = 100.0, installed: int = 1) -> AnodeBankDesignInput:
    return AnodeBankDesignInput(
        design_life_years=20.0,
        banks=[BankInput(
            bank_id="terminal-a",
            installed_anode_count=installed,
            structure=StructureDemandInput(
                area_m2=10.0,
                initial_current_density_A_m2=0.15,
                mean_current_density_A_m2=0.10,
                final_current_density_A_m2=0.12,
                initial_breakdown_factor=0.10,
                mean_breakdown_factor=0.20,
                final_breakdown_factor=0.30,
            ),
            anode=BankAnodeInput(
                material="aluminium", net_mass_kg=100.0, length_m=1.0,
                density_kg_m3=2750.0, utilisation_factor=0.90,
                electrolyte_resistivity_ohm_m=0.30,
            ),
            sides=[PipelineSideInput(
                side_id="flowline-east", outer_diameter_m=0.50,
                wall_thickness_m=0.025, length_m=length_m,
                linepipe_coating="single_or_dual_layer_fbe",
                exposure="Non-Buried", fluid_temperature_c=40.0,
                concrete_weight_coating=True,
            )],
        )],
    )


def test_bank_demand_resistance_and_far_potential_are_hand_derived() -> None:
    bank = design_anode_bank_cp(_input()).banks[0]
    # Table 6-2 i=.060; Table A-1 FBE+CWC a=.030,b=.0003.
    # pi*.5*100*.060*(.030/.033/.036) = .282743/.311018/.339292 A.
    assert bank.current_demand_A.pipeline.initial == pytest.approx(0.2827433388)
    assert bank.current_demand_A.pipeline.mean == pytest.approx(0.3110176727)
    assert bank.current_demand_A.pipeline.final == pytest.approx(0.3392920066)
    # Structure: 10*(.15*.10/.10*.20/.12*.30)=.15/.20/.36 A.
    assert bank.current_demand_A.structure.model_dump() == pytest.approx(
        {"initial": 0.15, "mean": 0.20, "final": 0.36}
    )
    # ri=sqrt(100/(pi*1*2750)); R=.3/(2*pi)*(ln(4/ri)-1).
    assert bank.anode_resistance_ohm.individual.initial == pytest.approx(0.1248929728)
    # rf uses (1-.9)*100=10 kg remaining in the same equation.
    assert bank.anode_resistance_ohm.individual.final == pytest.approx(0.1798631428)
    # Efar=-1.05+(.36+.3392920066)*.1798631428+.0001818947.
    expected_far = -1.05 + 0.6992920066 * 0.1798631428 + 0.000181894736842
    assert bank.sides[0].far_potential_V == pytest.approx(expected_far)
    assert bank.sides[0].protection_ok is True


def test_two_sides_and_structure_all_load_the_bank() -> None:
    data = _input()
    side = data.banks[0].sides[0].model_copy(update={"side_id": "flowline-west"})
    data.banks[0].sides.append(side)
    bank = design_anode_bank_cp(data).banks[0]
    # Ibank=.36+2*(pi*.5*100*.060*.036)=1.038584013 A.
    expected_bank = -1.05 + 1.0385840132 * 0.1798631428
    assert bank.sides[0].potential_envelope.potential_V[0] == pytest.approx(expected_bank)
    assert bank.sides[0].potential_envelope.distance_m == [0.0, 25.0, 50.0, 75.0, 100.0]


def test_field_joint_ratio_uses_linepipe_length_denominator() -> None:
    data = _input()
    data.banks[0].sides[0] = data.banks[0].sides[0].model_copy(update={
        "field_joint_area_fraction": 0.10, "field_joint_coating": "3A+4E(2)"
    })
    side = design_anode_bank_cp(data).banks[0].sides[0]
    # Eq. (11): r=L_FJC/L_linepipe=0.1/0.9=1/9, not 0.1.
    assert side.coating.field_joint_to_linepipe_ratio == pytest.approx(1.0 / 9.0)
    assert side.effective_final_breakdown_factor == pytest.approx(
        side.coating.linepipe_final_factor
        + (1.0 / 9.0) * side.coating.field_joint_final_factor
    )


def test_long_side_fails_far_potential_and_governs() -> None:
    result = design_anode_bank_cp(_input(length_m=5000.0, installed=10))
    assert result.status.result == "FAIL"
    assert "attenuation:flowline-east" in result.status.governing_case
    assert result.banks[0].sides[0].protection_ok is False


def test_duplicate_side_ids_are_rejected() -> None:
    data = _input()
    data.banks.append(data.banks[0].model_copy(update={"bank_id": "terminal-b"}))
    with pytest.raises(ValueError, match="side_id"):
        design_anode_bank_cp(data)


def test_formula_references_separate_standard_and_circuit_relation() -> None:
    result = design_anode_bank_cp(_input())
    joined = " ".join(result.formula_references)
    assert "Eq. (20)" in joined
    assert "ideal parallel circuit" in joined
    assert "Table A-7" in joined
    assert math.isfinite(result.banks[0].sides[0].f103_eq20_protected_length_m)


def test_2010_uses_renumbered_sections_and_companion_table() -> None:
    data = _input()
    data.edition = "2010"
    result = design_anode_bank_cp(data)
    joined = " ".join(result.formula_references)
    assert "Sec. 5.6" in joined
    assert "Table 10-7" in joined
    assert result.edition == "2010"


def test_cable_limited_search_is_not_reported_as_finite_cap_exhaustion() -> None:
    data = _input()
    data.max_anode_count = 3
    data.banks[0].anode.cable_resistance_ohm = 1.0
    result = design_anode_bank_cp(data)
    assert result.banks[0].anode_requirements["search_outcome"] == "cable_limited_impossible"


def test_result_contract_has_frozen_top_level_keys() -> None:
    assert set(design_anode_bank_cp(_input()).model_dump()) == {
        "standard", "edition", "provenance", "design_life_years", "banks",
        "citations", "formula_references", "model_limitations", "status",
    }


def test_calculation_fails_closed_when_citation_page_is_missing(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    (tmp_path / "wikis").mkdir()
    monkeypatch.setenv("LLM_WIKI_PATH", str(tmp_path))
    with pytest.raises(CitationResolutionError, match="page_missing"):
        design_anode_bank_cp(_input())


def test_interaction_factor_cannot_reduce_ideal_parallel_resistance() -> None:
    raw = _input().model_dump()
    raw["banks"][0]["anode"]["interaction_factor"] = 0.5
    with pytest.raises(ValueError, match="interaction_factor"):
        AnodeBankDesignInput.model_validate(raw)


def test_duplicate_bank_ids_are_rejected() -> None:
    data = _input()
    second = data.banks[0].model_copy(update={
        "sides": [data.banks[0].sides[0].model_copy(update={"side_id": "flowline-west"})]
    })
    data.banks.append(second)
    with pytest.raises(ValueError, match="bank_id"):
        design_anode_bank_cp(data)


def test_fjc_extension_uses_area_weighted_side_current_per_length() -> None:
    data = _input()
    data.banks[0].sides[0] = data.banks[0].sides[0].model_copy(update={
        "field_joint_area_fraction": 0.10, "field_joint_coating": "3A+4E(2)"
    })
    bank = design_anode_bank_cp(data).banks[0]
    # Eq. (11) q_metal=.00433539786 A/m; q_bank=I_side/L=.00390185808 A/m.
    # A=R'*q_metal=2.32421053e-8, B=Rbank*q_bank=.000701800456,
    # C=-1.05+.1798631428*.36-(-.8)=-.185249269; root=261.694831 m.
    assert bank.sides[0].extended_protected_length_m == pytest.approx(261.694831321)
    # Exact Eq. (20) uses q_metal in both A and 2*R*q, giving 159.920836 m.
    assert bank.sides[0].f103_eq20_protected_length_m == pytest.approx(159.920836269)


def test_finite_search_cap_is_distinct_from_physical_impossibility() -> None:
    data = _input(length_m=1000.0)
    data.max_anode_count = 1
    result = design_anode_bank_cp(data)
    assert result.banks[0].anode_requirements["search_outcome"] == "search_cap_exhausted"


def test_equal_bank_scores_govern_by_lexical_bank_id() -> None:
    data = _input()
    first = data.banks[0]
    first.bank_id = "z-bank"
    second = first.model_copy(deep=True)
    second.bank_id = "a-bank"
    second.sides[0].side_id = "flowline-west"
    data.banks.append(second)
    assert design_anode_bank_cp(data).status.governing_case.startswith("bank:a-bank/")


def test_recommended_count_recomputes_n_minus_one_and_n() -> None:
    lower = design_anode_bank_cp(_input(length_m=500.0, installed=1))
    design = design_anode_bank_cp(_input(length_m=500.0, installed=2))
    assert design.banks[0].anode_requirements["recommended_anode_count"] == 2
    assert lower.status.result == "FAIL"
    assert design.status.result == "PASS"


def test_field_joint_count_cutback_matches_area_fraction() -> None:
    fraction = _input()
    fraction.banks[0].sides[0] = fraction.banks[0].sides[0].model_copy(update={
        "field_joint_area_fraction": 0.10, "field_joint_coating": "3A+4E(2)"
    })
    count = _input()
    raw = count.model_dump()
    raw["banks"][0]["sides"][0].update({
        "field_joint_count": 10, "field_joint_length_m": 1.0,
        "field_joint_coating": "3A+4E(2)",
    })
    count = AnodeBankDesignInput.model_validate(raw)
    a = design_anode_bank_cp(fraction).banks[0].sides[0]
    b = design_anode_bank_cp(count).banks[0].sides[0]
    assert a.current_demand_A == b.current_demand_A
    assert a.far_potential_V == pytest.approx(b.far_potential_V)


def test_public_regression_fixture_contains_no_source_identity_or_absolute_path() -> None:
    fixture = (Path(__file__).resolve().parents[1] / "fixtures" /
               "cathodic_protection" / "workflow_inputs" / "anode_bank.yml")
    text = fixture.read_text(encoding="utf-8")
    assert re.search(r"(?:^|[\s:'\"])(?:/|[A-Za-z]:\\)", text) is None
    assert not {"source_path", "document_number", "client", "operator", "project"} & {
        line.split(":", 1)[0].strip() for line in text.splitlines() if ":" in line
    }
