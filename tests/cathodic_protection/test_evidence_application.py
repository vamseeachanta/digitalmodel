"""Evidence application regressions; sources are locators, not qualification."""
import pytest

from digitalmodel.cathodic_protection import stray_current as sc
from digitalmodel.cathodic_protection._provisional import (
    ProvisionalValue, ProvisionalValueError, render_provisional, render_provisional_table,
)
from digitalmodel.cathodic_protection.iccp_design import (
    AnodeMaterial, ICCP_ANODE_RECORDS, DEFAULT_UTILISATION_FACTOR,
    anode_bed_design, iccp_anode_life,
)


def test_legacy_evidence_defaults() -> None:
    pv = ProvisionalValue(1.0, "mV", "literature", pending_standard="pending")
    assert pv.evidence_class == "none"
    assert pv.evidence_source == ""
    assert pv.provisional


@pytest.mark.parametrize("evidence_class", ["qualified", "", None])
def test_invalid_evidence_class_rejected(evidence_class: str) -> None:
    with pytest.raises(ProvisionalValueError, match="evidence_class"):
        ProvisionalValue(1.0, "mV", "source", pending_standard="pending",
                         evidence_class=evidence_class)


def test_nonempty_evidence_locator_required() -> None:
    with pytest.raises(ProvisionalValueError, match="evidence_source"):
        ProvisionalValue(1.0, "mV", "source", pending_standard="pending",
                         evidence_class="inferred")


@pytest.mark.parametrize("rho, expected, key", [
    # SIST EN 50162 preview Table 1, printed p10 / PDF p12:
    # lower row rho<15:20; middle 15<=rho<=200:1.5*rho; upper rho>200:300.
    (14.0, 20.0, "DC_SHIFT_LIMIT_LOW_RHO_MV"),
    (15.0, 22.5, "DC_SHIFT_SLOPE_MV_PER_OHM_M"),
    (200.0, 300.0, "DC_SHIFT_SLOPE_MV_PER_OHM_M"),
    (201.0, 300.0, "DC_SHIFT_LIMIT_HIGH_RHO_MV"),
])
def test_en_boundary_value_and_report_provenance(rho: float, expected: float, key: str) -> None:
    result = sc.assess_stray_current(sc.StrayCurrentInput(
        interference_type=sc.InterferenceType.DC_TRANSIT,
        soil_resistivity_ohm_m=rho, measured_shift_mV=0.0,
    ), experimental=True)
    assert result.shift_limit_mV == expected
    assert key in result.provenance
    assert "confirmed-by-official-preview" in result.provenance[key]
    assert "printed p10" in result.provenance[key]


def test_all_stray_values_evidence_classified() -> None:
    for name, pv in sc.PROVISIONAL_VALUES.items():
        expected = "reproduced-by-secondary"
        if name.startswith("DC_RHO") or name.startswith("DC_SHIFT"):
            expected = "confirmed-by-official-preview"
        elif name == "STEEL_RESISTIVITY_OHM_M":
            expected = "inferred"
        assert pv.evidence_class == expected
        assert "https://" in pv.evidence_source
        assert pv.provisional
        assert expected in render_provisional(pv)
    assert "Evidence class" in render_provisional_table(sc.PROVISIONAL_VALUES)


def test_iccp_records_have_reviewed_evidence_without_qualification() -> None:
    for rec in ICCP_ANODE_RECORDS.values():
        values = list(rec.consumption_rate.values()) + list(rec.max_current_density.values())
        if rec.density is not None:
            values.append(rec.density)
        for pv in values:
            assert pv.evidence_source
            assert pv.provisional
    assert DEFAULT_UTILISATION_FACTOR.evidence_class == "reproduced-by-secondary"


def test_graphite_requires_measured_mass() -> None:
    # TP-16 Table 1 p2 gives a MAXIMUM, not nominal density; geometry cannot give mass.
    with pytest.raises(ValueError, match="anode_mass_kg"):
        iccp_anode_life(5.0, 3, AnodeMaterial.GRAPHITE,
                        anode_length_m=1.0, anode_diameter_m=0.1, experimental=True)
    assert ICCP_ANODE_RECORDS[AnodeMaterial.GRAPHITE].density is None


def test_graphite_measured_mass_life() -> None:
    # TP-16 s5.2.2.3 p56: Y=N*m*u/(C*I); s1.1.4.3 p4: C=2.5 lb/(A yr).
    # With measured 20 kg/anode, N=3, u=0.8, I=5: Y=48/(2.5*0.45359237*5).
    assert iccp_anode_life(5.0, 3, AnodeMaterial.GRAPHITE,
                           anode_mass_kg=20.0, experimental=True) == pytest.approx(
                               48.0 / (2.5 * 0.45359237 * 5.0))


def test_iccp_report_carries_each_used_value_evidence() -> None:
    result = anode_bed_design(1.0, 50.0, anode_material=AnodeMaterial.GRAPHITE,
                              anode_mass_kg=20.0, experimental=True)
    rendered = "\n".join(result.provisional_sources)
    for name in ("consumption_rate", "max_current_density", "utilisation_factor"):
        assert name in rendered
    assert "reproduced-by-secondary" in rendered
    assert "PROVISIONAL" in rendered


def test_graphite_bed_mass_requirement_preserves_geometry_gate() -> None:
    plain = anode_bed_design(5.0, 50.0, anode_material=AnodeMaterial.GRAPHITE,
                             anode_spacing_m=5.0)
    assert plain.estimated_life_years is None
    with pytest.raises(ValueError, match="anode_mass_kg"):
        anode_bed_design(5.0, 50.0, experimental=True,
                         anode_material=AnodeMaterial.GRAPHITE, anode_spacing_m=5.0)
    exp = anode_bed_design(5.0, 50.0, anode_mass_kg=20.0, experimental=True,
                           anode_material=AnodeMaterial.GRAPHITE, anode_spacing_m=5.0)
    assert exp.number_of_anodes == plain.number_of_anodes
    assert exp.bed_resistance_ohm == plain.bed_resistance_ohm
    assert not any("consumption_rate" in source for source in plain.provisional_sources)


def test_iccp_crosswalk_confidence_is_not_supplier_confirmation() -> None:
    # Crosswalk ICCP materials: only graphite/scrap rates and HSCBCI SG were corroborated.
    for material, rec in ICCP_ANODE_RECORDS.items():
        expected = ("reproduced-by-secondary" if material in
                    (AnodeMaterial.GRAPHITE, AnodeMaterial.SCRAP_STEEL) else "none")
        for value in (*rec.consumption_rate.values(), *rec.max_current_density.values()):
            assert value.evidence_class == expected
    density = ICCP_ANODE_RECORDS[AnodeMaterial.HIGH_SILICON_CAST_IRON].density
    assert density is not None
    assert density.evidence_class == "reproduced-by-secondary"
    assert "HSCBCI" in density.evidence_source


def test_exact_upper_boundary_excludes_high_band_provenance() -> None:
    # EN Table 1 printed p10/PDF p12: middle row owns 200 even though both give 300 mV.
    assert "DC_SHIFT_LIMIT_HIGH_RHO_MV" not in sc._dc_limit_keys(
        200.0, sc.ShiftBasis.INCLUDING_IR)


def test_all_evidence_classes_preserve_provisional_status() -> None:
    for evidence_class in ("confirmed-by-official-preview", "quoted-by-regulation",
                           "reproduced-by-secondary", "inferred", "none"):
        pv = ProvisionalValue(1.0, "mV", "literature", pending_standard="pending",
                              evidence_class=evidence_class,
                              evidence_source="Evidence title; https://example.org; p1")
        assert pv.provisional
        assert "PROVISIONAL" in render_provisional(pv)
