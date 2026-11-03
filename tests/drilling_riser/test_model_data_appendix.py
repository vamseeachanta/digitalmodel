"""Synthetic snapshots only; no solver import or licence is required.

All fixture numbers are chosen mathematical inputs, not design values.
Loading equations: Orcina, Morison's equation, online help retrieved 2026-10-19
(reproduces Morison et al., 1950). Each expected calculation is given below.
"""

import hashlib
import json
import math
from dataclasses import replace

import pytest

from digitalmodel.drilling_riser.model_data_appendix import (
    ComponentSnapshot,
    build_appendix,
)


MODEL_BYTES = b"synthetic circular line snapshot"
DIGEST = hashlib.sha256(MODEL_BYTES).hexdigest()


def component(name="upper", top=10.0, bottom=5.0, **changes):
    """Arbitrary SI inputs; 'synthetic' explicitly identifies their source."""
    properties = {
        "stress_od_m": 2.0,
        "stress_id_m": 1.0,
        "section_modulus_m3": 3.0,
        "hydrodynamic_diameter_m": 4.0,
        "added_mass_diameter_m": 2.0,
        "buoyancy_od_m": 4.0,
        "dry_mass_kg_m": 10.0,
        "wet_mass_kg_m": 7.0,
        "contents_mass_kg_m": 5.0,
        "ea_n": 20.0,
        "ei_n_m2": 30.0,
        "gj_n_m2": 40.0,
        "cd_normal": 0.5,
        "ca_normal": 2.0,
        "cd_axial": 0.1,
        "ca_axial": 0.2,
        "internal_fluid_density_kg_m3": 3.0,
        "marine_growth_thickness_m": 0.0,
        "segment_length_m": 0.5,
        "length_m": 5.0,
    }
    properties.update(changes)
    return ComponentSnapshot(
        name, top, bottom, DIGEST, "synthetic elevation input", properties,
        {key: "synthetic input" for key, value in properties.items()
         if value is not None},
    )


def build(*components, limits=None, density=8.0):
    return build_appendix(
        MODEL_BYTES, components, water_density_kg_m3=density,
        water_density_source="synthetic density input",
        screening_limits=limits,
    )


def test_tables_are_ordered_digest_bound_and_json_native():
    result = build(component("lower", 5, 0), component())
    assert [row["component_id"] for row in result["tables"]["C.1"]["rows"]] == [
        "upper", "lower",
    ]
    for table in result["tables"].values():
        assert DIGEST in table["caption"]
    assert result["model_sha256"] == DIGEST
    assert result["model_extraction_status"] == "NOT_EVALUATED"
    assert json.loads(json.dumps(result, allow_nan=False)) == result
    assert result["tables"]["C.1"]["units"]["ei_n_m2"] == "N m^2"
    assert result["tables"]["C.1"]["rows"][0]["sources"]["ei_n_m2"]


def test_circular_added_mass_and_drag_are_hand_checked():
    """Ca*rho*pi*D_added^2/4 = 2*8*pi*4/4 = 16*pi kg/m.

    Total = dry + contents + added = 15+16*pi, not wet+added.
    Drag factor = rho*Cd*D_drag/2 = 8*0.5*4/2 = 8 kg/m^2.
    Ca*D_added^2 = 2*4 = 8 m^2; differing diameters are intentional.
    """
    row = build(component())["tables"]["C.3"]["rows"][0]
    assert row["added_mass_kg_m"] == pytest.approx(16 * math.pi)
    assert row["total_inertial_mass_kg_m"] == pytest.approx(15 + 16 * math.pi)
    assert row["normal_drag_factor_kg_m2"] == pytest.approx(8.0)
    assert row["ca_d2_m2"] == pytest.approx(8.0)
    assert all(value == "CALCULATED" for value in row["status"].values())


def test_only_changed_interface_is_flagged_in_either_direction():
    """EI ratios: 30/30=1, 90/30=3, 30/90=1/3; step factors 1,3,3.

    The synthetic screen is 2, so only the last two boundaries exceed it.
    """
    result = build(
        component("a", 20, 15), component("b", 15, 10),
        component("c", 10, 5, ei_n_m2=90), component("d", 5, 0),
        limits={"ei_n_m2": 2.0},
    )
    checks = [row["metrics"]["ei_n_m2"]
              for row in result["tables"]["C.2"]["rows"]]
    assert [check["ratio_lower_over_upper"] for check in checks] == [
        1, 3, pytest.approx(1 / 3),
    ]
    assert [check["flag"] for check in checks] == [False, True, True]
    assert all(check["limit"] == 2 for check in checks)
    assert result["screening_is_code_criterion"] is False


def test_ratio_limit_equality_does_not_flag_and_no_limit_is_not_evaluated():
    """Synthetic EI step 60/30=2 equals the supplied limit; strict > is false."""
    result = build(component(), component("lower", 5, 0, ei_n_m2=60),
                   limits={"ei_n_m2": 2})
    metrics = result["tables"]["C.2"]["rows"][0]["metrics"]
    assert metrics["ei_n_m2"]["flag"] is False
    assert metrics["ea_n"]["ratio_lower_over_upper"] == 1
    assert metrics["ea_n"]["screening_status"] == "NOT_EVALUATED"
    assert metrics["ea_n"]["flag"] is None


def test_missing_properties_are_not_silent_zeroes():
    result = build(component(ca_normal=None, contents_mass_kg_m=None))
    section = result["tables"]["C.1"]["rows"][0]
    row = result["tables"]["C.3"]["rows"][0]
    assert section["properties"]["ca_normal"] is None
    assert section["status"]["ca_normal"] == "NOT_EVALUATED"
    assert row["added_mass_kg_m"] is None
    assert row["total_inertial_mass_kg_m"] is None
    assert row["normal_drag_factor_kg_m2"] == 8
    assert "ca_normal" in row["missing_inputs"]["added_mass_kg_m"]


def test_no_added_mass_diameter_is_inferred_from_drag_diameter():
    row = build(component(added_mass_diameter_m=None))["tables"]["C.3"]["rows"][0]
    assert row["added_mass_kg_m"] is None
    assert row["normal_drag_factor_kg_m2"] == 8


def test_zero_coefficient_is_calculated_but_zero_ratio_is_not_evaluated():
    result = build(component(ca_normal=0), component("lower", 5, 0))
    assert result["tables"]["C.3"]["rows"][0]["added_mass_kg_m"] == 0
    check = result["tables"]["C.2"]["rows"][0]["metrics"]["ca_d2_m2"]
    assert check["ratio_lower_over_upper"] is None
    assert check["ratio_status"] == "NOT_EVALUATED"


@pytest.mark.parametrize("value", [math.nan, math.inf, -1, True, "2"])
def test_invalid_property_is_rejected(value):
    with pytest.raises(ValueError, match="ei_n_m2"):
        build(component(ei_n_m2=value))


def test_signed_wet_mass_is_retained_but_never_used_as_inertia():
    row = build(component(wet_mass_kg_m=-7))["tables"]["C.1"]["rows"][0]
    assert row["properties"]["wet_mass_kg_m"] == -7


@pytest.mark.parametrize("change, message", [
    ({"model_sha256": "0" * 64}, "digest"),
    ({"source": " "}, "source"),
    ({"sources": {}}, "source"),
    ({"top_elevation_m": math.nan}, "elevation"),
    ({"bottom_elevation_m": 11}, "elevation"),
    ({"component_id": ""}, "component_id"),
])
def test_invalid_snapshot_is_rejected(change, message):
    with pytest.raises(ValueError, match=message):
        build(replace(component(), **change))


def test_typo_and_invalid_tube_geometry_are_rejected():
    with pytest.raises(ValueError, match="unknown"):
        build(component(EI=30))
    with pytest.raises(ValueError, match="stress_id_m"):
        build(component(stress_id_m=3))


@pytest.mark.parametrize("items", [
    (), (component(), component()),
    (component(), component("lower", 4, 0)),
    (component(), component("lower", 6, 0)),
])
def test_empty_duplicate_gap_and_overlap_are_rejected(items):
    with pytest.raises(ValueError):
        build(*items)


@pytest.mark.parametrize("limits", [{"typo": 2}, {"ei_n_m2": 1},
                                   {"ei_n_m2": math.inf}, {"ei_n_m2": True}])
def test_screens_are_explicit_valid_inputs(limits):
    with pytest.raises(ValueError):
        build(component(), limits=limits)


@pytest.mark.parametrize("density", [0, -1, math.nan, True])
def test_invalid_density_is_rejected(density):
    with pytest.raises(ValueError, match="density"):
        build(component(), density=density)


def test_empty_density_source_is_rejected():
    with pytest.raises(ValueError, match="source"):
        build_appendix(MODEL_BYTES, [component()], water_density_kg_m3=8,
                       water_density_source="", screening_limits={})


def test_overflow_is_rejected_instead_of_exporting_infinity():
    with pytest.raises(ValueError, match="finite"):
        build(component(added_mass_diameter_m=1e308))


def test_unused_source_metadata_cannot_break_json_serialization():
    item = component(ca_normal=None)
    with pytest.raises(ValueError, match="source"):
        build(replace(item, sources={**item.sources, "ca_normal": math.nan}))


def test_unrepresentable_integer_is_rejected_as_invalid_input():
    with pytest.raises(ValueError, match="finite"):
        build(component(ei_n_m2=10**1000))


@pytest.mark.parametrize("model_bytes", [b"", "not bytes"])
def test_model_bytes_are_required(model_bytes):
    with pytest.raises(ValueError, match="model bytes"):
        build_appendix(model_bytes, [component()], water_density_kg_m3=8,
                       water_density_source="synthetic", screening_limits={})


def test_source_snapshot_and_output_are_detached():
    item = component()
    result = build(item)
    item.properties["ei_n_m2"] = 999
    item.sources["ei_n_m2"] = "changed"
    row = result["tables"]["C.1"]["rows"][0]
    assert row["properties"]["ei_n_m2"] == 30
    assert row["sources"]["ei_n_m2"] == "synthetic input"


def test_unknown_source_and_overflowing_ratio_are_rejected():
    item = component()
    with pytest.raises(ValueError, match="absent property"):
        build(replace(item, sources={**item.sources, "typo": "synthetic"}))
    with pytest.raises(ValueError, match="finite"):
        build(component(ei_n_m2=1e-300), component("lower", 5, 0, ei_n_m2=1e300))
