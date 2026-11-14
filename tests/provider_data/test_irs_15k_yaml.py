"""Approved pilot assertions against the separately maintained provider library."""

import pytest
import yaml

from digitalmodel.provider_data import ProviderDataRecord, load_provider_yaml


def test_load_provider_yaml_irs_15k_nominal(irs_15k_record, tmp_path):
    record = irs_15k_record
    assert isinstance(record, ProviderDataRecord)
    assert record.provider.vendor_slug == "helix-esg"
    assert record.system_slug == "irs-15k"
    record.validate()
    specs = record.specifications
    assert specs["design_temp_min_degF"].value < specs["design_temp_max_degF"].value
    assert specs["subsea_package_weight_kg"].value == pytest.approx(
        specs["subsea_package_weight_lbs"].value * 0.45359237, abs=1.0
    )
    assert specs["subsea_package_weight_with_safety_head_kg"].value == pytest.approx(
        specs["subsea_package_weight_with_safety_head_lbs"].value * 0.45359237,
        abs=0.1,
    )
    malformed = tmp_path / "malformed.yaml"
    malformed.write_text("source: [", encoding="utf-8")
    with pytest.raises(yaml.YAMLError):
        load_provider_yaml(malformed)


def test_irs_15k_mwd_value(irs_15k_record):
    specs = irs_15k_record.specifications
    assert specs["mwd_m"].value == 3048.0
    assert specs["mwd_m"].unit == "m"
    assert specs["mwd_m"].value == pytest.approx(specs["mwd_ft"].value * 0.3048)


def test_irs_15k_mwp_value(irs_15k_record):
    spec = irs_15k_record.specifications["mwp_psi"]
    assert spec.value == 15000.0
    assert spec.unit == "psi"


def test_irs_15k_bore_production_range(irs_15k_record):
    specs = irs_15k_record.specifications
    assert 0.15 <= specs["bore_production_id_m"].value <= 0.17
    assert specs["bore_production_id_m"].unit == "m"
    assert specs["bore_production_id_m"].value == pytest.approx(
        specs["bore_production_id_in"].value * 0.0254, abs=0.00001
    )


def test_irs_15k_categorical_control_system_present(irs_15k_record):
    spec = irs_15k_record.specifications["control_system"]
    assert isinstance(spec.value, str)
    assert "MUX" in spec.value
    assert spec.unit == "categorical"
