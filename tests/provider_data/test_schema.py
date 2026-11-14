"""Provider provenance and numeric/categorical validation, independent of solvers."""

from dataclasses import FrozenInstanceError, asdict

import pytest

from digitalmodel.provider_data import ProviderDataRecord, ProviderDataSource, SpecValue


def _record(source_fields, parameters_payload):
    return ProviderDataRecord(
        provider=ProviderDataSource(**source_fields),
        system_name="15k Intervention Riser System",
        system_slug="irs-15k",
        specifications={
            key: SpecValue(**value)
            for key, value in parameters_payload["specifications"].items()
            if key != "bore_variants"
        },
    )


def test_provider_data_source_missing_field_raises(source_fields):
    source_fields.pop("vendor")
    with pytest.raises((TypeError, ValueError)):
        ProviderDataSource(**source_fields)


def test_provider_data_source_empty_field_raises(source_fields):
    for key in (
        "vendor",
        "vendor_slug",
        "document_name",
        "origin_url",
        "private_mount_path",
        "inventory_ref",
        "retrieved_date",
    ):
        for value in ("", "  ", None, 42):
            with pytest.raises(ValueError, match=key):
                ProviderDataSource(**(source_fields | {key: value}))


def test_spec_value_empty_unit_raises():
    for unit in ("", "  ", None, 42):
        with pytest.raises(ValueError, match="unit"):
            SpecValue(value=1.0, unit=unit)


def test_spec_value_categorical_round_trip(source_fields):
    spec = SpecValue(value="MUX", unit="categorical", note="Control system")
    assert SpecValue(**asdict(spec)) == spec
    assert spec.value == "MUX"
    with pytest.raises(FrozenInstanceError):
        spec.value = "modified"
    for value in ("", "  "):
        with pytest.raises(ValueError, match="categorical"):
            SpecValue(value=value, unit="categorical")
    source = ProviderDataSource(**source_fields)
    assert source.sha256 == ""
    assert source.note == ""
    assert ProviderDataSource(**asdict(source)) == source
    with pytest.raises(FrozenInstanceError):
        source.vendor = "modified"


def test_spec_value_string_with_non_categorical_unit_raises():
    for value, unit in (("MUX", "psi"), (15000, "categorical")):
        with pytest.raises(ValueError, match="categorical"):
            SpecValue(value=value, unit=unit)
    for value in (None, True, False, {}, []):
        with pytest.raises(ValueError, match="value"):
            SpecValue(value=value, unit="psi")


def test_provider_data_record_missing_required_spec_raises(
    source_fields,
    parameters_payload,
):
    record = _record(source_fields, parameters_payload)
    record.validate()
    record.specifications.pop("mwd_m")
    with pytest.raises(ValueError, match="mwd_m"):
        record.validate()


def test_provider_data_record_negative_value_raises(source_fields, parameters_payload):
    record = _record(source_fields, parameters_payload)
    for value in (-100.0, float("nan"), float("inf"), -float("inf")):
        for spec in (
            SpecValue(value=value, unit="psi"),
            [SpecValue(value=value, unit="psi")],
        ):
            record.specifications["mwp_psi"] = spec
            with pytest.raises(ValueError, match="mwp_psi"):
                record.validate()
    record.specifications["mwp_psi"] = SpecValue(value=0.0, unit="psi")
    record.validate()
