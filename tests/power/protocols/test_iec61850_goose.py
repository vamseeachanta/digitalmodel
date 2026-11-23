"""Tests for IEC 61850 GOOSE messaging model.

Covers dataclasses, enums, validation functions, and SCL structure
generation for Generic Object Oriented Substation Event (GOOSE)
protocol modeling.

References
----------
IEC 61850-8-1 — Communication networks and systems for power utility
automation — Specific communication service mapping (SCSM).
"""

from __future__ import annotations

import pytest

from digitalmodel.power.protocols.iec61850_goose import (
    DAType,
    GOOSEControlBlock,
    GOOSEDataAttribute,
    GOOSEDataset,
    GOOSEPublisher,
    GOOSESubscriber,
    LogicalNode,
    LogicalNodeType,
    generate_scl_structure,
    validate_binding,
    validate_dataset,
)


# -- Fixtures ----------------------------------------------------------------


@pytest.fixture()
def sample_attribute():
    return GOOSEDataAttribute(
        name="stVal", da_type=DAType.BOOLEAN, value=True
    )


@pytest.fixture()
def sample_dataset(sample_attribute):
    return GOOSEDataset(
        dataset_ref="IED1/LLN0$DS_GOOSE1",
        attributes=[sample_attribute],
    )


@pytest.fixture()
def sample_control_block():
    return GOOSEControlBlock(
        cb_ref="IED1/LLN0$GO$gcb01",
        app_id=0x0001,
        conf_rev=1,
        dataset_ref="IED1/LLN0$DS_GOOSE1",
        max_time_ms=1000,
        min_time_ms=4,
    )


@pytest.fixture()
def sample_publisher(sample_control_block, sample_dataset):
    return GOOSEPublisher(
        ied_name="IED1",
        access_point="AP1",
        logical_device="LD0",
        control_block=sample_control_block,
        dataset=sample_dataset,
    )


# -- LogicalNodeType enum ---------------------------------------------------


class TestLogicalNodeType:
    def test_all_ln_types_exist(self):
        """Enum must include all seven IEC 61850 logical node types."""
        types = {t.value for t in LogicalNodeType}
        expected = {"XCBR", "XSWI", "CSWI", "PTOC", "PDIF", "MMXU", "CILO"}
        assert types == expected

    def test_xcbr_is_circuit_breaker(self):
        assert LogicalNodeType.XCBR.value == "XCBR"


# -- GOOSEDataAttribute -----------------------------------------------------


class TestGOOSEDataAttribute:
    def test_create_boolean_attribute(self):
        attr = GOOSEDataAttribute(
            name="stVal", da_type=DAType.BOOLEAN, value=True
        )
        assert attr.name == "stVal"
        assert attr.da_type == DAType.BOOLEAN
        assert attr.value is True

    def test_create_float_attribute(self):
        attr = GOOSEDataAttribute(
            name="mag", da_type=DAType.FLOAT32, value=13.8
        )
        assert attr.da_type == DAType.FLOAT32
        assert attr.value == 13.8

    def test_all_da_types_exist(self):
        types = {t.value for t in DAType}
        expected = {
            "BOOLEAN", "INT32", "FLOAT32",
            "VISIBLE_STRING", "QUALITY", "TIMESTAMP",
        }
        assert types == expected


# -- GOOSEDataset -----------------------------------------------------------


class TestGOOSEDataset:
    def test_valid_dataset_creation(self, sample_attribute):
        ds = GOOSEDataset(
            dataset_ref="IED1/LLN0$DS1",
            attributes=[sample_attribute],
        )
        assert ds.dataset_ref == "IED1/LLN0$DS1"
        assert len(ds.attributes) == 1

    def test_empty_attributes_raises(self):
        with pytest.raises(ValueError, match="at least one attribute"):
            GOOSEDataset(
                dataset_ref="IED1/LLN0$DS1",
                attributes=[],
            )


# -- GOOSEControlBlock ------------------------------------------------------


class TestGOOSEControlBlock:
    def test_valid_control_block(self):
        cb = GOOSEControlBlock(
            cb_ref="IED1/LLN0$GO$gcb01",
            app_id=0x0000,
            conf_rev=1,
            dataset_ref="IED1/LLN0$DS1",
            max_time_ms=1000,
            min_time_ms=4,
        )
        assert cb.app_id == 0x0000
        assert cb.conf_rev == 1

    def test_app_id_above_max_raises(self):
        with pytest.raises(ValueError, match="app_id"):
            GOOSEControlBlock(
                cb_ref="ref",
                app_id=0x4000,
                conf_rev=1,
                dataset_ref="ds",
                max_time_ms=1000,
                min_time_ms=4,
            )

    def test_app_id_negative_raises(self):
        with pytest.raises(ValueError, match="app_id"):
            GOOSEControlBlock(
                cb_ref="ref",
                app_id=-1,
                conf_rev=1,
                dataset_ref="ds",
                max_time_ms=1000,
                min_time_ms=4,
            )

    def test_app_id_at_upper_boundary(self):
        cb = GOOSEControlBlock(
            cb_ref="ref",
            app_id=0x3FFF,
            conf_rev=1,
            dataset_ref="ds",
            max_time_ms=1000,
            min_time_ms=4,
        )
        assert cb.app_id == 0x3FFF


# -- GOOSEPublisher ---------------------------------------------------------


class TestGOOSEPublisher:
    def test_publisher_creation(self, sample_publisher):
        assert sample_publisher.ied_name == "IED1"
        assert sample_publisher.access_point == "AP1"
        assert sample_publisher.logical_device == "LD0"
        assert sample_publisher.control_block.app_id == 0x0001


# -- GOOSESubscriber --------------------------------------------------------


class TestGOOSESubscriber:
    def test_subscriber_binding(self, sample_publisher):
        sub = GOOSESubscriber(
            ied_name="IED2",
            subscribed_app_id=sample_publisher.control_block.app_id,
            subscribed_dataset_ref=(
                sample_publisher.control_block.dataset_ref
            ),
            inputs=["stVal"],
        )
        assert sub.ied_name == "IED2"
        assert sub.subscribed_app_id == 0x0001


# -- LogicalNode -------------------------------------------------------------


class TestLogicalNode:
    def test_logical_node_creation(self):
        ln = LogicalNode(
            ln_class=LogicalNodeType.XCBR,
            ln_inst="1",
            description="Bay circuit breaker",
        )
        assert ln.ln_class == LogicalNodeType.XCBR
        assert ln.ln_inst == "1"

    def test_all_ln_types_mappable(self):
        """Every LogicalNodeType can be used to create a LogicalNode."""
        for ln_type in LogicalNodeType:
            ln = LogicalNode(
                ln_class=ln_type, ln_inst="1", description="test"
            )
            assert ln.ln_class == ln_type


# -- validate_dataset -------------------------------------------------------


class TestValidateDataset:
    def test_valid_dataset_passes(self, sample_dataset):
        result = validate_dataset(sample_dataset)
        assert result["pass"] is True
        assert result["issues"] == []

    def test_duplicate_attribute_names_flagged(self):
        attrs = [
            GOOSEDataAttribute("stVal", DAType.BOOLEAN, True),
            GOOSEDataAttribute("stVal", DAType.BOOLEAN, False),
        ]
        ds = GOOSEDataset(
            dataset_ref="IED1/LLN0$DS1", attributes=attrs
        )
        result = validate_dataset(ds)
        assert result["pass"] is False
        assert any("duplicate" in i.lower() for i in result["issues"])


# -- validate_binding -------------------------------------------------------


class TestValidateBinding:
    def test_matching_binding_passes(self, sample_publisher):
        sub = GOOSESubscriber(
            ied_name="IED2",
            subscribed_app_id=sample_publisher.control_block.app_id,
            subscribed_dataset_ref=(
                sample_publisher.control_block.dataset_ref
            ),
            inputs=["stVal"],
        )
        result = validate_binding(sample_publisher, sub)
        assert result["pass"] is True

    def test_mismatched_app_id_fails(self, sample_publisher):
        sub = GOOSESubscriber(
            ied_name="IED2",
            subscribed_app_id=0x0099,
            subscribed_dataset_ref=(
                sample_publisher.control_block.dataset_ref
            ),
            inputs=["stVal"],
        )
        result = validate_binding(sample_publisher, sub)
        assert result["pass"] is False
        assert any("app_id" in i.lower() for i in result["issues"])

    def test_mismatched_dataset_ref_fails(self, sample_publisher):
        sub = GOOSESubscriber(
            ied_name="IED2",
            subscribed_app_id=sample_publisher.control_block.app_id,
            subscribed_dataset_ref="WRONG/REF",
            inputs=["stVal"],
        )
        result = validate_binding(sample_publisher, sub)
        assert result["pass"] is False
        assert any("dataset_ref" in i.lower() for i in result["issues"])


# -- generate_scl_structure --------------------------------------------------


class TestGenerateSCLStructure:
    def test_scl_contains_ied(self, sample_publisher):
        scl = generate_scl_structure(sample_publisher)
        assert scl["IED"]["name"] == "IED1"

    def test_scl_contains_access_point(self, sample_publisher):
        scl = generate_scl_structure(sample_publisher)
        ap = scl["IED"]["AccessPoint"]
        assert ap["name"] == "AP1"

    def test_scl_contains_logical_device(self, sample_publisher):
        scl = generate_scl_structure(sample_publisher)
        ld = scl["IED"]["AccessPoint"]["Server"]["LDevice"]
        assert ld["inst"] == "LD0"

    def test_scl_contains_goose_control(self, sample_publisher):
        scl = generate_scl_structure(sample_publisher)
        ln0 = scl["IED"]["AccessPoint"]["Server"]["LDevice"]["LN0"]
        gcb = ln0["GOOSEControl"]
        assert gcb["appID"] == hex(0x0001)
        assert gcb["confRev"] == 1

    def test_scl_contains_dataset(self, sample_publisher):
        scl = generate_scl_structure(sample_publisher)
        ln0 = scl["IED"]["AccessPoint"]["Server"]["LDevice"]["LN0"]
        ds = ln0["DataSet"]
        assert ds["name"] == sample_publisher.dataset.dataset_ref
        assert len(ds["FCDA"]) == 1
