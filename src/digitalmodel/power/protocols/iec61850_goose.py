"""IEC 61850 GOOSE messaging model — enums, dataclasses, validators.

Provides data structures for modeling Generic Object Oriented Substation
Event (GOOSE) messaging: data attributes, datasets, control blocks,
publishers, subscribers, logical nodes, and SCL structure generation.

References
----------
IEC 61850-7-2 — Abstract communication service interface (ACSI).
IEC 61850-8-1 — Specific communication service mapping (SCSM) —
Mappings to MMS and to ISO/IEC 8802-3.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from enum import Enum


class LogicalNodeType(Enum):
    """IEC 61850 logical node class types.

    Attributes
    ----------
    XCBR : str
        Circuit breaker.
    XSWI : str
        Disconnector or switch.
    CSWI : str
        Switch controller.
    PTOC : str
        Time overcurrent protection.
    PDIF : str
        Differential protection.
    MMXU : str
        Measurement (voltage, current, power).
    CILO : str
        Interlocking.
    """

    XCBR = "XCBR"
    XSWI = "XSWI"
    CSWI = "CSWI"
    PTOC = "PTOC"
    PDIF = "PDIF"
    MMXU = "MMXU"
    CILO = "CILO"


class DAType(Enum):
    """Data attribute type for GOOSE data attributes.

    Attributes
    ----------
    BOOLEAN : str
        Boolean value.
    INT32 : str
        32-bit signed integer.
    FLOAT32 : str
        32-bit floating point.
    VISIBLE_STRING : str
        Visible ASCII string.
    QUALITY : str
        IEC 61850 quality descriptor.
    TIMESTAMP : str
        IEC 61850 timestamp.
    """

    BOOLEAN = "BOOLEAN"
    INT32 = "INT32"
    FLOAT32 = "FLOAT32"
    VISIBLE_STRING = "VISIBLE_STRING"
    QUALITY = "QUALITY"
    TIMESTAMP = "TIMESTAMP"


@dataclass
class GOOSEDataAttribute:
    """A single data attribute within a GOOSE dataset.

    Parameters
    ----------
    name : str
        Attribute name (e.g. 'stVal', 'mag').
    da_type : DAType
        Data attribute type.
    value : object
        Current value of the attribute.
    """

    name: str
    da_type: DAType
    value: object


@dataclass
class GOOSEDataset:
    """GOOSE dataset containing one or more data attributes.

    Parameters
    ----------
    dataset_ref : str
        Dataset reference path (e.g. 'IED1/LLN0$DS_GOOSE1').
    attributes : list[GOOSEDataAttribute]
        Data attributes in the dataset. Must contain at least one.
    """

    dataset_ref: str
    attributes: list[GOOSEDataAttribute] = field(default_factory=list)

    def __post_init__(self) -> None:
        if not self.attributes:
            raise ValueError(
                "GOOSEDataset must contain at least one attribute"
            )


@dataclass
class GOOSEControlBlock:
    """GOOSE control block configuration.

    Parameters
    ----------
    cb_ref : str
        Control block reference path.
    app_id : int
        Application identifier (0x0000–0x3FFF per IEC 61850-8-1).
    conf_rev : int
        Configuration revision number.
    dataset_ref : str
        Reference to the associated dataset.
    max_time_ms : int
        Maximum retransmission time [ms].
    min_time_ms : int
        Minimum retransmission time [ms].
    """

    cb_ref: str
    app_id: int
    conf_rev: int
    dataset_ref: str
    max_time_ms: int
    min_time_ms: int

    def __post_init__(self) -> None:
        if self.app_id < 0x0000 or self.app_id > 0x3FFF:
            raise ValueError(
                f"app_id must be in range 0x0000–0x3FFF, "
                f"got {hex(self.app_id)}"
            )


@dataclass
class GOOSEPublisher:
    """GOOSE publishing IED with control block and dataset.

    Parameters
    ----------
    ied_name : str
        IED name.
    access_point : str
        Communication access point name.
    logical_device : str
        Logical device instance identifier.
    control_block : GOOSEControlBlock
        GOOSE control block configuration.
    dataset : GOOSEDataset
        Dataset published by this IED.
    """

    ied_name: str
    access_point: str
    logical_device: str
    control_block: GOOSEControlBlock
    dataset: GOOSEDataset


@dataclass
class GOOSESubscriber:
    """GOOSE subscribing IED bound to a publisher.

    Parameters
    ----------
    ied_name : str
        Subscriber IED name.
    subscribed_app_id : int
        Application ID of the publisher to subscribe to.
    subscribed_dataset_ref : str
        Dataset reference of the publisher.
    inputs : list[str]
        List of mapped attribute names consumed from the dataset.
    """

    ied_name: str
    subscribed_app_id: int
    subscribed_dataset_ref: str
    inputs: list[str] = field(default_factory=list)


@dataclass
class LogicalNode:
    """IEC 61850 logical node instance.

    Parameters
    ----------
    ln_class : LogicalNodeType
        Logical node class type.
    ln_inst : str
        Instance identifier (e.g. '1', '2').
    description : str
        Human-readable description.
    """

    ln_class: LogicalNodeType
    ln_inst: str
    description: str


def validate_dataset(dataset: GOOSEDataset) -> dict:
    """Validate a GOOSE dataset for completeness.

    Parameters
    ----------
    dataset : GOOSEDataset
        Dataset to validate.

    Returns
    -------
    dict
        ``{"pass": bool, "issues": list[str]}``
    """
    issues: list[str] = []

    names = [attr.name for attr in dataset.attributes]
    seen: set[str] = set()
    for name in names:
        if name in seen:
            issues.append(f"Duplicate attribute name: '{name}'")
        seen.add(name)

    return {"pass": len(issues) == 0, "issues": issues}


def validate_binding(
    publisher: GOOSEPublisher, subscriber: GOOSESubscriber
) -> dict:
    """Validate subscriber binding against a publisher.

    Parameters
    ----------
    publisher : GOOSEPublisher
        The GOOSE publisher.
    subscriber : GOOSESubscriber
        The GOOSE subscriber to validate.

    Returns
    -------
    dict
        ``{"pass": bool, "issues": list[str]}``
    """
    issues: list[str] = []

    if subscriber.subscribed_app_id != publisher.control_block.app_id:
        issues.append(
            f"app_id mismatch: subscriber={subscriber.subscribed_app_id}, "
            f"publisher={publisher.control_block.app_id}"
        )

    if (
        subscriber.subscribed_dataset_ref
        != publisher.control_block.dataset_ref
    ):
        issues.append(
            f"dataset_ref mismatch: "
            f"subscriber='{subscriber.subscribed_dataset_ref}', "
            f"publisher='{publisher.control_block.dataset_ref}'"
        )

    return {"pass": len(issues) == 0, "issues": issues}


def generate_scl_structure(publisher: GOOSEPublisher) -> dict:
    """Generate an SCL-like structure dict for a GOOSE publisher.

    Parameters
    ----------
    publisher : GOOSEPublisher
        Publisher to represent in SCL format.

    Returns
    -------
    dict
        Nested dict representing IED > AccessPoint > Server >
        LDevice > LN0 with GOOSEControl and DataSet.
    """
    cb = publisher.control_block
    ds = publisher.dataset

    fcda_list = [
        {"daName": attr.name, "daType": attr.da_type.value}
        for attr in ds.attributes
    ]

    return {
        "IED": {
            "name": publisher.ied_name,
            "AccessPoint": {
                "name": publisher.access_point,
                "Server": {
                    "LDevice": {
                        "inst": publisher.logical_device,
                        "LN0": {
                            "GOOSEControl": {
                                "cbRef": cb.cb_ref,
                                "appID": hex(cb.app_id),
                                "confRev": cb.conf_rev,
                                "datSet": cb.dataset_ref,
                                "maxTime": cb.max_time_ms,
                                "minTime": cb.min_time_ms,
                            },
                            "DataSet": {
                                "name": ds.dataset_ref,
                                "FCDA": fcda_list,
                            },
                        },
                    },
                },
            },
        },
    }
