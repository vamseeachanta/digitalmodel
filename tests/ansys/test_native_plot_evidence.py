# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Synthetic byte-binding and plot-state tests; no images or solver are opened."""

from dataclasses import replace
from hashlib import sha256

import pytest

from digitalmodel.ansys.native_plot_evidence import validate_plot_evidence
from digitalmodel.ansys.native_tensor_audit import CitedLimit
from tests.ansys.test_native_tensor_audit import RESULT, KEYS, audit, sample


# All values are artificial fixture inputs, not engineering acceptance limits.
RESULT_BYTES = b"synthetic result bytes"
PNG_BYTES = b"synthetic native export bytes; no decoded pixels"
DIGEST = sha256(RESULT_BYTES).hexdigest()


def evidence():
    return {
        "result_sha256_before": DIGEST,
        "result_sha256_after": DIGEST,
        "png_sha256": sha256(PNG_BYTES).hexdigest(),
        "result_set": RESULT,
        "pressure_mpa": "2",
        "native_command": "PLESOL,S,EQV",
        "camera": "front",
        "surface": "SOLID",
        "averaging": "UNAVERAGED",
        "dscale_command": "/DSCALE,1,OFF",
        "legend_min": "0",
        "legend_max": "10",
        "native_raw_vm_max": "6",
    }


def validate(record=None, **kwargs):
    """Uniaxial SX=6 gives VM=6 by sqrt((36+0+36)/2)."""
    report = audit(
        [sample(k, ("6", "0", "0", "0", "0", "0")) for k in KEYS],
        source_result_sha256=DIGEST,
    )
    return validate_plot_evidence(
        evidence() if record is None else record,
        result_before=RESULT_BYTES,
        result_after=RESULT_BYTES,
        png=PNG_BYTES,
        result_set=RESULT,
        pressure_mpa="2",
        tensor_audit=report,
        peak_tolerance=CitedLimit("0", "Exact synthetic reconciliation"),
        **kwargs,
    )


def test_plot_bytes_state_and_tensor_maximum_agree():
    result = validate(expected_legend=("0", "10"))
    assert result["status"] == "COMPLETE"
    assert result["native_peak_reconciliation"]["status"] == "PASS"
    assert result["shared_scale"]["status"] == "PASS"
    assert result["annotation_overlap"]["status"] == "NOT_EVALUATED"
    assert result["contour_pixels"]["status"] == "NOT_EVALUATED"


@pytest.mark.parametrize("command", ("/DSCALE,1,0", "/DSCALE,1,AUTO", "/DSCALE,1,1"))
def test_automatic_or_true_scale_is_not_undeformed(command):
    """2024 R2 /DSCALE: 0/AUTO magnifies, 1 is true scale, OFF removes deformation."""
    record = evidence()
    record["dscale_command"] = command
    assert validate(record)["status"] == "INCOMPLETE"


@pytest.mark.parametrize("field", tuple(evidence()))
def test_every_provenance_field_is_required(field):
    record = evidence()
    record.pop(field)
    assert validate(record)["status"] == "INCOMPLETE"


@pytest.mark.parametrize(
    "field,value",
    (
        ("result_sha256_before", "a" * 64),
        ("result_sha256_after", "a" * 64),
        ("png_sha256", "a" * 64),
        ("result_set", replace(RESULT, substep=2)),
        ("pressure_mpa", "3"),
        ("native_command", "PLNSOL,S,EQV"),
        ("averaging", "AVERAGED"),
        ("camera", ""),
        ("surface", ""),
        ("legend_min", "11"),
        ("legend_max", "NaN"),
        ("native_raw_vm_max", "5"),
        ("legend_max", "4"),
        ("surface", "TOP"),
    ),
)
def test_mismatched_evidence_refuses(field, value):
    record = evidence()
    record[field] = value
    assert validate(record)["status"] == "INCOMPLETE"


def test_comparison_requires_identical_shared_legend():
    assert validate(expected_legend=("0", "9"))["status"] == "INCOMPLETE"
    assert validate()["shared_scale"]["status"] == "NOT_EVALUATED"


def test_changed_source_bytes_are_a_raw_evidence_failure():
    report = audit([sample(k) for k in KEYS], source_result_sha256=DIGEST)
    result = validate_plot_evidence(
        evidence(),
        result_before=RESULT_BYTES,
        result_after=RESULT_BYTES + b"changed",
        png=PNG_BYTES,
        result_set=RESULT,
        pressure_mpa="2",
        tensor_audit=report,
        peak_tolerance=CitedLimit("0", "Synthetic exact comparison"),
    )
    assert result["status"] == "INCOMPLETE"
    assert result["native_peak_reconciliation"]["status"] == "NOT_EVALUATED"
