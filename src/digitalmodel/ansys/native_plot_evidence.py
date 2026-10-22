# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Check declared native plot provenance against retained bytes and tensor peaks.

This validates metadata and byte binding, not native execution, PNG decoding,
annotation layout or pixel equality. Those claims remain NOT_EVALUATED.
"""

from hashlib import sha256

from digitalmodel.ansys.cylinder_results import EvidenceError, decimal_context
from digitalmodel.ansys.native_tensor_audit import number


REQUIRED = (
    "result_sha256_before",
    "result_sha256_after",
    "png_sha256",
    "result_set",
    "pressure_mpa",
    "native_command",
    "camera",
    "surface",
    "averaging",
    "dscale_command",
    "legend_min",
    "legend_max",
    "native_raw_vm_max",
)


def _unknown(reason):
    return {"status": "NOT_EVALUATED", "reason": reason}


@decimal_context
def validate_plot_evidence(
    record,
    *,
    result_before,
    result_after,
    png,
    result_set,
    pressure_mpa,
    tensor_audit,
    peak_tolerance,
    expected_legend=None,
):
    """Validate an undeformed, unaveraged native equivalent-stress export.

    PLESOL,S,EQV and /DSCALE,1,OFF are the supported capture contract.
    Source: Mechanical APDL Command Reference, 2024 R2, PLESOL and /DSCALE.
    Result-file identity must be attached to the tensor audit by its caller.
    Every limit is supplied explicitly; none is an engineering allowable here.
    """
    errors = []
    result = {
        "status": "INCOMPLETE",
        "errors": (),
        "native_peak_reconciliation": _unknown("Capture evidence incomplete"),
        "shared_scale": _unknown("No comparison legend supplied"),
        "annotation_overlap": _unknown("Native annotation layout not inspected"),
        "contour_pixels": _unknown("Preceding native contour pixels not compared"),
    }
    missing = [field for field in REQUIRED if field not in record]
    if missing:
        result["errors"] = tuple(f"Missing plot field: {field}" for field in missing)
        return result
    if not all(isinstance(b, bytes) and b for b in (result_before, result_after, png)):
        result["errors"] = ("Retained nonempty result and export bytes required",)
        return result
    before, after, png_digest = (
        sha256(b).hexdigest() for b in (result_before, result_after, png)
    )
    if (
        before != after
        or record["result_sha256_before"] != before
        or record["result_sha256_after"] != after
    ):
        errors.append("Source result digest changed or differs from capture record")
    if record["png_sha256"] != png_digest:
        errors.append("Export digest differs from capture record")
    if record["result_set"] != result_set:
        errors.append("Selected plot result set differs")
    if (
        tensor_audit.get("raw_status") != "COMPLETE"
        or tensor_audit.get("source_result_sha256") != before
        or tensor_audit.get("result_set") != result_set
    ):
        errors.append("Tensor audit incomplete or source/result identity differs")
    if record["averaging"] != "UNAVERAGED":
        errors.append("Unaveraged plot state required")
    if tensor_audit.get("sample_surfaces") != (record["surface"],):
        errors.append("Plot surface differs from audited tensor selection")
    if not all(
        isinstance(record[f], str) and record[f].strip() for f in ("camera", "surface")
    ):
        errors.append("Camera and surface required")
    # 2024 R2 /DSCALE: AUTO/0 magnifies, 1 is true geometry scale, OFF removes
    # deformation. The window index is a command argument, not a coefficient.
    for field, expected in (
        ("dscale_command", "/DSCALE,1,OFF"),
        ("native_command", "PLESOL,S,EQV"),
    ):
        command = record[field]
        if not isinstance(command, str) or "".join(command.upper().split()) != expected:
            errors.append(f"Unsupported native plot state: {field}")
    try:
        if number(record["pressure_mpa"]) != number(pressure_mpa):
            errors.append("Selected pressure differs")
        low, high = number(record["legend_min"]), number(record["legend_max"])
        if low >= high:
            errors.append("Legend endpoints not increasing")
        if expected_legend is not None:
            expected = tuple(number(v) for v in expected_legend)
            same = (low, high) == expected
            result["shared_scale"] = {"status": "PASS" if same else "NON_PASS"}
            if not same:
                errors.append("Comparison legend differs")
        native_peak = number(record["native_raw_vm_max"])
        if native_peak < 0:
            errors.append("Native equivalent-stress maximum is negative")
        if not low <= native_peak <= high:
            errors.append("Legend excludes native equivalent-stress maximum")
        if not errors:
            delta = abs(native_peak - tensor_audit["raw_von_mises"])
            same = delta <= peak_tolerance.value
            result["native_peak_reconciliation"] = {
                "status": "PASS" if same else "NON_PASS",
                "difference": delta,
                "tolerance": peak_tolerance.value,
                "reference": peak_tolerance.reference,
            }
            if not same:
                errors.append("Native raw maximum differs from coherent tensors")
    except (EvidenceError, TypeError):
        errors.append("Invalid numeric plot metadata")
    result.update(status="INCOMPLETE" if errors else "COMPLETE", errors=tuple(errors))
    return result
