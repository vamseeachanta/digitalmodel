"""Parent-side validation independent of the native child's success flags."""
import math
import re

HISTORIES = {"tension_end_a", "tension_end_b", "buoy_x", "buoy_z",
             "buoy_rotation_2", "sea_surface_z"}


def numeric(values, count=None):
    if not isinstance(values, list) or not values or (count is not None and len(values) != count):
        raise ValueError("missing or incomplete numeric array")
    if any(type(v) not in (int, float) or not math.isfinite(v) for v in values):
        raise ValueError("nonfinite or nonnumeric result")


def validate_results(results):
    try:
        times = results["times"]
        numeric(times, 1001)
        if any(abs(value - i / 10) > 1e-9 for i, value in enumerate(times)):
            raise ValueError("main-stage time grid differs")
        if set(results["histories"]) != HISTORIES:
            raise ValueError("required histories differ")
        for values in results["histories"].values():
            numeric(values, 1001)
        static = results["static"]
        numeric(static["line_arclength"])
        if set(static["line_position"]) != set("XYZ"):
            raise ValueError("required static axes differ")
        for values in static["line_position"].values():
            numeric(values, len(static["line_arclength"]))
        for key, labels in (("buoy_position", "XYZ"), ("end_tensions", "AB")):
            if set(static[key]) != set(labels):
                raise ValueError("required static scalars differ")
            numeric(list(static[key].values()), len(labels))
        if not results["units"] or not results["frames"]:
            raise ValueError("units or frames missing")
    except (KeyError, TypeError, AttributeError) as error:
        raise ValueError("incomplete result proof") from error


def validate_report(report, phase, manifest_sha256, baseline=None):
    if not isinstance(report, dict) or report.get("phase") != phase or report.get("stage") != "complete":
        raise ValueError("phase completion proof missing")
    for key in ("ok", "settings_verified", "input_readback_verified", "simulation_complete"):
        if report.get(key) is not True:
            raise ValueError("required phase verification missing")
    for key in ("thread_count_requested", "thread_count_observed"):
        if type(report.get(key)) is not int or report[key] != 1:
            raise ValueError("one-thread proof missing")
    if report.get("manifest_sha256") != manifest_sha256:
        raise ValueError("phase manifest binding differs")
    for key in ("loaded_data_sha256", "simulation_sha256"):
        if not isinstance(report.get(key), str) or not re.fullmatch("[0-9a-f]{64}", report[key]):
            raise ValueError("required artifact hash missing")
    if not isinstance(report.get("version"), str) or not report["version"]:
        raise ValueError("native version missing")
    if not isinstance(report.get("warnings"), list):
        raise ValueError("warning evidence missing")
    validate_results(report.get("results"))
    if phase == "readback":
        if not isinstance(baseline, dict) or report.get("fidelity_verified") is not True:
            raise ValueError("readback baseline or verification missing")
        validate_report(baseline, "solve", manifest_sha256)
        for key in ("version", "results", "simulation_sha256"):
            if report[key] != baseline[key]:
                raise ValueError("independent readback differs")
