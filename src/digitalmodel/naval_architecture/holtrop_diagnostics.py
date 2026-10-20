# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Component diagnostics and explicit qualification gaps for legacy fixtures.

The resistance engine is called without changing its equations. Approximate
fixture numbers have no traced worked-example source and cannot establish
accuracy, even when the computed and reference numbers coincide.
"""

from digitalmodel.naval_architecture import holtrop_coefficients as hc
from digitalmodel.naval_architecture import holtrop_mennen as hm

UNVERIFIED = "unverified_approximate_fixture_values"
_WAVE_INPUTS = (
    "lwl",
    "beam",
    "draft",
    "cb",
    "cm",
    "cwp",
    "cp",
    "lcb_pct",
    "cstern",
    "speed_ms",
    "abt",
    "tf",
    "hb",
    "at",
)
_FACTOR_INPUTS = ("lwl", "beam", "draft", "cp", "lcb_pct", "cstern")
_SUPPORTED_OUTPUTS = {
    "ct_approx",
    "ct_tolerance_pct",
    "froude_number",
    "k1_plus_1_min",
    "k1_plus_1_max",
}


def diagnose(case: dict, speed: float | None = None) -> dict:
    """Return dimensional components and their common qS normalization.

    qS = rho*S*V**2/2 and PE = RT*V: EN400, Summer 2020, Chapter 7
    (also used in the existing resistance and friction-scaling modules).
    Values are diagnostic predictions, not qualified reference values.
    """
    inputs = dict(case["inputs"])
    if speed is not None:
        inputs["speed_ms"] = speed
    area = hm.wetted_surface_holtrop(
        **{
            key: inputs[key]
            for key in ("lwl", "beam", "draft", "cb", "cm", "cwp", "abt")
        }
    )
    factor = hm.form_factor_k1(**{key: inputs[key] for key in _FACTOR_INPUTS})
    length, velocity = inputs["lwl"], inputs["speed_ms"]
    rf = hm.frictional_resistance(length, velocity, area)
    rw = hm.wave_resistance(**{key: inputs[key] for key in _WAVE_INPUTS})
    rapp = hm.appendage_resistance(
        velocity,
        inputs.get("appendage_areas", []),
        inputs.get("appendage_factors", []),
        length,
    )
    rb = hm.bulbous_bow_resistance(velocity, inputs["abt"], inputs["tf"], inputs["hb"])
    rtr = hm.transom_resistance(
        velocity, inputs["at"], inputs["beam"], inputs["cwp"], length
    )
    ca = hm.correlation_allowance(length, inputs["cb"], inputs["draft"])
    denominator = hc.RHO_SW * area * velocity**2 / 2
    ra = denominator * ca
    rt = hm.total_resistance(**inputs)
    row = {
        "id": case["id"],
        "speed_ms": velocity,
        "lwl_m": length,
        "froude_number": velocity / (hc.G * length) ** 0.5,
        "wetted_surface_m2": area,
        "form_factor": factor,
        "rf_n": rf,
        "rw_n": rw,
        "rapp_n": rapp,
        "rb_n": rb,
        "rtr_n": rtr,
        "ra_n": ra,
        "rt_n": rt,
        "component_sum_n": rf * factor + rapp + rw + rb + rtr + ra,
        "effective_power_w": hm.effective_power(rt, velocity),
        "viscous_ct": rf * factor / denominator,
        "wave_ct": rw / denominator,
        "ca": ca,
        "floor_ct": rf * factor / denominator + ca,
        "ct": rt / denominator,
    }
    if speed is None and "ct_approx" in case["outputs"]:
        reference = case["outputs"]["ct_approx"]
        row["ct_approx"] = reference
        # Percent conversion is a unit definition, not an acceptance threshold.
        row["error_pct"] = 100 * (row["ct"] / reference - 1)
    return row


def assess_fixture(fixture: dict) -> list[dict]:
    """Account for each enabled row and expected key without asserting accuracy.

    This deliberately supports only the unverified legacy reference state.
    Qualifying a reference requires a separate reviewed change that identifies
    the formulation, worked example, units, scale and rounding tolerance.
    Unknown output keys and ambiguous row selection are errors, not omissions.
    """
    qualification = fixture.get("qualification", {})
    if qualification.get("reference_status") != UNVERIFIED:
        raise ValueError("fixture must declare unverified reference status")
    reason = qualification.get("reason")
    sources = qualification.get("required_sources")
    if not reason or not sources:
        raise ValueError("qualification reason and required sources must be nonempty")
    records = []
    seen = set()
    for case in fixture["test_cases"]:
        case_id = case["id"]
        if case_id in seen:
            raise ValueError(f"duplicate fixture id: {case_id}")
        seen.add(case_id)
        if type(case.get("use_as_test")) is not bool:
            raise ValueError(f"{case_id}: use_as_test must be boolean")
        if not case["use_as_test"]:
            continue
        outputs = case["outputs"]
        if not outputs or set(outputs) - _SUPPORTED_OUTPUTS:
            raise ValueError(f"{case_id}: empty or unsupported outputs")
        if "ct_tolerance_pct" in outputs and "ct_approx" not in outputs:
            raise ValueError(f"{case_id}: ct_tolerance_pct requires ct_approx")
        computed = {}
        if set(outputs) & {"ct_approx", "froude_number"}:
            row = diagnose(case)
            computed.update(ct_approx=row["ct"], froude_number=row["froude_number"])
        if set(outputs) & {"k1_plus_1_min", "k1_plus_1_max"}:
            factor = hm.form_factor_k1(
                **{key: case["inputs"][key] for key in _FACTOR_INPUTS}
            )
            computed.update(k1_plus_1_min=factor, k1_plus_1_max=factor)
        records.append(
            {
                "id": case_id,
                "checks": {
                    key: {
                        "status": "NOT_EVALUATED",
                        "reference": value,
                        "computed": computed.get(key),
                        "reason": reason,
                        "required_sources": list(sources),
                    }
                    for key, value in outputs.items()
                },
            }
        )
    return records
