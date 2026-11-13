"""Unmodified Python workflow oracle and measured parity evidence helpers."""

import json
from pathlib import Path

import numpy as np

from digitalmodel.marine_ops.artificial_lift.dynacard.data_loader import load_from_json_file
from digitalmodel.marine_ops.artificial_lift.dynacard.diagnostics import (
    MODEL_PATH, SHAPE_MODEL_PATH, _predict_from_trees, classify_shape,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.feature_extraction import FeatureExtractor
from digitalmodel.marine_ops.artificial_lift.dynacard.models import DynacardAnalysisContext
from digitalmodel.marine_ops.artificial_lift.dynacard.power_consumption import calculate_card_area
from digitalmodel.marine_ops.artificial_lift.dynacard.solver import DynacardWorkflow

ROOT = Path(__file__).resolve().parents[3]
FIXTURES = ROOT / "tests/marine_ops/artificial_lift/dynacard/testdata"
SCALAR_KEYS = ("surface_stroke", "peak_polished_rod_load", "minimum_polished_rod_load",
               "pump_card_max_load", "pump_card_min_load", "polished_rod_work_per_stroke",
               "pump_work_per_stroke",
               "pump_fillage", "inferred_production")
NESTED_SCALARS = {
    "fluid_load": ("fluid_load", "upstroke_load", "downstroke_load"),
    "cpip": ("pump_intake_pressure", "pump_discharge_pressure", "fluid_density", "tubing_gradient"),
    "fillage": ("gross_stroke", "net_stroke", "fillage"),
    "production": ("gross_displacement", "net_displacement", "theoretical_production",
                   "pump_efficiency", "runtime_hours"),
}


def full_diagnosis(card, result, path, shape=False):
    model = json.loads(path.read_text(encoding="utf-8"))
    extract = FeatureExtractor.extract_shape_vector if shape else FeatureExtractor.extract_classifier_vector
    _, probabilities = _predict_from_trees(FeatureExtractor.normalize(extract(card), model["scaling"]), model)
    answer = result.model_dump(include={"classification", "confidence", "differential"})
    answer["probabilities"] = dict(zip(model["class_labels"], probabilities.tolist(), strict=True))
    return answer


def python_analysis(request):
    context = DynacardAnalysisContext(**request["well"], surface_card=request["card"])
    workflow = DynacardWorkflow(context)
    result = workflow.run_full_analysis()
    answer = result.model_dump(exclude={"ctx", "diagnostic_message"}, exclude_none=True)
    # None means no survey, whereas an explicit vertical survey reports 0.
    answer["max_inclination_deg"] = result.max_inclination_deg
    card = result.downhole_card
    answer.update(surface_stroke=float(np.ptp(context.surface_card.position)),
                  pump_card_max_load=max(card.load), pump_card_min_load=min(card.load),
                  polished_rod_work_per_stroke=calculate_card_area(context.surface_card),
                  pump_work_per_stroke=calculate_card_area(card),
                  corners=result.fillage.corners)
    answer["diagnosis"] = full_diagnosis(card, workflow._last_diagnostic_result, MODEL_PATH)
    answer["shape_diagnosis"] = full_diagnosis(card, classify_shape(card), SHAPE_MODEL_PATH, True)
    return answer


def field_requests():
    cases = []
    for path in sorted(FIXTURES.glob("field_well_*.json")):
        context = load_from_json_file(path)
        well = context.model_dump(exclude={"surface_card"})
        rejection = "deviation|survey" if context.survey and any(context.survey.inclination) else None
        cases.append({"name": path.name, "rejection": rejection,
                      "request": {"well": well, "card": context.surface_card.model_dump()}})
    return cases


def scalar_values(result):
    values = {key: result[key] for key in SCALAR_KEYS}
    for parent, children in NESTED_SCALARS.items():
        values.update({f"{parent}.{key}": result[parent][key] for key in children})
    return values


def measure(example, actual, expected):
    errors = {axis: float(np.max(np.abs(np.asarray(actual["downhole_card"][axis])
                                       - expected["downhole_card"][axis])))
              for axis in ("position", "load")}
    actual_scalars, expected_scalars = scalar_values(actual), scalar_values(expected)
    scalar_error = max(abs(actual_scalars[key] - value) / max(abs(value), 1)
                       for key, value in expected_scalars.items())
    probability_error = max(abs(actual[k]["probabilities"][label] - probability)
                            for k in ("diagnosis", "shape_diagnosis")
                            for label, probability in expected[k]["probabilities"].items())
    return {"example": example, "max_pump_load_difference_lbf": errors["load"],
            "max_pump_position_difference_in": errors["position"],
            "max_scalar_relative_difference": scalar_error,
            "labels_equal": all(actual[k]["classification"] == expected[k]["classification"]
                                for k in ("diagnosis", "shape_diagnosis")),
            "max_probability_difference": probability_error,
            "polished_rod_work_difference_ft_lbf": abs(actual["polished_rod_work_per_stroke"] - expected["polished_rod_work_per_stroke"]),
            "pump_work_difference_ft_lbf": abs(actual["pump_work_per_stroke"] - expected["pump_work_per_stroke"]),
            "pump_load_range_lbf": float(np.ptp(expected["downhole_card"]["load"])),
            "pump_position_range_in": float(np.ptp(expected["downhole_card"]["position"]))}
