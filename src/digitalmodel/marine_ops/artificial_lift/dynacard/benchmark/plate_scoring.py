"""Score the CC0 diagnostic plate as dependent shape evidence, not field data."""

from __future__ import annotations

import hashlib
import json
from pathlib import Path

import numpy as np

from ..card_generators import ALL_GENERATORS
from ..diagnostic_plate import (
    DiagnosticPlateCurve, _LOAD_CLASSES, _SOURCE_SHA256, load_diagnostic_plate,
)
from ..diagnostics import (
    MODEL_PATH, SHAPE_MODEL_PATH, _classify_shape_with_model, classify_with_model,
    read_model_bytes,
)
from ..models import CardData, DiagnosticResult
from .measurements import wilson_interval

# Literal modes remain owned by the pinned plate reader. These two mechanisms
# are ASSUMED crosswalks, not diagnoses established by the source labels.
_ASSUMED_MODES = {"供液不足": "FLUID_POUND", "出砂": "SAND_ABRASION"}
_GLOSSES = {
    "工况正常": "Normal operation", "供液不足": "Insufficient supply",
    "气体影响": "Gas interference", "抽油杆断脱": "Rod parting",
    "上碰泵": "Tagging up", "下碰泵": "Tagging down",
    "游动阀漏失": "Traveling valve leakage", "固定阀漏失": "Standing valve leakage",
    "活塞脱出工作筒": "Plunger out of barrel", "出砂": "Sand production",
}


def prepare_plate_card(
    curve: DiagnosticPlateCurve, *, samples: int = 100, stroke_in: float,
    load_range_lbf: float, minimum_load_lbf: float = 0.0,
) -> CardData:
    """Close and uniformly resample a dimensionless load loop, then ASSUME units.

    Each source axis is normalised before Euclidean arc-length interpolation.
    Repeated vertices are removed; the closing endpoint is not sampled twice.
    Clockwise traversal (negative signed area) matches the generators' loaded
    upstroke. The first minimum-position sample becomes the start of the card.
    Resampled extrema are normalised again to give exactly the ASSUMED stroke
    (in) and load range (lbf). The source supplies neither engineering units nor
    card space: treating these as pump-card shapes is also ASSUMED.
    """
    if isinstance(samples, bool) or not isinstance(samples, int) or samples < 4:
        raise ValueError("samples must be an integer of at least four")
    scale = np.asarray([stroke_in, load_range_lbf, minimum_load_lbf], dtype=float)
    if not np.all(np.isfinite(scale)) or np.any(scale[:2] <= 0):
        raise ValueError("ASSUMED stroke and load range must be finite and positive")
    if curve.ordinate != "load":
        raise ValueError("Power curves are not load cards")
    axes = [np.asarray(axis, dtype=float) for axis in (curve.position, curve.values)]
    if (any(axis.ndim != 1 for axis in axes) or len(axes[0]) != len(axes[1])
            or len(axes[0]) < 3 or not all(np.all(np.isfinite(a)) for a in axes)):
        raise ValueError("A load loop requires at least three finite paired points")
    if any(np.ptp(axis) <= 0 for axis in axes):
        raise ValueError("A load loop requires nonzero position and load spans")
    points = np.column_stack([(a - a.min()) / np.ptp(a) for a in axes])
    signed_area = np.sum(points[:, 0] * np.roll(points[:, 1], -1)
                         - np.roll(points[:, 0], -1) * points[:, 1])
    if signed_area > 0:
        points = points[::-1]
    points = np.vstack([points, points[0]])
    lengths = np.linalg.norm(np.diff(points, axis=0), axis=1)
    points = points[np.r_[True, lengths > 0]]
    distance = np.r_[0, np.cumsum(lengths[lengths > 0])]
    target = np.linspace(0, distance[-1], samples, endpoint=False)
    sampled = np.column_stack([np.interp(target, distance, a) for a in points.T])
    if np.any(np.ptp(sampled, axis=0) <= 0):
        raise ValueError("Resampling lost a coordinate span; increase samples")
    sampled = (sampled - sampled.min(axis=0)) / np.ptp(sampled, axis=0)
    sampled = np.roll(sampled, -int(np.argmin(sampled[:, 0])), axis=0)
    return CardData(position=(sampled[:, 0] * stroke_in).tolist(),
                    load=(minimum_load_lbf + sampled[:, 1] * load_range_lbf).tolist())


def _summarise(rows: list[dict], crosswalk_status: str) -> dict:
    expected = sorted({row["expected_mode"] for row in rows})
    columns = sorted(set(expected) | {row["predicted_mode"] for row in rows})
    matrix = {mode: dict.fromkeys(columns, 0) for mode in expected}
    for row in rows:
        matrix[row["expected_mode"]][row["predicted_mode"]] += 1
    correct = sum(row["expected_mode"] == row["predicted_mode"] for row in rows)
    count = len(rows)
    interval = list(wilson_interval(correct, count, confidence=0.95)) if count else None
    if interval is not None:
        # Recorded to 12 decimals so the JSON is identical across Python
        # runtimes whose floating-point results differ in the last bit.
        interval = [round(bound, 12) for bound in interval]
    if interval is not None:
        if correct == 0:
            interval[0] = 0.0  # Exact binomial boundary, not floating residue.
        if correct == count:
            interval[1] = 1.0
    return {
        "crosswalk_status": crosswalk_status, "count": count, "correct": correct,
        "wrong": count - correct, "accuracy": correct / count if count else None,
        "interval": interval,
        "confidence": 0.95, "interval_method": "Wilson score",
        "confusion_matrix": matrix,
    }


def score_plate(classify, cards) -> dict:
    """Score ``(source_curve, prepared_card)`` pairs through a supplied callable.

    The callable returns a mode string or DiagnosticResult. Eight literal
    mappings and two ASSUMED mechanisms are reported separately and together.
    Empty groups retain zero counts and null estimates. Unexpected prediction
    columns are retained. No field-accuracy or engineering acceptance is implied.
    """
    rows, seen = [], set()
    for curve, card in cards:
        if curve.ordinate != "load" or curve.label not in _LOAD_CLASSES:
            raise ValueError("Only recognised plate load curves can be scored")
        if curve.label in seen:
            raise ValueError("Repeated plate label; one exemplar per class is required")
        seen.add(curve.label)
        literal_mode = _LOAD_CLASSES[curve.label]
        predicted = classify(card)
        if isinstance(predicted, DiagnosticResult):
            predicted = predicted.classification
        if not isinstance(predicted, str) or not predicted.strip():
            raise ValueError("Predictions must be nonempty mode strings")
        rows.append({
            "source_label": curve.label, "english_gloss": _GLOSSES[curve.label],
            "expected_mode": literal_mode or _ASSUMED_MODES[curve.label],
            "crosswalk_kind": "literal" if literal_mode else "assumed",
            "crosswalk_status": "LITERAL" if literal_mode else "ASSUMED",
            "predicted_mode": predicted,
        })
    return {
        "rows": rows,
        "literal": _summarise([r for r in rows if r["crosswalk_kind"] == "literal"], "LITERAL"),
        "assumed": _summarise([r for r in rows if r["crosswalk_kind"] == "assumed"], "ASSUMED"),
        "including_assumed": _summarise(rows, "INCLUDES_ASSUMED"),
    }


def score_shipped_model_scale_sweep(workbook: str | Path, *, samples: int = 100) -> dict:
    """Score exact shipped-model bytes at four ASSUMED physical scales.

    The source is dimensionless; these scales are reporting experiments, not
    measured inputs. Counts describe shape consistency under assumed units.
    No cached model or fallback rules are consulted.
    """
    curves = [c for c in load_diagnostic_plate(workbook) if c.ordinate == "load"]
    model_bytes = MODEL_PATH.read_bytes()
    model = json.loads(model_bytes)
    scores = []
    for stroke, load_range in ((60, 4000), (80, 12000), (100, 6000), (140, 8000)):
        cards = [(c, prepare_plate_card(c, samples=samples, stroke_in=stroke,
                                       load_range_lbf=load_range)) for c in curves]
        scores.append({
            "stroke_in": stroke, "load_range_lbf": load_range,
            "minimum_load_lbf": 0.0, "scale_status": "ASSUMED",
            **score_plate(lambda card: classify_with_model(card, model), cards),
        })
    counts = [s["including_assumed"]["correct"] for s in scores]
    return {
        "model_sha256": hashlib.sha256(model_bytes).hexdigest(),
        "scales": scores, "min_correct": min(counts), "max_correct": max(counts),
        "count": len(curves), "crosswalk_status": "INCLUDES_ASSUMED",
    }


def plate_score_document(workbook: str | Path) -> dict:
    """Return a deterministic score record bound to workbook and model bytes."""
    curves = [c for c in load_diagnostic_plate(workbook) if c.ordinate == "load"]
    shape_bytes = read_model_bytes(SHAPE_MODEL_PATH)
    shape_model = json.loads(shape_bytes)
    cards = [(c, prepare_plate_card(c, stroke_in=100, load_range_lbf=6000)) for c in curves]
    shape_score = score_plate(lambda c: _classify_shape_with_model(c, shape_model), cards)
    sweep = score_shipped_model_scale_sweep(workbook)
    expected_modes = {r["expected_mode"] for r in shape_score["rows"]}
    return {
        "schema_version": 1,
        "source": {
            "doi": "10.17632/hnfwm9mjs6.1", "version": "1", "licence": "CC0 1.0",
            "sha256": _SOURCE_SHA256, "position_units": "dimensionless",
            "load_units": "dimensionless", "load_curve_count": len(curves),
        },
        "preparation": {
            "samples": 100, "arc_length": "Euclidean after per-axis normalisation",
            "closure": "Implicit closing segment; endpoint not duplicated",
            "traversal": "Clockwise, negative signed area; first minimum position",
            "scaling": "Resampled extrema renormalised to ASSUMED stroke and load range",
            "card_space": "ASSUMED pump-card shapes; source does not specify surface or pump",
        },
        "models": {
            "shape_only": {"sha256": hashlib.sha256(shape_bytes).hexdigest(),
                           "training_metadata": shape_model["training_metadata"]},
            "shipped": {"sha256": sweep["model_sha256"], "n_features": 19},
        },
        "shape_only": {"stroke_in": 100, "load_range_lbf": 6000,
                       "minimum_load_lbf": 0.0, "scale_status": "ASSUMED", **shape_score},
        "shipped_model_scale_sweep": sweep,
        "modes_without_plate_exemplar": sorted(set(ALL_GENERATORS) - expected_modes),
        "modes_without_literal_exemplar": sorted(set(ALL_GENERATORS)
                                                 - {c.fault_mode for c in curves}),
        "field_accuracy": {
            "status": "NOT_EVALUATED",
            "reason": "Independent labelled, dimensioned field cards are unavailable",
            "issues": ["https://github.com/vamseeachanta/digitalmodel/issues/1864",
                       "https://github.com/vamseeachanta/digitalmodel/issues/1872"],
        },
        "limitations": [
            "n = 10: eight literal plus two ASSUMED crosswalks; one exemplar per class.",
            "Wilson 95 % intervals are wide; plate counts are not field accuracy.",
            "The plate is NOT independent of training: test_card_generator_shapes.py constrains generators to it.",
            "Consistency with dependent public shapes is an upper indication at best.",
            "Normalised axes have no engineering units; all physical scales are ASSUMED.",
            "Pump-card interpretation is ASSUMED; the source does not specify card space.",
            "Modes without plate exemplars are unscored; synthetic CV is not field validation.",
        ],
    }


def main() -> None:
    """Print the plate score JSON; no training, downloads or model mutation."""
    import argparse
    import sys

    sys.stdout.reconfigure(encoding="utf-8")
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--workbook", type=Path, required=True)
    args = parser.parse_args()
    try:
        result = plate_score_document(args.workbook)
    except (OSError, ValueError, TypeError, KeyError) as exc:
        parser.exit(status=2, message=f"Plate input or model error: {exc}\n")
    print(json.dumps(result, indent=2, ensure_ascii=False, allow_nan=False))


if __name__ == "__main__":
    main()
