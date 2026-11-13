"""Ten dependent plate-shape illustrations; no measured cards or field verdicts.

The retained CC0 workbook owns the dimensionless curves. All physical scales,
card space and timing are ASSUMED. Classifier outputs remain HYPOTHESIS.
"""
from __future__ import annotations

import hashlib
import json
import math
from pathlib import Path
import re

import numpy as np

from .benchmark.plate_scoring import _ASSUMED_MODES, _GLOSSES, prepare_plate_card
from .card_generators import surface_card_from_pump_card
from .comparison import _card_arrays, canonicalize_card_direction
from .constants import BUCKLING_DETECTION_LOAD_THRESHOLD_LBS
from .diagnostic_plate import _SOURCE_SHA256, load_diagnostic_plate
from .diagnostics import (
    MODEL_PATH, SHAPE_MODEL_PATH, FeatureExtractor, PumpDiagnostics,
    _classify_shape_with_model, _predict_from_trees,
)
from .models import CardData, DynacardAnalysisContext
from .power_consumption import PowerConsumptionCalculator
from .solver import DynacardWorkflow


class _PinnedDiagnostics(PumpDiagnostics):
    """Use unchanged workflow inference with build-local bytes, never global cache."""

    def __init__(self, model: dict):
        self._pinned_model = model

    def _load_model(self) -> dict:
        return self._pinned_model


# Published surface-card CSV precision. The workflow analyses the card at this
# precision so that a reader rerunning the published CSV reproduces the stored
# results: corner detection moves in sample steps, and rounding after the
# analysis shifted example 4's fluid load by 15 lbf.
SURFACE_POSITION_DECIMALS = 4
SURFACE_LOAD_DECIMALS = 2


def published_surface_card(card: CardData) -> CardData:
    """Round a synthesised surface card to its published CSV precision."""
    return CardData(
        position=[round(float(v), SURFACE_POSITION_DECIMALS) for v in card.position],
        load=[round(float(v), SURFACE_LOAD_DECIMALS) for v in card.load],
    )


def example_well() -> dict:
    """Return independent dictionaries of the fixed ASSUMED oilfield basis."""
    return {
        "api14": "PLATE-EXAMPLE", "spm": 6.42, "fluid_density": 63.237,
        "pump": {"diameter": 1.500, "depth": 4844.0},
        "input_params": {"strokes_per_minute": 6.42, "tubing_pressure": 100.0},
        "rod_string": [
            {"diameter": 0.875, "length": 1400.0, "weight_per_foot": 2.224,
             "modulus_of_elasticity": 30.5e6},
            {"diameter": 0.750, "length": 3350.0, "weight_per_foot": 1.634,
             "modulus_of_elasticity": 30.5e6},
        ],
    }


def fluid_load_scale_lbf() -> float:
    """Static plunger fluid load at ASSUMED zero intake pressure, in lbf."""
    well = example_well()
    area = math.pi / 4 * well["pump"]["diameter"] ** 2
    pressure = (well["fluid_density"] * well["pump"]["depth"] / 144
                + well["input_params"]["tubing_pressure"])
    return area * pressure


def _round_trip(actual: CardData, reference: CardData) -> dict:
    """Use compare_cards' direction and joint phase objective, without min-load %.

    Reference minimum is exactly zero. Calling compare_cards itself would raise
    for its unrelated minimum-load relative error. The shift search below is
    identical to its position/load range-normalised objective; no datum or load
    offset is fitted. A duplicated closing endpoint is omitted for comparison.
    """
    ax, ay = _card_arrays(actual, "actual")
    rx, ry = _card_arrays(reference, "reference")
    if len(ax) == len(rx) + 1 and ax[-1] == ax[0] and ay[-1] == ay[0]:
        ax, ay = ax[:-1], ay[:-1]
    if len(ax) != len(rx):
        raise ValueError("Round-trip alignment requires equal non-closing sample counts")
    ax, ay, _ = canonicalize_card_direction(ax, ay)
    rx, ry, _ = canonicalize_card_direction(rx, ry)
    xr, yr = float(np.ptp(rx)), float(np.ptp(ry))
    if min(xr, yr) <= 0:
        raise ValueError("Reference stroke and load range must be positive")
    scores = [float(np.mean(((np.roll(ax, s) - rx) / xr) ** 2
                            + ((np.roll(ay, s) - ry) / yr) ** 2))
              for s in range(len(ax))]
    aligned_load = np.roll(ay, int(np.argmin(scores)))
    return {
        "load_nrmse_pct": float(100 * np.sqrt(np.mean((aligned_load - ry) ** 2)) / yr),
        "stroke_difference_in": float(np.ptp(ax) - xr),
        "alignment": "compare_cards direction and joint position/load phase objective",
        "closing_endpoint": "Duplicate computed closing endpoint omitted for comparison only",
        "tolerance_pct": 3.0,
        "tolerance_basis": "Pre-task probe 0.4 to 2.4%; 0.6 percentage-point numeric headroom",
    }


def _shape_diagnosis(card: CardData, model: dict) -> dict:
    """Keep public classifier call plus unrounded full probabilities for aggregation."""
    result = _classify_shape_with_model(card, model).model_dump()
    features = FeatureExtractor.extract_shape_vector(card)
    features = FeatureExtractor.normalize(features, model["scaling"])
    mode, probabilities = _predict_from_trees(features, model)
    if mode != result["classification"]:
        raise ValueError("Shape model changed while constructing the example")
    result["probabilities"] = dict(zip(model["class_labels"], map(float, probabilities), strict=True))
    return result


def _analysis_values(ctx, result) -> dict:
    """Retain module quantities and their native units without reinterpreting P1."""
    pump = result.downhole_card
    return {
        "surface_stroke_in": float(np.ptp(ctx.surface_card.position)),
        "peak_polished_rod_load_lbf": result.peak_polished_rod_load,
        "minimum_polished_rod_load_lbf": result.minimum_polished_rod_load,
        "pump_gross_stroke_in": result.fillage.gross_stroke,
        "pump_net_stroke_in": result.fillage.net_stroke,
        "fillage_pct": result.fillage.fillage,
        "fluid_load_lbf": result.fluid_load.fluid_load,
        "upstroke_mean_load_lbf": result.fluid_load.upstroke_load,
        "downstroke_mean_load_lbf": result.fluid_load.downstroke_load,
        "pump_intake_pressure_psi": result.cpip.pump_intake_pressure,
        "pump_discharge_pressure_psi": result.cpip.pump_discharge_pressure,
        "cpip_fluid_density_lb_ft3": result.cpip.fluid_density,
        "cpip_tubing_gradient_psi_ft": result.cpip.tubing_gradient,
        "gross_displacement_bpd": result.production.gross_displacement,
        "net_displacement_bpd": result.production.net_displacement,
        "theoretical_production_bpd": result.production.theoretical_production,
        "pump_efficiency_pct": result.production.pump_efficiency,
        "runtime_hours": result.production.runtime_hours,
        "runtime_source": result.production.runtime_source,
        "pump_maximum_load_lbf": max(pump.load),
        "pump_minimum_load_lbf": min(pump.load),
        "pump_work_per_stroke_ft_lbf": PowerConsumptionCalculator(ctx)._calculate_card_area(pump),
        "polished_rod_work_per_stroke_ft_lbf": PowerConsumptionCalculator(ctx)._calculate_card_area(ctx.surface_card),
        "buckling_detected": result.buckling_detected,
        "buckling_criterion": f"Computed minimum pump load < {BUCKLING_DETECTION_LOAD_THRESHOLD_LBS} lbf; compression flag only",
        "buckling_limitation": "Full sinusoidal/helical buckling analysis is not exposed by this workflow; rod_buckling is null.",
        "rod_buckling": result.rod_buckling.model_dump() if result.rod_buckling else None,
        "solver_method": result.solver_method,
    }


def _published_precision_effect(unrounded: CardData, published: CardData) -> dict:
    """Fluid load and fillage on the unrounded card minus the published-card values.

    Reported, not limited: corner detection moves in whole samples, so these
    results can step when the input changes in its last published digit.
    """
    values = []
    for card in (unrounded, published):
        result = DynacardWorkflow(
            DynacardAnalysisContext(**example_well(), surface_card=card)
        ).run_full_analysis()
        values.append((result.fluid_load.fluid_load, result.fillage.fillage,
                       list(result.fillage.corners)))
    (load_u, fill_u, corners_u), (load_p, fill_p, corners_p) = values
    return {
        "fluid_load_difference_lbf": load_u - load_p,
        "fillage_difference_pct": fill_u - fill_p,
        "corners_changed": corners_u != corners_p,
    }


def _build_example(curve, index, manifest, models, digests) -> dict:
    source = prepare_plate_card(curve, samples=100, stroke_in=40.0,
                                load_range_lbf=fluid_load_scale_lbf())
    ctx = DynacardAnalysisContext(**example_well(), surface_card=source)
    unrounded = surface_card_from_pump_card(source, ctx)
    ctx.surface_card = published_surface_card(unrounded)
    precision = _published_precision_effect(unrounded, ctx.surface_card)
    workflow = DynacardWorkflow(ctx)
    workflow.diagnostics = _PinnedDiagnostics(models["shipped"])
    result = workflow.run_full_analysis()
    shape = _shape_diagnosis(result.downhole_card, models["shape_only"])
    shape["input_card"] = "computed_pump_card"
    source_shape = _shape_diagnosis(source, models["shape_only"])
    source_shape["input_card"] = "source_pump_card"
    return {
        "index": index, "slug": re.sub(r"[^a-z]+", "-", _GLOSSES[curve.label].lower()).strip("-"),
        "source_label": curve.label, "english_gloss": _GLOSSES[curve.label],
        "crosswalk_mode": curve.fault_mode or _ASSUMED_MODES[curve.label],
        "crosswalk_kind": "literal" if curve.fault_mode else "assumed",
        "source_doi": manifest["dataset_doi"], "dataset_url": manifest["dataset_url"],
        "file_url": manifest["file"]["url"], "sheet_name": curve.label,
        "retrieval_date": manifest["retrieval_date"], "licence": manifest["licence"],
        "model_digests": dict(digests),
        "source_pump_card": source.model_dump(exclude_none=True),
        "surface_card": ctx.surface_card.model_dump(exclude_none=True),
        "computed_pump_card": result.downhole_card.model_dump(exclude_none=True),
        "analysis": _analysis_values(ctx, result),
        "diagnosis": {
            "status": "HYPOTHESIS", "shipped": workflow._last_diagnostic_result.model_dump(),
            "shape_only": shape,
        },
        "consistency": {"source_shape_only": source_shape},
        "round_trip": _round_trip(result.downhole_card, source),
        "published_precision_effect": precision,
    }


def build_examples(workbook_path: str | Path) -> list[dict]:
    """Build ten examples in retained workbook sheet order, using no network."""
    workbook = Path(workbook_path)
    curves = [c for c in load_diagnostic_plate(workbook) if c.ordinate == "load"]
    manifest = json.loads(workbook.with_name("manifest.json").read_text(encoding="utf-8"))
    if manifest["file"]["sha256"] != _SOURCE_SHA256:
        raise ValueError("Manifest does not identify the pinned workbook")
    model_bytes = {"shipped": MODEL_PATH.read_bytes(), "shape_only": SHAPE_MODEL_PATH.read_bytes()}
    models = {key: json.loads(raw) for key, raw in model_bytes.items()}
    digests = {key: hashlib.sha256(raw).hexdigest() for key, raw in model_bytes.items()}
    return [_build_example(c, i, manifest, models, digests) for i, c in enumerate(curves, 1)]


def _basis() -> dict:
    well = example_well()
    context = DynacardAnalysisContext(**well, surface_card=CardData(position=[0, 1, 0], load=[0, 1, 0]))
    defaults = context.model_dump(exclude={"surface_card"})
    rows = {
        "well": (well, "Illustrative design values; no measured well is represented. Rod order is surface first; lengths ft, diameters in, weights lb/ft, moduli psi."),
        "resolved_context_defaults": (defaults, "All unspecified context, rod, pump and surface-unit fields retain model defaults; no survey or well test was supplied."),
        "stroke_in": (40.0, "Dimensionless source has no measured stroke."),
        "minimum_load_lbf": (0.0, "Dimensionless source has no measured load datum."),
        "load_range_lbf": (fluid_load_scale_lbf(), "Fo = (pi/4 * 1.5^2) * (63.237 * 4844 / 144 + 100); common static fluid-load scale at zero intake pressure."),
        "scale_intake_pressure_psi": (0.0, "Defines only the static load scale; computed CPIP need not be zero."),
        "card_space": ("pump (downhole)", "Source does not identify surface or pump space."),
        "sampling": ({"samples": 100, "timing": "uniform", "preparation": "closed, uniform normalised Euclidean arc length, clockwise, renormalised"}, "Arc-length samples are treated as uniformly timed; measured kinematics are unavailable."),
        "surface_card": ({"origin": "synthesised from the plate shape", "position_decimals": SURFACE_POSITION_DECIMALS, "load_decimals": SURFACE_LOAD_DECIMALS}, "Forward rod-string model, not a measured polished-rod card. Rounded to the published CSV precision before analysis, so the published card reproduces the stored results."),
        "crosswalks": (_ASSUMED_MODES, "Supply insufficiency does not establish fluid pound; sand production does not establish abrasion."),
        "uniform_scale": ("All ten shapes use the same stroke and load range", "Rod parting and other near-zero physical-load conditions are shape illustrations, not physical pump cards."),
        "cpip_defaults": ({"oil_api": 35.0, "water_sg": 1.03, "water_cut_pct": 50.0}, "CPIP derives density from this default mixture, not context fluid_density; retained as the module reports it."),
        "production": ({"runtime_hours": context.input_params.runtime, "pump_efficiency_pct": 100 * context.pump.efficiency}, "InputParameters defaults runtime to 24 h (reported source: card); no measured runtime or efficiency. Production = net displacement * runtime / 24; reported efficiency is not a multiplier."),
        "solver_defaults": ({"method": "everitt_jennings", "nodes": 200, "viscosity_cp": 10.0, "tubing_id_in": 2.441, "zero_surface_position_datum": True}, "Forward and default inverse use matching grids, damping defaults and vertical survey; inverse output smoothing remains enabled."),
    }
    return {key: {"status": "ASSUMED", "value": value, "reason": reason}
            for key, (value, reason) in rows.items()}


def _rounded(value):
    """Round finite floats to nine significant digits before any serialization."""
    if isinstance(value, float):
        if not math.isfinite(value):
            raise ValueError("Nonfinite example output")
        return float(format(value, ".9g"))
    if isinstance(value, dict):
        return {key: _rounded(item) for key, item in value.items()}
    if isinstance(value, (list, tuple)):
        return [_rounded(item) for item in value]
    return value


def results_document(records: list[dict]) -> dict:
    """Return the deterministic, JSON-compatible publication source of truth."""
    first = records[0]
    digests = first["model_digests"]
    if any(r["model_digests"] != digests for r in records):
        raise ValueError("Examples were generated with different model bytes")
    source = {key: first[key] for key in ("dataset_url", "file_url", "retrieval_date", "licence")}
    source.update(doi=first["source_doi"], sha256=_SOURCE_SHA256, version="1",
                  sheet_names=[r["sheet_name"] for r in records],
                  position_units="dimensionless", load_units="dimensionless")
    return _rounded({
        "schema_version": 1, "source": source, "basis": _basis(),
        "model_digests": digests,
        "examples": records,
    })


def diagnostics_matrix(records: list[dict]) -> dict:
    """Shape-only probabilities (%) on computed cards, not field probabilities."""
    columns = [{"mode": r["crosswalk_mode"], "crosswalk_kind": r["crosswalk_kind"]} for r in records]
    modes = [c["mode"] for c in columns]
    columns.append({"mode": "ALL_OTHER_MODES", "crosswalk_kind": "aggregate"})
    rows = []
    for record in records:
        diagnosis = record["diagnosis"]
        vector = diagnosis["shape_only"]["probabilities"]
        expected = record["crosswalk_mode"]
        shape, shipped = diagnosis["shape_only"]["classification"], diagnosis["shipped"]["classification"]
        rows.append({
            "index": record["index"], "english_gloss": record["english_gloss"],
            "expected_mode": expected, "shape_only_argmax": shape, "shipped_model_call": shipped,
            "shape_only_matches_expected": shape == expected,
            "shipped_matches_expected": shipped == expected,
            "probabilities_pct": [100 * vector.get(mode, 0.0) for mode in modes]
            + [100 * sum(p for mode, p in vector.items() if mode not in modes)],
        })
    return {"columns": columns, "rows": rows}
