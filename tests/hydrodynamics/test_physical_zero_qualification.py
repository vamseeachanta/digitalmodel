# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Physical-zero diagnostics without importing optional solver adapters.

All numbers in these tests are synthetic inputs, not engineering limits.
For a yaw candidate, row/column 6 are set to exact zero; other diagonal
entries are f*(1,2,3,4,5), so each matrix has nonzero information. Thus a
zero pattern is observed but no geometry or lineage can be inferred.
"""

import importlib.util
from dataclasses import replace
import hashlib
import json
from pathlib import Path
import subprocess
import sys
from types import SimpleNamespace

import pytest


def load_module(filename="physical_zero.py"):
    path = (
        Path(__file__).resolve().parents[2]
        / "src/digitalmodel/hydrodynamics/diffraction"
        / filename
    )
    spec = importlib.util.spec_from_file_location(Path(filename).stem, path)
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


def matrix_set():
    """By hand: diag(f,2f,3f,4f,5f,0) has a zero yaw row and column."""
    frequencies = [1.0, 2.0, 3.0]
    matrices = [
        SimpleNamespace(
            matrix=[
                [f * (i + 1) if i == j and i != 5 else 0.0 for j in range(6)]
                for i in range(6)
            ],
            frequency=f,
            matrix_type="added_mass",
            units={"linear": "kg", "coupling": "kg.m", "angular": "kg.m^2"},
            source="solver",
        )
        for f in frequencies
    ]
    return SimpleNamespace(
        frequencies=SimpleNamespace(
            values=frequencies, count=len(frequencies), unit="rad/s"
        ),
        matrices=matrices,
    )


def test_zero_pattern_cannot_establish_physical_qualification():
    """All yaw terms are 0 by construction; physical acceptance stays unknown."""
    result = load_module().assess_physical_zero(matrix_set(), rotational_dof=6)
    assert result.status == "NOT_EVALUATED"
    assert result.zero_mode_observed is True
    assert result.correlation is None
    assert "MODEL_CONTEXT_MISSING" in result.reasons
    assert "EXTRACTION_LINEAGE_UNESTABLISHED" in result.reasons
    assert "PHYSICAL_MODE_UNESTABLISHED" in result.reasons


def bound_inputs(module, coefficients):
    """Digest inputs are literal synthetic bytes, unrelated to a native run."""
    model_bytes = b"synthetic model input"
    output_bytes = b"synthetic native output"
    frame = module.BodyFrame(
        origin_m=(0.0, 0.0, 0.0),
        axes=((1.0, 0.0, 0.0), (0.0, 1.0, 0.0), (0.0, 0.0, 1.0)),
    )
    context = module.ModelContext(
        model_digest="sha256:" + hashlib.sha256(model_bytes).hexdigest(),
        body_id="body-a",
        frame=frame,
    )
    extraction = module.ExtractionClaim(
        context=context,
        native_output_digest="sha256:" + hashlib.sha256(output_bytes).hexdigest(),
        coefficient_digest=module.coefficient_digest(coefficients),
        extractor_revision="synthetic-reader-v1",
    )
    return dict(
        model_context=context,
        extraction=extraction,
        model_input=model_bytes,
        native_output=output_bytes,
    )


def test_matching_independent_bindings_never_authenticate_lineage():
    """Equal SHA-256 values prove byte identity, not model-to-output causality."""
    module = load_module()
    coefficients = matrix_set()
    result = module.assess_physical_zero(
        coefficients,
        rotational_dof=6,
        **bound_inputs(module, coefficients),
    )
    assert result.status == "NOT_EVALUATED"
    assert result.zero_mode_observed
    assert result.correlation is None
    assert set(result.reasons) == {
        "MODEL_GEOMETRY_UNVERIFIED",
        "EXTRACTION_LINEAGE_UNESTABLISHED",
        "PHYSICAL_MODE_UNESTABLISHED",
    }


@pytest.mark.parametrize(
    "binding, reason",
    [
        ("model", "MODEL_BINDING_MISMATCH"),
        ("body", "BODY_BINDING_MISMATCH"),
        ("origin", "ORIGIN_BINDING_MISMATCH"),
        ("axes", "AXES_BINDING_MISMATCH"),
        ("output", "NATIVE_OUTPUT_BINDING_MISMATCH"),
        ("coefficients", "COEFFICIENT_BINDING_MISMATCH"),
    ],
)
def test_reassigned_extraction_is_reported(binding, reason):
    """One independently supplied binding is changed; equality must fail."""
    module = load_module()
    coefficients = matrix_set()
    inputs = bound_inputs(module, coefficients)
    claim = inputs["extraction"]
    context = claim.context
    other_digest = "sha256:" + hashlib.sha256(b"different bytes").hexdigest()
    if binding == "model":
        claim = replace(claim, context=replace(context, model_digest=other_digest))
    elif binding == "body":
        claim = replace(claim, context=replace(context, body_id="body-b"))
    elif binding == "origin":
        frame = replace(context.frame, origin_m=(1.0, 0.0, 0.0))
        claim = replace(claim, context=replace(context, frame=frame))
    elif binding == "axes":
        frame = replace(
            context.frame, axes=((0.0, 1.0, 0.0), (-1.0, 0.0, 0.0), (0.0, 0.0, 1.0))
        )
        claim = replace(claim, context=replace(context, frame=frame))
    elif binding == "output":
        claim = replace(claim, native_output_digest=other_digest)
    else:
        claim = replace(claim, coefficient_digest=other_digest)
    inputs["extraction"] = claim
    result = module.assess_physical_zero(coefficients, rotational_dof=6, **inputs)
    assert result.status == "NOT_EVALUATED"
    assert reason in result.reasons


@pytest.mark.parametrize(
    "mutation, reason",
    [
        ("placeholder", "UNTRUSTED_SOURCE"),
        ("unknown", "UNTRUSTED_SOURCE"),
        ("all_zero", "ALL_ZERO_COEFFICIENTS"),
        ("nonfinite", "INVALID_COEFFICIENT_DATA"),
        ("wrong_count", "INVALID_COEFFICIENT_DATA"),
        ("wrong_frequency", "INVALID_COEFFICIENT_DATA"),
        ("reversed_axis", "INVALID_COEFFICIENT_DATA"),
        ("missing_units", "INVALID_COEFFICIENT_DATA"),
        ("coupled_row", "ROTATIONAL_ZERO_NOT_OBSERVED"),
        ("coupled_column", "ROTATIONAL_ZERO_NOT_OBSERVED"),
    ],
)
def test_incomplete_or_inconsistent_arrays_cannot_qualify(mutation, reason):
    """Each counterexample contradicts one explicitly observed input property."""
    module = load_module()
    coefficients = matrix_set()
    if mutation in {"placeholder", "unknown"}:
        coefficients.matrices[0].source = mutation
    elif mutation == "all_zero":
        for matrix in coefficients.matrices:
            matrix.matrix = [[0.0] * 6 for _ in range(6)]
    elif mutation == "nonfinite":
        coefficients.matrices[0].matrix[0][0] = float("nan")
    elif mutation == "wrong_count":
        coefficients.frequencies.count += 1
    elif mutation == "wrong_frequency":
        coefficients.matrices[0].frequency += 1.0
    elif mutation == "reversed_axis":
        coefficients.frequencies.values.reverse()
    elif mutation == "missing_units":
        coefficients.matrices[0].units.pop("angular")
    elif mutation == "coupled_row":
        coefficients.matrices[0].matrix[5][0] = 1.0
    else:
        coefficients.matrices[0].matrix[0][5] = 1.0
    result = module.assess_physical_zero(coefficients, rotational_dof=6)
    assert result.status == "NOT_EVALUATED"
    assert result.correlation is None
    assert reason in result.reasons


def test_stale_model_bytes_and_missing_extractor_are_visible():
    """Literal replacement bytes differ, and a blank revision supplies no version."""
    module = load_module()
    coefficients = matrix_set()
    inputs = bound_inputs(module, coefficients)
    inputs["model_input"] = b"changed synthetic model input"
    inputs["extraction"] = replace(inputs["extraction"], extractor_revision="")
    result = module.assess_physical_zero(coefficients, rotational_dof=6, **inputs)
    assert "MODEL_DIGEST_MISMATCH" in result.reasons
    assert "EXTRACTOR_REVISION_MISSING" in result.reasons


@pytest.mark.parametrize(
    "frame_change",
    [
        {"origin_m": (0.0, 0.0)},
        {"origin_m": (float("nan"), 0.0, 0.0)},
        {"axes": ((1.0, 0.0),) * 3},
    ],
)
def test_malformed_frame_cannot_enter_binding_contract(frame_change):
    """A frame needs a finite three-dimensional origin and three vector axes."""
    module = load_module()
    context = bound_inputs(module, matrix_set())["model_context"]
    with pytest.raises(ValueError):
        replace(context.frame, **frame_change)


def test_coefficient_digest_detects_changed_units_kind_and_axis():
    """Different units, frequencies or coefficient kind imply different payloads."""
    module = load_module()
    coefficients = matrix_set()
    original = module.coefficient_digest(coefficients)
    for matrix in coefficients.matrices:
        matrix.matrix_type = "damping"
        matrix.units = {
            "linear": "N.s/m",
            "coupling": "N.s/rad",
            "angular": "N.m.s/rad",
        }
    assert module.coefficient_digest(coefficients) != original
    original = module.coefficient_digest(coefficients)
    coefficients.frequencies.values = [2.0, 3.0, 4.0]
    for matrix, frequency in zip(
        coefficients.matrices, coefficients.frequencies.values
    ):
        matrix.frequency = frequency
    assert module.coefficient_digest(coefficients) != original


@pytest.mark.parametrize("rotational_dof", [0, 1, 3, 7, True, 6.0, "yaw"])
def test_only_explicit_rotational_dof_indices_are_supported(rotational_dof):
    """DOF indices come from output_schemas.DOF, not a guessed frame mapping."""
    with pytest.raises(ValueError):
        load_module().assess_physical_zero(matrix_set(), rotational_dof=rotational_dof)


def test_serialized_result_preserves_null_and_missing_evidence():
    """JSON null represents undefined correlation; no finite score is invented."""
    result = load_module().assess_physical_zero(matrix_set(), rotational_dof=6)
    restored = json.loads(json.dumps(result.to_dict(), allow_nan=False))
    assert restored["correlation"] is None
    assert restored["status"] == "NOT_EVALUATED"
    assert "MODEL_CONTEXT_MISSING" in restored["reasons"]


def test_mutated_extracted_array_is_not_covered_by_the_old_binding():
    """Changing A11 from 1 to 2 changes data while leaving the yaw zero intact."""
    module = load_module()
    coefficients = matrix_set()
    inputs = bound_inputs(module, coefficients)
    coefficients.matrices[0].matrix[0][0] = 2.0
    result = module.assess_physical_zero(coefficients, rotational_dof=6, **inputs)
    assert result.zero_mode_observed
    assert "COEFFICIENT_BINDING_MISMATCH" in result.reasons
    assert result.status == "NOT_EVALUATED"


def test_worked_example_prints_the_fail_closed_answer():
    """The documented synthetic yaw example cannot authenticate a native run."""
    root = Path(__file__).resolve().parents[2]
    command = [
        sys.executable,
        str(root / "examples/hydrodynamics/physical_zero_diagnostic.py"),
    ]
    completed = subprocess.run(command, check=True, capture_output=True, text=True)
    result = json.loads(completed.stdout)
    assert result["status"] == "NOT_EVALUATED"
    assert result["zero_mode_observed"] is True
    assert result["correlation"] is None
    assert set(result["reasons"]) == {
        "MODEL_GEOMETRY_UNVERIFIED",
        "EXTRACTION_LINEAGE_UNESTABLISHED",
        "PHYSICAL_MODE_UNESTABLISHED",
    }


def test_existing_matrix_schema_objects_are_supported_without_solver_imports():
    """For the actual schema, diag(f,2f,3f,4f,5f,0) still has a zero yaw mode."""
    import numpy as np

    schemas = load_module("output_schemas.py")
    diagnostic = load_module()
    synthetic = matrix_set()
    frequencies = schemas.FrequencyData(
        values=np.array(synthetic.frequencies.values),
        periods=None,
        count=0,
        min_freq=0.0,
        max_freq=0.0,
    )
    # The zero metadata above is recomputed by FrequencyData.__post_init__.
    matrices = [
        schemas.HydrodynamicMatrix(
            matrix=np.array(item.matrix),
            frequency=item.frequency,
            matrix_type=item.matrix_type,
            units=item.units,
            source=item.source,
        )
        for item in synthetic.matrices
    ]
    existing = schemas.AddedMassSet(
        vessel_name="synthetic",
        analysis_tool="synthetic",
        water_depth=1.0,
        matrices=matrices,
        frequencies=frequencies,
        created_date="",
    )
    result = diagnostic.assess_physical_zero(
        existing, rotational_dof=schemas.DOF.YAW.value
    )
    assert result.zero_mode_observed
    assert result.status == "NOT_EVALUATED"
    assert result.correlation is None
    assert "OrcFxAPI" not in sys.modules
