# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Synthetic radiation-zero example: no solver or geometry qualification.

Inputs: f=(1,2,3) rad/s; A(f)=diag(f,2f,3f,4f,5f,0), arbitrary SI units.
Hand calculation: every yaw row/column entry is exactly zero. Other
diagonals contain information. Matching byte bindings alone cannot establish
physical provenance, so the answer remains NOT_EVALUATED, correlation=null.
All numeric inputs are synthetic, not coefficients or acceptance limits.
"""

import hashlib
import importlib.util
import json
from pathlib import Path
import sys
from types import SimpleNamespace


def load_diagnostic():
    """Load the dependency-free module without optional solver adapters."""
    source = (
        Path(__file__).resolve().parents[2]
        / "src/digitalmodel/hydrodynamics/diffraction/physical_zero.py"
    )
    spec = importlib.util.spec_from_file_location("physical_zero_example", source)
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


def example_matrices():
    """Provide the same attribute protocol as output_schemas.AddedMassSet."""
    frequencies = [1.0, 2.0, 3.0]
    matrices = [
        SimpleNamespace(
            matrix=[
                [f * (i + 1) if i == j and i != 5 else 0.0 for j in range(6)]
                for i in range(6)
            ],
            frequency=f,
            matrix_type="added_mass",
            source="solver",
            units={"linear": "kg", "coupling": "kg.m", "angular": "kg.m^2"},
        )
        for f in frequencies
    ]
    return SimpleNamespace(
        frequencies=SimpleNamespace(
            values=frequencies, count=len(frequencies), unit="rad/s"
        ),
        matrices=matrices,
    )


def main():
    """Print the assessment of explicitly synthetic provenance claims."""
    diagnostic = load_diagnostic()
    matrices = example_matrices()
    model_input = b"synthetic model input"
    native_output = b"synthetic native output; no native solver ran"
    frame = diagnostic.BodyFrame(
        origin_m=(0.0, 0.0, 0.0),
        axes=((1.0, 0.0, 0.0), (0.0, 1.0, 0.0), (0.0, 0.0, 1.0)),
    )
    context = diagnostic.ModelContext(
        model_digest="sha256:" + hashlib.sha256(model_input).hexdigest(),
        body_id="body-a",
        frame=frame,
    )
    extraction = diagnostic.ExtractionClaim(
        context=context,
        native_output_digest="sha256:" + hashlib.sha256(native_output).hexdigest(),
        coefficient_digest=diagnostic.coefficient_digest(matrices),
        extractor_revision="synthetic-reader-v1",
    )
    result = diagnostic.assess_physical_zero(
        matrices,
        rotational_dof=6,
        model_context=context,
        extraction=extraction,
        model_input=model_input,
        native_output=native_output,
    )
    print(json.dumps(result.to_dict(), indent=2, allow_nan=False))


if __name__ == "__main__":
    main()
