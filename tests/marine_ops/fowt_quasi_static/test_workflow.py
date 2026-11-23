# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Input execution, resonance resolution and final numerical acceptance."""
import json
from hashlib import sha256
from pathlib import Path

import numpy as np
import pytest
import yaml

ROOT = Path(__file__).resolve().parents[3]


def test_case_input_analysis_overrides_are_consumed(tmp_path):
    from digitalmodel.marine_ops.fowt_quasi_static.workflow import load_basis
    basis_path = ROOT/"docs/domains/orcaflex/wind/fowt-volturnus/basis.yml"
    (tmp_path/"basis.yml").write_bytes(basis_path.read_bytes())
    config = {"basis": "basis.yml", "analysis": {
        "additional_damping": {"value": 0.08, "unit": "fraction of critical",
                               "source": "synthetic test override"}}}
    (tmp_path/"input.yml").write_text(yaml.safe_dump(config))
    basis = load_basis(tmp_path, tmp_path/"input.yml")
    assert basis["analysis"]["additional_damping"]["value"] == 0.08
    assert basis["mooring"]["line_count"]["value"] == 3


def test_coefficients_are_interpolated_before_narrow_resonance_solution():
    from digitalmodel.marine_ops.fowt_quasi_static.workflow import interpolate_hydrodynamics
    from digitalmodel.marine_ops.fowt_quasi_static.response import response_operators
    source = np.array([0.8, 0.95, 1.05, 1.2])
    data = {"omega": source, "added": np.zeros((4, 6, 6)),
            "radiation": np.tile(np.eye(6)*0.002, (4, 1, 1)),
            "excitation": np.ones((4, 3, 6), dtype=complex)}
    dense = interpolate_hydrodynamics(data, np.linspace(0.8, 1.2, 4001))
    response = response_operators(dense["omega"], dense["added"], dense["radiation"],
                                  dense["excitation"], np.eye(6),
                                  np.diag(np.arange(1., 7.)), 0.0)
    assert abs(response[:, 0, 0]).max() == pytest.approx(500, rel=1e-4)


def test_fixed_attitude_reactions_include_weight_and_buoyancy():
    from digitalmodel.marine_ops.fowt_quasi_static.workflow import constraint_reaction
    residual = np.array([0., 0., -50., 0., 0., 0.])
    reaction = constraint_reaction(residual, 100., np.array([1., 0., -2.]),
                                   60., np.zeros(3), 2., 10.)
    np.testing.assert_allclose(reaction, [-150., 0., -1000., 0.])


def test_retained_panel_outputs_require_matching_input_provenance(tmp_path):
    from digitalmodel.marine_ops.fowt_quasi_static.workflow import load_basis, verify_retained_panels
    basis = load_basis(ROOT)
    (tmp_path/"coarse-provenance.json").write_text(json.dumps({"inputs": {"stale": True}}))
    with pytest.raises(ValueError, match="BEM input provenance"):
        verify_retained_panels(basis, tmp_path)


def test_retained_panel_output_tampering_is_detected_and_restored(tmp_path):
    from digitalmodel.marine_ops.fowt_quasi_static.workflow import bem_inputs, executed_code_digests, load_basis, verify_retained_panels
    basis = load_basis(ROOT)
    for size, name in ((2.5, "coarse"), (2.0, "fine")):
        outputs = {}
        for suffix in (".npz", "-hydrostatics.json"):
            path = tmp_path/f"{name}{suffix}"
            path.write_bytes(b"synthetic retained-output fixture")
            outputs[path.name] = sha256(path.read_bytes()).hexdigest()
        metadata = {"inputs": bem_inputs(basis, size), "outputs": outputs,
                    "executed_code_sha256": executed_code_digests()}
        (tmp_path/f"{name}-provenance.json").write_text(json.dumps(metadata))
    verify_retained_panels(basis, tmp_path)
    path = tmp_path/"fine.npz"
    original = path.read_bytes()
    before = sha256(original).hexdigest()
    path.write_bytes(original+b"tampered")
    tampered = sha256(path.read_bytes()).hexdigest()
    assert tampered != before
    with pytest.raises(ValueError, match="BEM output digest"):
        verify_retained_panels(basis, tmp_path)
    path.write_bytes(original)
    assert sha256(path.read_bytes()).hexdigest() == before
    verify_retained_panels(basis, tmp_path)


def test_engine_signature_distinguishes_syntax_from_licence_comments(tmp_path):
    from digitalmodel.marine_ops.fowt_quasi_static.workflow import source_digest
    source = tmp_path/"calculation.py"
    source.write_text("coefficient = 1\n")
    before = source_digest(source)
    source.write_text("# Licence header\ncoefficient=1\n")
    formatted = source_digest(source)
    assert before["raw"] != formatted["raw"]
    assert before["syntax"] == formatted["syntax"]
    source.write_text("coefficient = 2\n")
    assert before["syntax"] != source_digest(source)["syntax"]


def test_final_assembled_rao_symmetry_and_inertia_conclusions():
    manifest = json.loads((ROOT/"docs/reports/fowt-volturnus-mooring-analysis.manifest.json").read_text())
    results = manifest["results"]
    assert results["symmetry_ratio"] < 1e-6
    assert "inertia_case_sensitivity" in results
    assert len(results["cases"]) == 12
    for case in results["cases"]:
        final = case.get("solution", case)
        assert final["governing"]["utilisation"] <= 1.0
        assert len(final["lines"]) == (2 if case["state"] == "damaged" else 3)
def test_importing_workflow_does_not_load_optional_solvers():
    """Keep report and basis imports usable without installing solver packages."""
    import os
    import subprocess
    import sys
    from pathlib import Path
    environment = os.environ.copy()
    for name in ("GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR"):
        environment.pop(name, None)
    source = Path(__file__).resolve().parents[3]/"src"
    code = ("import sys; sys.path.insert(0, sys.argv[1]); "
            "from digitalmodel.marine_ops.fowt_quasi_static import workflow; "
            "assert {'capytaine', 'moorpy'}.isdisjoint(sys.modules)")
    subprocess.run([sys.executable, "-I", "-B", "-c", code, str(source)],
                   check=True, env=environment, capture_output=True, text=True)
