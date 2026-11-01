# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Report controls from independent harmonic force balance, without a BEM run.

Basis: m*qdd + b*qd + k*q = F, exp(-i*omega*t), with the synthetic
inputs specified in the worked calculation. Fractions, square roots and
arctangents below are hand-derived, never obtained from another solver.
The absolute tolerance permits double-precision arithmetic only.
"""

import hashlib
import importlib.util
import json
import math
import os
from pathlib import Path
import re
import subprocess
import sys

import pytest


ROOT = Path(__file__).resolve().parents[2]
BUILDER = ROOT / "scripts/reports/build_capytaine_module_report.py"
REPORT = ROOT / "docs/reports/modules/capytaine"
DELIVERABLES = {"index.html", "changes.json", "manifest.json", "README.md"}


def assert_standard_header_only(before, after, license_text):
    """Prove required hook normalization changes only the standard header."""
    original = license_text.rstrip(b"\n") + b"\n"
    standard = b"\n".join(b"# " + line for line in license_text.splitlines()) + b"\n"
    assert before.startswith(original)
    assert after.startswith(standard)
    assert before[len(original):].lstrip(b"\n") == after[len(standard):].lstrip(b"\n")


@pytest.fixture
def builder():
    assert BUILDER.is_file(), "The standalone module report builder is missing"
    spec = importlib.util.spec_from_file_location("capytaine_module_report", BUILDER)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def _payload(path):
    text = path.read_text(encoding="utf-8")
    match = re.search(r'<script id="calculation" type="application/json">(.*?)</script>', text, re.S)
    assert match is not None
    return json.loads(match.group(1))


def test_worked_rao_matches_hand_complex_values(builder):
    data = builder.calculate()
    checks = {case["id"]: case for case in data["checks"]}
    # Z=2-i and Z=2-2i; multiply each reciprocal by F=+/-2 N.
    positive = [(4 + 2j) / 5, (1 + 1j) / 2]
    magnitudes = [2 / math.sqrt(5), 1 / math.sqrt(2)]
    phases = [math.degrees(math.atan(1 / 2)), math.degrees(math.atan(1))]
    for key, q, amplitude, phase in zip(
        ("single_radiation", "single_extra_damping"), positive, magnitudes, phases
    ):
        case = checks[key]
        assert case["status"] == "PASS"
        assert case["coordinates"] == {
            "axis_order": ["omega", "heading", "dof"],
            "omegas_rad_s": [1.0], "headings_rad": [0.0, math.pi],
            "dof_names": ["Heave"],
        }
        assert len(case["responses"]) == 2
        for row, sign, angle in zip(case["responses"], (1, -1), (phase, phase - 180)):
            assert complex(row["real_m"], row["imag_m"]) == pytest.approx(sign * q, abs=1e-12)
            assert row["amplitude_m"] == pytest.approx(amplitude, abs=1e-12)
            assert row["phase_deg"] == pytest.approx(angle, abs=1e-12)
    coupled = checks["coupled_existing_hand"]
    assert len(coupled["responses"]) == 2
    # Existing coupled control: Z=[[1-i,1],[1,1-i]], F=[1,i].
    for row, q in zip(coupled["responses"], ((3 + 4j) / 5, (-2 - 1j) / 5)):
        assert complex(row["real_m"], row["imag_m"]) == pytest.approx(q, abs=1e-12)
    assert data["units"]["amplitude"] == "m"
    assert data["units"]["phase"] == "deg"
    assert data["units"]["heading"] == "rad"
    assert data["units"]["omega"] == "rad/s"


def test_report_distinguishes_manual_and_native_evidence(builder, tmp_path):
    builder.build_report(scratch=tmp_path / "scratch", output_dir=tmp_path / "report")
    page = tmp_path / "report/index.html"
    data = _payload(page)
    assert data["evidence"]["manual_rao"]["status"] == "PASS"
    for name in ("native_bem", "mesh_conversion", "mesh_convergence",
                 "radiation_diffraction_accuracy", "delegated_rao", "native_netcdf"):
        assert data["evidence"][name]["status"] == "NOT_EVALUATED"
        assert data["evidence"][name]["source_needed"]
    text = page.read_text(encoding="utf-8")
    assert len(re.findall(r'<h2 id="s\d+">', text)) == 10
    assert re.search(r'</section>\s*<section id="revision-history">\s*<h3>Revision history</h3>', text)
    assert text.index("Revision history") < text.index('id="s2"')
    assert 'id="changes-panel"' in text
    assert "synthetic coefficients are supplied" in text.lower()
    assert "neither mesh convergence nor physical accuracy" in text
    assert "m/m" in text  # Explicit explanation of the absent wave normalization.
    assert "single-degree" in text and "coupled" in text
    assert "export_netcdf" in text and "compute_rao_manual" in text
    assert "Amplitude (m)" in text and "Phase (deg)" in text
    changes = json.loads((page.parent / "changes.json").read_text())
    assert changes == data["revision_history"]


def test_rebuild_is_deterministic(builder, tmp_path):
    outputs = [tmp_path / "first", tmp_path / "second"]
    for directory in outputs:
        builder.build_report(scratch=directory / "scratch", output_dir=directory / "report")
    for name in sorted(DELIVERABLES):
        first = (outputs[0] / "report" / name).read_bytes()
        assert first == (outputs[1] / "report" / name).read_bytes()
        assert first == (REPORT / name).read_bytes()
    assert {p.name for p in (outputs[0] / "report").iterdir()} == DELIVERABLES


def test_cli_manifest_matches_written_files(tmp_path):
    output = tmp_path / "report"
    env = os.environ.copy()
    # Child commands must not inherit repository bindings from a caller.
    for name in ("GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR"):
        env.pop(name, None)
    result = subprocess.run(
        [sys.executable, str(BUILDER), "--scratch", str(tmp_path / "scratch"),
         "--output-dir", str(output)], cwd=ROOT, env=env,
        capture_output=True, text=True, check=False,
    )
    assert result.returncode == 0, result.stderr
    manifest = json.loads((output / "manifest.json").read_text())
    assert set(manifest["artifacts"]) == DELIVERABLES - {"manifest.json"}
    for name, digest in manifest["artifacts"].items():
        assert hashlib.sha256((output / name).read_bytes()).hexdigest() == digest
    required_sources = {
        BUILDER.relative_to(ROOT).as_posix(),
        "src/digitalmodel/hydrodynamics/capytaine/models.py",
        "src/digitalmodel/hydrodynamics/capytaine/rao.py",
        "src/digitalmodel/hydrodynamics/capytaine/solver.py",
        "src/digitalmodel/hydrodynamics/capytaine/mesh_adapter.py",
        "src/digitalmodel/hydrodynamics/capytaine/results.py",
        "tests/hydrodynamics/capytaine/test_module_report_hand_values.py",
        "tests/hydrodynamics/capytaine/test_rao_manual_sign.py",
        "tests/reporting/test_capytaine_module_report.py",
    }
    assert required_sources <= set(manifest["sources"])
    for name, digest in manifest["sources"].items():
        assert not Path(name).is_absolute()
        assert hashlib.sha256((ROOT / name).read_bytes()).hexdigest() == digest
    assert "self" in manifest["digest_convention"]


@pytest.mark.parametrize("alias", [False, True])
def test_scratch_cannot_resolve_inside_checkout(builder, tmp_path, alias):
    scratch = ROOT / "uncreated-report-scratch"
    if alias:
        link = tmp_path / "checkout-alias"
        link.symlink_to(ROOT, target_is_directory=True)
        scratch = link / "uncreated-report-scratch"
    assert not scratch.exists()
    with pytest.raises(ValueError, match="outside the checkout"):
        builder.build_report(scratch=scratch, output_dir=tmp_path / "report")
    assert not scratch.exists()
    assert not (tmp_path / "report").exists()


@pytest.mark.parametrize("overlap", ["same", "scratch_parent", "output_parent"])
def test_scratch_and_output_are_separate(builder, tmp_path, overlap):
    scratch, output = tmp_path / "workspace", tmp_path / "workspace"
    if overlap == "scratch_parent":
        output = scratch / "report"
    elif overlap == "output_parent":
        scratch = output / "scratch"
    with pytest.raises(ValueError, match="separate"):
        builder.build_report(scratch=scratch, output_dir=output)


def test_output_inside_checkout_is_limited_to_report(builder, tmp_path):
    with pytest.raises(ValueError, match="report directory"):
        builder.build_report(scratch=tmp_path / "scratch", output_dir=ROOT / "src")


def test_visible_results_follow_public_calculation(builder, monkeypatch):
    from digitalmodel.hydrodynamics.capytaine.rao import compute_rao_manual

    def changed_response(*args, **kwargs):
        result = compute_rao_manual(*args, **kwargs)
        # Independent sentinel, deliberately inconsistent with hand controls.
        result.rao_amplitude[:] = 7.25
        return result

    monkeypatch.setattr("digitalmodel.hydrodynamics.capytaine.rao.compute_rao_manual", changed_response)
    data = builder.calculate()
    text = builder.render(data)
    assert all(row["amplitude_m"] == 7.25 for case in data["checks"] for row in case["responses"])
    assert "7.2" in text
    assert all(case["status"] == "FAIL" for case in data["checks"])
    assert data["evidence"]["manual_rao"]["status"] == "FAIL"
    assert "PASS for the named synthetic controls" not in text


@pytest.mark.parametrize("damage", [
    "missing_heading", "surplus_heading", "amplitude_shape", "phase_shape",
    "heading_labels", "omega_labels", "period_labels", "dof_labels",
    "heading_labels_inplace", "omega_labels_inplace", "period_labels_inplace", "dof_labels_inplace",
])
def test_incomplete_or_mislabeled_response_cannot_be_qualified(builder, monkeypatch, damage):
    import numpy as np
    from digitalmodel.hydrodynamics.capytaine.rao import compute_rao_manual

    def damaged_response(*args, **kwargs):
        result = compute_rao_manual(*args, **kwargs)
        # Deliberate contract mutations, not alternative engineering inputs.
        if damage == "missing_heading":
            result.rao = result.rao[:, :1, :]
        elif damage == "surplus_heading":
            result.rao = np.concatenate((result.rao, result.rao), axis=1)
        elif damage == "amplitude_shape":
            result.rao_amplitude = result.rao_amplitude[:, :1, :]
        elif damage == "phase_shape":
            result.rao_phase = result.rao_phase[:, :1, :]
        elif damage == "heading_labels":
            result.headings = result.headings[::-1]
        elif damage == "omega_labels":
            result.omegas = result.omegas + 1
        elif damage == "period_labels":
            result.periods = result.periods + 1
        elif damage == "dof_labels":
            result.dof_names = ["unexpected"]
        elif damage == "heading_labels_inplace":
            result.headings[:] = 0
        elif damage == "omega_labels_inplace":
            result.omegas[:] += 1
        elif damage == "period_labels_inplace":
            result.periods[:] += 1
        elif damage == "dof_labels_inplace":
            result.dof_names[:] = ["unexpected"]
        return result

    monkeypatch.setattr("digitalmodel.hydrodynamics.capytaine.rao.compute_rao_manual", damaged_response)
    before = json.dumps(builder.INPUTS, sort_keys=True)
    with pytest.raises(ValueError, match="response contract"):
        builder.calculate()
    assert json.dumps(builder.INPUTS, sort_keys=True) == before
