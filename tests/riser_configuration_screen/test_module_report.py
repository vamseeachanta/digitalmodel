# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Report data, analytical anchors and portable rebuild contracts."""

import importlib.util
import json
import math
import re
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[2]
FOLDER = ROOT / "docs/reports/modules/riserconfigurationscreen"
REBUILD = "python scripts/reporting/build_riser_configuration_report.py"


def builder():
    spec = importlib.util.spec_from_file_location(
        "riser_report", ROOT / "scripts/reporting/build_riser_configuration_report.py")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_report_rebuild():
    rebuilt = builder().render_report()
    assert rebuilt == (FOLDER / "report.html").read_text(
        encoding="utf-8"), f"Report is stale; rebuild with: {REBUILD}"
    assert b"\r\n" not in rebuilt.encode("utf-8")


def test_all_example_numbers_match_module():
    module = builder()
    data = module.calculate()
    text = (FOLDER / "report.html").read_text(encoding="utf-8")
    embedded = json.loads(re.search(
        r'<script id="report-data" type="application/json">(.*?)</script>',
        text, re.S).group(1))
    assert embedded == module.display_data(data), f"Rebuild with: {REBUILD}"
    for path, digits, shown in re.findall(
            r'data-number="([^"]+)" data-digits="(\d+)">([^<]+)', text):
        value = data
        for key in path.split("."):
            value = value[int(key)] if isinstance(value, list) else value[key]
        assert shown == module.number(value, int(digits)), path


def test_hand_calculation_anchors():
    row = builder().calculate()["anchor"]
    # Assumed depth 100 m and angle 30 degrees: sin(angle)=1/2;
    # a = depth*sin(angle)/(1-sin(angle)) = 100 m.
    assert row["touchdown_curvature_per_m"] == pytest.approx(0.01, rel=1e-7)
    # Weight 10 N/m gives H=w*a = 1000 N.
    assert row["horizontal_tension_n"] == pytest.approx(1000.0, rel=1e-7)
    # T_top = w*(a+depth) = 2000 N, from vertical equilibrium.
    assert row["top_tension_n"] == pytest.approx(2000.0, rel=1e-7)


def test_structure_and_public_wording():
    text = (FOLDER / "report.html").read_text(encoding="utf-8")
    assert 'class="hs-doc"' in text
    assert len(re.findall(r'<h2 id="s\d+">', text)) == 10
    assert 'id="comments"' in text and "showSaveFilePicker" in text
    assert "Draft for maintainer review" in text
    assert "ASSUMED - to be confirmed" in text
    assert "<script src=" not in text
    assert '<link rel="stylesheet" href="../../../api/_assets/brand.css">' in text
    assert (FOLDER / "../../../api/_assets/brand.css").resolve().is_file()
    visible = re.sub(r"<(script|style)\b.*?</\1>", "", text, flags=re.S)
    visible = re.sub(r"<[^>]+>", " ", visible)
    assert not re.search(r"\b(owner|client|lane|seat|session|could|should|would)\b", visible, re.I)
    assert "<strong" not in text and "<b>" not in text


def test_style_and_changes_are_embedded():
    text = (FOLDER / "report.html").read_text(encoding="utf-8")
    changes = json.loads((FOLDER / "changes.json").read_text(encoding="utf-8"))
    assert changes["changes"][0]["description"] in text
    assert "var(--brand)" in text
    from scripts import brand_guard
    assert brand_guard.CORE_RE.search(text) is None
    assert "Limit" in text and "<title>" in text


def test_hand_worked_example_headlines():
    """Pin report values independently of calculate() and display helpers.

    For h=1500, w=1000 and alpha=20 degrees, a=h*sin(alpha)/(1-sin(alpha)).
    Fix L=sqrt(h*h+2*a*h)+500. Solve a*asinh(s/a)+L-s=X+offset by hand
    iteration; T=w*(a+h), g=L-s, kappa=1/a. The far-position hangoff
    stress is T/A + E*D*w*H/(2*T*T), with A=pi*(D*D-(D-2*t)**2)/4.
    These rounded literals are independent of the module and its renderer.
    """
    text = (FOLDER / "report.html").read_text(encoding="utf-8")
    shown = dict((path, float(value.replace("\u2212", "-"))) for path, value in
                 re.findall(r'data-number="([^"]+)" data-digits="\d+">([^<]+)', text))
    prefix = "result.ranked_candidates.0."
    assert shown[prefix + "configuration.hangoff_angle_deg"] == 20.0
    assert shown[prefix + "curvature_range_per_m"] == pytest.approx(4.9461e-4, abs=5e-9)
    # Excursions are -50, 0 and +50 m, in the supplied order.
    for index, tension, ground in ((0, 2143957.6, 597.26),
                                   (1, 2279705.0, 500.00),
                                   (2, 2444926.8, 387.28)):
        row = prefix + f"offset_results.{index}."
        assert shown[row + "top_tension_n"] == pytest.approx(tension, rel=0, abs=0.05)
        assert shown[row + "grounded_length_m"] == pytest.approx(ground, abs=0.005)
    assert shown[prefix + "offset_results.2.static_von_mises_utilisation"] == 0.2625


@pytest.mark.parametrize("direction", [-math.inf, math.inf])
def test_computed_ulp_perturbations_leave_html_identical(monkeypatch, direction):
    module = builder()
    original = module.calculate()
    expected = module.render_report()

    def perturb(value):
        if isinstance(value, dict):
            return {key: perturb(item) for key, item in value.items()}
        if isinstance(value, list):
            return [perturb(item) for item in value]
        return math.nextafter(value, direction) if isinstance(value, float) else value

    original["result"] = perturb(original["result"])
    original["anchor"] = perturb(original["anchor"])
    monkeypatch.setattr(module, "calculate", lambda: original)
    assert module.render_report() == expected


@pytest.mark.parametrize("direction", [-math.inf, math.inf])
def test_anchor_depth_ulp_leaves_html_identical(monkeypatch, direction):
    module = builder()
    expected = module.render_report()
    solve = module.scr_offsets

    def perturbed(settings, offsets):
        return solve({**settings, "water_depth_m": math.nextafter(
            settings["water_depth_m"], direction)}, offsets)

    monkeypatch.setattr(module, "scr_offsets", perturbed)
    assert module.render_report() == expected


def test_anchor_agreement_is_checked_before_display_rounding(monkeypatch):
    module = builder()
    data = module.calculate()
    data["anchor"]["top_tension_n"] *= 1 + 2e-7
    monkeypatch.setattr(module, "calculate", lambda: data)
    with pytest.raises(ValueError, match="analytical anchor"):
        module.render_report()


def test_embedded_data_excludes_solver_diagnostics():
    text = builder().render_report()
    payload = re.search(r'<script id="report-data" type="application/json">(.*?)</script>',
                        text, re.S).group(1)
    assert "closure" not in payload
    assert "differences_percent" not in payload
    assert "runtime_s" not in payload
    assert "input_digest" not in payload
    assert "relative tolerance of 1e-7" in text


def test_report_basis_and_grid_boundary_are_explicit():
    text = builder().render_report()
    for phrase in ("minimum is not bracketed", "(2-42b)", "Yield and Plastic Flow",
                   "Stresses in Beams", "Revision history", "2026-10-21", "2026-10-22"):
        assert phrase in text
    visible = re.sub(r"<(script|style)\b.*?</\1>", "", text, flags=re.S)
    for phrase in ("Drilling offset", "do not connect", "watch circle", "tensioner",
                   "recoil", "Weak point", "mud weight", "foam zone", "telescopic joint"):
        assert phrase.lower() not in visible.lower()


def test_report_suites_are_executed_by_ci():
    import yaml

    gates = yaml.safe_load((ROOT / ".claude/quality-gates.yaml").read_text(encoding="utf-8"))["gates"]
    assert "tests/riser_configuration_screen/" in gates["tests-subsea"]["command"]
    assert "tests/capabilities/" in gates["tests-capabilities"]["command"]
    workflow = yaml.safe_load((ROOT / ".github/workflows/docs.yml").read_text(encoding="utf-8"))
    commands = [step.get("run", "") for step in workflow["jobs"]["docs"]["steps"]]
    assert any("python -m pytest tests/riser_configuration_screen/" in command
               for command in commands)
    # PyYAML treats the unquoted workflow key as a YAML 1.1 boolean.
    triggers = workflow.get("on", workflow.get(True))
    for event in ("push", "pull_request"):
        assert "docs/reports/modules/**" in triggers[event]["paths"]
        assert "tests/riser_configuration_screen/**" in triggers[event]["paths"]


def test_registered_report_builds_in_isolation(tmp_path):
    """The real freshness driver must have all inputs outside the checkout."""
    import os
    import subprocess
    import sys

    from scripts import check_generated_html as checker

    entries = [entry for entry in checker.GENERATORS
               if entry.script.endswith("build_riser_configuration_report.py")]
    assert len(entries) == 1
    entry = entries[0]
    assert set(entry.inputs) >= {
        "examples/workflows/riser-configuration-screen/scr.yml",
        "docs/reports/modules/riserconfigurationscreen/report.template.html",
        "docs/reports/modules/riserconfigurationscreen/changes.json",
    }
    # Copy only declared inputs, the executable and its shared style/package inputs.
    import shutil

    for relative in (entry.script, *entry.inputs,
                     "docs/api/cfd/turbulent-flat-plate-verification.html",
                     "docs/api/_assets/brand.css"):
        target = tmp_path / relative
        target.parent.mkdir(parents=True, exist_ok=True)
        shutil.copyfile(ROOT / relative, target)
    shutil.copytree(ROOT / "src", tmp_path / "src")
    env = {key: value for key, value in os.environ.items()
           if key not in {"GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR"}}
    env["PYTHONPATH"] = str(tmp_path / "src")
    completed = subprocess.run([sys.executable, entry.script], cwd=tmp_path, env=env,
                               capture_output=True, text=True, encoding="utf-8")
    assert completed.returncode == 0, completed.stderr
    assert (tmp_path / entry.outputs[0]).read_bytes() == (FOLDER / "report.html").read_bytes()


def test_utf8_crlf_inputs_and_non_default_cwd(tmp_path, monkeypatch):
    """A text-mode checkout and a legacy default encoding preserve LF output."""
    module = builder()
    expected = module.render_report()
    relatives = ("docs/reports/modules/riserconfigurationscreen/report.template.html",
                 "docs/reports/modules/riserconfigurationscreen/changes.json",
                 "examples/workflows/riser-configuration-screen/scr.yml",
                 "docs/api/cfd/turbulent-flat-plate-verification.html",
                 "docs/api/_assets/brand.css")
    for relative in relatives:
        target = tmp_path / relative
        target.parent.mkdir(parents=True, exist_ok=True)
        target.write_text((ROOT / relative).read_text(encoding="utf-8"),
                          encoding="utf-8", newline="\r\n")
    monkeypatch.setattr(module, "ROOT", tmp_path)
    monkeypatch.setattr(module, "FOLDER", tmp_path / FOLDER.relative_to(ROOT))
    monkeypatch.setattr(module, "EXAMPLE", tmp_path / module.EXAMPLE.relative_to(ROOT))
    monkeypatch.chdir(tmp_path)
    original_open = Path.open

    def legacy_open(path, *args, **kwargs):
        kwargs.setdefault("encoding", "cp1252")
        return original_open(path, *args, **kwargs)

    monkeypatch.setattr(Path, "open", legacy_open)
    output = tmp_path / "rebuilt.html"
    module.build_report(output)
    with original_open(output, "rb") as stream:
        assert stream.read() == expected.encode("utf-8")
