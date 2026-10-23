# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Keep the committed capability page reproducible from the engine example."""

import importlib.util
from pathlib import Path

import pytest


REPO = Path(__file__).resolve().parents[3]
SCRIPT = REPO / "scripts/capabilities/build_air_gap_capability.py"
PAGE = REPO / "docs/api/structural/air-gap-analysis.html"


def builder():
    spec = importlib.util.spec_from_file_location("air_gap_capability", SCRIPT)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_generated_page_matches_committed_code(tmp_path, capsys):
    module = builder()
    output = tmp_path / "air-gap-analysis.html"
    module.main(["--output", str(output)])
    assert output.read_bytes() == PAGE.read_bytes()
    assert "wall time:" in capsys.readouterr().out


def test_invented_example_uses_engine_values_and_exposes_limits():
    module = builder()
    example = module.build_example()
    assert example["assessment"].snapshots[-1].crest_clearance_m == pytest.approx(-1.0)
    assert example["loads"][-1].horizontal_force_kn == pytest.approx(164.0)
    assert example["remediation"].options[1].required_lift_m == pytest.approx(2.7)
    page = module.render_page(example)
    assert "invented" in page
    assert "assumed, based on previous experience" in page
    assert page.index("Summary</h2>") < page.index("Revision history</h2>")
    assert page.index("Revision history</h2>") < page.index("Design basis</h2>")
    assert "screening_only" in page
    assert "full assessment" in page
    assert "slamming" in page
    assert "Code-specific" in page
    for option in ("live_with_it", "raise_topside", "decommission"):
        assert option in page
    assert "2022-01-01" in page


def test_page_escapes_text_from_data():
    module = builder()
    example = module.build_example()
    example["label"] = "<script>unsafe</script>"
    page = module.render_page(example)
    assert "<script>unsafe</script>" not in page
    assert "&lt;script&gt;unsafe&lt;/script&gt;" in page
