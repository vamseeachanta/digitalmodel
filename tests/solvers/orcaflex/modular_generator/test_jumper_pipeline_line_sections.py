"""Regression tests for jumper pipeline line-section YAML output (issue #601).

The jumper calculation layer returns generated OrcaFlex line-section YAML
under the key ``orcaflex_sections_yaml``; the pipeline's stage 4 must read
that same key or the 27 line sections are silently dropped and
``line_sections.yml`` is never written.

No OrcaFlex license or OrcFxAPI import is required.

CI command:
    uv run pytest tests/solvers/orcaflex/modular_generator/test_jumper_pipeline_line_sections.py -q
"""
from pathlib import Path

import pytest
import yaml

from digitalmodel.marine_ops.installation.jumper_installation import (
    run_pipeline,
    stage_4_generate_yaml,
)
from digitalmodel.marine_ops.installation.jumper_lift import run_jumper_analysis

REPO_ROOT = Path(__file__).resolve().parents[4]
SPEC_DIR = REPO_ROOT / "docs/domains/orcaflex/subsea/jumper/installation"

BALLYMORE_SPECS = [
    SPEC_DIR / "ballymore_mf_plet" / "spec.yml",
    SPEC_DIR / "ballymore_plet_plem" / "spec.yml",
]

EXPECTED_SECTION_COUNT = 27


def test_run_jumper_analysis_exposes_sections_yaml_key():
    """Lock the producer-side key contract (jumper_lift)."""
    results = run_jumper_analysis()
    assert "orcaflex_sections_yaml" in results
    assert results["orcaflex_sections_yaml"].strip()


def test_stage_4_reads_producer_key():
    """Lock the consumer-side key contract (jumper_installation stage 4).

    Guards the #601 drift: stage 4 must return exactly what the
    calculation layer produced, not an empty fallback from a stale key.
    """
    results = run_jumper_analysis()
    assert stage_4_generate_yaml(results) == results["orcaflex_sections_yaml"]


def test_stage_4_yaml_parses_with_expected_sections():
    parsed = yaml.safe_load(stage_4_generate_yaml(run_jumper_analysis()))
    assert isinstance(parsed, dict)
    assert len(parsed["line_sections"]) == EXPECTED_SECTION_COUNT


@pytest.mark.parametrize(
    "spec_path", BALLYMORE_SPECS, ids=[p.parent.name for p in BALLYMORE_SPECS]
)
def test_pipeline_writes_line_sections_yml(spec_path, tmp_path):
    """run_pipeline(..., run_go_no_go=False) persists line_sections.yml."""
    assert spec_path.exists(), f"missing spec fixture: {spec_path}"

    out_dir = tmp_path / spec_path.parent.name
    output = run_pipeline(
        str(spec_path),
        output_dir=str(out_dir),
        run_go_no_go=False,
    )

    yaml_path = out_dir / "line_sections.yml"
    assert yaml_path.exists(), "line_sections.yml was not written (issue #601)"
    assert str(yaml_path) in output.output_files

    parsed = yaml.safe_load(yaml_path.read_text())
    sections = parsed["line_sections"]
    assert len(sections) == EXPECTED_SECTION_COUNT
    for section in sections:
        assert section["name"]
        assert section["line_type"]
        assert section["length_m"] > 0
