"""Repository-file regressions for the C06 admission screen.

These assert the current admission policy, not physical equilibrium.
"""

from pathlib import Path

from digitalmodel.solvers.orcaflex.yaml_validator import (
    OrcaFlexYAMLValidator,
    Severity,
)

ROOT = Path(__file__).resolve().parents[3]


def test_library_starting_shape_passes():
    source = ROOT / "docs/domains/orcaflex/examples/raw/D04/D04 J-tube pull in.yml"
    result = OrcaFlexYAMLValidator().validate_file(source)
    arc = [f for f in result.starting_shapes if f.check == "arc_length"]
    assert result.valid
    assert len(arc) == 1
    assert arc[0].status == "PASS"
    assert not any(f.status == "FAIL" for f in result.starting_shapes)


def test_hand_authored_model_is_rejected_only_by_arc_length_screen():
    source = ROOT / (
        "docs/domains/orcaflex/pipeline/installation/floating/"
        "30in_pipeline/monolithic.yml"
    )
    result = OrcaFlexYAMLValidator().validate_file(source)
    failed = [f for f in result.starting_shapes if f.status == "FAIL"]
    assert not result.valid
    assert len(failed) == 1
    assert failed[0].check == "arc_length"
    # The mismatch is about 0.265%, above the C06 limit of 0.1%.
    assert 0.0026 < failed[0].actual < 0.0027
    assert failed[0].limit == 0.001
    assert len(result.errors) == 1
    assert result.errors[0].severity == Severity.ERROR
    assert "arc_length" in result.errors[0].message
