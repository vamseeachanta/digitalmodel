"""Generation must reject validator errors before publishing model files."""

import pytest

from digitalmodel.solvers.orcaflex.modular_generator import ModularModelGenerator
from digitalmodel.solvers.orcaflex.modular_generator.schema import ProjectInputSpec


def invalid_spec(defect):
    data = {
        "metadata": {"name": "validation_gate", "description": "Validation gate regression",
                     "structure": "generic", "operation": "generic", "project": "generic"},
        "environment": {"water": {"depth": 100},
                        "seabed": {"stiffness": {"normal": 100, "shear": 100}}},
        "generic": {},
    }
    if defect == "duplicate":
        # Native object names are unique and case-insensitive across sections:
        # https://www.orcina.com/webhelp/OrcaFlex/Content/html/Datafields.htm
        # https://www.orcina.com/webhelp/OrcaFlex/Content/html/Themodel.htm
        data["generic"] = {"line_types": [{"name": "shared"}],
                           "lines": [{"name": "SHARED"}]}
    else:
        # Raw extra trains bypass the typed WaveTrain rejection; the production
        # validator must still reject the known invalid native enum.
        data["environment"].update(
            waves={"type": "airy", "height": 0, "period": 8},
            raw_properties={"WaveTrains": [{"Name": "valid"},
                                          {"Name": "extra", "WaveType": "No waves"}]},
        )
    return ProjectInputSpec(**data)


@pytest.mark.parametrize("method", ["generate", "generate_with_overrides"])
@pytest.mark.parametrize("defect,category", [("duplicate", "duplicate"), ("wave_type", "wave_type")])
@pytest.mark.parametrize("existing", [False, True])
def test_validation_errors_do_not_publish(tmp_path, method, defect, category, existing):
    output = tmp_path / "output"
    original = {"master.yml": b"previous master\n", "includes/03_environment.yml": b"previous environment\n"}
    if existing:
        for name, content in original.items():
            path = output / name
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_bytes(content)
    generator = ModularModelGenerator.from_spec(invalid_spec(defect))
    with pytest.raises(ValueError, match="Post-generation validation failed") as caught:
        getattr(generator, method)(output)
    assert any(w.category == category and w.level == "error" for w in caught.value.findings)
    if existing:
        assert {str(p.relative_to(output)): p.read_bytes() for p in output.rglob("*") if p.is_file()} == original
    else:
        assert not output.exists()


@pytest.mark.parametrize("method", ["generate", "generate_with_overrides"])
@pytest.mark.parametrize("defect,category", [("duplicate", "duplicate"), ("wave_type", "wave_type")])
def test_diagnostic_opt_out_is_explicit(tmp_path, caplog, method, defect, category):
    generator = ModularModelGenerator.from_spec(invalid_spec(defect))
    output = tmp_path / "diagnostic"
    result = getattr(generator, method)(output, allow_validation_errors=True)
    assert (output / "master.yml").is_file()
    assert category in caplog.text
    assert any(r.levelname == "ERROR" for r in caplog.records)
    if method == "generate_with_overrides":
        assert any(f"[error] {category}" in w for w in result.warnings)
        assert result.master_file == output / "master.yml"
        assert all(p.is_file() and p.is_relative_to(output) for p in result.include_files)


@pytest.mark.parametrize("method", ["generate", "generate_with_overrides"])
def test_warning_only_findings_allow_publication(validated_spec, tmp_path, monkeypatch, method):
    from digitalmodel.solvers.orcaflex.modular_generator.post_validator import (
        PostGenerationValidator, ValidationWarning,
    )
    warning = ValidationWarning(level="warning", category="coherence", message="Review geometry")
    monkeypatch.setattr(PostGenerationValidator, "validate_directory", lambda *args: [warning])
    output = tmp_path / "valid"
    result = getattr(ModularModelGenerator.from_spec(validated_spec), method)(output)
    assert (output / "master.yml").is_file()
    if method == "generate_with_overrides":
        assert result.warnings == ["[warning] coherence: Review geometry"]


def test_custom_override_is_validated_before_publication(validated_spec, tmp_path):
    from digitalmodel.solvers.orcaflex.modular_generator.schema.campaign import InstallationSection
    template = tmp_path / "environment.yml"
    template.write_text("Environment:\n  WaveTrains:\n    - Name: extra\n      WaveType: No waves\n")
    sections = [InstallationSection(builder_file="03_environment.yml", template=template.name)]
    output = tmp_path / "rejected"
    with pytest.raises(ValueError, match="Post-generation validation failed"):
        ModularModelGenerator.from_spec(validated_spec).generate_with_overrides(
            output, sections=sections, template_base_dir=tmp_path,
        )
    assert not output.exists()


@pytest.mark.parametrize("defect", ["duplicate", "wave_type"])
def test_cli_rejects_invalid_model_before_output(tmp_path, defect):
    import argparse
    import yaml
    from digitalmodel.solvers.orcaflex.modular_generator.cli import cmd_generate
    spec = invalid_spec(defect)
    input_path = tmp_path / "invalid.yml"
    input_path.write_text(yaml.safe_dump(spec.model_dump(mode="json")))
    output = tmp_path / "output"
    assert cmd_generate(argparse.Namespace(input=str(input_path), output=str(output))) == 1
    assert not output.exists()
