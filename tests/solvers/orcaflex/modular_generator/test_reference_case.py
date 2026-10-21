# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Offline object-name lookup regressions.

Basis: Orcina OrcaFlex webhelp, "The model", object names paragraph
(unversioned, accessed 2026-10-19):
https://www.orcina.com/webhelp/OrcaFlex/Content/html/Themodel.htm
The published Riser/riser/RISER example supplies the equivalent spellings.
These are string-identity checks; no physical inputs or limits are used.
"""

from copy import deepcopy

import pytest
import yaml

from digitalmodel.solvers.orcaflex.modular_generator.post_validator import (
    PostGenerationValidator,
    ValidationWarning,
)


_ROUTES = (
    "line_type",
    "line_connection",
    "winch_vessel",
    "winch_line",
    "link",
    "InFrameConnection",
    "OutFrameConnection",
    "Connection",
)
_DEFINITION_SECTIONS = {
    "line_type": "LineTypes",
    "line_connection": "6DBuoys",
    "winch_vessel": "Vessels",
    "winch_line": "Lines",
    "link": "Shapes",
    "InFrameConnection": "Constraints",
    "OutFrameConnection": "Constraints",
    "Connection": "Constraints",
}


def _reference_model(route, target):
    definition = {_DEFINITION_SECTIONS[route]: [{"Name": "Riser"}]}
    if route == "line_type":
        section, name = "Lines", "LineUnderTest"
        fields = {"LineType, Length, TargetSegmentLength": [[target]]}
        message = (
            f"Line '{name}' references LineType '{target}' which is not defined"
        )
    elif route == "line_connection":
        section, name = "Lines", "LineUnderTest"
        fields = {"Connection, ConnectionX": [[target]]}
        message = (
            f"Line '{name}' End A references '{target}' which is not a defined "
            "Vessel, Buoy, or built-in connection"
        )
    elif route in ("winch_vessel", "winch_line"):
        section, name = "Winches", "WinchUnderTest"
        kind = "vessel" if route == "winch_vessel" else "line"
        fields = {"Connection": [target] if kind == "vessel" else ["", target]}
        message = f"Winch '{name}' references {kind} '{target}' which is not defined"
    elif route == "link":
        section, name = "Links", "LinkUnderTest"
        fields = {"Connection": [target]}
        message = f"Link '{name}' references '{target}' which is not defined"
    else:
        section, name = "Constraints", "ConstraintUnderTest"
        fields = {route: target}
        message = (
            f"Constraint '{name}' {route} references '{target}' which is not defined"
        )
    files = {
        "definitions.yml": definition,
        "references.yaml": {section: [{"Name": name, **fields}]},
    }
    missing = ValidationWarning(
        level="error",
        category="reference",
        message=message,
        file="references.yaml",
        object_name=name,
    )
    return files, missing


def _both_entry_points(tmp_path, files):
    original = deepcopy(files)
    includes = tmp_path / "includes"
    includes.mkdir()
    for filename, data in files.items():
        (includes / filename).write_text(yaml.safe_dump(data), encoding="utf-8")
    validator = PostGenerationValidator()
    findings = validator.validate_data(files)
    assert validator.validate_directory(tmp_path) == findings
    assert validator.validate_directory(includes) == findings
    assert files == original
    return findings


@pytest.mark.parametrize("route", _ROUTES)
@pytest.mark.parametrize("target", ("Riser", "riser", "RISER"))
def test_case_variants_resolve(tmp_path, route, target):
    """Vendor worked example: each spelling denotes the defined Riser object.

    Each model has exactly one definition and one reference. The cited name
    rule therefore predicts an empty finding list for every route, without
    deriving expected results from the validator.
    """
    files, _ = _reference_model(route, target)
    assert _both_entry_points(tmp_path, files) == []


@pytest.mark.parametrize("route", _ROUTES)
@pytest.mark.parametrize("target", ("rIsEr_typo", " Riser", "Riser "))
def test_missing_names_keep_original_diagnostic(tmp_path, route, target):
    """Independent comparison: each target differs by more than letter case.

    No definition has the suffix or spaces, so exactly the literal missing
    reference finding is expected. File, object and target spelling survive.
    """
    files, missing = _reference_model(route, target)
    assert _both_entry_points(tmp_path, files) == [missing]


@pytest.mark.parametrize("route", ("line_type", "winch_vessel", "winch_line"))
def test_case_insensitive_lookup_retains_required_section(tmp_path, route):
    """A Shapes definition cannot supply a LineType, Vessel or Line.

    Hand comparison of section names leaves the required definition absent;
    matching letters alone must still produce the missing-reference finding.
    """
    files, missing = _reference_model(route, "riser")
    files["definitions.yml"] = {"Shapes": [{"Name": "Riser"}]}
    # Keep a nonmatching LineType so the existing partial-model skip is unused.
    if route == "line_type":
        files["definitions.yml"]["LineTypes"] = [{"Name": "OtherType"}]
    assert _both_entry_points(tmp_path, files) == [missing]
