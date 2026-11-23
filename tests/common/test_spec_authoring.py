"""Tests for the shared spec-authoring plumbing + hardened ProjectBundle contract."""

from __future__ import annotations

import subprocess

import pytest
from pydantic import ValidationError

from digitalmodel.common.spec_authoring import (
    ProjectBundle,
    ProvenanceEntry,
    _extract_json_object,
    build_outcome_menu,
    claude_cli_complete,
)

# --- ProjectBundle contract (hardening) ------------------------------------


def test_unknown_top_level_key_is_rejected() -> None:
    # A misspelled field is an error, not a silently-dropped value.
    with pytest.raises(ValidationError):
        ProjectBundle(vessel_particular={"loa": 100})  # missing trailing 's'


def test_units_codes_references_render_in_context() -> None:
    bundle = ProjectBundle(
        project_name="P",
        units="SI",
        codes=["DNV-OS-E301"],
        references=["doc://datasheet-revB"],
        vessel_particulars={"loa": 245},
    )
    ctx = bundle.to_prompt_context()
    assert "Units: SI" in ctx
    assert "DNV-OS-E301" in ctx
    assert "doc://datasheet-revB" in ctx
    assert "loa: 245" in ctx


def test_contract_warnings_flags_missing_units_with_numeric_data() -> None:
    warned = ProjectBundle(vessel_particulars={"loa": 245})
    assert any("units" in w.lower() for w in warned.contract_warnings())
    # Declaring units clears the warning.
    ok = ProjectBundle(units="SI", vessel_particulars={"loa": 245})
    assert ok.contract_warnings() == []


def test_contract_warnings_flags_empty_bundle() -> None:
    b = ProjectBundle()
    assert b.is_empty()
    assert any("empty" in w.lower() for w in b.contract_warnings())


def test_blank_strings_normalize_to_none_and_empty_docs_dropped() -> None:
    b = ProjectBundle(
        project_name="   ",
        notes="",
        documents=["  ", "real doc", ""],
        codes=["", " DNV "],
    )
    assert b.project_name is None
    assert b.notes is None
    assert b.documents == ["real doc"]
    assert b.codes == ["DNV"]


def test_to_json_schema_describes_the_contract() -> None:
    schema = ProjectBundle.to_json_schema()
    assert schema["type"] == "object"
    for key in ("project_name", "units", "vessel_particulars", "codes"):
        assert key in schema["properties"]


def test_from_yaml_roundtrip(tmp_path) -> None:
    p = tmp_path / "bundle.yml"
    p.write_text("project_name: P\nunits: SI\nvessel_particulars:\n  loa: 100\n")
    b = ProjectBundle.from_yaml(p)
    assert b.project_name == "P" and b.units == "SI"
    assert b.vessel_particulars["loa"] == 100


# --- shared helpers --------------------------------------------------------


def test_build_outcome_menu_handles_enum_and_str_keys() -> None:
    import enum

    class Outcome(str, enum.Enum):
        A = "alpha"

    menu = build_outcome_menu({Outcome.A: "first", "beta": "second"})
    assert "- alpha: first" in menu
    assert "- beta: second" in menu


def test_extract_json_object_tolerates_fences_and_prose() -> None:
    assert _extract_json_object('x ```json\n{"a": 1}\n``` y') == '{"a": 1}'
    assert _extract_json_object('{"s": "}"}') == '{"s": "}"}'
    with pytest.raises(ValueError):
        _extract_json_object("no json here")


def _completed(stdout: str, rc: int = 0, stderr: str = ""):
    return subprocess.CompletedProcess(["claude"], rc, stdout=stdout, stderr=stderr)


def test_claude_cli_complete_parses_envelope_and_passes_flags() -> None:
    import json

    captured = {}

    def runner(argv, stdin):
        captured["argv"] = argv
        captured["stdin"] = stdin
        return _completed(json.dumps({"is_error": False, "result": '{"ok": true}'}))

    out = claude_cli_complete("sys", "usr", model="claude-opus-4-8", runner=runner)
    assert out == {"ok": True}
    assert "-p" in captured["argv"] and "--output-format" in captured["argv"]
    assert "claude-opus-4-8" in captured["argv"]
    assert captured["stdin"] == "usr"


def test_claude_cli_complete_raises_on_error_envelope_and_rc() -> None:
    import json

    err_env = claude_cli_complete
    with pytest.raises(RuntimeError, match="boom"):
        err_env(
            "s",
            "u",
            model="m",
            runner=lambda a, s: _completed(
                json.dumps({"is_error": True, "result": "boom"})
            ),
        )
    with pytest.raises(RuntimeError, match="no auth"):
        claude_cli_complete(
            "s", "u", model="m", runner=lambda a, s: _completed("", 1, "no auth")
        )


def test_provenance_entry_shape() -> None:
    e = ProvenanceEntry(field="loa", value="245 m", source="data sheet")
    assert e.field == "loa" and e.source == "data sheet"
