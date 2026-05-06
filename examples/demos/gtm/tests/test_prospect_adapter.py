"""Tests for digitalmodel/examples/demos/gtm/prospect_adapter.py.

Covers plan section C (validation + materialization) and section G
(fallback sidecar). Delivery state-machine tests live in
test_prospect_pipeline_e2e.py to keep file responsibility narrow.
"""
from __future__ import annotations

import json
import sys
from pathlib import Path

import pytest

_HERE = Path(__file__).resolve().parent
_GTM_DIR = _HERE.parent
if str(_GTM_DIR) not in sys.path:
    sys.path.insert(0, str(_GTM_DIR))

from prospect_adapter import (  # noqa: E402
    CANONICAL_ALIASES,
    DeliveryResult,
    DemoInputBundle,
    FallbackCode,
    ProspectIntakeError,
    ProspectInput,
    compute_gated_url_hash,
    load_and_validate,
    materialize_demo_inputs,
    package_delivery_files,
    record_fallback,
)


FIXTURES_DIR = _HERE / "fixtures"


# ---------------------------------------------------------------------------
# Load + validate
# ---------------------------------------------------------------------------


def test_valid_yaml_passes_schema() -> None:
    prospect = load_and_validate(FIXTURES_DIR / "prospect-valid.yaml")
    assert isinstance(prospect, ProspectInput)
    assert prospect.target_demo == "demo_05"
    assert prospect.vessel_shape == "csv_hlv"
    assert prospect.structure_kind == "rigid_jumper"
    assert prospect.company == "Acme Marine Contractors"
    assert prospect.contact == "jane.doe@acme.example"


def test_missing_vessel_body_fails() -> None:
    # prospect-missing-vessel.yaml is a demo_03 intake without the
    # required vessel block — schema should reject it via the Q6
    # allOf conditional.
    with pytest.raises(ProspectIntakeError):
        load_and_validate(FIXTURES_DIR / "prospect-missing-vessel.yaml")


def test_depth_exceeds_vessel_rating() -> None:
    with pytest.raises(ProspectIntakeError, match=r"(?i)deeper water"):
        load_and_validate(FIXTURES_DIR / "prospect-depth-exceeds.yaml")


def test_canonical_alias_resolves_seven_borealis_to_heavy_lift_csv(tmp_path: Path) -> None:
    # Write a valid intake that uses the legacy alias canonical_ref.
    fixture = FIXTURES_DIR / "prospect-valid.yaml"
    yaml_text = fixture.read_text(encoding="utf-8").replace(
        'canonical_ref: "heavy-lift-csv"',
        'canonical_ref: "seven-borealis"',
    )
    # Depth must not exceed max_water_depth_m of the canonical vessel.
    yaml_text = yaml_text.replace(
        "water_depths_m: [1500, 2000, 2500]",
        "water_depths_m: [1500]",
    )
    alias_intake = tmp_path / "alias.yaml"
    alias_intake.write_text(yaml_text, encoding="utf-8")
    prospect = load_and_validate(alias_intake)
    assert prospect.target_demo == "demo_05"
    assert CANONICAL_ALIASES["seven-borealis"] == "heavy-lift-csv"


def test_enum_target_demo_rejects_demo_99(tmp_path: Path) -> None:
    fixture = FIXTURES_DIR / "prospect-valid.yaml"
    yaml_text = fixture.read_text(encoding="utf-8").replace(
        'target_demo: "demo_05"',
        'target_demo: "demo_99"',
    )
    bad = tmp_path / "bad.yaml"
    bad.write_text(yaml_text, encoding="utf-8")
    with pytest.raises(ProspectIntakeError):
        load_and_validate(bad)


def test_unknown_top_level_key_fails(tmp_path: Path) -> None:
    fixture = FIXTURES_DIR / "prospect-valid.yaml"
    yaml_text = fixture.read_text(encoding="utf-8").replace(
        "prospect:",
        "prospct:",  # typo — additionalProperties=false must reject it
        1,
    )
    bad = tmp_path / "typo.yaml"
    bad.write_text(yaml_text, encoding="utf-8")
    with pytest.raises(ProspectIntakeError):
        load_and_validate(bad)


def test_canonical_ref_unknown_id_fails(tmp_path: Path) -> None:
    fixture = FIXTURES_DIR / "prospect-valid.yaml"
    yaml_text = fixture.read_text(encoding="utf-8").replace(
        'canonical_ref: "heavy-lift-csv"',
        'canonical_ref: "does-not-exist"',
    )
    bad = tmp_path / "no-such.yaml"
    bad.write_text(yaml_text, encoding="utf-8")
    with pytest.raises(ProspectIntakeError, match=r"(?i)canonical vessel reference not found"):
        load_and_validate(bad)


def test_canonical_ref_traversal_rejected(tmp_path: Path) -> None:
    # Adversarial: prospect tries to read a file outside the canonical
    # library via ../../../ traversal. The resolver MUST refuse.
    fixture = FIXTURES_DIR / "prospect-valid.yaml"
    yaml_text = fixture.read_text(encoding="utf-8").replace(
        'canonical_ref: "heavy-lift-csv"',
        'canonical_ref: "../../../../../etc/passwd"',
    )
    bad = tmp_path / "traversal.yaml"
    bad.write_text(yaml_text, encoding="utf-8")
    with pytest.raises(ProspectIntakeError):
        load_and_validate(bad)


def test_wrong_demo_vessel_shape_mismatch(tmp_path: Path) -> None:
    fixture = FIXTURES_DIR / "prospect-valid.yaml"
    yaml_text = (
        fixture.read_text(encoding="utf-8")
        .replace('target_demo: "demo_05"', 'target_demo: "demo_04"')
        .replace('kind: "rigid_jumper"', 'kind: "pipeline"')
        # shape remains csv_hlv, but demo_04 requires pipelay
    )
    bad = tmp_path / "shape-mismatch.yaml"
    bad.write_text(yaml_text, encoding="utf-8")
    with pytest.raises(ProspectIntakeError, match=r"pipelay"):
        load_and_validate(bad)


# ---------------------------------------------------------------------------
# Materialization
# ---------------------------------------------------------------------------


def test_materialize_csv_hlv_writes_correct_file(tmp_path: Path) -> None:
    prospect = load_and_validate(FIXTURES_DIR / "prospect-valid.yaml")
    bundle = materialize_demo_inputs(prospect, tmp_path)

    assert isinstance(bundle, DemoInputBundle)
    assert bundle.demo_id == "demo_05"
    assert bundle.data_dir == tmp_path / "data"
    assert bundle.vessel_file == bundle.data_dir / "csv_hlv_vessels.json"
    assert bundle.structure_file == bundle.data_dir / "rigid_jumpers.json"
    assert bundle.vessel_file.exists()
    assert bundle.structure_file.exists()

    vessel_payload = json.loads(bundle.vessel_file.read_text(encoding="utf-8"))
    assert vessel_payload["vessels"][0]["id"] == "heavy-lift-csv"
    assert "crane_main" in vessel_payload["vessels"][0]
    structure_payload = json.loads(bundle.structure_file.read_text(encoding="utf-8"))
    assert structure_payload["jumpers"][0]["length_m"] == pytest.approx(45.0)


def test_materialize_pipelay_writes_correct_file(tmp_path: Path) -> None:
    fixture = FIXTURES_DIR / "prospect-valid.yaml"
    yaml_text = (
        fixture.read_text(encoding="utf-8")
        .replace('target_demo: "demo_05"', 'target_demo: "demo_04"')
        .replace('shape: "csv_hlv"', 'shape: "pipelay"')
        .replace('canonical_ref: "heavy-lift-csv"', 'canonical_ref: "pipelay-barge"')
        .replace('kind: "rigid_jumper"', 'kind: "pipeline"')
        # pipelay-barge has max_water_depth_m 1200, so cap depths:
        .replace(
            "water_depths_m: [1500, 2000, 2500]",
            "water_depths_m: [200, 500, 800]",
        )
    )
    intake = tmp_path / "pipelay.yaml"
    intake.write_text(yaml_text, encoding="utf-8")
    prospect = load_and_validate(intake)
    bundle = materialize_demo_inputs(prospect, tmp_path)

    assert bundle.demo_id == "demo_04"
    assert bundle.vessel_file == bundle.data_dir / "pipelay_vessels.json"
    assert bundle.structure_file == bundle.data_dir / "pipelines.json"
    vessel_payload = json.loads(bundle.vessel_file.read_text(encoding="utf-8"))
    assert vessel_payload["vessels"][0]["pipelay_system"]["method"] == "S-lay"


def test_materialize_env_override(tmp_path: Path) -> None:
    prospect = load_and_validate(FIXTURES_DIR / "prospect-valid.yaml")
    bundle = materialize_demo_inputs(prospect, tmp_path)
    assert bundle.env_override_path is not None
    env_payload = json.loads(bundle.env_override_path.read_text(encoding="utf-8"))
    assert env_payload["water_depths_m"] == [1500, 2000, 2500]
    assert env_payload["hs_values_m"] == [1.5, 2.0, 2.5]


def test_materialize_demo_03_writes_mudmat_file(tmp_path: Path) -> None:
    fixture = FIXTURES_DIR / "prospect-valid.yaml"
    yaml_text = (
        fixture.read_text(encoding="utf-8")
        .replace('target_demo: "demo_05"', 'target_demo: "demo_03"')
        .replace('kind: "rigid_jumper"', 'kind: "mudmat"')
        .replace(
            """body:
    outer_diameter_m: 0.3239
    wall_thickness_m: 0.0254
    length_m: 45.0
    material: "X65\"""",
            """body:
    plan_area_m2: 120.0
    skirt_penetration_m: 2.0
    bearing_capacity_kpa: 150""",
        )
    )
    intake = tmp_path / "mudmat.yaml"
    intake.write_text(yaml_text, encoding="utf-8")
    prospect = load_and_validate(intake)
    bundle = materialize_demo_inputs(prospect, tmp_path)

    assert bundle.demo_id == "demo_03"
    assert bundle.structure_file == bundle.data_dir / "mudmat_structures.json"
    structure_payload = json.loads(bundle.structure_file.read_text(encoding="utf-8"))
    assert structure_payload["structures"][0]["plan_area_m2"] == 120.0


# ---------------------------------------------------------------------------
# Canonical coverage
# ---------------------------------------------------------------------------


def test_canonical_coverage_spans_three_vessel_shapes() -> None:
    # Plan acceptance: three canonical vessel files exist at
    # {pipelay-barge, heavy-lift-csv, plsv}.yaml under the canonical library.
    from prospect_adapter import _canonical_dir

    canonical = _canonical_dir()
    expected_stems = {"heavy-lift-csv", "pipelay-barge", "plsv"}
    actual_stems = {p.stem for p in canonical.glob("*.yaml")}
    missing = expected_stems - actual_stems
    assert not missing, f"missing canonical vessel files: {missing}"


# ---------------------------------------------------------------------------
# Fallback sidecar
# ---------------------------------------------------------------------------


def test_record_fallback_writes_json_schema_compliant(tmp_path: Path) -> None:
    sidecar = tmp_path / "private-log" / "fallback-applied.json"
    record_fallback(
        prospect_id="acme-marine-2026-04-21",
        fallback_code=FallbackCode.F2_CLOSEST_CANONICAL,
        failure_mode="vessel block missing; pre-authorized canonical substitution",
        canonical_source="heavy-lift-csv",
        pre_authorization="explicit",
        engineer="test",
        sidecar_path=sidecar,
    )
    assert sidecar.exists()
    payload = json.loads(sidecar.read_text(encoding="utf-8"))
    assert isinstance(payload, list)
    assert len(payload) == 1
    rec = payload[0]
    assert rec["fallback_code"] == "F2"
    assert rec["prospect_id"] == "acme-marine-2026-04-21"
    assert rec["pre_authorization"] == "explicit"
    assert rec["canonical_source"] == "heavy-lift-csv"
    # Schema-required keys:
    for key in ("prospect_id", "timestamp_utc", "fallback_code", "failure_mode"):
        assert key in rec


def test_record_fallback_appends_on_multiple_calls(tmp_path: Path) -> None:
    sidecar = tmp_path / "private-log" / "fallback-applied.json"
    record_fallback(
        prospect_id="p1",
        fallback_code=FallbackCode.F1_REFUSE,
        failure_mode="schema missing top-level block",
        sidecar_path=sidecar,
    )
    record_fallback(
        prospect_id="p2",
        fallback_code=FallbackCode.F5_REDUCED_SCOPE,
        failure_mode="depth exceeds vessel rating",
        pre_authorization="explicit",
        sidecar_path=sidecar,
    )
    payload = json.loads(sidecar.read_text(encoding="utf-8"))
    assert len(payload) == 2
    assert payload[0]["fallback_code"] == "F1"
    assert payload[1]["fallback_code"] == "F5"


def test_package_delivery_files_excludes_private_log(tmp_path: Path) -> None:
    # Construct a fake bundle dir with both a normal file and a
    # private-log sidecar. The packager MUST filter the private-log entry.
    bundle = tmp_path / "bundle"
    bundle.mkdir()
    (bundle / "report.html").write_text("<html></html>", encoding="utf-8")
    (bundle / "chart_data.json").write_text("{}", encoding="utf-8")
    private = bundle / "private-log"
    private.mkdir()
    (private / "fallback-applied.json").write_text("[]", encoding="utf-8")

    files = package_delivery_files(bundle)
    names = {p.name for p in files}
    assert "report.html" in names
    assert "chart_data.json" in names
    assert "fallback-applied.json" not in names


def test_package_delivery_files_handles_missing_dir(tmp_path: Path) -> None:
    missing = tmp_path / "does-not-exist"
    assert package_delivery_files(missing) == []
