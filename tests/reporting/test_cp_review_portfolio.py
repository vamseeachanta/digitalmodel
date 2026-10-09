"""Fail-closed release and completeness contracts for the CP portfolio."""
from copy import deepcopy
import importlib.util
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[2]
SPEC = importlib.util.spec_from_file_location(
    "cp_portfolio_contract", ROOT / "scripts/reporting/cp_portfolio_contract.py"
)
assert SPEC and SPEC.loader
contract = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(contract)


def test_registry_requires_every_row_and_unique_ids() -> None:
    rows = [{"id": key} for key in contract.REQUIRED_IDS]
    contract.validate_registry(rows)
    with pytest.raises(ValueError, match="coverage"):
        contract.validate_registry(rows[:-1])
    with pytest.raises(ValueError, match="duplicate"):
        contract.validate_registry(rows + [rows[0]])


def test_release_allowlist_checks_values_not_only_names() -> None:
    allowed = {"case": "A", "metric": {"class": "qualitative"}}
    contract.validate_release(allowed, deepcopy(allowed))
    for unsafe in ({**allowed, "private_source_hash": "abc"},
                   {**allowed, "metric": {"class": "exact", "value": 37.25}}):
        with pytest.raises(ValueError, match="release"):
            contract.validate_release(unsafe, allowed)


def test_review_document_identity_is_neutral_and_unissued() -> None:
    doc = contract.review_document("S01", "Jacket")
    assert doc["number"] == "R2281-CP-001-00"
    assert doc["revision_history"][0]["description"] == "Internal review draft"
    assert doc["approved_by"] == "Pending"
    with pytest.raises(ValueError):
        contract.review_document("../private", "Wrong")


def test_ready_is_not_inferred_from_folder_or_calculation_pass() -> None:
    rows = [{"id": key, "pack_state": "blocked"} for key in contract.REQUIRED_IDS]
    rows[0].update(calculation_result="PASS", pack_state="ready")
    summary = contract.coverage_summary(rows)
    assert not summary["complete"]
    assert summary["verified"] == 0
    assert summary["manual_visual_review"] == "deferred_by_owner"


def test_phase_child_requires_resolvable_specific_result_and_anchor() -> None:
    results = {"phases": [{"name": "storage", "mass": 1.0}]}
    child = {"result_pointer": "/phases/0/mass", "anchor": "storage-mass"}
    contract.validate_child(child, results, '<p id="storage-mass">1</p>')
    for change in ({"result_pointer": "/phases/1/mass"}, {"anchor": "absent"}):
        with pytest.raises(ValueError, match="child"):
            contract.validate_child({**child, **change}, results, '<p id="storage-mass">1</p>')


def test_pack_verifier_rejects_changed_report_and_missing_citations(tmp_path: Path) -> None:
    import sys
    sys.path.insert(0, str(ROOT / "scripts/reporting"))
    from build_cp_review_portfolio import write_json, write_receipt, verify_pack
    (tmp_path / "report.html").write_text("<html>FAIL</html>")
    write_json(tmp_path / "results.json", {"status": {"result": "FAIL"}})
    write_json(tmp_path / "report_citations.json", {"citations": []})
    write_receipt(tmp_path)
    verify_pack(tmp_path)
    (tmp_path / "report.html").write_text("<html>PASS</html>")
    with pytest.raises(ValueError, match="checksum"):
        verify_pack(tmp_path)
    (tmp_path / "report.html").write_text("<html>FAIL</html>")
    (tmp_path / "report_citations.json").unlink()
    with pytest.raises(ValueError, match="missing"):
        verify_pack(tmp_path)


def test_existing_comments_are_never_silently_reset(tmp_path: Path) -> None:
    import sys
    sys.path.insert(0, str(ROOT / "scripts/reporting"))
    from build_cp_review_portfolio import check_existing_pack, write_json
    write_json(tmp_path / "report.comments.json", {"comments": [{"text": "Keep this"}]})
    with pytest.raises(ValueError, match="review comments"):
        check_existing_pack(tmp_path)


def test_b401_2021_citation_uses_matching_edition_page_only() -> None:
    from build_cp_review_portfolio import canonical_citations
    record = {"code_id": "dnv-rp-b401", "publisher": "DNV", "revision": "2021-05",
              "wiki_path": "wikis/engineering-standards/wiki/standards/dnv-rp-b401.md",
              "section": "Table 8-1"}
    repaired = canonical_citations([record])[0]
    assert repaired["wiki_path"].endswith("dnv-rp-b401-2021.md")
    assert {k: v for k, v in repaired.items() if k != "wiki_path"} == {
        k: v for k, v in record.items() if k != "wiki_path"}
    older = {**record, "revision": "2011"}
    assert canonical_citations([older]) == [older]
    year_label = {**record, "revision": "2021"}
    assert canonical_citations([year_label])[0]["revision"] == "2021-05"
