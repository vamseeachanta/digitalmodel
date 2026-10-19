"""Publication contract tests use synthetic arithmetic, not licensed constants."""

import importlib.util
import json
from pathlib import Path
from unittest.mock import Mock

import pandas as pd
import pytest
import yaml

SCRIPT = Path(__file__).resolve().parents[2] / "scripts/hf/publish_atlas_explorer.py"
spec = importlib.util.spec_from_file_location("atlas_publisher", SCRIPT)
publisher = importlib.util.module_from_spec(spec)
spec.loader.exec_module(publisher)


@pytest.fixture
def atlas(tmp_path):
    """Hand calculation: synthetic D=n/N, N=100; n=10,20 gives D=0.1,0.2.

    These are invented arithmetic inputs, not an engineering S-N curve.
    Zero validation error follows from identical synthetic reference values;
    the zero threshold is a test requirement, not a standard acceptance limit.
    """
    folder = tmp_path / "mooring_fatigue" / "example"
    folder.mkdir(parents=True)
    (folder.parent / "default.txt").write_text("example\n")
    frame = pd.DataFrame({"n_cycles": [10.0, 20.0], "damage": [0.1, 0.2]})
    frame.to_parquet(folder / "grid.parquet", index=False)
    manifest = {
        "atlas_id": "example",
        "response": "damage",
        "axes": [{"name": "n_cycles", "grid": [10.0, 20.0]}],
        "validation": {
            "metric": "max_rel_error",
            "max_rel_error": 0.0,
            "threshold": 0.0,
            "passes": True,
        },
        "provenance": {"standards": [{"id": "SYNTHETIC", "edition": "test"}]},
    }
    (folder / "manifest.yaml").write_text(yaml.safe_dump(manifest))
    return tmp_path, folder, frame, manifest


def test_preparation_records_exact_source_hashes(atlas):
    import hashlib

    root, folder, _, _ = atlas
    entry = publisher.prepare_entry(root, "mooring_fatigue", "mooring_fatigue")
    for filename in ("grid.parquet", "manifest.yaml"):
        assert (
            entry["hashes"][filename]
            == hashlib.sha256((folder / filename).read_bytes()).hexdigest()
        )
    assert entry["frame"]["damage"].tolist() == [0.1, 0.2]


@pytest.mark.parametrize("value", [float("nan"), float("inf"), -0.1])
def test_invalid_response_blocks_publication(atlas, value):
    root, folder, frame, _ = atlas
    frame.loc[0, "damage"] = value
    frame.to_parquet(folder / "grid.parquet", index=False)
    with pytest.raises(ValueError, match="response"):
        publisher.prepare_entry(root, "mooring_fatigue", "mooring_fatigue")


@pytest.mark.parametrize("change", ["failed", "over_threshold", "missing", "nan"])
def test_unverified_atlas_blocks_publication(atlas, change):
    root, folder, _, manifest = atlas
    if change == "failed":
        manifest["validation"]["passes"] = False
    elif change == "over_threshold":
        manifest["validation"]["max_rel_error"] = 1.0
    elif change == "nan":
        manifest["validation"]["threshold"] = float("nan")
    else:
        del manifest["validation"]
    (folder / "manifest.yaml").write_text(yaml.safe_dump(manifest))
    with pytest.raises(ValueError, match="validation"):
        publisher.prepare_entry(root, "mooring_fatigue", "mooring_fatigue")


@pytest.mark.parametrize("change", ["empty", "duplicate", "missing_axis", "off_grid"])
def test_grid_contract_rejected(atlas, change):
    root, folder, frame, _ = atlas
    if change == "empty":
        frame = frame.iloc[:0]
    elif change == "duplicate":
        frame = pd.concat([frame.iloc[:1], frame.iloc[:1]])
    elif change == "missing_axis":
        frame = frame.drop(columns=["n_cycles"])
    else:
        frame.loc[0, "n_cycles"] = 30.0
    frame.to_parquet(folder / "grid.parquet", index=False)
    with pytest.raises(ValueError, match="grid"):
        publisher.prepare_entry(root, "mooring_fatigue", "mooring_fatigue")


@pytest.mark.parametrize("pointer", ["../escape", "/absolute", ""])
def test_pointer_cannot_escape_atlas_root(atlas, pointer):
    root, folder, _, _ = atlas
    (folder.parent / "default.txt").write_text(pointer)
    with pytest.raises(ValueError, match="pointer"):
        publisher.prepare_entry(root, "mooring_fatigue", "mooring_fatigue")


def test_card_discloses_citation_gap_and_hashes(atlas):
    root, _, _, _ = atlas
    entry = publisher.prepare_entry(root, "mooring_fatigue", "mooring_fatigue")
    card = publisher.build_card([entry])
    front = yaml.safe_load(card.split("---")[1])
    assert (
        front["configs"][0]["data_files"][0]["path"] == "mooring_fatigue/train.parquet"
    )
    assert "NOT_EVALUATED" in card
    assert "schema_version" in card
    assert entry["hashes"]["grid.parquet"] in card


def test_citation_sidecar_survives_card(atlas):
    root, folder, _, manifest = atlas
    sidecar = {
        "code_id": "SYNTHETIC",
        "revision": "test",
        "section": "Arithmetic",
        "publisher": "Test specification",
        "wiki_path": "wikis/test/example.md",
    }
    manifest["provenance"]["citations"] = [sidecar]
    (folder / "manifest.yaml").write_text(yaml.safe_dump(manifest))
    entry = publisher.prepare_entry(root, "mooring_fatigue", "mooring_fatigue")
    assert json.dumps(sidecar, sort_keys=True) in publisher.build_card([entry])


def test_publish_is_atomic_and_pins_remote_readback(atlas):
    root, _, _, _ = atlas
    entry = publisher.prepare_entry(root, "mooring_fatigue", "mooring_fatigue")
    api = Mock()
    api.repo_info.return_value.private = False
    api.create_commit.return_value.oid = "published-revision"
    api.hf_hub_download.side_effect = lambda **kw: str(root / kw["filename"])
    for filename, content in publisher.publication_files([entry]).items():
        path = root / filename
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_bytes(content)
    revision = publisher.publish_entries(api, "test/dataset", [entry])
    assert revision == "published-revision"
    api.update_repo_settings.assert_called_once_with(
        repo_id="test/dataset", repo_type="dataset", private=False
    )
    assert api.create_commit.call_count == 1
    for call in api.hf_hub_download.call_args_list:
        assert call.kwargs["revision"] == revision


def test_remote_digest_mismatch_blocks_success(atlas):
    root, folder, _, _ = atlas
    entry = publisher.prepare_entry(root, "mooring_fatigue", "mooring_fatigue")
    api = Mock()
    api.repo_info.return_value.private = False
    api.hf_hub_download.return_value = str(folder / "default-wrong.txt")
    (folder / "default-wrong.txt").write_text("wrong bytes")
    with pytest.raises(ValueError, match="digest"):
        publisher.publish_entries(api, "test/dataset", [entry])


def test_rows_readback_matches_local_values(atlas):
    root, _, _, _ = atlas
    entry = publisher.prepare_entry(root, "mooring_fatigue", "mooring_fatigue")
    payload = {
        "num_rows_total": 2,
        "rows": [
            {"row": {"n_cycles": 10.0, "damage": 0.1}},
            {"row": {"n_cycles": 20.0, "damage": 0.2}},
        ],
    }
    publisher.validate_rows(entry, payload)
    payload["rows"][0]["row"]["damage"] = 0.9
    with pytest.raises(ValueError, match="rows"):
        publisher.validate_rows(entry, payload)


def test_short_viewer_sample_is_not_ready(atlas):
    root, _, _, _ = atlas
    entry = publisher.prepare_entry(root, "mooring_fatigue", "mooring_fatigue")
    with pytest.raises(ValueError, match="rows"):
        publisher.validate_rows(
            entry,
            {"num_rows_total": 2, "rows": [{"row": {"n_cycles": 10.0, "damage": 0.1}}]},
        )


def test_nonfinite_axis_is_rejected_even_if_declared(atlas):
    root, folder, frame, manifest = atlas
    frame.loc[0, "n_cycles"] = float("inf")
    manifest["axes"][0]["grid"][0] = float("inf")
    frame.to_parquet(folder / "grid.parquet", index=False)
    (folder / "manifest.yaml").write_text(yaml.safe_dump(manifest))
    with pytest.raises(ValueError, match="grid"):
        publisher.prepare_entry(root, "mooring_fatigue", "mooring_fatigue")


def test_viewer_error_cannot_report_ready(atlas, monkeypatch):
    root, _, _, _ = atlas
    entry = publisher.prepare_entry(root, "mooring_fatigue", "mooring_fatigue")

    def unavailable(*args, **kwargs):
        raise OSError("indexing unavailable")

    monkeypatch.setattr(publisher, "urlopen", unavailable)
    with pytest.raises(RuntimeError, match="NOT_READY"):
        publisher.verify_viewer("test/dataset", [entry], attempts=1, delay=0)


def test_sources_remain_frozen_after_preparation(atlas):
    root, folder, _, _ = atlas
    entry = publisher.prepare_entry(root, "mooring_fatigue", "mooring_fatigue")
    before = publisher.publication_files([entry])
    (folder / "grid.parquet").write_bytes(b"changed after preparation")
    assert publisher.publication_files([entry]) == before
