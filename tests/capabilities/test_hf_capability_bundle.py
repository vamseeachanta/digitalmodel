"""Offline bundles implement the website registry's stable-config contract."""

import hashlib
import json
from pathlib import Path

import pytest

from scripts.hf import capability_bundle as bundles


def test_bundle_carries_every_ipr_point_and_source_digest(tmp_path):
    """Independent count from build_ipr_explorer.py: 3 pressures * 10 PI * 11 points.

    The digest is calculated directly over the source bytes, independently of the
    exporter. No model output is used as its own engineering reference answer.
    """
    destination = tmp_path / "bundle"
    bundles.write_bundle("digitalmodel-artificial-lift", destination, license_id="mit")
    manifest = json.loads((destination / "manifest.json").read_text())
    table = manifest["tables"]["ipr"]
    assert table["row_count"] == 3 * 10 * 11
    assert (
        table["source_sha256"]
        == hashlib.sha256((bundles.REPO / table["source"]).read_bytes()).hexdigest()
    )
    rows = [
        json.loads(line)
        for line in (destination / "ipr/train.jsonl").read_text().splitlines()
    ]
    assert len(rows) == 330
    assert (
        rows[0]["flow_rate_bopd"] == 0
    )  # closed-flow endpoint; source curve starts at Pr
    assert set(rows[0]) == set(table["columns"])
    assert table["engineering_readiness"] == "NOT_EVALUATED"
    assert table["data_limits"]
    card = (destination / "README.md").read_text()
    assert '"config_name": "ipr"' in card
    assert '"path": "ipr/train.jsonl"' in card
    assert '"license": "mit"' in card
    assert "source_sha256" in (destination / "manifest.json").read_text()
    before = {
        p.relative_to(destination): p.read_bytes()
        for p in destination.rglob("*")
        if p.is_file()
    }
    bundles.verify_bundle("digitalmodel-artificial-lift", destination, license_id="mit")
    assert before == {
        p.relative_to(destination): p.read_bytes()
        for p in destination.rglob("*")
        if p.is_file()
    }


def test_export_projects_the_hashed_snapshot_instead_of_reopening_source(
    tmp_path, monkeypatch
):
    """A second source read could disagree with the bytes whose digest is recorded.

    The substituted second read supplies arbitrary bogus flow. The exported first
    endpoint must still equal the zero in the actual hashed source snapshot.
    """
    other = {"series": {"100": {"2.0": {"qmax": 9, "curve": [{"pwf": 0, "q": 9}]}}}}
    monkeypatch.setattr(bundles.builder, "_load", lambda _: other)
    bundles.write_bundle(
        "digitalmodel-artificial-lift", tmp_path / "bundle", license_id="mit"
    )
    first = (tmp_path / "bundle/ipr/train.jsonl").read_text().splitlines()[0]
    assert json.loads(first)["flow_rate_bopd"] == 0


@pytest.mark.parametrize("damage", ["value", "missing", "extra"])
def test_verification_rejects_tampered_incomplete_or_stale_bundles(tmp_path, damage):
    """A digest is insufficient when an omitted or extra config could remain live."""
    target = tmp_path / "bundle"
    bundles.write_bundle("digitalmodel-artificial-lift", target, license_id="mit")
    data = target / "ipr/train.jsonl"
    if damage == "value":
        data.write_text("{}\n")
    elif damage == "missing":
        data.rename(target / "missing.jsonl")
    else:
        (target / "obsolete.jsonl").write_text("{}\n")
    before = {
        p.relative_to(target): p.read_bytes() for p in target.rglob("*") if p.is_file()
    }
    with pytest.raises(ValueError, match="differs"):
        bundles.verify_bundle("digitalmodel-artificial-lift", target, license_id="mit")
    assert before == {
        p.relative_to(target): p.read_bytes() for p in target.rglob("*") if p.is_file()
    }


def test_missing_large_input_never_leaves_a_partial_bundle(tmp_path, monkeypatch):
    """The optional integrity sweep is absent in Git; absence must stay visible."""
    target = tmp_path / "bundle"
    source, generator, limits = bundles.SOURCES["wall_thickness_tension_moment"]
    # Use an isolated missing path even when a full sweep has been generated.
    missing = (tmp_path / "missing.json").relative_to(tmp_path.anchor).as_posix()
    monkeypatch.setitem(
        bundles.SOURCES, "wall_thickness_tension_moment", (missing, generator, limits)
    )
    with pytest.raises(FileNotFoundError):
        bundles.write_bundle("digitalmodel-integrity", target, license_id="mit")
    assert not target.exists()


def test_existing_destination_is_preserved(tmp_path):
    target = tmp_path / "bundle"
    target.mkdir()
    sentinel = target / "sentinel"
    sentinel.write_text("existing data")
    with pytest.raises(FileExistsError):
        bundles.write_bundle("digitalmodel-artificial-lift", target, license_id="mit")
    assert sentinel.read_text() == "existing data"


@pytest.mark.parametrize("kind", ["home", "mnt", "windows", "contact"])
def test_source_locator_is_rejected_before_any_output(tmp_path, monkeypatch, kind):
    """A retained source sidecar must not leak a private locator in its metadata."""
    repo = tmp_path / "repo"
    if kind == "windows":
        restricted = str(Path("C:") / "Users" / tmp_path.name / "data").replace(
            "/", "\\"
        )
    elif kind == "contact":
        restricted = "example" + chr(64) + "example.org"
    else:
        restricted = (Path("/") / kind / tmp_path.name / "data").as_posix()
    source = repo / "docs/api/structural/ipr-explorer.json"
    source.parent.mkdir(parents=True)
    original = json.loads(
        (bundles.REPO / "docs/api/structural/ipr-explorer.json").read_bytes()
    )
    original["private_locator"] = restricted
    source.write_text(json.dumps(original))
    generator = repo / bundles.SOURCES["ipr"][1]
    generator.parent.mkdir(parents=True)
    generator.write_text("# synthetic generator fixture\n")
    monkeypatch.setattr(bundles, "REPO", repo)
    with pytest.raises(ValueError, match="restricted locator"):
        bundles.write_bundle(
            "digitalmodel-artificial-lift", tmp_path / "bundle", license_id="mit"
        )
    assert not (tmp_path / "bundle").exists()


@pytest.mark.parametrize("bad_cell", [float("nan"), float("inf"), {"nested": 3}, [3]])
def test_non_tabular_or_non_finite_cells_cannot_be_published(
    tmp_path, monkeypatch, bad_cell
):
    """These are invalid data shapes, not engineering rejection thresholds."""
    monkeypatch.setitem(
        bundles.builder.DATASETS, "synthetic", {"ipr": lambda _: [{"q": bad_cell}]}
    )
    with pytest.raises(ValueError):
        bundles.write_bundle("synthetic", tmp_path / "bundle", license_id="mit")
    assert not (tmp_path / "bundle").exists()


@pytest.mark.parametrize(
    "dataset",
    [
        "digitalmodel-corrosion",
        "digitalmodel-artificial-lift",
        "digitalmodel-mooring",
        "digitalmodel-hydro",
        "digitalmodel-well",
        "digitalmodel-field-development",
    ],
)
def test_every_available_group_roundtrips_with_declared_configs(tmp_path, dataset):
    """Exact file sets and bytes must reproduce across independent destinations."""
    first, second = tmp_path / "first", tmp_path / "second"
    bundles.write_bundle(dataset, first, license_id="mit")
    bundles.write_bundle(dataset, second, license_id="mit")

    def files(folder):
        return {
            p.relative_to(folder): p.read_bytes()
            for p in folder.rglob("*")
            if p.is_file()
        }

    assert files(first) == files(second)
    bundles.verify_bundle(dataset, first, license_id="mit")
    manifest = json.loads((first / "manifest.json").read_text())
    assert set(manifest["tables"]) == set(bundles.builder.DATASETS[dataset])
    for name, table in manifest["tables"].items():
        data = (first / name / "train.jsonl").read_bytes()
        assert hashlib.sha256(data).hexdigest() == table["data_sha256"]
        assert all(
            set(json.loads(row)) == set(table["columns"]) for row in data.splitlines()
        )


def test_command_line_build_and_readonly_verify(tmp_path, monkeypatch, capsys):
    args = [
        "capability_bundle",
        "--dataset",
        "digitalmodel-artificial-lift",
        "--license",
        "mit",
        "--out",
        str(tmp_path / "bundle"),
    ]
    monkeypatch.setattr("sys.argv", args)
    bundles.main()
    assert "prepared (not published)" in capsys.readouterr().out
    monkeypatch.setattr("sys.argv", [*args, "--verify"])
    bundles.main()
    assert "verified (not published)" in capsys.readouterr().out
