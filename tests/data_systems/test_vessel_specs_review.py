"""Offline regressions for the second-provider vessel-reference review."""

import json
from datetime import datetime
from pathlib import Path
from urllib.parse import urlsplit

import pytest

from tests.data_systems import test_vessel_specs as batch_contract

DATA = Path(__file__).resolve().parents[2] / "assets" / "data"


def test_retrieval_conduct_discloses_both_delays_and_observed_spacing():
    readme = (DATA / "vessel_specs_README.md").read_text(encoding="utf-8")
    manifest = json.loads(
        (DATA / "vessel_specs_provenance.json").read_text(encoding="utf-8")
    )
    for operator, domain, delay in (
        ("Jan De Nul", "www.jandenul.com", 10),
        ("DEME", "www.deme-group.com", 30),
    ):
        instants = [
            datetime.fromisoformat(source["retrieved_utc"])
            for source in manifest["sources"]
            if urlsplit(source["source_url"]).netloc == domain
        ]
        assert len(instants) == 3
        intervals = ", ".join(
            f"{(end - start).total_seconds():.3f}"
            for start, end in zip(instants, instants[1:])
        )
        assert (
            f"- {operator}: {delay} seconds advertised; {intervals} seconds" in readme
        )
    text = " ".join(readme.split())
    assert "did not meet either advertised crawl delay" in text
    assert (
        "Follow-up: `scripts/fetch_vessel_specs.py` must honor advertised "
        "crawl delays before further collection."
    ) in text


def test_gulliver_discloses_both_operator_brands():
    readme = (DATA / "vessel_specs_README.md").read_text(encoding="utf-8")
    text = " ".join(readme.split())
    assert "Gulliver is listed by both DEME and Jan De Nul" in text
    assert "`owner` retains the DEME source brand" in text
    rows = json.loads((DATA / "vessel_specs_hlv.json").read_text(encoding="utf-8"))
    row = next(row for row in rows if row["vessel_name"] == "Gulliver")
    assert row["owner"] == "DEME"
    assert row["field_sources"]["owner"] == row["source_url"]


def test_castorone_uses_pipe_diameter_without_coating():
    rows = json.loads((DATA / "vessel_specs_pipelay.json").read_text(encoding="utf-8"))
    row = next(row for row in rows if row["vessel_name"] == "Castorone")
    assert row["max_pipe_diameter_inches"] == 48
    assert row["field_sources"]["max_pipe_diameter_inches"] == row["source_url"]
    readme = (DATA / "vessel_specs_README.md").read_text(encoding="utf-8")
    assert "Castorone's JSON value is 48 inches without coating" in readme


def test_batch_two_locator_rows_follow_existing_column_padding():
    lines = (DATA / "vessel_specs_README.md").read_text(encoding="utf-8").splitlines()
    header = next(line for line in lines if line.startswith("| Vessel "))
    widths = [len(cell) - 2 for cell in header.split("|")[1:-1]]
    batch_names = {"Les Alizés", "Orion", "Gulliver", "Castorone"}
    rows = [
        line
        for line in lines
        if line.split("|")[1:2] and line.split("|")[1].strip() in batch_names
    ]
    assert len(rows) == 5
    for row in rows:
        assert [len(cell) - 2 for cell in row.split("|")[1:-1]] == widths
        cells = [cell.strip() for cell in row.split("|")[1:-1]]
        assert (
            row
            == "| "
            + " | ".join(
                cell.ljust(width) for cell, width in zip(cells, widths, strict=True)
            )
            + " |"
        )


def write_sourceable_batch(directory, picked):
    """Build synthetic facts; every unselected roster entry remains sourceable."""
    readme, sources = [], []
    for kind, queue in (
        ("pipelay", batch_contract.PIPELAY_QUEUE),
        ("hlv", batch_contract.HLV_QUEUE),
    ):
        rows = []
        for name, owner, keys in queue:
            if (kind, name) not in picked:
                readme.append(f"- {name} ({kind}): batch cap")
                continue
            url = f"https://example.org/{kind}/{keys[0]}"
            row = {"vessel_name": name, "owner": owner, "source_url": url}
            if kind == "hlv":
                row.update(crane_capacity_tonnes=1, crane_capacity_basis="individual")
            else:
                row.update(
                    tensioner_capacity_tonnes=None,
                    max_pipe_diameter_inches=1,
                    lay_methods=None,
                )
            rows.append(row)
            sources.append({"source_url": url, "status": "retrieved"})
            readme.append(f"| {name} | Synthetic fact | {url} |")
        (directory / f"vessel_specs_{kind}.json").write_text(
            json.dumps(rows), encoding="utf-8"
        )
    (directory / "vessel_specs_provenance.json").write_text(
        json.dumps({"sources": sources}), encoding="utf-8"
    )
    (directory / "vessel_specs_README.md").write_text(
        "\n".join(readme), encoding="utf-8"
    )


def test_all_sourceable_batch_requires_p1_h1_p2_h2_h3(tmp_path):
    write_sourceable_batch(
        tmp_path,
        {
            ("pipelay", "DB50"),
            ("hlv", "Bokalift 2"),
            ("pipelay", "DB30"),
            ("hlv", "Bokalift 1"),
            ("hlv", "Les Alizés"),
        },
    )
    batch_contract.assert_batch_two(tmp_path)


@pytest.mark.parametrize(
    "pipelay_names,hlv_names,error",
    [
        (
            ["DB50"],
            ["Bokalift 2", "Bokalift 1", "Les Alizés", "Orion"],
            "Batch selection must alternate",
        ),
        (
            [],
            ["Bokalift 2", "Bokalift 1", "Les Alizés", "Orion", "Gulliver"],
            "New pipelay rows: 0",
        ),
        (
            ["DB50", "Seven Borealis"],
            ["Bokalift 2", "Bokalift 1", "Les Alizés"],
            "Queue entry skipped for batch cap",
        ),
    ],
    ids=["four-hlv-one-pipelay", "hlv-only", "skipped-sourceable-pipelay"],
)
def test_sourceable_batch_rejects_wrong_allocation(
    tmp_path, pipelay_names, hlv_names, error
):
    picked = {("pipelay", name) for name in pipelay_names}
    picked.update(("hlv", name) for name in hlv_names)
    write_sourceable_batch(tmp_path, picked)
    with pytest.raises(AssertionError, match=error):
        batch_contract.assert_batch_two(tmp_path)


def test_all_sourceable_batch_accepts_four_row_cap(tmp_path):
    write_sourceable_batch(
        tmp_path,
        {
            ("pipelay", "DB50"),
            ("hlv", "Bokalift 2"),
            ("pipelay", "DB30"),
            ("hlv", "Bokalift 1"),
        },
    )
    batch_contract.assert_batch_two(tmp_path)
