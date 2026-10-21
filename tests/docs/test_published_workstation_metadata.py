# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Published documentation describes compute capacity without internal aliases."""

import json
import os
from pathlib import Path

import pytest

from scripts.capabilities import build_cfd_runtime_estimator as estimator
from scripts.capabilities import build_sloshing_cfd_showcase as study
from scripts.cfd import run_sloshing_3d_benchmark as benchmark


ROOT = Path(__file__).resolve().parents[2]
# Assemble aliases so the regression check does not publish them verbatim.
INTERNAL_HOSTS = (
    "-".join(("dev", "secondary")),
    "-".join(("a", "l", "2")),
    "-".join(("dev", "primary")),
    "-".join(("gpu", "claw")),
)
PUBLISHED_SUFFIXES = (".html", ".htm", ".md", ".json", ".yaml", ".yml")


def assert_no_internal_host(root: Path, aliases=INTERNAL_HOSTS) -> None:
    pages = sorted(path for path in root.rglob("*")
                   if path.is_file() and path.suffix.lower() in PUBLISHED_SUFFIXES)
    assert pages, "No published documentation was scanned"
    offenders = []
    for path in pages:
        relative = path.relative_to(root).as_posix()
        text = path.read_text(encoding="utf-8", errors="replace")
        if any(alias in (relative + "\n" + text).casefold() for alias in aliases):
            offenders.append(relative)
    assert offenders == [], f"Internal workstation alias in: {offenders}"


@pytest.mark.parametrize("suffix", PUBLISHED_SUFFIXES)
@pytest.mark.parametrize("uppercase", (False, True))
@pytest.mark.parametrize("alias", INTERNAL_HOSTS, ids=("retired", "shared", "primary", "gpu"))
def test_scan_rejects_a_planted_alias(tmp_path, suffix, uppercase, alias):
    page = tmp_path / "nested" / f"study{suffix}"
    page.parent.mkdir()
    alias = alias.upper() if uppercase else alias
    page.write_text(f"<p>{alias}</p>", encoding="utf-8")
    with pytest.raises(AssertionError, match="nested/study"):
        assert_no_internal_host(tmp_path)


@pytest.mark.parametrize("alias", INTERNAL_HOSTS, ids=("retired", "shared", "primary", "gpu"))
@pytest.mark.parametrize("uppercase", (False, True))
def test_scan_rejects_an_alias_in_a_published_filename(tmp_path, alias, uppercase):
    alias = alias.upper() if uppercase else alias
    (tmp_path / f"{alias}.json").write_text("{}", encoding="utf-8")
    with pytest.raises(AssertionError, match="Internal workstation alias"):
        assert_no_internal_host(tmp_path)


def test_scan_accepts_workstation_capacity(tmp_path):
    (tmp_path / "study.html").write_text("a 32-core workstation", encoding="utf-8")
    assert_no_internal_host(tmp_path)


@pytest.mark.parametrize("alias", INTERNAL_HOSTS, ids=("retired", "shared", "primary", "gpu"))
def test_scan_rejects_an_alias_in_legacy_encoded_documentation(tmp_path, alias):
    (tmp_path / "legacy.md").write_bytes(b"\xb1 " + alias.encode("ascii"))
    with pytest.raises(AssertionError, match="legacy.md"):
        assert_no_internal_host(tmp_path)


def test_scan_rejects_an_empty_census(tmp_path):
    with pytest.raises(AssertionError, match="No published documentation"):
        assert_no_internal_host(tmp_path)


def test_published_documentation_has_no_internal_host():
    docs = ROOT / "docs"
    assert (docs / "api/cfd/sloshing-cfd-study.html").is_file()
    assert (docs / "api/cfd/cfd-runtime-estimator.html").is_file()
    # Preserve the repository-wide guard; expanded coverage follows this change's
    # publication directories rather than unrelated historical documents.
    assert_no_internal_host(docs, INTERNAL_HOSTS[:1])
    assert_no_internal_host(docs / "api/cfd")


def test_rendered_examples_have_no_internal_host():
    assert_no_internal_host(ROOT / "examples", INTERNAL_HOSTS[:1])
    assert_no_internal_host(ROOT / "examples/reporting")


def test_study_renders_capacity_instead_of_input_host():
    compute = json.loads((ROOT / "docs/api/cfd/sloshing-compute.json").read_text(encoding="utf-8"))
    compute["machine"]["host"] = INTERNAL_HOSTS[0]
    section = study._compute_section(compute)
    assert not any(alias in section for alias in INTERNAL_HOSTS)
    assert "on a 32-core workstation (" in section
    assert compute["machine"]["solver"] in section


@pytest.mark.parametrize("cores, expected", [(32, "32-core workstation"), (None, "workstation")])
def test_estimator_does_not_embed_input_host(tmp_path, monkeypatch, cores, expected):
    compute = json.loads((ROOT / "docs/api/cfd/sloshing-compute.json").read_text(encoding="utf-8"))
    compute["machine"]["host"] = INTERNAL_HOSTS[0]
    if cores is None:
        compute["machine"].pop("cores")
    else:
        compute["machine"]["cores"] = cores
    (tmp_path / "sloshing-compute.json").write_text(json.dumps(compute), encoding="utf-8")
    monkeypatch.setattr(estimator, "_REPO", tmp_path)
    monkeypatch.setattr(estimator, "_CFD", tmp_path)
    monkeypatch.setattr(estimator, "_OUT", tmp_path / "estimator.html")
    estimator.main()
    page = (tmp_path / "estimator.html").read_text(encoding="utf-8")
    assert not any(alias in page for alias in INTERNAL_HOSTS)
    data = json.loads(page.split("const DATA = ", 1)[1].split(";", 1)[0])
    assert data["machine"]["host"] == expected
    assert compute["machine"]["host"] == INTERNAL_HOSTS[0]


@pytest.mark.parametrize("label", [None, "parallel"])
@pytest.mark.parametrize("cores, expected", [(8, "8-core workstation"), (None, "workstation")])
def test_benchmark_collect_publishes_capacity(tmp_path, monkeypatch, label, cores, expected):
    monkeypatch.setattr(benchmark, "_REPO", tmp_path)
    monkeypatch.setattr(benchmark, "_MANIFEST", tmp_path / "benchmark.json")
    monkeypatch.setattr(os, "cpu_count", lambda: cores)
    monkeypatch.setattr(os, "uname", lambda: pytest.fail("Metadata must not query a hostname"),
                        raising=False)
    manifest = benchmark.collect(60, 0.3, [], label=label)
    saved = json.loads((tmp_path / manifest["_out_path"]).read_text(encoding="utf-8"))
    assert saved["meta"]["box"] == expected
    assert saved["meta"]["cores"] == cores
    assert "hostname" not in saved["meta"]


def test_published_benchmark_uses_capacity_filename():
    path = ROOT / "docs/api/cfd/sloshing-3d-benchmark-8-core-workstation.json"
    manifest = json.loads(path.read_text(encoding="utf-8"))
    assert manifest["meta"]["box"] == "8-core workstation"
    assert manifest["meta"]["cores"] == 8
    assert "hostname" not in manifest["meta"]
