# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Published documentation describes compute capacity without internal aliases."""

import json
from pathlib import Path

import pytest

from scripts.capabilities import build_cfd_runtime_estimator as estimator
from scripts.capabilities import build_sloshing_cfd_showcase as study


ROOT = Path(__file__).resolve().parents[2]
# Assemble the retired alias so the regression check does not publish it verbatim.
INTERNAL_HOST = "-".join(("dev", "secondary"))
PUBLISHED_SUFFIXES = (".html", ".htm", ".md", ".json")


def assert_no_internal_host(root: Path) -> None:
    pages = sorted(path for path in root.rglob("*")
                   if path.is_file() and path.suffix.lower() in PUBLISHED_SUFFIXES)
    assert pages, "No published documentation was scanned"
    offenders = [path.relative_to(root).as_posix() for path in pages
                 if INTERNAL_HOST in path.read_text(encoding="utf-8", errors="replace").casefold()]
    assert offenders == [], f"Internal workstation alias in: {offenders}"


@pytest.mark.parametrize("suffix", PUBLISHED_SUFFIXES)
@pytest.mark.parametrize("uppercase", (False, True))
def test_scan_rejects_a_planted_alias(tmp_path, suffix, uppercase):
    page = tmp_path / "nested" / f"study{suffix}"
    page.parent.mkdir()
    alias = INTERNAL_HOST.upper() if uppercase else INTERNAL_HOST
    page.write_text(f"<p>{alias}</p>", encoding="utf-8")
    with pytest.raises(AssertionError, match="nested/study"):
        assert_no_internal_host(tmp_path)


def test_scan_accepts_workstation_capacity(tmp_path):
    (tmp_path / "study.html").write_text("a 32-core workstation", encoding="utf-8")
    assert_no_internal_host(tmp_path)


def test_scan_rejects_an_alias_in_legacy_encoded_documentation(tmp_path):
    (tmp_path / "legacy.md").write_bytes(b"\xb1 " + INTERNAL_HOST.encode("ascii"))
    with pytest.raises(AssertionError, match="legacy.md"):
        assert_no_internal_host(tmp_path)


def test_scan_rejects_an_empty_census(tmp_path):
    with pytest.raises(AssertionError, match="No published documentation"):
        assert_no_internal_host(tmp_path)


def test_published_documentation_has_no_internal_host():
    docs = ROOT / "docs"
    assert (docs / "api/cfd/sloshing-cfd-study.html").is_file()
    assert (docs / "api/cfd/cfd-runtime-estimator.html").is_file()
    assert_no_internal_host(docs)


def test_rendered_examples_have_no_internal_host():
    assert_no_internal_host(ROOT / "examples")


def test_study_renders_capacity_instead_of_input_host():
    compute = json.loads((ROOT / "docs/api/cfd/sloshing-compute.json").read_text())
    compute["machine"]["host"] = INTERNAL_HOST
    section = study._compute_section(compute)
    assert INTERNAL_HOST not in section
    assert "on a 32-core workstation (" in section
    assert compute["machine"]["solver"] in section


def test_estimator_does_not_embed_input_host(tmp_path, monkeypatch):
    compute = json.loads((ROOT / "docs/api/cfd/sloshing-compute.json").read_text())
    compute["machine"]["host"] = INTERNAL_HOST
    (tmp_path / "sloshing-compute.json").write_text(json.dumps(compute))
    monkeypatch.setattr(estimator, "_REPO", tmp_path)
    monkeypatch.setattr(estimator, "_CFD", tmp_path)
    monkeypatch.setattr(estimator, "_OUT", tmp_path / "estimator.html")
    estimator.main()
    page = (tmp_path / "estimator.html").read_text(encoding="utf-8")
    assert INTERNAL_HOST not in page
    assert "32-core workstation" in page
    assert compute["machine"]["host"] == INTERNAL_HOST
