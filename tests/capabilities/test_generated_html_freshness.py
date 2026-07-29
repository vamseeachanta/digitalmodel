"""Regression tests for the generated HTML freshness gate."""

from pathlib import Path

from scripts import check_generated_html as checker


def test_discovery_includes_non_write_text_generator(tmp_path: Path) -> None:
    script = tmp_path / "scripts" / "build_page.py"
    script.parent.mkdir()
    script.write_text(
        'from pathlib import Path\n'
        'FORMAT = "HTML"\n'
        'output = Path("docs") / "api" / f"example.{FORMAT.lower()}"\n'
        'with open(output, "w", encoding="utf-8") as stream:\n'
        '    stream.write("<html></html>")\n',
        encoding="utf-8",
    )

    assert checker.discover_candidate_generators(tmp_path) == {
        "scripts/build_page.py"
    }


def test_shadow_copies_inputs_instead_of_linking_live_checkout(
    tmp_path: Path,
) -> None:
    repo = tmp_path / "repo"
    shadow = tmp_path / "shadow"
    for relative in (
        "scripts",
        "docs/api",
        "assets",
        "atlases",
        "data",
        "src",
        "tests/asset_integrity/test_data/real_inspection",
        "tests/drilling_riser/fixtures",
    ):
        directory = repo / relative
        directory.mkdir(parents=True, exist_ok=True)
        (directory / "sentinel.txt").write_text("original", encoding="utf-8")

    checker.prepare_shadow(repo, shadow)

    copied_inputs = ("assets", "atlases", "data", "src", "tests")
    assert all(not (shadow / name).is_symlink() for name in copied_inputs)
    (shadow / "src" / "sentinel.txt").write_text("shadow", encoding="utf-8")
    assert (repo / "src" / "sentinel.txt").read_text(encoding="utf-8") == "original"


def test_registry_has_reasoned_complete_ownership() -> None:
    outputs = checker.all_registered_outputs(checker.REPO)

    assert checker.validate_registry(checker.REPO) == []
    assert len(checker.GENERATORS) == 19
    assert len(outputs) == 54
    assert checker.PAGE_EXCLUSIONS
    assert all(reason.strip() for reason in checker.PAGE_EXCLUSIONS.values())
    assert len(checker.PAGE_EXCLUSIONS) == 22
    assert len(checker.EXCLUDED_GENERATORS) == 9
    assert len(checker.MANUAL_PAGES) == 11


def test_page_census_rejects_unclassified_html(tmp_path: Path) -> None:
    pages = (
        tmp_path / "docs" / "api" / "new-output.HTML",
        tmp_path / "docs" / "api" / "_assets" / "new-output.html",
    )
    for page in pages:
        page.parent.mkdir(parents=True, exist_ok=True)
        page.write_text("<html></html>", encoding="utf-8")

    errors = checker.validate_page_census(tmp_path)
    message = "\n".join(errors)

    assert "docs/api/new-output.HTML" in message
    assert "docs/api/_assets/new-output.html" in message
