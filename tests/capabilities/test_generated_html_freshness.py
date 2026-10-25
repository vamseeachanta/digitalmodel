# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Generated-page/generator agreement gate — digitalmodel#1888.

The brand migration (#1474/#1485/#1487) was applied to generated pages under
``docs/api/**`` instead of to the generators that emit them, so re-running any
generator silently reverted the brand on a public surface.

These tests fix that class of defect in place: every registered generator is run
into an isolated shadow tree and its output is byte-compared against the
committed page, and the regenerated page — not the committed one — is what must
be free of hard-coded core brand tokens.
"""

from pathlib import Path

import pytest

from scripts import brand_guard
from scripts import check_generated_html as checker


@pytest.fixture(scope="module")
def regenerated(tmp_path_factory: pytest.TempPathFactory) -> Path:
    """Every registered generator, re-run against a copy of the repository."""
    shadow = tmp_path_factory.mktemp("generated-html-shadow")
    checker.prepare_shadow(checker.REPO, shadow)
    assert checker.regenerate(shadow) == []
    return shadow


def test_every_registered_page_is_byte_identical_to_its_generator(
    regenerated: Path,
) -> None:
    drifted = [path for path, _diff in checker.compare_outputs(checker.REPO, regenerated)]

    assert drifted == []


def test_regenerated_pages_inherit_core_brand_tokens(regenerated: Path) -> None:
    offenders = sorted(
        path.relative_to(regenerated).as_posix()
        for path in checker.all_registered_outputs(regenerated)
        if brand_guard.CORE_RE.search(path.read_text(encoding="utf-8", errors="ignore"))
    )

    assert offenders == []


def test_discovery_includes_non_write_text_generator(tmp_path: Path) -> None:
    script = tmp_path / "scripts" / "build_page.py"
    script.parent.mkdir()
    script.write_text(
        'from pathlib import Path\n'
        'FORMAT = "HTML"\n'
        'output = Path("docs") / "api" / f"example.{FORMAT.lower()}"\n'
        'with open(output, "w", encoding="utf-8", newline="\\n") as stream:\n'
        '    stream.write("<html></html>")\n',
        encoding="utf-8",
        newline="\n",
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
        (directory / "sentinel.txt").write_text("original", encoding="utf-8", newline="\n")

    checker.prepare_shadow(repo, shadow)

    copied_inputs = ("assets", "atlases", "data", "src", "tests")
    assert all(not (shadow / name).is_symlink() for name in copied_inputs)
    (shadow / "src" / "sentinel.txt").write_text("shadow", encoding="utf-8", newline="\n")
    assert (repo / "src" / "sentinel.txt").read_text(encoding="utf-8") == "original"


def test_registry_has_reasoned_complete_ownership() -> None:
    outputs = checker.all_registered_outputs(checker.REPO)

    assert checker.validate_registry(checker.REPO) == []
    assert len(checker.GENERATORS) == 25
    assert len(outputs) == 29  # includes report sidecars and published copies
    assert {
        "docs/reports/modules/materials/materials-module-report.html",
        "docs/reports/modules/materials/changes.json",
    } <= {path.relative_to(checker.REPO).as_posix() for path in outputs}
    assert all(reason.strip() for reason in checker.PAGE_EXCLUSIONS.values())
    assert len(checker.PAGE_EXCLUSIONS) == 21
    assert len(checker.EXCLUDED_GENERATORS) == 9
    # The verification generator needs a caller-supplied solved case;
    # repository inputs alone cannot reproduce its page.
    assert set(checker.EXCLUDED_GENERATORS) == {
        "scripts/cfd/generate-kcs-verification.py",
        "docs/api/cfd/report_build/build_dam_break.py",
        "docs/api/cfd/report_build/build_decay_report.py",
        "docs/api/cfd/report_build/build_floating_body.py",
        "docs/api/cfd/report_build/build_kleefsman.py",
        "docs/api/cfd/report_build/build_wave_excited_body.py",
        "docs/api/cfd/report_build/build_wave_excited_body_rao.py",
        "docs/api/cfd/report_build/build_wave_tank.py",
        "docs/api/cfd/report_build/upgrade_reports.py",
    }
    # The solved-case verification page is counted as manual because its
    # solver output is unavailable for repository-only regeneration.
    assert len(checker.MANUAL_PAGES) == 12
    assert set(checker.MANUAL_PAGES) == {
        'docs/api/buckling/ship-panel-buckling.html',
        'docs/api/buckling/ship-plate-buckling.html',
        'docs/api/capabilities/index.html',
        'docs/api/cfd/index.html',
        'docs/api/cfd/kcs-calm-water-resistance-verification.html',
        'docs/api/cfd/tank-sloshing-verification.html',
        'docs/api/ffs/build-wave-2026-07.html',
        'docs/api/ffs/ffs-field-dashboard.html',
        'docs/api/ffs/ffs-showcase.html',
        'docs/api/hydro/rudder-maneuvering-explorer.html',
        'docs/api/hydro/short-horizon-motion-forecast.html',
        'docs/api/subsea/offshore-cross-section-report.html',
    }


def test_page_census_rejects_unclassified_html(tmp_path: Path, monkeypatch) -> None:
    monkeypatch.setattr(checker, "build_output_pages", lambda repo: set())
    pages = (
        tmp_path / "docs" / "api" / "new-output.HTML",
        tmp_path / "docs" / "api" / "_assets" / "new-output.html",
    )
    for page in pages:
        page.parent.mkdir(parents=True, exist_ok=True)
        page.write_text("<html></html>", encoding="utf-8", newline="\n")

    errors = checker.validate_page_census(tmp_path)
    message = "\n".join(errors)

    assert "docs/api/new-output.HTML" in message
    assert "docs/api/_assets/new-output.html" in message


@pytest.fixture
def build_output_registry(tmp_path, monkeypatch):
    from scripts import generated_html_ownership as ownership

    script = "scripts/capabilities/build_onepagers.py"
    source = tmp_path / script
    source.parent.mkdir(parents=True)
    source.write_text('SPECS: list = [dict(id="example")]\n', encoding="utf-8", newline="\n")
    monkeypatch.setattr(checker, "GENERATORS", ())
    for name in ("DISCOVERY_FALSE_POSITIVES", "EXCLUDED_GENERATORS", "PAGE_EXCLUSIONS", "MANUAL_PAGES"):
        monkeypatch.setattr(checker, name, {})
    monkeypatch.setattr(checker, "discover_candidate_generators", lambda repo: {script})
    monkeypatch.setattr(checker, "build_output_pages", lambda repo: set())
    entries = {script: {
        "command": "python3 " + script,
        "output_pattern": "docs/api/capabilities/pdf/<specification-id>.html",
        "reason": "synthetic working-tree print source",
    }}
    monkeypatch.setattr(ownership, "BUILD_OUTPUT_GENERATORS", entries)
    monkeypatch.setattr(ownership, "DISCOVERY_FALSE_POSITIVES", {})
    monkeypatch.setattr(checker, "BUILD_OUTPUT_GENERATORS", entries, raising=False)
    return tmp_path, script, entries, ownership


@pytest.mark.parametrize(
    "fault,field,value",
    [
        ("missing script", "script", "scripts/missing.py"),
        ("empty reason", "reason", ""),
        ("empty command", "command", ""),
        ("empty pattern", "output_pattern", ""),
        ("missing placeholder", "output_pattern", "docs/api/example.html"),
        ("outside docs/api", "output_pattern", "docs/example/<specification-id>.html"),
        ("non-HTML pattern", "output_pattern", "docs/api/<specification-id>.pdf"),
        ("extra placeholder", "output_pattern", "docs/api/<other>/<specification-id>.html"),
        ("repeated placeholder", "output_pattern", "docs/api/<specification-id>/<specification-id>.html"),
        ("parent traversal", "output_pattern", "docs/api/../<specification-id>.html"),
    ],
)
def test_build_output_registry_rejects_invalid_entries(build_output_registry, fault, field, value):
    repo, script, entries, _ownership = build_output_registry
    if field == "script":
        entries[value] = entries.pop(script)
        script = value
    else:
        entries[script][field] = value
    message = "\n".join(checker.validate_registry(repo))
    assert script in message
    expected = "missing script" if field == "script" else field.replace("_", " ")
    assert f"build-output generator {expected}" in message, (fault, message)


def test_build_output_generator_cannot_be_discovery_false_positive(build_output_registry):
    repo, script, _entries, _ownership = build_output_registry
    checker.DISCOVERY_FALSE_POSITIVES[script] = "synthetic explanation"
    assert f"generator has conflicting classifications: {script}" in checker.validate_registry(repo)


def test_build_output_enumeration_uses_every_registry_entry(build_output_registry, monkeypatch):
    repo, script, entries, ownership = build_output_registry
    second = repo / "scripts/second.py"
    second.write_text('SPECS: list = [dict(id="second")]\n', encoding="utf-8", newline="\n")
    entries[script]["output_pattern"] = "docs/api/print/<specification-id>.html"
    entries["scripts/second.py"] = dict(entries[script], output_pattern="docs/api/second/<specification-id>.html")
    monkeypatch.setattr(ownership, "BUILD_OUTPUT_GENERATORS", entries)
    assert ownership.build_output_pages(repo) == {"docs/api/print/example.html", "docs/api/second/second.html"}


@pytest.mark.parametrize("classification", ["manual", "excluded", "owned"])
def test_build_output_page_conflicts_with_other_classes(tmp_path, monkeypatch, classification):
    page = "docs/api/example.html"
    destination = tmp_path / page
    destination.parent.mkdir(parents=True)
    destination.write_text("<html></html>\n", encoding="utf-8", newline="\n")
    monkeypatch.setattr(checker, "build_output_pages", lambda repo: {page})
    monkeypatch.setattr(checker, "MANUAL_PAGES", {page: "synthetic manual page"} if classification == "manual" else {})
    monkeypatch.setattr(checker, "PAGE_EXCLUSIONS", {page: "synthetic exclusion"} if classification == "excluded" else {})
    monkeypatch.setattr(checker, "all_registered_outputs", lambda repo: {destination} if classification == "owned" else set())
    assert checker.validate_page_census(tmp_path) == [f"page has conflicting classifications: {page}"]


@pytest.mark.parametrize(
    "source,expected,found",
    [
        ("SPECS = []\n", "annotated assignment named SPECS", "no annotated SPECS"),
        ("SPECS: list = ()\n", "literal list", "Tuple"),
        ("SPECS: list\n", "literal list", "NoneType"),
        ('SPECS: list = [{"id": "example"}]\n', "dict(...) call with a literal id", "Dict"),
        ('SPECS: list = [other(id="example")]\n', "dict(...) call with a literal id", "Call"),
        ("SPECS: list = [dict()]\n", "dict(...) call with a literal id", "missing id"),
        ("SPECS: list = [dict(id=identifier)]\n", "dict(...) call with a literal id", "Name"),
        ('SPECS: list = [dict(id="")]\n', "dict(...) call with a literal id", "empty string"),
        ("SPECS: list = [dict(id=1)]\n", "dict(...) call with a literal id", "int"),
        ("SPECS: list = []\n", "at least one specification id", "empty list"),
    ],
    ids=["no-annotated-SPECS", "not-list", "no-list-value", "not-call", "not-dict", "missing-id", "nonliteral-id", "empty-id", "nonstring-id", "no-ids"],
)
def test_build_output_specs_fail_with_clear_error(build_output_registry, source, expected, found):
    repo, script, _entries, ownership = build_output_registry
    (repo / script).write_text(source, encoding="utf-8", newline="\n")
    with pytest.raises(ValueError) as error:
        ownership.build_output_pages(repo)
    message = str(error.value)
    assert script in message
    assert expected in message
    assert found in message


def test_census_reports_malformed_specification_error(build_output_registry, monkeypatch):
    repo, script, _entries, ownership = build_output_registry
    (repo / script).write_text("SPECS: list = []\n", encoding="utf-8", newline="\n")
    monkeypatch.setattr(checker, "build_output_pages", ownership.build_output_pages)
    assert checker.validate_page_census(repo) == [
        f"{script}: expected at least one specification id; found empty list"
    ]


def test_real_build_output_pages_match_specifications():
    from scripts.capabilities.build_onepagers import SPECS
    from scripts.generated_html_ownership import build_output_pages

    assert build_output_pages(checker.REPO) == set()
    assert checker.validate_page_census(checker.REPO) == []


def test_registry_reports_unsupported_specs(build_output_registry, monkeypatch):
    repo, script, _entries, ownership = build_output_registry
    (repo / script).write_text("SPECS: list = ()\n", encoding="utf-8", newline="\n")
    monkeypatch.setattr(checker, "build_output_pages", ownership.build_output_pages)
    monkeypatch.setattr(
        checker, "discover_candidate_generators",
        lambda root: {script, "scripts/unregistered.py"},
    )
    errors = checker.validate_registry(repo)
    assert f"{script}: expected SPECS to be a literal list; found Tuple" in errors
    assert "unregistered candidate generator(s): scripts/unregistered.py" in errors


def test_check_reports_unsupported_specs_without_traceback(
    build_output_registry, monkeypatch, capsys,
):
    repo, script, _entries, ownership = build_output_registry
    (repo / script).write_text("SPECS: list = ()\n", encoding="utf-8", newline="\n")
    monkeypatch.setattr(checker, "build_output_pages", ownership.build_output_pages)
    assert checker.check(repo) == 1
    assert capsys.readouterr().out == (
        "GENERATED HTML CHECK FAILED — registry coverage error\n"
        f"  x {script}: expected SPECS to be a literal list; found Tuple\n"
    )


def test_build_output_enumeration_accepts_repository_root(tmp_path, monkeypatch):
    from scripts import generated_html_ownership as ownership

    monkeypatch.setattr(ownership, "BUILD_OUTPUT_GENERATORS", {
        "scripts/capabilities/build_onepagers.py": {
            "output_pattern": "docs/api/capabilities/pdf/<specification-id>.html",
        },
    })
    script = tmp_path / "scripts/capabilities/build_onepagers.py"
    script.parent.mkdir(parents=True)
    script.write_text('SPECS: list = [dict(id="temporary-root")]\n', encoding="utf-8", newline="\n")
    assert ownership.build_output_pages(tmp_path) == {
        "docs/api/capabilities/pdf/temporary-root.html"
    }


@pytest.mark.parametrize(
    "identifier",
    ["a/b", "a.b", "..", "Upper", "under_score", "a b", ""],
    ids=["slash", "dot", "parent", "uppercase", "underscore", "space", "empty"],
)
def test_build_output_rejects_invalid_id_forms(build_output_registry, identifier):
    repo, script, _entries, ownership = build_output_registry
    (repo / script).write_text(
        f"SPECS: list = [dict(id={identifier!r})]\n", encoding="utf-8", newline="\n",
    )
    with pytest.raises(ValueError) as error:
        ownership.build_output_pages(repo)
    message = str(error.value)
    assert script in message
    assert "SPECS[0]" in message
    assert "[a-z0-9]+(?:-[a-z0-9]+)*" in message
    assert repr(identifier) in message


def test_build_output_rejects_duplicate_ids(build_output_registry):
    repo, script, _entries, ownership = build_output_registry
    (repo / script).write_text(
        'SPECS: list = [dict(id="repeat"), dict(id="other"), dict(id="repeat")]\n',
        encoding="utf-8",
        newline="\n",
    )
    with pytest.raises(ValueError) as error:
        ownership.build_output_pages(repo)
    message = str(error.value)
    assert script in message
    assert "repeat" in message
    assert "SPECS[0]" in message
    assert "SPECS[2]" in message
    assert "duplicate" in message


@pytest.mark.parametrize(
    "identifier", ["-", "-leading", "trailing-", "double--hyphen"],
    ids=["lone", "leading", "trailing", "doubled"],
)
def test_build_output_rejects_hyphen_edges(build_output_registry, identifier):
    repo, script, _entries, ownership = build_output_registry
    (repo / script).write_text(
        f"SPECS: list = [dict(id={identifier!r})]\n", encoding="utf-8", newline="\n",
    )
    with pytest.raises(ValueError) as error:
        ownership.build_output_pages(repo)
    message = str(error.value)
    assert script in message and "SPECS[0]" in message
    assert repr(identifier) in message
    assert "[a-z0-9]+(?:-[a-z0-9]+)*" in message


def test_build_output_accepts_single_internal_hyphens(build_output_registry):
    """Regression pin: single internal hyphens remain accepted."""
    repo, script, _entries, ownership = build_output_registry
    (repo / script).write_text(
        'SPECS: list = [dict(id="part-2-example")]\n', encoding="utf-8", newline="\n",
    )
    assert ownership.build_output_pages(repo) == {
        "docs/api/capabilities/pdf/part-2-example.html"
    }


def test_failed_enumeration_withholds_unclassified_pages(
    build_output_registry, monkeypatch, capsys,
):
    repo, script, _entries, ownership = build_output_registry
    (repo / script).write_text("SPECS: list = ()\n", encoding="utf-8", newline="\n")
    page = repo / "docs/api/capabilities/pdf/example.html"
    page.parent.mkdir(parents=True)
    page.write_text("<title>Example</title>", encoding="utf-8", newline="\n")
    monkeypatch.setattr(checker, "build_output_pages", ownership.build_output_pages)
    checker.MANUAL_PAGES["docs/api/missing.html"] = "synthetic manual page"
    assert checker.check(repo) == 1
    text = capsys.readouterr().out
    assert "GENERATED HTML CHECK FAILED — registry coverage error" in text
    assert f"{script}: expected SPECS to be a literal list; found Tuple" in text
    assert "stale manual-page classification(s): docs/api/missing.html" in text
    assert "unclassified" not in text
    assert "docs/api/capabilities/pdf/example.html" not in text


def test_working_enumeration_reports_unclassified_page(
    build_output_registry, monkeypatch, capsys,
):
    """Regression pin: an unknown page is still reported after enumeration."""
    repo, _script, _entries, ownership = build_output_registry
    page = repo / "docs/api/capabilities/pdf/unknown.html"
    page.parent.mkdir(parents=True)
    page.write_text("<title>Unknown</title>", encoding="utf-8", newline="\n")
    monkeypatch.setattr(checker, "build_output_pages", ownership.build_output_pages)
    assert checker.check(repo) == 1
    assert "unclassified docs/api HTML page(s): docs/api/capabilities/pdf/unknown.html" in capsys.readouterr().out


def test_registry_reports_registered_script_syntax_error(
    build_output_registry, monkeypatch, capsys,
):
    repo, script, _entries, ownership = build_output_registry
    (repo / script).write_text("SPECS: list = [\n", encoding="utf-8", newline="\n")
    monkeypatch.setattr(checker, "build_output_pages", ownership.build_output_pages)
    errors = checker.validate_registry(repo)
    assert len(errors) == 1
    assert script in errors[0]
    assert "1" in errors[0] and "never closed" in errors[0]
    assert checker.check(repo) == 1
    text = capsys.readouterr().out
    assert text == (
        "GENERATED HTML CHECK FAILED — registry coverage error\n"
        f"  x {errors[0]}\n"
    )
    assert "Traceback" not in text
