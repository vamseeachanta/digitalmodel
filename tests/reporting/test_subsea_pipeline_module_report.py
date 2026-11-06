# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Exercise report arithmetic, qualification boundaries and reproducible output.

Independent basis: polar area and second-moment integrals over radii 1..2 m
give A=3*pi, Ai=pi, Ao=4*pi m^2 and I=15*pi/4 m^4; t=1 m.
Uniform strain +/-1/5000 integrated over 1000 m gives +/-1/5 m.
Arbitrary supplied capacity 100 MPa times 4/5, 9/10, 3/5 gives 80/90/60 MPa.
These synthetic calculations supply every numeric expectation below. The
1e-12 comparison tolerance permits floating-point rounding only. Document
structure, SHA-256 coverage and output names follow the module report contract.
"""

import ast
import hashlib
import html
import importlib.util
import json
import math
import os
from pathlib import Path
import re
import stat
import subprocess
import sys
from types import SimpleNamespace

import pytest

ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts/reports/build_subsea_pipeline_module_report.py"
REPORT = ROOT / "docs/reports/modules/subsea_pipeline"
DELIVERABLES = {"index.html", "changes.json", "manifest.json", "README.md"}
REBUILD = "PYTHONPATH=src python scripts/reports/build_subsea_pipeline_module_report.py"


@pytest.fixture(params=["utf-8", "cp1252"], autouse=True)
def simulated_text_locale(request, monkeypatch):
    """Exercise report reads with UTF-8 and a legacy locale default."""
    original = Path.read_text

    def read_text(path, encoding=None, errors=None):
        return original(
            path,
            encoding=request.param if encoding is None else encoding,
            errors=errors,
        )

    monkeypatch.setattr(Path, "read_text", read_text)


def builder():
    """Load the standalone builder without invoking a CLI or workflow."""
    assert SCRIPT.is_file(), "The module report builder is missing"
    spec = importlib.util.spec_from_file_location("subsea_pipeline_report", SCRIPT)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


@pytest.fixture
def isolated_checkout(tmp_path, monkeypatch):
    """Confine destructive regressions to copied sources and disposable metadata."""
    build = builder()
    checkout = tmp_path / "checkout"
    for relative in build.source_digests():
        target = checkout / relative
        target.parent.mkdir(parents=True, exist_ok=True)
        target.write_bytes((ROOT / relative).read_bytes())
    (checkout / "README.md").write_bytes(b"Unrelated checkout documentation.\n")
    (checkout / ".git").mkdir()
    (checkout / ".git/HEAD").write_bytes(b"ref: refs/heads/example\n")
    monkeypatch.setattr(build, "ROOT", checkout)
    monkeypatch.setattr(build, "PACKAGE", checkout / "src/digitalmodel/subsea/pipeline")
    monkeypatch.setattr(
        build, "DESTINATION", checkout / "docs/reports/modules/subsea_pipeline"
    )
    return build


def _checkout_bytes(checkout):
    """Snapshot all fixture files, including the caller's Git metadata."""
    return {
        path.relative_to(checkout).as_posix(): path.read_bytes()
        for path in checkout.rglob("*")
        if path.is_file()
    }


def _symlink_or_skip(link, target, *, target_is_directory=False):
    """Keep link regressions runnable on systems without link privileges."""
    try:
        link.symlink_to(target, target_is_directory=target_is_directory)
    except OSError as error:
        pytest.skip(f"Symbolic link creation is unavailable: {error}")


@pytest.mark.parametrize("relative", [".", "src/digitalmodel/subsea/pipeline"])
def test_checkout_output_is_rejected_without_changes(isolated_checkout, relative):
    """Reject checkout outputs before they can replace documentation or sources."""
    build = isolated_checkout
    before = _checkout_bytes(build.ROOT)
    with pytest.raises(ValueError, match="[Oo]utput"):
        build.build_report(output_dir=build.ROOT / relative)
    assert _checkout_bytes(build.ROOT) == before


@pytest.mark.parametrize("name", sorted(DELIVERABLES))
@pytest.mark.parametrize("dangling", [False, True])
def test_linked_deliverable_is_rejected_without_writes(tmp_path, name, dangling):
    """Reject every existing or dangling output link before any deliverable write."""
    output = tmp_path / "output"
    output.mkdir()
    target = tmp_path / "unrelated.txt"
    if not dangling:
        target.write_bytes(b"Unrelated retained bytes.\n")
    _symlink_or_skip(output / name, target)
    with pytest.raises(ValueError, match="[Ll]ink"):
        builder().build_report(output_dir=output)
    assert {path.name for path in output.iterdir()} == {name}
    assert (output / name).is_symlink()
    if dangling:
        assert not target.exists()
    else:
        assert target.read_bytes() == b"Unrelated retained bytes.\n"


@pytest.mark.parametrize("nested", [False, True])
@pytest.mark.parametrize("inside_checkout", [False, True])
def test_output_alias_uses_resolved_destination(
    isolated_checkout, tmp_path, nested, inside_checkout
):
    """Allow outside aliases while protecting unrelated resolved checkout paths."""
    build = isolated_checkout
    retained = build.ROOT if inside_checkout else tmp_path / "retained"
    retained.mkdir(exist_ok=True)
    before = _checkout_bytes(retained)
    alias = tmp_path / "alias"
    _symlink_or_skip(alias, retained, target_is_directory=True)
    output = alias / "nested" if nested else alias
    if inside_checkout:
        with pytest.raises(ValueError, match="Checkout output"):
            build.build_report(output_dir=output)
        assert _checkout_bytes(retained) == before
    else:
        build.build_report(output_dir=output)
        resolved_output = retained / "nested" if nested else retained
        assert {path.name for path in resolved_output.iterdir()} == DELIVERABLES


@pytest.mark.parametrize(
    "contents",
    [
        {"README.md": b"Retained documentation.\n"},
        {"other.txt": b"Unrelated data.\n", "manifest.json": b'{"algorithm":"sha256"}'},
        {"manifest.json": b"not JSON"},
        {"manifest.json": b"[]"},
        {"manifest.json": b'{"algorithm":"md5"}'},
        {"manifest.json": b"\xff"},
    ],
    ids=["unrelated-readme", "extra-file", "invalid-json", "non-object", "wrong-algorithm", "invalid-utf8"],
)
def test_unrelated_outside_output_is_rejected_without_writes(tmp_path, contents):
    """Do not replace neighbouring documentation or an unrecognised report."""
    output = tmp_path / "outside"
    output.mkdir()
    for name, content in contents.items():
        (output / name).write_bytes(content)
    before = _checkout_bytes(output)
    with pytest.raises(ValueError, match="[Oo]utput"):
        builder().build_report(output_dir=output)
    assert _checkout_bytes(output) == before


@pytest.mark.parametrize("existing", [False, True])
def test_outside_report_can_be_built_and_rebuilt(tmp_path, existing):
    """Permit absent or empty destinations and later recognised report rebuilds."""
    output = tmp_path / "outside"
    if existing:
        output.mkdir()
    build = builder()
    build.build_report(output_dir=output)
    before = _checkout_bytes(output)
    build.build_report(output_dir=output)
    assert _checkout_bytes(output) == before
    assert set(before) == DELIVERABLES


def test_link_creation_without_privilege_skips(tmp_path, monkeypatch):
    """Report missing link privileges as a skip rather than a test error."""
    def denied(*args, **kwargs):
        raise OSError("Symbolic link creation is unavailable")

    monkeypatch.setattr(Path, "symlink_to", denied)
    with pytest.raises(pytest.skip.Exception, match="[Ss]ymbolic link"):
        test_linked_deliverable_is_rejected_without_writes(tmp_path, "README.md", False)


@pytest.mark.parametrize(
    "reparse_tag",
    [0xA000000C, 0xA0000003, 0xA000001D],
    ids=["symlink", "junction", "wsl-symlink"],
)
@pytest.mark.parametrize("target_name", ["output", "README.md"])
def test_output_reparse_metadata_preserves_target_guard(
    tmp_path, monkeypatch, target_name, reparse_tag
):
    """Allow resolved directories while rejecting linked deliverable targets."""
    output = tmp_path / "output"
    output.mkdir()
    target = output if target_name == "output" else output / target_name
    if target != output:
        target.mkdir()
    original = Path.lstat

    def reparse_metadata(path, *args, **kwargs):
        result = original(path, *args, **kwargs)
        if path == target:
            return SimpleNamespace(
                st_mode=result.st_mode,
                st_file_attributes=stat.FILE_ATTRIBUTE_REPARSE_POINT,
                st_reparse_tag=reparse_tag,
            )
        return result

    monkeypatch.setattr(Path, "lstat", reparse_metadata)
    if target == output:
        builder().build_report(output_dir=output)
        assert {path.name for path in output.iterdir()} == DELIVERABLES
    else:
        with pytest.raises(ValueError, match="[Ll]ink"):
            builder().build_report(output_dir=output)
        assert {path.name for path in output.iterdir()} == {target_name}


@pytest.mark.parametrize("name", sorted(DELIVERABLES))
@pytest.mark.parametrize(
    "reparse_tag", [0x9000001A, 0x9000301A], ids=["cloud", "cloud-variant"]
)
def test_cloud_placeholder_deliverable_can_be_rebuilt(
    tmp_path, monkeypatch, name, reparse_tag
):
    """Cloud placeholder metadata must permit rebuilding a recognised report."""
    build = builder()
    output = tmp_path / "output"
    build.build_report(output_dir=output)
    before = _checkout_bytes(output)
    original = Path.lstat

    def placeholder_metadata(path, *args, **kwargs):
        result = original(path, *args, **kwargs)
        if path == output / name:
            return SimpleNamespace(
                st_mode=result.st_mode,
                st_file_attributes=stat.FILE_ATTRIBUTE_REPARSE_POINT,
                st_reparse_tag=reparse_tag,
            )
        return result

    monkeypatch.setattr(Path, "lstat", placeholder_metadata)
    build.build_report(output_dir=output)
    assert _checkout_bytes(output) == before
    assert set(before) == DELIVERABLES


def test_canonical_report_destination_is_allowed(isolated_checkout):
    """Keep the documented in-checkout destination available for regeneration."""
    build = isolated_checkout
    before = _checkout_bytes(build.ROOT)
    build.build_report(output_dir=build.DESTINATION)
    assert {path.name for path in build.DESTINATION.iterdir()} == DELIVERABLES
    for name, content in before.items():
        assert (build.ROOT / name).read_bytes() == content


def test_annulus_payload_matches_integrals():
    """Catch omitted units, fields, dimension routes or substituted geometry."""
    data = builder().build_payload()["annulus"]
    assert data["inputs"] == {
        "outer_diameter": 4.0,
        "inner_diameter": 2.0,
        "wall_thickness": 1.0,
    }
    expected = {
        **data["inputs"],
        "A": 3 * math.pi,
        "Ai": math.pi,
        "Ao": 4 * math.pi,
        "I": 15 * math.pi / 4,
    }
    assert set(data["routes"]) == {"od_id", "od_t", "id_t", "od_id_t"}
    for result in data["routes"].values():
        assert result == pytest.approx(expected, rel=1e-12, abs=0.0)
    assert data["section"] == pytest.approx(
        {key: expected[key] for key in ("A", "Ai", "Ao", "I")}, rel=1e-12, abs=0.0
    )
    assert data["wall_thickness"] == 1.0
    assert data["derived_inner_diameter"] == 2.0
    assert data["derived_outer_diameter"] == 4.0
    assert data["consistent"] is True
    assert data["units"] == {
        "outer_diameter": "m",
        "inner_diameter": "m",
        "wall_thickness": "m",
        "A": "m^2",
        "Ai": "m^2",
        "Ao": "m^2",
        "I": "m^4",
    }
    assert data["evaluation"] == "HAND_CALCULATION_CHECKED"
    assert data["basis"] == "A"


def test_growth_payload_matches_uniform_strain():
    """Catch reversed cooling, coefficient substitution or unsupported restraint."""
    data = builder().build_payload()["growth"]
    assert data["inputs"] == {"length": 1000.0, "alpha": 1 / 100000}
    assert data["units"] == {
        "length": "m",
        "alpha": "K^-1",
        "temperature_change": "K",
        "strain": "1",
        "growth": "m",
    }
    assert len(data["cases"]) == 2
    for case, temperature, strain, growth in zip(
        data["cases"], [20.0, -20.0], [1 / 5000, -1 / 5000], [1 / 5, -1 / 5]
    ):
        assert case == pytest.approx(
            {"temperature_change": temperature, "strain": strain, "growth": growth},
            rel=1e-12,
            abs=0.0,
        )
    assert data["evaluation"] == "HAND_CALCULATION_CHECKED"
    assert data["buckling_and_walking"] == "NOT_EVALUATED"
    assert data["basis"] == "B"


def test_pressure_factor_payload_matches_supplied_arithmetic():
    """Catch accidental use of a calculated burst capacity or code factors."""
    data = builder().build_payload()["pressure_factors"]
    assert data["inputs"] == {
        "base_capacity": 100.0,
        "factors": {
            "design": 4 / 5,
            "incidentalPressure": 9 / 10,
            "hydroStaticTest": 3 / 5,
        },
    }
    assert data["results"] == pytest.approx(
        {"design": 80.0, "accidental": 90.0, "test": 60.0}, rel=1e-12, abs=0.0
    )
    assert data["units"] == {"base_capacity": "MPa", "factors": "1", "results": "MPa"}
    assert data["evaluation"] == "ARITHMETIC_ONLY"
    assert data["capacity_qualification"] == "NOT_EVALUATED"
    assert data["factor_qualification"] == "NOT_EVALUATED"
    assert data["basis"] == "C"


def _assert_manifest(directory):
    """Verify exact source and written-artifact bytes with an external digest."""
    manifest = json.loads((directory / "manifest.json").read_text(encoding="utf-8"))
    assert manifest["algorithm"] == "sha256"
    assert set(manifest["artifacts"]) == DELIVERABLES - {"manifest.json"}
    assert (
        manifest["self_digest"]
        == "Excluded to avoid a self-referential digest; compare manifest.json bytes externally."
    )
    for name, digest in manifest["artifacts"].items():
        assert digest == hashlib.sha256((directory / name).read_bytes()).hexdigest(), (
            f"Rebuild with {REBUILD}"
        )
    sources = set(manifest["sources"])
    assert sources == {
        SCRIPT.relative_to(ROOT).as_posix(),
        "src/digitalmodel/subsea/pipeline/calculations/pipe_properties.py",
        "src/digitalmodel/subsea/pipeline/calculations/stress_calculations.py",
        "src/digitalmodel/subsea/pipeline/thermal_expansion.py",
        "src/digitalmodel/units.py",
        "tests/subsea/pipeline/test_module_report_hand_values.py",
        "tests/reporting/test_subsea_pipeline_module_report.py",
    }
    for name, digest in manifest["sources"].items():
        assert not Path(name).is_absolute()
        assert digest == hashlib.sha256((ROOT / name).read_bytes()).hexdigest(), (
            f"Rebuild with {REBUILD}"
        )
    return manifest


def test_rebuild_is_deterministic(tmp_path):
    """Catch ambient paths, timestamps, incomplete output or stale committed bytes."""
    build = builder()
    first, second = tmp_path / "first", tmp_path / "second"
    build.build_report(scratch=tmp_path / "scratch-a", output_dir=first)
    build.build_report(scratch=tmp_path / "scratch-b", output_dir=second)
    report = (first / "index.html").read_text(encoding="utf-8")
    panel = report.split('<aside id="change-panel">', 1)[1].split("</aside>", 1)[0]
    assert html.escape(build.CHANGES[-1]["change"]) in panel
    assert {path.name for path in first.iterdir()} == DELIVERABLES
    assert {path.name for path in second.iterdir()} == DELIVERABLES
    for name in DELIVERABLES:
        assert (first / name).read_bytes() == (second / name).read_bytes(), (
            f"Rebuild with {REBUILD}"
        )
        assert (first / name).read_bytes() == (REPORT / name).read_bytes(), (
            f"Rebuild with {REBUILD}"
        )
    assert _assert_manifest(first) == _assert_manifest(second), (
        f"Rebuild with {REBUILD}"
    )
    assert re.findall(r'<h2 id="s\d+">(.*?)</h2>', report) == [
        "1. Executive Summary",
        "2. Introduction",
        "3. Design Data and Basis",
        "4. Analysis Methodology",
        "5. Detailed Results",
        "6. Key Findings",
        "7. Limitations and Not Covered",
        "8. Recommendations",
        "9. References and Appendices",
        "10. Acronyms and Review Notes",
    ]
    summary = report.split('<h2 id="s1">', 1)[1].split('<h2 id="s2">', 1)[0]
    assert re.search(r"</p>\s*<h3>Revision history</h3>", summary)
    assert 'id="change-panel"' in summary
    assert "free thermal growth does not predict buckling or walking" in report
    assert "factor multiplication does not qualify its supplied capacity" in report
    payload = json.loads(
        re.search(
            r'<script id="calculation" type="application/json">(.*?)</script>',
            report,
            re.S,
        )[1]
    )
    assert payload == build.build_payload()
    changes = json.loads((first / "changes.json").read_text(encoding="utf-8"))
    assert len(changes) == 6
    assert set(changes[0]) == {"revision", "date", "change"}
    for change in changes:
        for value in change.values():
            assert value in summary


def test_cli_manifest_matches_written_files(tmp_path):
    """Exercise the CLI in a different cwd, preserving caller Git bindings."""
    builder()
    environment = dict(os.environ, PYTHONPATH=str(ROOT / "src"))
    # Hostile bindings are inert: the builder locates sources from its own file.
    environment.update(
        GIT_DIR=str(tmp_path / "absent-git"), GIT_WORK_TREE=str(tmp_path)
    )
    destination = tmp_path / "cli-output"
    subprocess.run(
        [
            sys.executable,
            str(SCRIPT),
            "--output-dir",
            str(destination),
        ],
        cwd=tmp_path,
        env=environment,
        check=True,
        capture_output=True,
        text=True,
        encoding="utf-8",
    )
    _assert_manifest(destination)
    assert {path.name for path in destination.iterdir()} == DELIVERABLES
    for name in DELIVERABLES:
        assert (destination / name).read_bytes() == (REPORT / name).read_bytes(), (
            f"Rebuild with {REBUILD}"
        )


@pytest.mark.parametrize("include_async", [False, True], ids=["current", "with-async"])
def test_inventory_covers_public_methods_without_transferring_qualification(
    monkeypatch, include_async
):
    """Every public declaration has a locator; engineering methods stay unevaluated."""
    if include_async:
        original = Path.read_text
        target = ROOT / "src/digitalmodel/subsea/pipeline/free_span/span_allowable_length.py"

        def with_async_declarations(path, *args, **kwargs):
            text = original(path, *args, **kwargs)
            if path == target:
                text += (
                    "\nasync def untested_async_function():\n    pass\n"
                    "\nclass UntestedAsyncClass:\n"
                    "    async def untested_async_method(self):\n        pass\n"
                )
            return text

        monkeypatch.setattr(Path, "read_text", with_async_declarations)
    inventory = builder().build_payload()["methods"]
    expected = set()
    for source in (ROOT / "src/digitalmodel/subsea/pipeline").rglob("*.py"):
        path = source.relative_to(ROOT).as_posix()
        for node in ast.parse(source.read_text(encoding="utf-8")).body:
            if isinstance(
                node, (ast.FunctionDef, ast.AsyncFunctionDef)
            ) and not node.name.startswith("_"):
                expected.add((path, node.name))
            if isinstance(node, ast.ClassDef) and not node.name.startswith("_"):
                for method in node.body:
                    if isinstance(
                        method, (ast.FunctionDef, ast.AsyncFunctionDef)
                    ) and not method.name.startswith("_"):
                        expected.add((path, f"{node.name}.{method.name}"))
    assert {(row["path"], row["symbol"]) for row in inventory} == expected
    assert all("line" not in row for row in inventory)
    assert {row["category"] for row in inventory} == {
        "geometry",
        "thermal strain",
        "buckling",
        "span",
        "pressure",
        "workflow",
    }
    qualified = {
        (row["path"], row["symbol"]): row["evaluation"]
        for row in inventory
        if row["evaluation"] != "NOT_EVALUATED"
    }
    prefix = "src/digitalmodel/subsea/pipeline/"
    expected_qualified = {
        (prefix + "calculations/pipe_properties.py", symbol): "HAND_CALCULATION_CHECKED"
        for symbol in (
            "calculate_section_properties",
            "calculate_wall_thickness",
            "calculate_diameter_from_thickness",
            "calculate_geometric_properties",
            "validate_pipe_geometry",
        )
    }
    expected_qualified[(prefix + "thermal_expansion.py", "free_thermal_expansion")] = (
        "HAND_CALCULATION_CHECKED"
    )
    expected_qualified[
        (
            prefix + "calculations/stress_calculations.py",
            "APISTD2RDCalculations.calculate_design_pressures",
        )
    ] = "ARITHMETIC_ONLY"
    assert qualified == expected_qualified
    for row in inventory:
        assert row["evidence_needed"]
        if row["category"] in {"buckling", "span", "workflow"}:
            assert row["evaluation"] == "NOT_EVALUATED"
        if (
            row["category"] == "pressure"
            and row["symbol"] != "APISTD2RDCalculations.calculate_design_pressures"
        ):
            assert row["evaluation"] == "NOT_EVALUATED"


@pytest.mark.parametrize("scratch", [".", "docs/reports/modules/subsea_pipeline"])
def test_scratch_inside_checkout_is_rejected_before_writes(tmp_path, scratch):
    """An output path inside the checkout cannot turn scratch into retained data."""
    with pytest.raises(ValueError, match="outside the checkout"):
        builder().build_report(scratch=ROOT / scratch, output_dir=tmp_path / "out")
    assert not (tmp_path / "out").exists()


@pytest.mark.parametrize("relative", [".", "nested", "../external-scratch"])
def test_scratch_containment_resolves_checkout_alias(
    isolated_checkout, tmp_path, monkeypatch, relative
):
    """A checkout alias must preserve scratch rejection and external access."""
    build = isolated_checkout
    checkout = build.ROOT
    before = _checkout_bytes(checkout)
    alias = tmp_path / "checkout-alias"
    _symlink_or_skip(alias, checkout, target_is_directory=True)
    monkeypatch.setattr(build, "ROOT", alias)
    monkeypatch.setattr(build, "PACKAGE", alias / "src/digitalmodel/subsea/pipeline")
    output = tmp_path / "output"
    if relative == "../external-scratch":
        build.build_report(scratch=alias / relative, output_dir=output)
        assert {path.name for path in output.iterdir()} == DELIVERABLES
        assert not (tmp_path / "external-scratch").exists()
    else:
        with pytest.raises(ValueError, match="outside the checkout"):
            build.build_report(scratch=alias / relative, output_dir=output)
        assert not output.exists()
    assert _checkout_bytes(checkout) == before


@pytest.mark.parametrize(
    "relative, category",
    [
        ("calculations/pipe_properties.py", "geometry"),
        ("thermal_expansion.py", "thermal strain"),
    ],
)
def test_new_public_functions_do_not_inherit_qualification(
    tmp_path, monkeypatch, relative, category
):
    """Adding an untested public declaration cannot confer checked status."""
    build = builder()
    package = tmp_path / "package"
    source = package / relative
    source.parent.mkdir(parents=True, exist_ok=True)
    source.write_text(
        "def untested_function():\n    return 1\n", encoding="utf-8", newline="\n"
    )
    monkeypatch.setattr(build, "PACKAGE", package)
    monkeypatch.setattr(build, "ROOT", tmp_path)
    (row,) = build.method_inventory()
    assert row["symbol"] == "untested_function"
    assert row["category"] == category
    assert row["evaluation"] == "NOT_EVALUATED"
    # The same symbol name in another module has no evidence either.
    assert (
        build._qualification(
            package / "other.py", "free_thermal_expansion", "workflow"
        )[0]
        == "NOT_EVALUATED"
    )


def test_inventory_line_shifts_do_not_stale_report(monkeypatch):
    """Whitespace in an unevaluated module cannot invalidate delivered bytes."""
    build = builder()
    before = build.build_payload()
    sources = build.source_digests()
    original = Path.read_text
    target = build.PACKAGE / "free_span/span_allowable_length.py"

    def shifted(path, *args, **kwargs):
        text = original(path, *args, **kwargs)
        return "# Inventory line shift\n" + text if path == target else text

    monkeypatch.setattr(Path, "read_text", shifted)
    after = build.build_payload()
    assert after == before
    assert build.source_digests() == sources
    assert build.render(after, sources) == build.render(before, sources)


def test_incidental_output_mislabel_and_layout_are_disclosed():
    """Report the legacy key without treating two load categories as synonyms."""
    build = builder()
    page = build.render(build.build_payload(), build.source_digests())
    assert "incidental (returned as accidental)" in page
    for number, next_number in ((6, 7), (7, 8)):
        section = page.split(f'<h2 id="s{number}">', 1)[1].split(
            f'<h2 id="s{next_number}">', 1
        )[0]
        assert "incidentalPressure" in section
        assert "accidental" in section
        assert "distinct load categories" in section
    assert "Layout choice" in page
    assert "wide declaration inventory" in page
    assert "reserved" in build._readme()
