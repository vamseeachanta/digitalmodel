"""Destination regressions for the paired report writer (issue #2315).

Path expectations are independently constructed fixture names: replacing only
the final suffix maps ``analysis`` to ``analysis.md`` and ``analysis.html`` to
``analysis.md``; ``analysis.md`` would collide with itself. No engineering
calculation or acceptance criterion is exercised here.
"""

from hashlib import sha256

import pytest

from digitalmodel.subsea.catenary_riser.report import (
    RiserReportData,
    generate_riser_report,
)


def _data():
    # Synthetic display-only depth; no analysis is run (issue #2315).
    return RiserReportData("Example", "Example", "Example", water_depth_m=0)


@pytest.mark.parametrize("existing", [False, True])
@pytest.mark.parametrize("name", ["analysis.md", "analysis.MD"])
def test_markdown_destination_collision_is_rejected_before_writing(tmp_path, existing, name):
    output = tmp_path / name
    if existing:
        output.write_bytes(b"retained input")
    before = sha256(output.read_bytes()).digest() if existing else None

    with pytest.raises(ValueError, match="distinct"):
        generate_riser_report(_data(), output)

    if existing:
        assert sha256(output.read_bytes()).digest() == before
    else:
        assert not output.exists()


@pytest.mark.parametrize("alias", ["symlink", "hardlink", "dangling_symlink"])
def test_aliased_destinations_preserve_existing_bytes(tmp_path, alias):
    output = tmp_path / "analysis.html"
    twin = tmp_path / "analysis.md"
    if alias != "dangling_symlink":
        output.write_bytes(b"retained input")
    if alias == "hardlink":
        twin.hardlink_to(output)
    else:
        try:
            twin.symlink_to(output.name)
        except OSError:
            pytest.skip("Symlink creation is unavailable")
    before = sha256(output.read_bytes()).digest() if output.exists() else None

    with pytest.raises(ValueError, match="distinct"):
        generate_riser_report(_data(), output)

    if before is not None:
        assert sha256(output.read_bytes()).digest() == before
        assert sha256(twin.read_bytes()).digest() == before
    else:
        assert not output.exists()
        assert twin.is_symlink()


@pytest.mark.parametrize("name", ["analysis", "analysis.html"])
def test_extensionless_and_suffix_text_directory_names(tmp_path, name):
    directory = tmp_path / "archive.html.md"
    written = generate_riser_report(_data(), directory / name)

    assert written == {"html": directory / name, "markdown": directory / "analysis.md"}
    assert written["html"].read_text(encoding="utf-8").lstrip().lower().startswith("<!doctype html>")
    assert written["markdown"].read_text(encoding="utf-8").startswith("# ")


def test_single_output_may_use_markdown_suffix(tmp_path):
    output = tmp_path / "analysis.md"
    assert generate_riser_report(_data(), output, markdown=False) == {"html": output}
    assert output.read_text(encoding="utf-8").lstrip().lower().startswith("<!doctype html>")
