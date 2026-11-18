"""Multi-format destination expectations for issue #2315.

The expected names are hand-enumerated: base ``analysis`` produces
``analysis.html``, ``analysis.json`` and ``analysis.csv``. An aliased pair
must fail before the third destination is created. SHA-256 compares retained
fixture bytes, without deriving expectations from any exporter.
"""

import errno
from hashlib import sha256

import pytest

from digitalmodel.visualization.reporting.exporters import export_all_formats
from digitalmodel.visualization.reporting.models import ReportMetadata, StandardReport


def _report():
    return StandardReport(metadata=ReportMetadata(
        module="example", analysis_type="example", status="success",
        # Synthetic metadata only; no timing or engineering calculation.
        execution_time_seconds=0,
    ))


@pytest.mark.parametrize("pair", [("html", "json"), ("html", "csv"), ("json", "csv")])
def test_all_destinations_validated_before_first_export(tmp_path, pair):
    retained = tmp_path / "retained.data"
    retained.write_bytes(b"retained input")
    before = sha256(retained.read_bytes()).digest()
    for suffix in pair:
        try:
            (tmp_path / f"analysis.{suffix}").symlink_to(retained.name)
        except OSError as exc:
            if exc.errno not in (errno.EPERM, errno.EACCES) and getattr(exc, "winerror", None) != 1314:
                raise
            pytest.skip("Symlink creation is unavailable")

    with pytest.raises(ValueError, match="distinct"):
        export_all_formats(_report(), tmp_path / "analysis")

    assert sha256(retained.read_bytes()).digest() == before
    for suffix in ("html", "json", "csv"):
        output = tmp_path / f"analysis.{suffix}"
        if suffix in pair:
            assert output.is_symlink()
        else:
            assert not output.exists()


def test_unselected_formats_do_not_create_false_collisions(tmp_path):
    try:
        (tmp_path / "analysis.json").symlink_to("analysis.html")
    except OSError as exc:
        if exc.errno not in (errno.EPERM, errno.EACCES) and getattr(exc, "winerror", None) != 1314:
            raise
        pytest.skip("Symlink creation is unavailable")
    files = export_all_formats(_report(), tmp_path / "analysis", formats=["html"])
    assert files == {"html": tmp_path / "analysis.html"}
    assert files["html"].read_text(encoding="utf-8").lstrip().lower().startswith("<!doctype html>")


def test_suffix_text_in_parent_is_preserved(tmp_path):
    directory = tmp_path / "archive.html.json.csv"
    files = export_all_formats(_report(), directory / "analysis")
    assert files == {
        "html": directory / "analysis.html",
        "json": directory / "analysis.json",
        "csv": directory / "analysis.csv",
    }
    assert all(path.is_file() for path in files.values())
