"""Cross-section destination checks with disposable, hashed fixtures.

Expected names follow REPORT_MD/REPORT_HTML. Two aliases identify one retained
file and must fail before rendering. The old <name>.tmp is unrelated input,
not a scratch file owned by this invocation (issue #2315).
"""

from hashlib import sha256

import pytest

from digitalmodel.subsea.cross_sections import reporting


@pytest.mark.parametrize("alias", ["same", "symlink", "hardlink", "dangling"])
def test_aliases_rejected_before_rendering(tmp_path, monkeypatch, alias):
    retained = tmp_path / "retained.data"
    if alias != "dangling":
        retained.write_bytes(b"retained data")
    before = sha256(retained.read_bytes()).digest() if retained.exists() else None
    if alias == "same":
        monkeypatch.setattr(reporting, "REPORT_HTML", reporting.REPORT_MD)
    else:
        for name in (reporting.REPORT_MD, reporting.REPORT_HTML):
            path = tmp_path / name
            if alias == "hardlink":
                path.hardlink_to(retained)
            else:
                path.symlink_to(retained)

    def unexpected(*args, **kwargs):
        pytest.fail("No model loading or rendering before preflight")

    monkeypatch.setattr(reporting, "build_report_model", unexpected)
    with pytest.raises(ValueError, match="distinct"):
        reporting.write_report_files(tmp_path, {"md", "html"})
    if before is not None:
        assert sha256(retained.read_bytes()).digest() == before
    else:
        assert not retained.exists()


def test_fixed_temporary_name_is_not_overwritten_or_deleted(tmp_path):
    old_temp = tmp_path / (reporting.REPORT_MD + ".tmp")
    old_temp.write_bytes(b"retained data")
    before = sha256(old_temp.read_bytes()).digest()
    generated = reporting.write_report_files(tmp_path, {"md", "html"})
    assert [p.name for p in generated] == [reporting.REPORT_MD, reporting.REPORT_HTML]
    assert sha256(old_temp.read_bytes()).digest() == before
    assert {p.name for p in tmp_path.iterdir()} == {
        reporting.REPORT_MD, reporting.REPORT_HTML, old_temp.name,
    }


def test_unselected_alias_does_not_block_single_format(tmp_path):
    (tmp_path / reporting.REPORT_HTML).symlink_to(reporting.REPORT_MD)
    assert reporting.write_report_files(tmp_path, {"md"}) == [tmp_path / reporting.REPORT_MD]
