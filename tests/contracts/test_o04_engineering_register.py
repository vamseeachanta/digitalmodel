"""Engineering register regressions discovered during O04 review."""

import importlib.util
import subprocess
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[2]
SPEC = importlib.util.spec_from_file_location(
    "register_checker", ROOT / "scripts/enforcement/check-engineering-register.py"
)
CHECKER = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(CHECKER)


@pytest.mark.parametrize(
    "text",
    [
        "```python\nWe use safe values.\n```",
        "~~~text\nWe use safe values.\n~~~",
        "    We use safe values.",
        "The displacement is 12.5 mm.",
    ],
)
def test_non_prose_and_non_thickness_are_not_flagged(text):
    assert CHECKER.check_file_text(text) == []


def test_checker_accepts_its_own_rule():
    assert CHECKER.check_file(ROOT / ".claude/rules/engineering-register.md") == []


def test_invalid_diff_base_fails_instead_of_returning_no_files():
    with pytest.raises(subprocess.CalledProcessError):
        CHECKER.diff_files("refs/heads/nonexistent-o04-fixture")


def test_diff_includes_changed_documentation(tmp_path, monkeypatch):
    for name in ("GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR", "GIT_INDEX_FILE"):
        monkeypatch.delenv(name, raising=False)
    monkeypatch.chdir(tmp_path)
    subprocess.run(["git", "init", "-q"], check=True)
    document = Path("report.md")
    document.write_text("The analysis meets the stated criterion.\n")
    subprocess.run(["git", "add", "report.md"], check=True)
    subprocess.run(
        [
            "git",
            "-c",
            "user.name=Fixture",
            "-c",
            "user.email=fixture@example.invalid",
            "-c",
            "core.hooksPath=/dev/null",
            "commit",
            "-qm",
            "fixture",
        ],
        check=True,
    )
    document.write_text("The analysis meets the revised criterion.\n")
    assert CHECKER.diff_files("HEAD") == [document]


def test_other_measurements_on_thickness_lines_are_not_flagged():
    assert (
        CHECKER.check_file_text("The displacement is 12.5 mm; thickness is 12.500 mm.")
        == []
    )
    assert (
        CHECKER.check_file_text("The corrosion allowance is 0.250 mm; span is 3.5 mm.")
        == []
    )


def test_text_check_does_not_require_temporary_file(monkeypatch):
    import tempfile

    def forbidden(*args, **kwargs):
        raise AssertionError("Text checks must remain in memory")

    monkeypatch.setattr(tempfile, "NamedTemporaryFile", forbidden)
    assert CHECKER.check_file_text("The analysis meets the stated criterion.") == []
