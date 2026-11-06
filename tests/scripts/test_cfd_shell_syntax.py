"""Parse CFD shell sources without running a solver (regression for issue 2056).

Bash is the syntax authority, rather than counts of control-flow keywords.
Sources: GNU Bash Reference Manual, edition 5.2, sections "The Set Builtin"
(`-n`: read commands without executing them) and "Conditional Constructs"
(`if ... then ... fi` grammar). The installed `help set` and `help if` expose
these rules too. No engineering constants or acceptance limits are introduced.
"""

from __future__ import annotations

import os
import shutil
import subprocess
from pathlib import Path

import pytest

CFD = Path(__file__).resolve().parents[2] / "scripts" / "cfd"
DRIVER = CFD / "stage45_driver.sh"
SHELL_SOURCES = sorted(CFD.rglob("*.sh"))
BASH = shutil.which("bash")
pytestmark = pytest.mark.skipif(BASH is None, reason="Bash is required to parse Bash")


def _parse(source: str) -> subprocess.CompletedProcess[str]:
    """Run the Bash parser on stdin; never source or execute the input."""
    env = os.environ.copy()
    for key in ("BASH_ENV", "ENV", "GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR"):
        env.pop(key, None)
    return subprocess.run(
        [BASH, "--noprofile", "--norc", "-n"],
        input=source,
        capture_output=True,
        text=True,
        env=env,
        check=False,
    )


def test_syntax_inventory_includes_driver_and_sourced_library():
    """An empty discovery or missing entry point must not silently pass."""
    assert DRIVER in SHELL_SOURCES
    assert CFD / "lib" / "cfd_chain.sh" in SHELL_SOURCES


@pytest.mark.parametrize(
    "script", SHELL_SOURCES, ids=lambda path: path.relative_to(CFD).as_posix()
)
def test_cfd_shell_source_parses_without_execution(script: Path):
    """Each source is parsed separately: `bash -n` does not follow `source`."""
    result = _parse(script.read_text(encoding="utf-8"))
    assert result.returncode == 0, f"{script.relative_to(CFD)}: {result.stderr}"


def test_parser_rejects_driver_with_a_missing_fi():
    """Delete one complete closing token in memory, leaving an open `if`.

    Expected rejection follows Bash 5.2 Conditional Constructs grammar: each
    `if` construct requires a closing `fi`. The repository source is unchanged.
    """
    lines = DRIVER.read_text(encoding="utf-8").splitlines(keepends=True)
    closing = next(index for index, line in enumerate(lines) if line.strip() == "fi")
    del lines[closing]
    result = _parse("".join(lines))
    assert result.returncode != 0
    assert result.stderr


def test_parser_rejects_balanced_counts_with_invalid_nesting():
    """One `if`/`fi` and one `for`/`done` still violate the Bash grammar.

    Independent derivation: `done` cannot close an `if` body before its `fi`;
    token counts alone would accept this malformed nesting.
    """
    result = _parse("for item in example; do\nif true; then\n:; done\nfi\n")
    assert result.returncode != 0
    assert result.stderr


def test_parser_ignores_control_flow_words_in_comments_and_strings():
    """Quoted words and comments are not closing tokens (Bash 5.2 Quoting)."""
    result = _parse("if true; then\n# if for done\nprintf '%s' 'fi fi done'\nfi\n")
    assert result.returncode == 0, result.stderr


def test_parse_does_not_execute_commands_or_startup_files(tmp_path: Path, monkeypatch):
    """Bash 5.2 `set -n` reads commands only; neither marker may be written."""
    startup = tmp_path / "startup.sh"
    startup.write_text("printf executed > startup-marker\n", encoding="utf-8")
    monkeypatch.setenv("BASH_ENV", str(startup))
    monkeypatch.chdir(tmp_path)
    result = _parse("printf executed > script-marker\n$(touch substitution-marker)\n")
    assert result.returncode == 0, result.stderr
    assert not (tmp_path / "startup-marker").exists()
    assert not (tmp_path / "script-marker").exists()
    assert not (tmp_path / "substitution-marker").exists()
