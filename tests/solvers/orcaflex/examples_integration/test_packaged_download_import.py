# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Exercise the packaged download utility without running a download."""

import json
from pathlib import Path
import subprocess
import sys
import textwrap

import pytest


@pytest.fixture
def offline_python(tmp_path):
    """Run utility code in a fresh interpreter with side effects forbidden."""
    src = next(
        parent / "src"
        for parent in Path(__file__).resolve().parents
        if (parent / "pyproject.toml").is_file()
    )
    guard_script = textwrap.dedent(
        """\
        import importlib
        import json
        import os
        import sys
        from unittest.mock import patch

        # requests probes local socket capabilities during its own import.
        # Load that dependency before guarding all digitalmodel imports.
        import requests

        effects = []

        def guard(event, args):
            blocked = event.startswith("socket.") or event in {
                "os.mkdir", "os.remove", "os.rename", "os.rmdir",
                "os.link", "os.symlink", "os.truncate", "os.chdir",
                "subprocess.Popen", "os.system", "urllib.Request",
                "http.client.connect",
            }
            if event == "open":
                _, mode, flags = args
                blocked = (
                    mode is not None and any(char in mode for char in "wax+")
                ) or bool(flags & (os.O_WRONLY | os.O_RDWR | os.O_CREAT | os.O_TRUNC))
            if event == "import" and args[0].split(".")[0] == "OrcFxAPI":
                blocked = True
            if blocked:
                effects.append(event)
                raise AssertionError("Package import attempted a side effect: " + event)

        sys.addaudithook(guard)
        """
    )

    def run(script):
        result = subprocess.run(
            [sys.executable, "-I", "-B", "-c", guard_script + script, src.as_posix()],
            cwd=tmp_path,
            capture_output=True,
            text=True,
            encoding="utf-8",
            timeout=60,  # Bound a broken invocation; no download is performed.
        )
        assert result.returncode == 0, result.stdout + result.stderr
        assert not list(tmp_path.iterdir()), "Invocation created filesystem output"
        return result.stdout

    return run


@pytest.fixture
def packaged_import(offline_python):
    """Import with only src added to the package path."""
    script = textwrap.dedent(
        """\
        sys.path.insert(0, sys.argv[1])
        package = "digitalmodel.solvers.orcaflex.examples_integration"
        sibling = importlib.import_module(package + ".enhanced_downloader")
        with patch.object(
            sibling.EnhancedOrcaflexDownloader,
            "__init__",
            side_effect=AssertionError("Import must not construct a downloader"),
        ) as constructor:
            utility = importlib.import_module(package + ".download_all_examples")
            constructor.assert_not_called()

        assert not effects, effects
        assert "enhanced_downloader" not in sys.modules
        assert callable(utility.download_all_examples)
        print(json.dumps({
            "same_class": (
                utility.EnhancedOrcaflexDownloader is sibling.EnhancedOrcaflexDownloader
            ),
            "class_module": utility.EnhancedOrcaflexDownloader.__module__,
            "class_name": utility.EnhancedOrcaflexDownloader.__name__,
        }))
        """
    )
    return json.loads(offline_python(script))


def test_download_examples_package_import_is_offline(packaged_import):
    """Import succeeds while network, writes and construction are forbidden."""
    assert packaged_import


def test_download_examples_resolves_packaged_downloader(packaged_import):
    """The export is the real sibling class, not a substitute or bare module."""
    assert packaged_import == {
        "same_class": True,
        "class_module": (
            "digitalmodel.solvers.orcaflex.examples_integration.enhanced_downloader"
        ),
        "class_name": "EnhancedOrcaflexDownloader",
    }


def test_download_examples_script_help_is_offline(offline_python):
    """Direct-script help exits before constructing or running a downloader."""
    script = textwrap.dedent(
        """\
        import runpy
        from pathlib import Path

        utility = Path(sys.argv[1]).joinpath(
            "digitalmodel", "solvers", "orcaflex", "examples_integration",
            "download_all_examples.py",
        )
        # Match direct execution: the script directory, not src, is importable.
        sys.path.insert(0, utility.parent.as_posix())
        sibling = importlib.import_module("enhanced_downloader")
        sys.argv = [utility.as_posix(), "--help"]
        with patch.object(
            sibling.EnhancedOrcaflexDownloader,
            "__init__",
            side_effect=AssertionError("Help must not construct a downloader"),
        ) as constructor:
            try:
                runpy.run_path(utility.as_posix(), run_name="__main__")
            except SystemExit as exc:
                assert exc.code == 0, "Help must exit successfully"
            else:
                raise AssertionError("Help did not exit through argument parsing")
            constructor.assert_not_called()
        assert not effects, effects
        """
    )
    help_text = offline_python(script)
    assert "usage:" in help_text
    assert "--skip-large" in help_text
    assert "--test" in help_text
