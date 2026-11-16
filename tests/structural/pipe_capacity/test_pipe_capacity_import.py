# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Check pipe-capacity imports without legacy paths or cached test modules."""

import os
from pathlib import Path
import subprocess
import sys
import textwrap


def test_pipe_capacity_import_uses_packaged_helper(tmp_path):
    """Import and construct the real class offline in an isolated interpreter."""
    src = next(
        parent / "src"
        for parent in Path(__file__).resolve().parents
        if (parent / "pyproject.toml").is_file()
    )
    script = textwrap.dedent(
        """\
        import importlib
        import importlib.abc
        import sys

        blocked_roots = {
            "common", "custom", "OrcFxAPI", "ansys", "pythoncom", "win32com",
        }
        attempts = []

        class RejectLegacyAndLicensedImports(importlib.abc.MetaPathFinder):
            def find_spec(self, fullname, path=None, target=None):
                if fullname.split(".")[0] in blocked_roots:
                    attempts.append(fullname)
                    raise AssertionError("Forbidden import: " + fullname)
                return None

        def reject_network(event, args):
            if event.startswith("socket.") or event in {
                "urllib.Request", "http.client.connect",
            }:
                attempts.append(event)
                raise AssertionError("Forbidden network operation: " + event)

        sys.meta_path.insert(0, RejectLegacyAndLicensedImports())
        sys.addaudithook(reject_network)
        sys.path.insert(0, sys.argv[1])
        module = importlib.import_module(
            "digitalmodel.structural.pipe_capacity.common.PipeCapacity"
        )
        helper = importlib.import_module(
            "digitalmodel.infrastructure.utils.update_deep"
        )
        cfg = {}
        instance = module.PipeCapacity(cfg)
        assert instance.cfg is cfg
        assert module.update_deep_dictionary is helper.update_deep_dictionary
        assert not blocked_roots.intersection(sys.modules)
        assert not attempts, attempts
        """
    )
    env = {key: value for key, value in os.environ.items() if not key.startswith("GIT_")}
    result = subprocess.run(
        [sys.executable, "-I", "-B", "-c", script, str(src)],
        cwd=tmp_path,
        env=env,
        capture_output=True,
        text=True,
        encoding="utf-8",
        timeout=60,
        creationflags=subprocess.CREATE_NO_WINDOW if sys.platform == "win32" else 0,
    )
    assert result.returncode == 0, result.stdout + result.stderr
    assert not list(tmp_path.iterdir()), "Import created filesystem output"
