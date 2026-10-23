# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Keep disposable retrieval tests independent of inherited Git bindings."""
import os

import pytest


@pytest.fixture(autouse=True)
def clear_git_bindings(monkeypatch):
    """Git subprocesses must discover this tree rather than an inherited index."""
    for name in os.environ:
        if name.startswith("GIT_"):
            monkeypatch.delenv(name)
