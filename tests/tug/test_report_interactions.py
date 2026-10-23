# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Execute the report comment handlers against a small DOM test double."""

from pathlib import Path
import shutil
import subprocess

import pytest


@pytest.mark.parametrize("scenario", ["merge", "duplicate", "invalid", "pending"])
def test_comment_import_preserves_edits(scenario):
    node = shutil.which("node")
    if node is None:
        pytest.skip("Node is required for comment interaction checks")
    script = Path(__file__).with_name("report_interactions.cjs")
    subprocess.run([node, str(script), scenario], check=True, capture_output=True, text=True)
