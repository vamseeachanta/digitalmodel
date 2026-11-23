"""One definition of what a mock OrcaFlex artifact looks like (#1631, D6).

Three OrcaFlex writers used to forge a ``.sim`` at exactly the path a real
solve would have written: ``universal_runner``, ``batch_processor``, and
``core/model_interface`` (which wrote an *empty* file, leaving a downstream
consumer nothing to inspect at all). A consumer globbing ``*.sim`` could not
tell them apart.

A mock artifact now carries three independent signals:

1. a distinct directory -- a ``*_mock/`` sibling of the real output directory;
2. a ``.mock`` infix in the filename (``model.mock.sim``);
3. the in-band body marker ``Mock simulation for ...``.

They are independent on purpose, and asserted separately, so that removing one
cannot silently pass. The cost is that a caller which hardcodes the real path
stops finding mock output -- which is the desired failure, and the reason to
prefer this over a marker alone.

This lives in one module so the three writers cannot drift apart.
"""

from __future__ import annotations

from pathlib import Path

#: Filename infix. Signal 2 of 3.
MOCK_INFIX = "mock"

#: Directory suffix. Signal 1 of 3.
MOCK_DIR_SUFFIX = "_mock"

#: In-band body prefix. Signal 3 of 3. Retained from the original writers so an
#: existing consumer that greps for it keeps working.
MOCK_BODY_PREFIX = "Mock simulation for"


def mock_output_dir(output_dir: Path | str) -> Path:
    """The ``*_mock/`` sibling of a real output directory."""
    directory = Path(output_dir)
    return directory.parent / f"{directory.name}{MOCK_DIR_SUFFIX}"


def mock_artifact_path(real_path: Path | str) -> Path:
    """Where a mock stand-in for ``real_path`` belongs."""
    real = Path(real_path)
    name = f"{real.stem}.{MOCK_INFIX}{real.suffix}"
    return mock_output_dir(real.parent) / name


def mock_body(model_name: str, detail: str = "") -> str:
    """The in-band marker written into every mock artifact."""
    suffix = f" ({detail})" if detail else ""
    return f"{MOCK_BODY_PREFIX} {model_name}{suffix}"


def write_mock_artifact(real_path: Path | str, model_name: str, detail: str = "") -> Path:
    """Write a mock artifact carrying all three signals; return its path."""
    path = mock_artifact_path(real_path)
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(mock_body(model_name, detail) + "\n")
    return path
