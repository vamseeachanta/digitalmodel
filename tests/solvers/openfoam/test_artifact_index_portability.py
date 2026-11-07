"""Collection regressions for platforms without descriptor tree walking."""

from __future__ import annotations

import os
from hashlib import sha256
from html import unescape
from pathlib import Path
import re
import subprocess
import sys
import textwrap
from xml.etree import ElementTree

import pytest


@pytest.mark.skipif(sys.platform != "linux", reason="Linux coverage preservation")
def test_linux_snapshot_coverage_remains_enabled(tmp_path: Path) -> None:
    """A real Linux snapshot must run even if the capability predicate regresses.

    Independent oracle: one input file contains the literal b'x', hence one byte;
    hashlib hashes the input directly, without the artifact framing helpers.
    """
    from digitalmodel.solvers.openfoam.artifact_index import snapshot_tree

    (tmp_path / "field").write_bytes(b"x")
    records = snapshot_tree(tmp_path)
    assert [(record.path, record.size_bytes, record.sha256) for record in records] == [
        ("field", len(b"x"), sha256(b"x").hexdigest())
    ]


@pytest.mark.parametrize("missing_flag", ["O_DIRECTORY", "O_NOFOLLOW"])
def test_artifact_tests_collect_without_optional_open_flags(
    missing_flag: str, tmp_path: Path,
) -> None:
    """Independent oracle: pytest collection exits OK and discovers the hash test.

    Python 3.11 Library Reference, os.open constants: optional flags exist only
    when the platform defines them. No test body or solver is executed here.
    """
    root = Path(__file__).resolve().parents[3]
    script = textwrap.dedent(
        """
        import os
        import sys
        import pytest

        if hasattr(os, sys.argv[1]):
            delattr(os, sys.argv[1])
        raise SystemExit(pytest.main([
            'tests/solvers/openfoam/test_artifact_index.py',
            '--collect-only', '-q', '-p', 'no:cacheprovider',
            '--basetemp=' + sys.argv[2],
        ]))
        """
    )
    environment = os.environ.copy()
    environment["PYTHONDONTWRITEBYTECODE"] = "1"
    result = subprocess.run(
        [sys.executable, "-c", script, missing_flag, str(tmp_path / "collection")],
        cwd=root, env=environment, capture_output=True, text=True, check=False,
    )
    assert result.returncode == 0, result.stdout + result.stderr
    assert "test_single_record_tree_digest_golden_vector" in result.stdout


def test_portable_artifact_tests_run_without_descriptor_support(tmp_path: Path) -> None:
    """Only descriptor walk tests skip; the independent digest vectors still run.

    Python 3.11 os capability sets define the missing API contract. The expected
    disposition is derived from each test's need to traverse the filesystem,
    rather than a blanket platform skip or an engineering tolerance.
    """
    root = Path(__file__).resolve().parents[3]
    report = tmp_path / "result.xml"
    script = textwrap.dedent(
        """
        import os
        import sys
        import pytest

        for name in ('O_DIRECTORY', 'O_NOFOLLOW'):
            if hasattr(os, name):
                delattr(os, name)
        os.supports_dir_fd = set()
        os.supports_fd = set()
        os.supports_follow_symlinks = set()
        raise SystemExit(pytest.main([
            'tests/solvers/openfoam/test_artifact_index.py',
            '-q', '-p', 'no:cacheprovider', '--junitxml=' + sys.argv[1],
            '--basetemp=' + sys.argv[2],
        ]))
        """
    )
    result = subprocess.run(
        [sys.executable, "-c", script, str(report), str(tmp_path / "execution")],
        cwd=root, capture_output=True, text=True, check=False,
    )
    assert result.returncode == 0, result.stdout + result.stderr
    cases = {case.attrib["name"]: case for case in ElementTree.parse(report).iter("testcase")}
    digest = cases["test_single_record_tree_digest_golden_vector"]
    snapshot = cases["test_snapshot_tree_returns_exact_sorted_records"]
    assert digest.find("skipped") is None
    assert snapshot.find("skipped") is not None


@pytest.mark.parametrize(
    "capability",
    [
        "O_DIRECTORY", "O_NOFOLLOW", "supports_dir_fd:open",
        "supports_dir_fd:stat", "supports_fd:listdir",
        "supports_follow_symlinks:stat",
    ],
)
@pytest.mark.parametrize(
    "operation", ["snapshot_tree", "select_roots", "verify_unchanged", "build_index"],
)
def test_tree_operations_refuse_each_missing_capability(
    capability: str, operation: str, monkeypatch: pytest.MonkeyPatch, tmp_path: Path,
) -> None:
    """Each required Python 3.11 os capability independently gates tree access.

    Oracle: the documented unsupported-platform error precedes path inspection;
    the nonexistent input cannot produce a snapshot or an empty valid index.
    SHA-256 identities are synthetic metadata, with no engineering input values.
    """
    from digitalmodel.solvers.openfoam import artifact_index as index

    if ":" in capability:
        attribute, function = capability.split(":")
        monkeypatch.setattr(os, attribute, getattr(os, attribute) - {getattr(os, function)})
    else:
        monkeypatch.delattr(os, capability, raising=False)
    assert not index.descriptor_walk_supported()
    root = tmp_path / "nonexistent"
    digest = sha256(b"synthetic identity").hexdigest()
    with pytest.raises(index.ArtifactIndexError, match="this platform is unsupported"):
        if operation == "build_index":
            index.build_index(
                root, run_identity_sha256=digest, input_sha256=digest,
                source_sha256=digest, tool_sha256=digest, generation_id=digest,
            )
        elif operation == "verify_unchanged":
            index.verify_unchanged(root, [])
        else:
            getattr(index, operation)(root)
    assert not root.exists()


@pytest.mark.parametrize("unsupported", [False, True])
def test_documented_example_matches_input_bytes_or_refuses(
    unsupported: bool, monkeypatch: pytest.MonkeyPatch, tmp_path: Path, capsys,
) -> None:
    """The example's answer is six ASCII bytes or an explicit unsupported error.

    Independent input: the literal b'sample' has six characters/bytes. The digest
    oracle hashes that literal directly rather than using an artifact helper.
    """
    from digitalmodel.solvers.openfoam.artifact_index import descriptor_walk_supported

    if unsupported:
        monkeypatch.delattr(os, "O_DIRECTORY", raising=False)
    document = (
        Path(__file__).resolve().parents[3]
        / "docs/domains/openfoam/artifact-index-portability.html"
    )
    match = re.search(
        r'<pre id="worked-example"><code>(.*?)</code></pre>',
        document.read_text(encoding="utf-8"), re.DOTALL,
    )
    assert match is not None
    exec(compile(unescape(match.group(1)), str(document), "exec"), {"root": tmp_path})
    expected = (
        "one file; six bytes; digest matches input\n"
        if descriptor_walk_supported()
        else "snapshot unsupported; no index emitted\n"
    )
    assert capsys.readouterr().out == expected
