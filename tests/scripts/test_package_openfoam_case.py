"""Archive membership is checked against explicit trees, never the selector.

Source: issue 2057's 0.orig counterexample and the numeric-time grammar in
scripts/cfd/prune_case.sh. Digits below are filename fixtures, not engineering
constants. Python 3.11 Library Reference, tarfile: TarFile.add and tarfile.open
specify recursive traversal, symlink handling and exclusive creation.
"""

import importlib.util
import os
from pathlib import Path
import subprocess
import sys
import tarfile

import pytest


SCRIPT = Path(__file__).resolve().parents[2] / "scripts/cfd/package_case.py"


def load_packager():
    spec = importlib.util.spec_from_file_location("package_case", SCRIPT)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def write_tree(case, names):
    for name in names:
        path = case / name
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(name)


def test_preserves_templates_and_omits_only_root_numeric_directories(tmp_path):
    """Expected members are enumerated by hand from the input tree.

    Integer, decimal and exponent root directory names are times under the
    existing pruning policy. 0.orig, digit-prefixed words, numeric regular
    files and nested numeric directories are not root time directories.
    """
    case = tmp_path / "case with spaces"
    kept = (
        "0.orig/U", "system/controlDict", "constant/1/table",
        "postProcessing/forces/0/force.dat", "2notes/readme", "1.0.orig/U",
        "123", "line\nbreak/input",
    )
    omitted = ("0/U", "12/U", "0.25/U", "1e-05/U", "2E+3/U")
    write_tree(case, kept + omitted)
    before = {p.relative_to(case): p.read_bytes() for p in case.rglob("*") if p.is_file()}
    output = tmp_path / "case.tar.gz"
    load_packager().package_case(case, output)
    with tarfile.open(output) as archive:
        files = {m.name for m in archive.getmembers() if m.isfile()}
        assert files == set(kept)
        for name in kept:
            assert archive.extractfile(name).read() == name.encode()
    assert before == {
        p.relative_to(case): p.read_bytes() for p in case.rglob("*") if p.is_file()
    }


def test_existing_archive_is_not_overwritten(tmp_path):
    case = tmp_path / "case"
    write_tree(case, ("0.orig/U",))
    output = tmp_path / "existing.tar.gz"
    output.write_bytes(b"preserve existing bytes")
    with pytest.raises(FileExistsError):
        load_packager().package_case(case, output)
    assert output.read_bytes() == b"preserve existing bytes"


@pytest.mark.parametrize("destination", ["output.tar.gz", "nested/output.tar.gz"])
def test_archive_inside_case_is_rejected(tmp_path, destination):
    case = tmp_path / "case"
    write_tree(case, ("0.orig/U",))
    with pytest.raises(ValueError, match="outside"):
        load_packager().package_case(case, case / destination)
    assert not (case / destination).exists()


def test_symlinks_are_stored_without_reading_targets(tmp_path):
    case = tmp_path / "case"
    write_tree(case, ("0.orig/U",))
    external = tmp_path / "external"
    write_tree(external, ("private",))
    (case / "linked").symlink_to(external, target_is_directory=True)
    (case / "7").symlink_to("0.orig", target_is_directory=True)
    output = tmp_path / "case.tar.gz"
    load_packager().package_case(case, output)
    with tarfile.open(output) as archive:
        assert archive.getmember("linked").issym()
        assert archive.getmember("7").issym()
        assert "linked/private" not in archive.getnames()
        assert "7/U" not in archive.getnames()


def test_missing_case_does_not_create_archive(tmp_path):
    output = tmp_path / "case.tar.gz"
    with pytest.raises(NotADirectoryError):
        load_packager().package_case(tmp_path / "missing", output)
    assert not output.exists()


def test_cli_creates_readable_archive_without_solver(tmp_path):
    case = tmp_path / "case"
    write_tree(case, ("0.orig/U", "0/U"))
    output = tmp_path / "case.tar.gz"
    result = subprocess.run(
        [sys.executable, str(SCRIPT), str(case), str(output)],
        capture_output=True, text=True, check=False, env=os.environ.copy(),
    )
    assert result.returncode == 0, result.stderr
    with tarfile.open(output) as archive:
        assert "0.orig/U" in archive.getnames()
        assert "0/U" not in archive.getnames()
