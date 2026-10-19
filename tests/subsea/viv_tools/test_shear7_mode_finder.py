"""Synthetic parser examples, independent of any solver or restricted results."""

import csv
import io
import json
from pathlib import Path

import pytest

from digitalmodel.subsea.viv_tools.shear7_mode_finder import (
    ModeParseError,
    find_modes,
    main,
    node_list,
    parse_modes,
)


# Format source: ModesFind.m (2009), sentinel and strread loops. All values
# below are invented identifiers/counts, not engineering inputs or limits.
SAMPLE = """Potential modes and other unrelated output
mode no. frequency
99 1.0
No. of potentially excited modes: 1
  the number of modes above cutoff is: 2
  These modes are:
  3
  7

Mode No.   No. of Nodes   Length Ratio
--------------------------------------
3 3 0.5
7 2 0.2

mode number: 3
node number      Cl       Fn/Fvo
--------------------------------
4 0.1 1.0
8 0.2 1.1
9 0.3 1.2

mode number: 7
node number      Cl       Fn/Fvo
--------------------------------
20 0.1 1.0
21 0.2 1.1
"""


def test_parse_legacy_first_region():
    """Hand reading: above-cutoff=[3,7]; first region=[4,8,9], not [20,21]."""
    result = parse_modes(SAMPLE, filename="case.out")
    assert result.filename == "case.out"
    assert result.excited_modes == (3, 7)
    assert result.power_in_mode == 3
    assert result.power_in_nodes == (4, 8, 9)


@pytest.mark.parametrize(
    "transform",
    [str.upper, lambda s: s.replace(" ", "\t"), lambda s: s.replace("\n", "\r\n")],
)
def test_marker_variations(transform):
    """Whitespace and case cannot change the hand-read node IDs [4,8,9]."""
    assert parse_modes(transform(SAMPLE)).power_in_nodes == (4, 8, 9)


def test_fortran_exponents():
    """Coefficients are syntax-checked only; D exponents leave IDs unchanged."""
    text = SAMPLE.replace("4 0.1 1.0", "4 1.D-1 1.0D+0")
    assert parse_modes(text).power_in_nodes == (4, 8, 9)


def test_zero_modes():
    """A declared count of zero implies zero modes and no first region."""
    result = parse_modes("the number of modes above cutoff is: 0\n")
    assert result.excited_modes == ()
    assert result.power_in_mode is None
    assert result.power_in_nodes == ()


@pytest.mark.parametrize(
    "text",
    [
        "",
        SAMPLE.replace("is: 2", "is: 4"),
        SAMPLE.replace("is: 2", "is: -1"),
        SAMPLE.replace("  7\n", "  seven\n"),
        SAMPLE.replace("  7\n", "  3\n"),
        SAMPLE.replace("Mode No.   No. of Nodes   Length Ratio", "missing"),
        SAMPLE.replace("3 3 0.5", "3 4 0.5"),
        SAMPLE.replace("3 3 0.5", "3 -1 0.5"),
        SAMPLE.replace("3 3 0.5", "5 3 0.5"),
        SAMPLE.replace("node number      Cl       Fn/Fvo", "missing", 1),
        SAMPLE.replace("8 0.2 1.1", "8 broken 1.1"),
        SAMPLE.replace("8 0.2 1.1", "8 nan 1.1"),
        SAMPLE.replace("8 0.2 1.1", "4 0.2 1.1"),
        SAMPLE.replace("8 0.2 1.1", "-8 0.2 1.1"),
        SAMPLE.split("9 0.3")[0],
    ],
)
def test_invalid_input_is_not_empty_success(text):
    """Each mutation contradicts a declared count, identifier or row grammar."""
    with pytest.raises(ModeParseError):
        parse_modes(text)


def test_empty_first_region():
    """A declared zero node count creates no matrix rows, without a node table."""
    text = SAMPLE.split("mode number: 3")[0].replace("3 3 0.5", "3 0 0.0")
    assert parse_modes(text).power_in_nodes == ()


def test_node_list_padding():
    """Hand transpose: [4,8,9] and [2] become [[4,2],[8,0],[9,0]]."""
    first = parse_modes(SAMPLE, filename="a.out")
    second = parse_modes(
        SAMPLE.replace("3 3 0.5", "3 1 0.5").replace("4 0.1 1.0", "2 0.1 1.0"),
        filename="b.out",
    )
    assert node_list((first, second)) == ((4, 2), (8, 0), (9, 0))
    assert node_list((second,)) == ((2,),)
    assert node_list(()) == ()


def test_batch_order_extension_and_cwd(tmp_path):
    """Alphabetical a,b columns are independent of creation order or caller cwd."""
    (tmp_path / "b.out").write_text(SAMPLE)
    (tmp_path / "a.out").write_text(SAMPLE)
    (tmp_path / "ignored.txt").write_text("unrelated")
    cwd = Path.cwd()
    results = find_modes(tmp_path, "out")
    assert [r.filename for r in results] == ["a.out", "b.out"]
    assert Path.cwd() == cwd
    assert find_modes(tmp_path, ".out") == results


@pytest.mark.parametrize("extension", ["", "*", "../out", ".o?ut"])
def test_invalid_extension(tmp_path, extension):
    with pytest.raises(ValueError):
        find_modes(tmp_path, extension)


def test_empty_discovery(tmp_path):
    with pytest.raises(FileNotFoundError):
        find_modes(tmp_path)


@pytest.mark.parametrize("output_format", ["json", "yaml", "csv"])
def test_cli_roundtrip(tmp_path, output_format):
    """Expected modes and nodes are copied from the hand reading, not the parser."""
    (tmp_path / "a.out").write_text(SAMPLE)
    target = tmp_path / "exports"
    assert (
        main(
            [
                str(tmp_path),
                "--output-dir",
                str(target),
                "--output-format",
                output_format,
            ]
        )
        == 0
    )
    excited = target / f"ExcitedModes.{output_format}"
    power = target / f"PowerInRegion.{output_format}"
    if output_format == "csv":
        assert list(csv.reader(io.StringIO(excited.read_text()))) == [
            ["file", "excited_modes"],
            ["a.out", "3 7"],
        ]
        assert list(csv.reader(io.StringIO(power.read_text()))) == [
            ["a.out"],
            ["4"],
            ["8"],
            ["9"],
        ]
    else:
        if output_format == "yaml":
            import yaml

            load = yaml.safe_load
        else:
            load = json.loads
        assert load(excited.read_text()) == {
            "files": ["a.out"],
            "excited_modes": [[3, 7]],
        }
        assert load(power.read_text()) == {
            "files": ["a.out"],
            "power_in_modes": [3],
            "node_list": [[4], [8], [9]],
        }


def test_cli_failure_does_not_create_exports(tmp_path, capsys):
    (tmp_path / "bad.out").write_text("not a result")
    target = tmp_path / "exports"
    assert main([str(tmp_path), "--output-dir", str(target)]) == 2
    assert "above-cutoff" in capsys.readouterr().err
    assert not target.exists()


def test_cli_protects_inputs(tmp_path):
    """Choosing the exported CSV suffix as input cannot overwrite its bytes."""
    source = tmp_path / "ExcitedModes.csv"
    source.write_text(SAMPLE)
    assert (
        main(
            [
                str(tmp_path),
                "--extension",
                ".csv",
                "--output-format",
                "csv",
                "--output-dir",
                str(tmp_path),
            ]
        )
        == 2
    )
    assert source.read_text() == SAMPLE


def test_first_region_mode_mismatch():
    """Summary mode 7 cannot label the following node table explicitly for mode 3."""
    with pytest.raises(ModeParseError, match="mode"):
        parse_modes(SAMPLE.replace("3 3 0.5", "7 3 0.5"))


def test_cli_hardlink_input_protection(tmp_path):
    """An export hardlinked to an input denotes the same bytes despite its name."""
    source = tmp_path / "case.out"
    source.write_text(SAMPLE)
    target = tmp_path / "ExcitedModes.csv"
    target.hardlink_to(source)
    assert main([str(tmp_path), "--output-dir", str(tmp_path)]) == 2
    assert source.read_text() == SAMPLE


def test_cli_module_entrypoint(tmp_path):
    """The documented module route runs in a fresh interpreter without a solver."""
    import os
    import subprocess
    import sys

    (tmp_path / "a.out").write_text(SAMPLE)
    target = tmp_path / "exports"
    env = dict(os.environ, PYTHONPATH=str(Path(__file__).resolve().parents[3] / "src"))
    completed = subprocess.run(
        [
            sys.executable,
            "-m",
            "digitalmodel.subsea.viv_tools.shear7_mode_finder",
            str(tmp_path),
            "--output-dir",
            str(target),
        ],
        env=env,
        capture_output=True,
        text=True,
        check=False,
    )
    assert completed.returncode == 0, completed.stderr
    assert list(
        csv.reader(io.StringIO((target / "PowerInRegion.csv").read_text()))
    ) == [["a.out"], ["4"], ["8"], ["9"]]


def test_console_script_registration():
    """Installed command configuration resolves to the tested main callable."""
    import tomllib

    root = Path(__file__).resolve().parents[3]
    config = tomllib.loads((root / "pyproject.toml").read_text())
    assert config["project"]["scripts"]["shear7-mode-finder"] == (
        "digitalmodel.subsea.viv_tools.shear7_mode_finder:main"
    )


def test_worked_example(tmp_path):
    """Hand transpose [4,8,9] and [2] gives [[4,2],[8,0],[9,0]]."""
    root = Path(__file__).resolve().parents[3]
    inputs = root / "examples/viv/shear7_mode_finder"
    results = find_modes(inputs, ".txt")
    assert [r.excited_modes for r in results] == [(3, 7), (5,)]
    assert node_list(results) == ((4, 2), (8, 0), (9, 0))
    assert (
        main(
            [
                str(inputs),
                "--extension",
                ".txt",
                "--output-format",
                "json",
                "--output-dir",
                str(tmp_path),
            ]
        )
        == 0
    )
    assert json.loads((tmp_path / "PowerInRegion.json").read_text())["node_list"] == [
        [4, 2],
        [8, 0],
        [9, 0],
    ]
