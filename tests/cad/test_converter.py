# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""DWG converter contract tested only with synthetic fake executables."""

import json
import sys
from pathlib import Path

import ezdxf
import pytest

from digitalmodel.cad import CadReadError, DwgConverter, UnitsError, read_cad


def inputs(tmp_path):
    """Hand: the fake output's point (1000,0,0) mm becomes (1,0,0) m.

    NIST SI Prefixes, milli row, updated 2025-08-13. No DWG geometry is
    interpreted by the fake: a synthetic DXF is copied to its output argument.
    """
    source = tmp_path / "source drawing; literal.dwg"
    source.write_bytes(b"synthetic placeholder")
    dxf = tmp_path / "synthetic.dxf"
    doc = ezdxf.new()
    doc.units = 4  # ezdxf 1.4.5 DXF Units, INSUNITS table: 4=mm.
    doc.modelspace().add_point((1000, 0, 0))
    doc.saveas(dxf)
    script = tmp_path / "fake converter.py"
    script.write_text(
        "import json, shutil, sys\n"
        "from pathlib import Path\n"
        "source, output, fixture, record = map(Path, sys.argv[1:])\n"
        "shutil.copyfile(fixture, output)\n"
        "record.write_text(json.dumps(sys.argv[1:3]))\n",
        encoding="utf-8",
    )
    record = tmp_path / "arguments.json"
    command = (
        sys.executable,
        str(script),
        "{input}",
        "{output}",
        str(dxf),
        str(record),
    )
    # The timeout is a test input, not a domain acceptance limit.
    converter = DwgConverter(command=command, timeout_seconds=10, work_dir=tmp_path)
    return source, dxf, record, converter


def test_converter_uses_literal_arguments_and_preserves_source(tmp_path):
    source, _, record, converter = inputs(tmp_path)
    before = source.read_bytes()
    result = read_cad(source, converter=converter)
    assert result.format == "dwg"
    assert result.points == ((1, 0, 0),)
    assert result.unit_decision.source_units == "mm"
    arguments = json.loads(record.read_text())
    assert arguments[0] == str(source.resolve())
    output = Path(arguments[1])
    assert output.name == "converted.dxf"
    assert output.parent.parent == tmp_path
    assert output.exists()  # Converter outputs are retained for inspection.
    assert source.read_bytes() == before


def test_converter_requires_explicit_configuration(tmp_path):
    source, _, _, _ = inputs(tmp_path)
    with pytest.raises(CadReadError, match="converter"):
        read_cad(source)


def test_converter_cannot_hide_units_conflict(tmp_path):
    source, _, _, converter = inputs(tmp_path)
    with pytest.raises(UnitsError, match="conflict"):
        read_cad(source, units="m", converter=converter)


def test_converter_unitless_output_requires_caller_units(tmp_path):
    source, dxf, _, converter = inputs(tmp_path)
    doc = ezdxf.readfile(dxf)
    doc.units = 0  # ezdxf 1.4.5 DXF Units: 0=unitless.
    doc.saveas(dxf)
    with pytest.raises(UnitsError, match="State units"):
        read_cad(source, converter=converter)
    assert (
        read_cad(source, units="mm", converter=converter).unit_decision.basis
        == "caller"
    )


@pytest.mark.parametrize("mode", ["nonzero", "no_output", "symlink", "directory"])
def test_converter_output_failures(tmp_path, mode):
    source, dxf, _, _ = inputs(tmp_path)
    script = tmp_path / "failure.py"
    operations = {
        "nonzero": "raise SystemExit(2)",
        "no_output": "pass",
        "symlink": "Path(sys.argv[1]).symlink_to(sys.argv[2])",
        "directory": "Path(sys.argv[1]).mkdir()",
    }
    script.write_text(
        "import sys\nfrom pathlib import Path\n" + operations[mode] + "\n",
        encoding="utf-8",
    )
    converter = DwgConverter(
        command=(sys.executable, str(script), "{output}", str(dxf), "{input}"),
        timeout_seconds=10,
        work_dir=tmp_path,
    )
    with pytest.raises(CadReadError, match="Converter"):
        read_cad(source, converter=converter)


def test_converter_timeout(tmp_path, monkeypatch):
    """A simulated timeout avoids sleeping or starting a long computation."""
    import subprocess

    source, _, _, converter = inputs(tmp_path)

    def timeout(*args, **kwargs):
        raise subprocess.TimeoutExpired("fake-converter", converter.timeout_seconds)

    monkeypatch.setattr(subprocess, "run", timeout)
    with pytest.raises(CadReadError, match="timed out"):
        read_cad(source, converter=converter)


def test_converter_executable_missing(tmp_path):
    source, _, _, _ = inputs(tmp_path)
    converter = DwgConverter(
        command=(str(tmp_path / "absent-tool"), "{input}", "{output}"),
        timeout_seconds=10,
        work_dir=tmp_path,
    )
    with pytest.raises(CadReadError, match="could not start"):
        read_cad(source, converter=converter)


@pytest.mark.parametrize(
    "command",
    [
        (),
        ("tool", "{input}"),
        ("tool", "{output}"),
        ("tool", "{input}", "{output}", "{unknown}"),
        ("tool", "{input}", "{output}", "{input.name}"),
        ("tool", "{input!r}", "{output}"),
        ("tool", "{input:>20}", "{output}"),
        "tool {input} {output}",
    ],
)
def test_invalid_converter_template_is_refused(tmp_path, command):
    with pytest.raises(CadReadError, match="command"):
        DwgConverter(command=command, timeout_seconds=10, work_dir=tmp_path)


@pytest.mark.parametrize("timeout", [0, -1, float("nan"), float("inf")])
def test_converter_timeout_must_be_finite_positive(tmp_path, timeout):
    with pytest.raises(CadReadError, match="timeout"):
        DwgConverter(
            command=("tool", "{input}", "{output}"),
            timeout_seconds=timeout,
            work_dir=tmp_path,
        )


def test_converter_is_not_used_for_other_formats(tmp_path):
    _, dxf, _, converter = inputs(tmp_path)
    with pytest.raises(CadReadError, match="only.*DWG"):
        read_cad(dxf, converter=converter)
