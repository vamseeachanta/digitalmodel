# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Dispatch, optional dependencies and shared geometry invariants."""

import builtins
import math

import pytest

from digitalmodel.cad import CadReadError, GeometryEntity, UnitsError, read_cad
from digitalmodel.cad.models import normalized_geometry
from digitalmodel.cad.units import resolve_units


@pytest.mark.parametrize("suffix", ["step", "stp", "iges", "igs"])
def test_kernel_formats_are_explicitly_deferred(tmp_path, suffix):
    path = tmp_path / f"synthetic.{suffix}"
    path.touch()
    with pytest.raises(CadReadError, match="kernel adapters are not implemented"):
        read_cad(path, units="m")


def test_unrecognised_format(tmp_path):
    path = tmp_path / "synthetic.unknown"
    path.touch()
    with pytest.raises(CadReadError, match="Unsupported format"):
        read_cad(path, units="m")


@pytest.mark.parametrize("is_directory", [False, True])
def test_missing_or_directory_input(tmp_path, is_directory):
    path = tmp_path if is_directory else tmp_path / "absent.dxf"
    with pytest.raises(CadReadError, match="regular file"):
        read_cad(path)


@pytest.mark.parametrize(
    ("suffix", "dependency", "extra"),
    [("dxf", "ezdxf", "drawings"), ("stl", "meshio", "mesh")],
)
def test_missing_dependency_has_actionable_error(
    tmp_path, monkeypatch, suffix, dependency, extra
):
    path = tmp_path / f"synthetic.{suffix}"
    path.touch()
    actual_import = builtins.__import__

    def missing(name, *args, **kwargs):
        if name == dependency:
            raise ModuleNotFoundError(name)
        return actual_import(name, *args, **kwargs)

    monkeypatch.setattr(builtins, "__import__", missing)
    with pytest.raises(ImportError, match=rf"digitalmodel\[{extra}\]"):
        read_cad(path, units="m")


def test_invalid_units_fail_before_loading_reader(tmp_path):
    path = tmp_path / "synthetic.dxf"
    path.touch()
    with pytest.raises(UnitsError, match="Unsupported"):
        read_cad(path, units="guess")


def test_scale_overflow_is_refused():
    """Non-finite input is rejected; no geometric size acceptance limit is used."""
    with pytest.raises(CadReadError, match="finite"):
        normalized_geometry(
            [(math.inf, 0, 0)],
            [GeometryEntity("point", (0,))],
            resolve_units("m", None),
            "dxf",
        )


@pytest.mark.parametrize("indices", [(-1,), (1,), ()])
def test_invalid_connectivity_is_refused(indices):
    with pytest.raises(CadReadError, match="indices"):
        normalized_geometry(
            [(0, 0, 0)],
            [GeometryEntity("point", indices)],
            resolve_units("m", None),
            "dxf",
        )
