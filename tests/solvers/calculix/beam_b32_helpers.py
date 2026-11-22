# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Synthetic SI cantilevers with preordered connectivity; no Gmsh conversion."""

import math
import os
from pathlib import Path
import re
import subprocess

import numpy as np
import pytest

from digitalmodel.solvers.calculix.inp_writer import INPWriter

LENGTH, WIDTH, HEIGHT = 1.0, 0.02, 0.02  # m
YOUNG, POISSON, LOAD = 210e9, 0.3, 10.0  # Pa, dimensionless, N in +Y


def reference():
    """Euler-Bernoulli bending and separate shear estimate, in metres."""
    inertia = WIDTH * HEIGHT**3 / 12
    shear_modulus = YOUNG / (2 * (1 + POISSON))
    return (LOAD * LENGTH**3 / (3 * YOUNG * inertia),
            LOAD * LENGTH / ((5 / 6) * shear_modulus * WIDTH * HEIGHT))


def build_cantilever(directory, kind, count):
    """Use direct end-mid-end B32 nodes, bypassing imported mesh ordering."""
    if kind not in {"B31", "B32"} or type(count) is not int or count <= 0:
        raise ValueError("Expected B31/B32 and a positive element count")
    stride = 2 if kind == "B32" else 1
    nodes = np.zeros((stride * count + 1, 3))
    nodes[:, 0] = np.linspace(0, LENGTH, len(nodes))
    connectivity = np.array([list(range(stride * i, stride * i + stride + 1))
                             for i in range(count)])
    writer = INPWriter(nodes, {f"Line {stride + 1}": {"connectivity": connectivity}})
    writer.add_material("MAT", YOUNG, POISSON)
    writer.add_beam_section(f"EBEAM_{kind}", "MAT", "RECT", (WIDTH, HEIGHT),
                            direction=(0.0, 0.0, 1.0))
    writer.add_node_set("ROOT", [0])
    writer.add_node_set("TIP", [len(nodes) - 1])
    for dof in range(1, 7):
        writer.add_boundary_condition("ROOT", dof, 0.0)
    writer.add_load("TIP", "cload", LOAD, direction=(0, 1, 0))
    writer.add_step()
    path = writer.write(Path(directory) / f"cantilever_{kind}_{count}.inp")
    text = path.read_text(encoding="utf-8")
    requests = "*NODE PRINT, NSET=TIP\nU\n*NODE PRINT, NSET=ROOT\nRF\n"
    path.write_text(text.replace("*END STEP", requests + "*END STEP"),
                    encoding="utf-8", newline="\n")
    return path, len(nodes)


def read_vector(text, quantity, node_set, node):
    """Require exactly one named DAT table containing exactly the named node."""
    heading = {"U": r"displacements\s+\(vx,vy,vz\)", "RF": r"forces\s+\(fx,fy,fz\)"}[quantity]
    pattern = (rf"^[ \t]*{heading}\s+for set {re.escape(node_set)} "
               r"and time[^\n]*\n")
    matches = list(re.finditer(pattern, text, re.MULTILINE | re.IGNORECASE))
    if len(matches) != 1:
        raise ValueError(f"Expected one {quantity} table for {node_set}")
    block = text[matches[0].end():].lstrip(" \t\r\n")
    rows = re.split(r"\n[ \t\r]*\n", block, maxsplit=1)[0].splitlines()
    if len(rows) != 1:
        raise ValueError(f"Expected one node in {node_set}")
    fields = rows[0].split()
    if len(fields) != 4 or not fields[0].isdigit() or int(fields[0]) != node:
        raise ValueError(f"Invalid row in {node_set}")
    values = tuple(float(v.replace("D", "E").replace("d", "e")) for v in fields[1:])
    if not all(math.isfinite(v) for v in values):
        raise ValueError(f"Nonfinite row in {node_set}")
    return values


def probe_release(executable):
    """Accept the qualified banner only with documented status 0 or 201."""
    result = subprocess.run([str(executable), "-v"], capture_output=True, text=True,
                            timeout=60, env={**os.environ, "OMP_NUM_THREADS": "1"})
    banner = result.stdout + "\n" + result.stderr
    if result.returncode not in (0, 201) or not re.search(r"\bVersion 2\.23[ \t\r]*$", banner, re.MULTILINE):
        raise RuntimeError("CalculiX 2.23 version probe failed")


def native_executable():
    """Disabled means no lookup; explicit opt-in fails instead of skipping."""
    if os.environ.get("CALCULIX_NATIVE_TESTS") != "1":
        pytest.skip("Native tests disabled: set CALCULIX_NATIVE_TESTS=1 explicitly")
    value = os.environ.get("CALCULIX_NATIVE_EXECUTABLE")
    if not value or not Path(value).is_file():
        pytest.fail("CALCULIX_NATIVE_EXECUTABLE must name an existing file")
    executable = Path(value).resolve()
    try:
        probe_release(executable)
    except (OSError, subprocess.TimeoutExpired, RuntimeError) as error:
        pytest.fail(f"Native release qualification failed: {type(error).__name__}")
    return executable


def run_case(executable, path, tip):
    """Read named original-node U/RF only after a successful native run."""
    result = subprocess.run([str(executable), "-i", path.stem], cwd=path.parent,
                            capture_output=True, text=True, timeout=60,
                            env={**os.environ, "OMP_NUM_THREADS": "1"})
    assert result.returncode == 0, f"Native solver exit status {result.returncode}"
    assert "*ERROR" not in (result.stdout + result.stderr).upper(), "Native solver reported *ERROR"
    text = path.with_suffix(".dat").read_text(encoding="utf-8")
    return read_vector(text, "U", "TIP", tip), read_vector(text, "RF", "ROOT", 1)
