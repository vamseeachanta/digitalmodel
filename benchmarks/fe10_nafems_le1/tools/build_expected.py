# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Write expected.json for FE10 using the standard library.

Usage: python -I build_expected.py <package dir>

LE1: the reference value is retained from the received public-source record and
the MIT-licensed excerpt, with source revisions and line locations listed below.
Patch test: plane-stress Hooke's law for the imposed linear field
u = 1e-3 x + 0.5e-3 y, v = 0.5e-3 x + 1e-3 y (eps_xx = 1e-3, eps_yy = 1e-3, gamma_xy = 1e-3).
"""
import json
import sys
from pathlib import Path
from typing import Any

E, NU = 210000.0, 0.3
exx, eyy, gxy = 1e-3, 1e-3, 1e-3
c = E / (1 - NU ** 2)
out: dict[str, Any] = {
    "le1": {
        "quantity": "sigma_yy at point D (2000, 0) mm, inner ellipse on the x axis",
        "value_MPa": 92.7,
        "cited_as": "NAFEMS, The Standard NAFEMS Benchmarks, TNSB Rev. 3 (1990), test LE1 elliptic membrane",
        "licensed": True,
        "kept_because": [
            "axia-fem solver/scripts/gen_nafems.py, commit 884ec6fe7b74f0a1a9b9ae55e57ccfba625a93c1, line 8: 'LE1  sigma_yy at D (inner, +x) = 92.7 MPa' (MIT licence)",
            "featool-multiphysics examples/ex_planestress5.m, commit 8207417131bc90534931a694a5d8d28ec17ca9fa, line 112: 'out.err = abs(sy_D - 92.7e6)/92.7e6' (licence not asserted)",
            "feenox examples/nafems-le1.yaml, commit 51bde067cc8f283f3797ee8eaba814cf42c66ce4, line 15: 'sigma_y at point D = 92.7011 (reference is 92.7)' (licence not asserted)"],
        "definition_sources": "geometry, thickness, material, load and symmetry: axia-fem gen_nafems.py lines 44-106; FEATool ex_planestress5.m lines 26-27, 39-42, 66-90",
        "band_pct": 5.0,
        "band_source": "Fixed FE10 criteria: LE1 within 5 % on the fine benchmark mesh"},
    "patch": {
        "field": "u = 1e-3 x + 0.5e-3 y, v = 0.5e-3 x + 1e-3 y (mm); plane stress, E = 210000 MPa, nu = 0.3",
        "formula": "sigma_xx = E/(1-nu^2) (eps_xx + nu eps_yy); sigma_yy = E/(1-nu^2) (eps_yy + nu eps_xx); tau_xy = E/(2(1+nu)) gamma_xy",
        "expected_MPa": {"sigma_xx_MPa": c * (exx + NU * eyy), "sigma_yy_MPa": c * (eyy + NU * exx), "tau_xy_MPa": E / (2 * (1 + NU)) * gxy},
        "band_pct": 0.1,
        "band_source": "Fixed FE10 criteria: constant-stress patch test exact within 0.1 %"},
}
pkg = Path(sys.argv[1])
(pkg / "expected.json").write_text(json.dumps(out, indent=1) + "\n", encoding="utf-8", newline="\n")
print("expected.json: LE1 %.1f MPa; patch sigma_xx %.3f, sigma_yy %.3f, tau_xy %.3f MPa"
      % (out["le1"]["value_MPa"], *out["patch"]["expected_MPa"].values()))
