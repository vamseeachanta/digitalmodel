# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Sourced inlet-seed regressions; no solver execution is required.

Sources: Notes on CFD: General Principles (2022), sections 7.2 and 7.10,
equations (7.6), (7.38)-(7.40), for k, C_mu, nut and omega:
https://doc.cfd.direct/notes/cfd-general-principles/initialisation-of-the-k-epsilon-model
https://doc.cfd.direct/notes/cfd-general-principles/specific-dissipation-rate

The default ratio is SU2 v8.0.1 config_template.cfg, FREESTREAM_TURB2LAMVISCRATIO:
https://github.com/su2code/SU2/blob/v8.0.1/config_template.cfg#L224-L225

All other inputs below are synthetic arithmetic fixtures, not measurements
or empirical acceptance limits. pytest.approx checks arithmetic roundoff.
"""

import json
import math
import re

import pytest

from digitalmodel.solvers.openfoam.hull_case import HullCaseConfig, build_hull_case
from digitalmodel.solvers.openfoam.hull_double_body import (
    DoubleBodyCaseConfig,
    build_double_body_case,
)
from digitalmodel.solvers.openfoam.hull_manifest import HullManifest
from digitalmodel.solvers.openfoam.hull_turbulence import (
    TurbulenceMethod,
    derive_inlet_turbulence,
)


def test_default_seed_matches_independent_hand_calculation():
    """Section 7.2 (7.6) and 7.10 (7.38)-(7.40), with SU2's ratio.

    Choose U=2 m/s, I=0.1, nu=1e-6 m2/s, Lpp=6 m. Then
    k=3/2*(2*0.1)^2=0.06 m2/s2, nut=10*1e-6=1e-5 m2/s,
    omega=0.06/1e-5=6000 /s. Since C_mu=0.09,
    l=sqrt(0.06)/(0.09**0.25*6000)=sqrt(0.2)/6000 m.
    """
    result = derive_inlet_turbulence(2.0, 6.0, 1e-6, intensity=0.1)
    assert result.viscosity_ratio == 10.0
    assert result.k == pytest.approx(0.06)
    assert result.nut == pytest.approx(1e-5)
    assert result.omega == pytest.approx(6000.0)
    assert result.length_scale == pytest.approx(math.sqrt(0.2) / 6000.0)


@pytest.mark.parametrize(
    "ratio, expected_nut, expected_omega",
    [(0.1, 1e-7, 600000.0), (1.0, 1e-6, 60000.0), (20.0, 2e-5, 3000.0)],
)
def test_explicit_ratio_remains_a_caller_choice(ratio, expected_nut, expected_omega):
    """Same cited equations: k=0.06, nut=ratio*1e-6, omega=0.06/nut.

    Synthetic ratios below and above the default exercise caller control;
    they are not asserted to produce a qualified turbulent solution.
    """
    result = derive_inlet_turbulence(
        2.0, 6.0, 1e-6, intensity=0.1, viscosity_ratio=ratio
    )
    assert result.nut == pytest.approx(expected_nut)
    assert result.omega == pytest.approx(expected_omega)


def test_length_scale_method_keeps_its_explicit_closure():
    """Section 7.10 (7.40): choose l/Lpp=0.07, so l=0.07*6=0.42 m.

    With k=0.06 and C_mu=0.09, omega=sqrt(0.2)/0.42 /s and
    nut=0.06*0.42/sqrt(0.2) m2/s. The viscosity-ratio default is unused.
    """
    result = derive_inlet_turbulence(
        2.0,
        6.0,
        1e-6,
        intensity=0.1,
        method=TurbulenceMethod.LENGTH_SCALE,
        length_scale_factor=0.07,
    )
    assert result.length_scale == pytest.approx(0.42)
    assert result.omega == pytest.approx(math.sqrt(0.2) / 0.42)
    assert result.nut == pytest.approx(0.06 * 0.42 / math.sqrt(0.2))


@pytest.mark.parametrize(
    "config_type, builder",
    [(HullCaseConfig, build_hull_case), (DoubleBodyCaseConfig, build_double_body_case)],
)
@pytest.mark.parametrize("condition", ["default", "override", "reynolds"])
def test_both_builders_emit_the_seed_and_record_it(
    config_type,
    builder,
    condition,
    manifest_dict,
    stl_file,
    tmp_path,
):
    """Use the default-seed hand calculation above through each real builder.

    The synthetic manifest has Lpp=6 m. For the Reynolds-input variant,
    Re=12e6 gives nu=U*Lpp/Re=2*6/12e6=1e-6 m2/s by definition.
    The explicit legacy-ratio variant gives nut=0.1*1e-6=1e-7 m2/s and
    omega=0.06/1e-7=600000 /s. Otherwise nut=1e-5 and omega=6000.
    One rank is an execution fixture, not a physical coefficient.
    """
    options = {"kinematic_viscosity": 1e-6}
    if condition == "override":
        options["eddy_viscosity_ratio"] = 0.1
    elif condition == "reynolds":
        options = {"reynolds": 12e6}
    config = config_type(
        manifest=HullManifest.from_dict(manifest_dict),
        stl_path=stl_file,
        velocity=2.0,
        ranks=1,
        turbulence_intensity=0.1,
        **options,
    )
    case = builder(config, tmp_path / "cases")
    ratio = 0.1 if condition == "override" else 10.0
    nut = 1e-7 if condition == "override" else 1e-5
    omega = 600000.0 if condition == "override" else 6000.0
    for field, expected in (("k", 0.06), ("nut", nut), ("omega", omega)):
        text = (case / "0.orig" / field).read_text()
        entry = re.search(r"internalField\s+uniform\s+([^;]+);", text)
        assert entry is not None, field
        assert float(entry.group(1)) == pytest.approx(expected), field
        inlet = re.search(r"\binlet\s*\{([^}]+)\}", text)
        assert inlet is not None, field
        assert re.search(r"value\s+\$internalField\s*;", inlet.group(1)), field
    provenance = json.loads((case / "case_provenance.json").read_text())["turbulence"]
    assert provenance["eddy_viscosity_ratio"] == ratio
    assert provenance["k"] == pytest.approx(0.06)
    assert provenance["nut"] == pytest.approx(nut)
    assert provenance["omega"] == pytest.approx(omega)
    assert provenance["length_scale_m"] == pytest.approx(math.sqrt(0.2) / omega)
