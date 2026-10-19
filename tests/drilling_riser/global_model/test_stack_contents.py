"""Offline stack-contents regressions with independent vertical force balances.

Synthetic inputs are defined in conftest.synthetic_spec: stack lengths 7 and
9 m, masses 15000 and 30000 kg/m, displaced volumes 2.1 and 4 m3/m,
water density 1025 kg/m3 and contents density 1500 kg/m3. The alternative
0.4 m bore and zero density below are synthetic test inputs, not design data.
"""

from __future__ import annotations

import math

import pytest
import yaml

from digitalmodel.drilling_riser.global_model.build import write_model
from digitalmodel.drilling_riser.global_model.hand_checks import tension_references
from digitalmodel.drilling_riser.global_model.open_water import (
    OpenWaterRiserSpec,
    tension_references as open_water_references,
)
from digitalmodel.drilling_riser.global_model.spec import RiserGlobalModelSpec

# BIPM, SI Brochure, 9th edition (2019), Appendix 1, 3rd CGPM (1901):
# conventional standard gravity = 980.665 cm/s2 = 9.80665 m/s2.
GRAVITY = 9.80665
# BIPM, SI Brochure, 9th edition (2019), section 4, Table 8: 1 t = 10**3 kg.
KG_PER_TONNE = 1000.0
# Arithmetic regression tolerance only; not an engineering acceptance limit.
ARITHMETIC_REL = 1e-12


def _with_stack_bore(spec, bore, density):
    data = spec.model_dump()
    data["contents"]["density_kg_m3"] = density
    for section in data["stack"]:
        section["bore_id_m"] = bore
    return RiserGlobalModelSpec.model_validate(data)


@pytest.mark.parametrize("bore", [0.0, 0.4])
@pytest.mark.parametrize("density", [0.0, 1500.0])
@pytest.mark.parametrize("topology", ["drilling", "open_water"])
def test_stack_weight_matches_hand_balance_and_emitted_text(
    spec, tmp_path, bore, density, topology
):
    """Hand calculation, independent of the implementation under test:

    University Physics Volume 1 (OpenStax, 2016), section 14.4:
    buoyancy = water density * displaced volume * g. For this fully wet stack,
    first drilling section W/g = 7*(15000 - 1025*2.1) = 89932.5 kg,
    second W/g = 9*(30000 - 1025*4) = 233100 kg; total W/g = 323032.5 kg.
    This includes the stack fluid in the specified mass, with no separate bore
    contents (the drilling convention in build.build_generic_spec, issue #2532).

    Open-water has a continuous bore: add density*pi*bore**2/4 * L * g
    to each section weight, with L = 7 or 9 m. The circle-area factor 1/4
    follows from radius = diameter/2. Zero bore or zero density adds nothing.
    Tbottom = Ttop - W follows from static vertical force equilibrium.
    """
    model = _with_stack_bore(spec, bore, density)
    if topology == "open_water":
        # Reuse the synthetic section data with a continuous-bore topology.
        model = OpenWaterRiserSpec(
            name="synthetic-continuous-bore",
            environment=model.environment,
            contents=model.contents,
            tension_frame={
                "mass_kg": model.tension_ring.mass_kg,
                "z_m": model.upper_flex_joint.pivot_z_m,
            },
            tensioners={
                "total_vertical_tension_n": model.tensioners.total_vertical_tension_n
            },
            rotary_z_m=model.ring_z_geometric_m,
            upper=model.inner_barrel,
            riser=model.riser,
            stack=model.stack,
            wellhead_datum_z_m=model.wellhead_datum_z_m,
        )
        ref = open_water_references(model)
        contents_mass_per_m = density * math.pi * bore**2 / 4
        emitted_density = density / KG_PER_TONNE
    else:
        ref = tension_references(model)
        contents_mass_per_m = 0.0
        emitted_density = 0.0

    weights = [
        (89932.5 + contents_mass_per_m * 7) * GRAVITY,
        (233100.0 + contents_mass_per_m * 9) * GRAVITY,
    ]
    chain = ref["stack_chain"]
    assert [row["submerged_weight_n"] for row in chain] == pytest.approx(
        weights, rel=ARITHMETIC_REL
    )
    assert chain[0]["te_top_n"] == ref["riser_chain"][-1]["te_bottom_n"]
    assert chain[1]["te_top_n"] == chain[0]["te_bottom_n"]
    assert ref["stack_bottom_n"] == pytest.approx(
        chain[0]["te_top_n"] - sum(weights), rel=ARITHMETIC_REL
    )
    top_key = "upper_top_n" if topology == "open_water" else "riser_top_n"
    assert ref["submerged_weight_n"] == pytest.approx(
        ref[top_key] - ref["stack_bottom_n"], rel=ARITHMETIC_REL
    )

    # Exercise the actual YAML writer; never load a solver binding.
    output = write_model(model, tmp_path / topology)
    documents = [
        yaml.safe_load(path.read_text(encoding="utf-8"))
        for path in (output / "includes").glob("*.yml")
    ]
    lines = {
        row["Name"]: row for doc in documents for row in (doc or {}).get("Lines", [])
    }
    types = {
        row["Name"]: row
        for doc in documents
        for row in (doc or {}).get("LineTypes", [])
    }
    assert lines["Stack"]["ContentsDensity"] == emitted_density
    for section in model.stack:
        emitted = types[section.name]
        assert emitted["ID"] == bore
        assert emitted["MassPerUnitLength"] * KG_PER_TONNE == section.mass_per_m_kg
