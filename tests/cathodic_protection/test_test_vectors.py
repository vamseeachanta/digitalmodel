"""Run the cathodic-protection YAML test vectors against the public API (#2213).

``tests/fixtures/test_vectors/cathodic_protection/*.yaml`` were read by
nothing before this file. Each vector file has ``worked_examples`` with
``description`` / ``inputs`` / ``outputs`` / ``use_as_test`` (there is no
``function`` key), so every file is mapped here by *output key*: the
``_MAPPINGS`` table names, per file and per output key, the callable that
turns the example's ``inputs`` into that output. Numeric outputs are asserted
within the example's ``tolerance`` (absolute; falls back to the file-level
``outputs[0].tolerance``, then to 1e-3 relative); integer outputs exactly.

Vector files revised by #2213 carry a ``revision_note`` explaining which
expected values were re-derived and why.
"""

from __future__ import annotations

import math
from pathlib import Path
from typing import Any, Callable

import pytest
import yaml

from digitalmodel.cathodic_protection import _kernels as kernel
from digitalmodel.cathodic_protection import api_rp_1632, dnv_rp_b401, iso_15589_2
from digitalmodel.cathodic_protection.b401_tables import (
    AnodeEnvironment,
    AnodeMaterial,
    Climate,
    DepthBand,
    DesignPhase,
    PaintCategory,
    coating_breakdown_constants,
    design_current_density,
    design_driving_voltage,
)
from digitalmodel.cathodic_protection.dnv_rp_f103 import (
    protected_length as f103_protected_length,
)
from digitalmodel.cathodic_protection.fuel_system_cp import (
    COATING_BREAKDOWN_FACTOR,
    PROTECTION_POTENTIAL_CSE as FUEL_PROTECTION_POTENTIAL_CSE,
    CoatingType,
    FuelPipeSegment,
    effective_bare_area,
    pipe_surface_area,
)

VECTOR_DIR = Path(__file__).resolve().parents[1] / "fixtures" / "test_vectors" / "cathodic_protection"
B401_EDITION = "2021"
F103_EDITION = "2010"

Resolver = Callable[[dict[str, Any]], Any]


# ---------------------------------------------------------------------------
# Per-file resolvers: output key -> callable(inputs) -> value
# ---------------------------------------------------------------------------


def _b401_current_demand(i: dict[str, Any]) -> float:
    if "climate" in i:
        # Table 10-1 / 10-2 lookup path.
        density = design_current_density(
            Climate(i["climate"]), DepthBand(i["depth_band"]), DesignPhase(i["phase"]),
            edition=i["edition"],
        ).value
    else:
        density = i["current_density_A_m2"]
    return dnv_rp_b401.current_demand(
        i["surface_area_m2"], density, i["breakdown_factor"], edition=B401_EDITION
    )


def _b401_breakdown(i: dict[str, Any]) -> float:
    if "category" in i:
        a, b = coating_breakdown_constants(
            PaintCategory(i["category"]), DepthBand(i["depth_band"]), edition=i["edition"]
        )
        return dnv_rp_b401.coating_breakdown_factor(a.value, b.value, i["t_years"], edition=B401_EDITION)
    return dnv_rp_b401.coating_breakdown_factor(i["a"], i["b"], i["t_years"], edition=B401_EDITION)


def _b401_resistance(i: dict[str, Any]) -> float:
    if "exposed_area_m2" in i:
        return kernel.short_flush_or_bracelet(i["rho"], i["exposed_area_m2"])
    return dnv_rp_b401.anode_resistance_slender_standoff(
        i["rho"], i["L_a"], i["r_a"], proximity_factor=i.get("proximity_factor", 1.0),
        edition=B401_EDITION,
    )


def _fuel_segment(i: dict[str, Any]) -> FuelPipeSegment:
    return FuelPipeSegment(
        segment_id="vector",
        length_m=i["length_m"],
        outer_diameter_m=i["outer_diameter_m"],
        coating_type=CoatingType[i["coating_type"]],
        soil_resistivity_ohm_m=i.get("soil_resistivity_ohm_m", 50.0),
    )


def _fuel_final_factor(i: dict[str, Any]) -> float:
    # effective_bare_area / pipe_surface_area on a unit segment isolates
    # min(f_initial + 0.01 * years, 1.0) without re-implementing it here.
    segment = FuelPipeSegment(
        segment_id="unit",
        length_m=1.0,
        outer_diameter_m=1.0 / math.pi,
        coating_type=CoatingType[i["coating_type"]],
        soil_resistivity_ohm_m=50.0,
    )
    return effective_bare_area(segment, years=i["years"]) / pipe_surface_area(segment)


_MAPPINGS: dict[str, dict[str, Resolver]] = {
    "anode_count_b401.yaml": {
        "anode_count": lambda i: dnv_rp_b401.number_of_anodes(
            i["total_mass_kg"], i["anode_net_mass_kg"], i["round_to_even"], edition=B401_EDITION
        ),
    },
    "anode_mass_b401.yaml": {
        "anode_mass_kg": lambda i: dnv_rp_b401.anode_mass_requirement(
            i["I_mean_A"], i["T_design_years"], i["E_capacity"], i["u_f"], edition=B401_EDITION
        ),
    },
    "anode_resistance_b401.yaml": {
        "resistance_ohm": _b401_resistance,
    },
    "api_rp_1632_underground.yaml": {
        "resistance_ohm": lambda i: api_rp_1632.anode_resistance_vertical_rod(i["rho"], i["L"], i["d"]),
        "driving_voltage_V": lambda i: api_rp_1632.anode_driving_voltage(i["anode_type"]),
        "current_per_anode_A": lambda i: api_rp_1632.current_per_anode(
            i["anode_type"], i["rho"], i["L"], i["d"]
        ),
        "current_demand_A": lambda i: api_rp_1632.current_demand(
            i["surface_area_m2"], i["current_density_mA_m2"]
        ),
        "anode_count": lambda i: api_rp_1632.number_of_anodes(
            i["surface_area_m2"], i["current_density_mA_m2"], i["anode_type"], i["rho"], i["L"], i["d"]
        ),
        "life_years": lambda i: api_rp_1632.anode_life_years(
            i["W_anode_kg"], i["anode_type"], i["I_per_anode_A"]
        ),
    },
    "coating_breakdown_b401.yaml": {
        "breakdown_factor": _b401_breakdown,
    },
    "current_demand_b401.yaml": {
        "current_demand_A": _b401_current_demand,
    },
    "fuel_system_cp.yaml": {
        "effective_bare_area_m2": lambda i: effective_bare_area(_fuel_segment(i), years=i["years"]),
        "breakdown_factors": lambda i: {
            name: COATING_BREAKDOWN_FACTOR[CoatingType[name]] for name in i["coating_types"]
        },
        "final_factor": _fuel_final_factor,
        "criterion_v_cse": lambda i: FUEL_PROTECTION_POTENTIAL_CSE,
    },
    "iso_15589_2_pipeline.yaml": {
        "current_density_mA_m2": lambda i: iso_15589_2.initial_current_density(i["T_seawater_C"]),
        "breakdown_factor": lambda i: iso_15589_2.coating_breakdown_factor(
            i["fc_i"], i["fc_f"], i["t_years"], i["T_design_years"]
        ),
        "current_demand_A": lambda i: iso_15589_2.pipeline_current_demand(
            i["D"], i["L"], i["fc"], i["ic_mA_m2"]
        ),
        "resistance_ohm": lambda i: iso_15589_2.anode_resistance(i["rho"], i["L_a"], i["r_a"]),
        "output_current_A": lambda i: iso_15589_2.anode_output_current(
            i["R_a"], i["E_anode_V"], i["E_struct_V"]
        ),
        "anode_mass_kg": lambda i: iso_15589_2.anode_mass_requirement(
            i["I_mean_A"], i["T_design_years"], i["E_capacity"], i["u_f"]
        ),
    },
    "protected_length_f103.yaml": {
        "protected_length_m": lambda i: f103_protected_length(
            i["delta_E_me"], i["WT"], i["D"], i["rho_me"], i["f_cf"], i["i_cm"], edition=F103_EDITION
        ),
    },
    "sacrificial_anode_b401.yaml": {
        "net_mass_kg": lambda i: kernel.anode_mass(i["I_c"], i["T"], i["epsilon"], i["u"]),
        "anode_count": lambda i: kernel.anode_count(
            i["m_total"] if "m_total" in i else kernel.anode_mass(i["I_c"], i["T"], i["epsilon"], i["u"]),
            i["m_anode"],
        ),
        "resistance_ohm": lambda i: kernel.short_flush_or_bracelet(i["rho"], i["A"]),
        "bracelet_resistance_ohm": lambda i: kernel.short_flush_or_bracelet(i["rho"], i["A_bracelet"]),
        "driving_voltage_V": lambda i: design_driving_voltage(
            AnodeMaterial(i["material"]), edition=i["edition"],
            environment=AnodeEnvironment(i["environment"]),
        ).value,
        "current_output_A": lambda i: kernel.anode_current_output(i["E_c"] - i["E_a"], i["R_a"]),
        "current_per_anode_A": lambda i: kernel.anode_current_output(
            i["E_c"] - i["E_a"], kernel.short_flush_or_bracelet(i["rho"], i["A_bracelet"])
        ),
    },
}


# ---------------------------------------------------------------------------
# Collection
# ---------------------------------------------------------------------------


def _load_vectors() -> list[tuple[str, dict[str, Any]]]:
    files = sorted(VECTOR_DIR.glob("*.yaml"))
    assert files, f"no vector files under {VECTOR_DIR}"
    return [(path.name, yaml.safe_load(path.read_text(encoding="utf-8"))) for path in files]


_VECTORS = _load_vectors()


def _example_cases() -> list[pytest.param]:
    cases = []
    for name, data in _VECTORS:
        default_tol = (data.get("outputs") or [{}])[0].get("tolerance")
        for idx, example in enumerate(data.get("worked_examples", [])):
            if not example.get("use_as_test", False):
                continue
            tol = example.get("tolerance", default_tol)
            for key, expected in example["outputs"].items():
                cases.append(
                    pytest.param(
                        name, example["inputs"], key, expected, tol,
                        id=f"{name}::{idx}::{key}",
                    )
                )
    return cases


def _assert_close(actual: Any, expected: Any, tol: float | None) -> None:
    if isinstance(expected, bool) or isinstance(expected, int) and not isinstance(actual, float):
        assert actual == expected
    elif isinstance(expected, dict):
        assert set(actual) == set(expected)
        for k, v in expected.items():
            _assert_close(actual[k], v, tol)
    elif tol is None:
        assert actual == pytest.approx(expected, rel=1e-3)
    elif tol == 0:
        assert actual == pytest.approx(expected, abs=0.0)
    else:
        assert actual == pytest.approx(expected, abs=tol)


class TestVectorFiles:
    def test_every_vector_file_is_mapped(self):
        """Every YAML under the vector directory has a resolver table here."""
        assert {name for name, _ in _VECTORS} == set(_MAPPINGS)

    @pytest.mark.parametrize("name,data", _VECTORS, ids=[n for n, _ in _VECTORS])
    def test_schema(self, name, data):
        assert data["category"] == "cathodic_protection"
        assert data["legal_scan_passed"] is True
        examples = data["worked_examples"]
        assert examples, f"{name} has no worked examples"
        for example in examples:
            assert set(example) >= {"description", "inputs", "outputs", "use_as_test"}
            for key in example["outputs"]:
                assert key in _MAPPINGS[name], f"{name}: no resolver for output {key!r}"

    @pytest.mark.parametrize("name,inputs,key,expected,tol", _example_cases())
    def test_worked_example(self, name, inputs, key, expected, tol):
        actual = _MAPPINGS[name][key](inputs)
        _assert_close(actual, expected, tol)
