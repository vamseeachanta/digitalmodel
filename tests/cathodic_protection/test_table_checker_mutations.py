# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Mutation probes for existing CSV table oracles, including optimized Python.

No standard values are repeated here. Expected numbers come from the existing
B401 and F103 CSV parsers; their editions and table locators are documented in
the corresponding test modules and dataset PROVENANCE.md. Each probe changes
only metadata, leaving the numeral fixed. Rejection must use an explicit
exception, rather than an assertion that Python optimization can remove.
"""

from dataclasses import replace
import importlib.util
import os
from pathlib import Path
import subprocess
import sys

import pytest

from digitalmodel.citations.table_verification import TableVerificationError


def _load_checks(name):
    path = Path(__file__).with_name(f"test_{name}_tables.py")
    spec = importlib.util.spec_from_file_location(f"audit_{name}", path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def checker_case(name):
    """Return a production module, lookup name and independent CSV check."""
    b = _load_checks("b401")
    f = _load_checks("f103")
    a = _load_checks("abs")
    climate, band = b.Climate.TEMPERATE, b.DepthBand.M0_30
    material, environment = b.AnodeMaterial.ALUMINIUM, b.AnodeEnvironment.SEAWATER
    cases = {
        "b401-initial": (b.tbl, "design_current_density", lambda:
            b.test_design_current_density_matches_table_1(
                "2017", climate, band, b.DesignPhase.INITIAL)),
        "b401-mean": (b.tbl, "design_current_density", lambda:
            b.test_design_current_density_mean_matches_table_2("2021", climate, band)),
        "b401-reinforcement": (b.tbl, "reinforcement_current_density", lambda:
            b.test_reinforcement_current_density_matches_table_3("2021", climate, band)),
        "b401-coating": (b.tbl, "coating_breakdown_constants", lambda:
            b.test_coating_breakdown_constants_match_table_4("2021", b.PaintCategory.III, band)),
        "b401-ambient-capacity": (b.tbl, "anode_capacity", lambda:
            b.test_anode_capacity_and_potential_match_ambient_table_6("2017", material, environment)),
        "b401-temperature-potential": (b.tbl, "anode_closed_circuit_potential", lambda:
            b.test_anode_values_2021_match_table_8_6(material, environment, "≤30")),
        "f103-density": (f.tbl, "mean_current_density", lambda:
            f.test_mean_current_density_matches_density_table("2019", f.Exposure.NON_BURIED, f.FluidTemperatureBand.LE_25)),
        "f103-linepipe-2010": (f.tbl, "linepipe_coating_constants", lambda:
            f.test_linepipe_coating_constants_match_table_a1_2010(f.LinepipeCoating.FBE)),
        "f103-linepipe-2019": (f.tbl, "linepipe_coating_constants", lambda:
            f.test_linepipe_coating_constants_match_table_a1_2019(*next(iter(f.table_a1_2019())))),
        "f103-joint-2010": (f.tbl, "field_joint_coating_constants", lambda:
            f.test_field_joint_coating_constants_match_table_a2_2010(f.FieldJointCoating.NONE)),
        "f103-joint-2019": (f.tbl, "field_joint_coating_constants", lambda:
            f.test_field_joint_coating_constants_match_table_a2_2019(next(iter(f.FieldJointCoating2019)))),
        "f103-potential": (f.tbl, "anode_closed_circuit_potential", lambda:
            f.test_anode_values_2019_match_table_6_3(material, environment, "≤30")),
        "abs-coating": (a.tbl, "coating_breakdown_range", lambda:
            a.test_coating(next(r for r in a._rows("section-2-table-4.csv") if r["durability"] != "all"))),
        "abs-average-density": (a.tbl, "average_coated_current_density", lambda:
            a.test_average_density(a._rows("section-2-table-5.csv")[0])),
        "abs-design-density": (a.tbl, "design_current_density", lambda:
            a.test_design_density(next(r for r in a._rows("section-2-table-3.csv") if r["situation"] in a.SITUATIONS), "bare")),
        "abs-aluminium": (a.tbl, "aluminium_properties", lambda:
            a.test_aluminium(next(r for r in a._rows("section-3-table-4.csv") if r["alloy"] == "A1"))),
        "abs-zinc": (a.tbl, "zinc_properties", lambda:
            a.test_zinc(a._rows("section-3-table-2.csv")[0])),
        "abs-protection": (a.tbl, "protection_potential", lambda:
            a.test_protection(next(r for r in a._rows("section-2-table-1.csv") if r["condition"] == "aerobic"))),
    }
    return cases[name]


CASES = (
    "b401-initial", "b401-mean", "b401-reinforcement", "b401-coating",
    "b401-ambient-capacity", "b401-temperature-potential", "f103-density",
    "f103-linepipe-2010", "f103-linepipe-2019", "f103-joint-2010",
    "f103-joint-2019", "f103-potential",
    "abs-coating", "abs-average-density", "abs-design-density",
    "abs-aluminium", "abs-zinc", "abs-protection",
)
MUTATIONS = ("units", "revision", "wiki_path", "note", "section")
TUPLE_CASES = {
    "b401-coating", "f103-linepipe-2010", "f103-linepipe-2019",
    "f103-joint-2010", "f103-joint-2019", "abs-coating",
    "abs-average-density", "abs-design-density", "abs-aluminium", "abs-zinc",
}
PROBES = tuple((case, mutation) for case in CASES for mutation in MUTATIONS) + tuple(
    (case, "second-units") for case in CASES if case in TUPLE_CASES
)


def run_probe(case, mutation):
    """Invoke the real CSV checker with a metadata-only production mutation."""
    module, lookup, check = checker_case(case)
    original = getattr(module, lookup)

    def mutate(value):
        if mutation in ("units", "second-units"):
            return replace(value, units="unrelated-unit")
        changed = "wikis/unrelated.md" if mutation == "wiki_path" else "unrelated"
        return replace(value, citation=replace(value.citation, **{mutation: changed}))

    def changed_lookup(*args, **kwargs):
        result = original(*args, **kwargs)
        if isinstance(result, tuple):
            if mutation == "second-units":
                return (*result[:-1], mutate(result[-1]))
            return tuple(mutate(value) for value in result)
        return mutate(result)

    with pytest.MonkeyPatch.context() as patch:
        if mutation != "match":
            patch.setattr(module, lookup, changed_lookup)
        try:
            check()
        except TableVerificationError:
            if mutation != "match":
                return
            raise
    if mutation != "match":
        raise RuntimeError("table checker accepted a metadata mutation")


@pytest.mark.parametrize(("case", "mutation"), PROBES)
def test_csv_checker_rejects_metadata_mutation(case, mutation):
    run_probe(case, mutation)


def test_csv_checker_under_optimized_python():
    program = """
import importlib.util
import sys
spec = importlib.util.spec_from_file_location('probes', sys.argv[1])
module = importlib.util.module_from_spec(spec)
spec.loader.exec_module(module)
for case in module.CASES:
    module.run_probe(case, "match")
for case, mutation in module.PROBES:
    module.run_probe(case, mutation)
print(len(module.CASES), len(module.PROBES))
"""
    env = dict(os.environ, PYTHONPATH=os.pathsep.join(sys.path))
    result = subprocess.run(
        [sys.executable, "-O", "-c", program, __file__],
        env=env, capture_output=True, text=True, check=False,
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout.strip() == f"{len(CASES)} {len(PROBES)}"


def test_documented_worked_example(tmp_path):
    """Independent oracle: 3 m + 3 m = 6 m; relabelled units/source must fail."""
    document = (
        Path(__file__).resolve().parents[2] / "docs" / "domains"
        / "cathodic_protection" / "table-checker-audit.md"
    ).read_text(encoding="utf-8")
    program = document.split("```python\n", 1)[1].split("```", 1)[0]
    namespace = {"reference_root": tmp_path}
    exec(compile(program, "worked-example", "exec"), namespace)
    assert namespace["outcomes"] == ["PASS", "REJECTED", "REJECTED"]
