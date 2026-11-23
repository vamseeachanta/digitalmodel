"""Semantic proof: PLET-to-PLEM rigid jumper spec.yml -> native OrcaFlex YAML (#2455).

Locks in the forward-path contract for the rigid-jumper family:

    canonical spec.yml
        --> ProjectInputSpec (pydantic validation)
        --> ModularModelGenerator.from_spec(spec).generate(tmpdir)
        --> includes/*.yml

Assertions inspect the generated include tree to prove that
analysis-significant jumper properties survive generation:

* Three jumper line-type variants (bare coated / with buoyancy / with strakes)
* OCS 200-V collet connector line type mass-per-length
* CoatingThickness/CoatingMaterialDensity on the coated variant
* JumperLine instance with all three variants referenced cross-section
* Every LineType reference in Lines resolves to a LineType definition by name
* PLET/PLEM-side CraneBoomBase constraint with ConstraintType preserved
* Water depth / density carried through to the Environment include
* Multi-stage simulation durations carried through to parameters.yml

Runs deterministic YAML generation only; does not require OrcFxAPI.

See also: ``test_semantic_roundtrip.py`` which covers the reverse
(``extract -> spec -> generate``) roundtrip for the generic family.
"""

from __future__ import annotations

import tempfile
from pathlib import Path

import pytest
import yaml

from digitalmodel.solvers.orcaflex.modular_generator import ModularModelGenerator
from digitalmodel.solvers.orcaflex.modular_generator.schema import ProjectInputSpec


SPEC_PATH = (
    Path(__file__).resolve().parents[4]
    / "docs"
    / "domains"
    / "orcaflex"
    / "jumper"
    / "plet_to_plem"
    / "spec.yml"
)


# ---------------------------------------------------------------------------
# Module-scoped generate() cache — loading + generating from a 68k-line spec
# is expensive; every assertion reads from the same generated tree.
# ---------------------------------------------------------------------------


@pytest.fixture(scope="module")
def generated_includes(tmp_path_factory: pytest.TempPathFactory) -> dict[str, dict]:
    """Load the canonical PLET-to-PLEM spec and generate the modular model once.

    Returns a dict mapping include filename -> loaded YAML body, plus
    ``_parameters`` and ``_master_text`` entries for parameters.yml / master.yml.
    """
    if not SPEC_PATH.exists():
        pytest.skip(f"Canonical jumper spec not present: {SPEC_PATH}")

    with open(SPEC_PATH, encoding="utf-8") as f:
        data = yaml.safe_load(f)
    spec = ProjectInputSpec(**data)
    assert spec.is_generic(), "plet_to_plem spec should be a generic-track model"

    out_dir = tmp_path_factory.mktemp("jumper_plet_to_plem_proof")
    ModularModelGenerator.from_spec(spec).generate(out_dir)

    includes_dir = out_dir / "includes"
    loaded: dict[str, dict] = {}
    for yml in sorted(includes_dir.glob("*.yml")):
        with open(yml, encoding="utf-8") as f:
            loaded[yml.name] = yaml.safe_load(f) or {}

    params_path = out_dir / "inputs" / "parameters.yml"
    if params_path.exists():
        with open(params_path, encoding="utf-8") as f:
            loaded["_parameters"] = yaml.safe_load(f) or {}

    master_path = out_dir / "master.yml"
    if master_path.exists():
        loaded["_master_text"] = master_path.read_text(encoding="utf-8")

    return loaded


def _generic_section(generated_includes: dict[str, dict], key: str):
    """Pull a section out of ``20_generic_objects.yml``."""
    generic = generated_includes.get("20_generic_objects.yml", {})
    assert generic, "20_generic_objects.yml missing — generic builder did not run"
    return generic.get(key)


# ---------------------------------------------------------------------------
# LineTypes — jumper coating/buoyancy/strake variants
# ---------------------------------------------------------------------------


class TestJumperLineTypes:
    """Three jumper LineType variants plus the OCS 200-V connector must survive."""

    def test_three_jumper_variants_emitted(self, generated_includes):
        line_types = _generic_section(generated_includes, "LineTypes") or []
        names = {lt.get("Name") for lt in line_types}
        expected = {
            '10.75"Jumper_wCoat',
            '10.75"Jumper_wCoat_wBuoy',
            '10.75"Jumper_wCoat_wStrake',
        }
        missing = expected - names
        assert not missing, (
            f"Missing jumper LineType variants in generated output: {missing}. "
            f"Present: {sorted(names)[:20]}"
        )

    def test_wcoat_coating_properties_preserved(self, generated_includes):
        """CoatingThickness/CoatingMaterialDensity on the bare-coated variant
        must appear with their exact spec values — they drive heat-transfer
        and wet-weight calculations and cannot silently default."""
        line_types = _generic_section(generated_includes, "LineTypes") or []
        coated = next(
            (lt for lt in line_types if lt.get("Name") == '10.75"Jumper_wCoat'),
            None,
        )
        assert coated is not None, '10.75"Jumper_wCoat LineType not emitted'
        assert coated.get("CoatingThickness") == 0.0762, (
            f"CoatingThickness drift: expected 0.0762, got {coated.get('CoatingThickness')!r}"
        )
        assert coated.get("CoatingMaterialDensity") == 0.97873, (
            f"CoatingMaterialDensity drift: expected 0.97873, "
            f"got {coated.get('CoatingMaterialDensity')!r}"
        )

    def test_wbuoy_and_wstrake_reference_named_variable_data(self, generated_includes):
        """The buoyancy and strake layers are expressed via a VariableData
        reference on CoatingThickness (Insulation+Buoyancy / Insulation+Strakes),
        not a float — that name-ref must not be downgraded to a numeric default."""
        line_types = _generic_section(generated_includes, "LineTypes") or []
        by_name = {lt.get("Name"): lt for lt in line_types}
        wbuoy = by_name.get('10.75"Jumper_wCoat_wBuoy')
        wstrake = by_name.get('10.75"Jumper_wCoat_wStrake')
        assert wbuoy is not None and wstrake is not None
        assert wbuoy.get("CoatingThickness") == "Insulation+Buoyancy", (
            f"wBuoy CoatingThickness ref lost: {wbuoy.get('CoatingThickness')!r}"
        )
        assert wstrake.get("CoatingThickness") == "Insulation+Strakes", (
            f"wStrake CoatingThickness ref lost: {wstrake.get('CoatingThickness')!r}"
        )

    def test_ocs_200v_connector_mass_per_length(self, generated_includes):
        """OCS 200-V collet connector mass-per-length carries the weight of
        the in-water connector assembly; rounding loses the field's engineering intent."""
        line_types = _generic_section(generated_includes, "LineTypes") or []
        connector = next(
            (lt for lt in line_types if lt.get("Name") == "OCS 200-V"), None
        )
        assert connector is not None, "OCS 200-V connector LineType not emitted"
        assert connector.get("MassPerUnitLength") == pytest.approx(2.514045409), (
            f"OCS 200-V MassPerUnitLength drift: "
            f"got {connector.get('MassPerUnitLength')!r}"
        )
        assert connector.get("Category") == "General"


# ---------------------------------------------------------------------------
# Lines — JumperLine references jumper line-types by name
# ---------------------------------------------------------------------------


class TestJumperLineReferences:
    """JumperLine must reference the three variants by name and every
    reference must resolve to a defined LineType."""

    def test_jumper_line_present(self, generated_includes):
        lines = _generic_section(generated_includes, "Lines") or []
        names = {ln.get("Name") for ln in lines}
        assert "JumperLine" in names, (
            f"JumperLine missing from generated Lines. Present: {sorted(names)}"
        )

    def test_jumper_line_uses_all_three_variants(self, generated_includes):
        lines = _generic_section(generated_includes, "Lines") or []
        jumper = next((ln for ln in lines if ln.get("Name") == "JumperLine"), None)
        assert jumper is not None
        # OrcaFlex compact-key format: the sections array is keyed by a
        # comma-joined string starting with "LineType, Length, ...".
        sections_key = next(
            (k for k in jumper if isinstance(k, str) and k.startswith("LineType,")),
            None,
        )
        assert sections_key is not None, (
            f"JumperLine has no LineType sections array. Keys: {list(jumper)[:10]}"
        )
        referenced = {row[0] for row in jumper[sections_key] if row}
        expected = {
            '10.75"Jumper_wCoat',
            '10.75"Jumper_wCoat_wBuoy',
            '10.75"Jumper_wCoat_wStrake',
        }
        missing = expected - referenced
        assert not missing, (
            f"JumperLine does not reference all three jumper variants: missing {missing}"
        )

    def test_every_line_linetype_reference_resolves(self, generated_includes):
        """Cross-reference closure: every LineType token in every Line
        section must name a LineType defined in the LineTypes section."""
        line_types = _generic_section(generated_includes, "LineTypes") or []
        lines = _generic_section(generated_includes, "Lines") or []
        defined = {lt.get("Name") for lt in line_types if lt.get("Name")}
        assert defined, "LineTypes section emitted but empty"

        unresolved: list[tuple[str, str]] = []
        for line in lines:
            line_name = line.get("Name", "<unnamed>")
            for key, val in line.items():
                if not (isinstance(key, str) and key.startswith("LineType,")):
                    continue
                for row in val or []:
                    if not row:
                        continue
                    ref = row[0]
                    if ref not in defined:
                        unresolved.append((line_name, ref))
        assert not unresolved, (
            f"Unresolved LineType references: {unresolved[:10]}"
        )


# ---------------------------------------------------------------------------
# Constraints — CraneBoomBase anchors the installation kinematics
# ---------------------------------------------------------------------------


class TestCraneConstraint:
    def test_crane_boom_base_constraint_emitted(self, generated_includes):
        constraints = _generic_section(generated_includes, "Constraints") or []
        crane = next(
            (c for c in constraints if c.get("Name") == "CraneBoomBase"), None
        )
        assert crane is not None, (
            "CraneBoomBase constraint missing from generated Constraints"
        )
        assert crane.get("ConstraintType") == "Calculated DOFs", (
            f"CraneBoomBase ConstraintType drift: got {crane.get('ConstraintType')!r}"
        )
        assert crane.get("InFrameConnection") == "Main Crane Pedestal", (
            f"CraneBoomBase InFrameConnection drift: "
            f"got {crane.get('InFrameConnection')!r}"
        )


# ---------------------------------------------------------------------------
# Environment & Simulation — deep-water parameters must not be defaulted
# ---------------------------------------------------------------------------


class TestEnvironmentAndSimulation:
    def test_water_depth_and_density_carried_through(self, generated_includes):
        env_yml = generated_includes.get("03_environment.yml", {})
        env = env_yml.get("Environment") if isinstance(env_yml, dict) else None
        assert env, "Environment section missing from 03_environment.yml"
        # The spec records water depth 1996 m and density 1.025 te/m3.
        assert env.get("WaterDepth") == 1996 or env.get("SeabedOriginDepth") == 1996, (
            f"WaterDepth drift: depth fields {{"
            f"WaterDepth: {env.get('WaterDepth')!r}, "
            f"SeabedOriginDepth: {env.get('SeabedOriginDepth')!r}}}"
        )
        assert env.get("Density") == 1.025, (
            f"Water density drift: got {env.get('Density')!r}"
        )

    def test_stage_durations_preserved_in_parameters(self, generated_includes):
        params = generated_includes.get("_parameters", {})
        assert params, "parameters.yml missing from generated output"
        stages = params.get("stage_durations")
        assert stages == [10, 10, 1], (
            f"simulation.stages drift: expected [10, 10, 1], got {stages!r}"
        )


# ---------------------------------------------------------------------------
# Master file — proves the includes are actually wired into the model
# ---------------------------------------------------------------------------


class TestMasterIncludesWired:
    def test_master_includes_generic_objects_and_environment(self, generated_includes):
        master = generated_includes.get("_master_text", "")
        assert "includes/20_generic_objects.yml" in master, (
            "master.yml does not include 20_generic_objects.yml"
        )
        assert "includes/03_environment.yml" in master, (
            "master.yml does not include 03_environment.yml"
        )
