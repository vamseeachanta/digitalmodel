"""Regression tests: spec.yml JumperConfig overrides must reach the analysis.

Covers the review findings (issues #476/#602/#603):
  - run_jumper_analysis previously rebuilt pipe/buoyancy/strake/clamp from
    hardcoded defaults, silently ignoring every JumperConfig override.
  - compute_orcaflex_sections hardcoded buoyancy-module counts and the
    connector length.
  - compute_weight_check hardcoded kit weights and 22 modules.
  - Negative section lengths were silently emitted into line_sections.yml.
  - run_pipeline never threaded the spec's metocean Hs into the Go/No-Go
    splash-zone criterion.
  - build_modular_spec used config.connector_length_m for geometry/mass but
    hardcoded 1.3 m section rows.

No OrcFxAPI or OrcaFlex license required.
"""
from pathlib import Path

import pytest
import yaml

from digitalmodel.marine_ops.installation.jumper_lift import (
    INCH_TO_M,
    JumperConfig,
    BarePipeProperties,
    PipeSectionLengths,
    compute_orcaflex_sections,
    compute_pipe_geometry,
    compute_weight_check,
    run_jumper_analysis,
)
from digitalmodel.marine_ops.installation.jumper_installation import (
    extract_metocean_conditions,
    parse_jumper_config,
    run_pipeline,
)
from digitalmodel.marine_ops.installation.jumper_to_modular_spec import (
    LT_CONNECTOR,
    build_modular_spec,
)

REPO_ROOT = Path(__file__).resolve().parents[3]
SPEC_DIR = REPO_ROOT / "docs/domains/orcaflex/subsea/jumper/installation"
MF_PLET_SPEC = SPEC_DIR / "ballymore_mf_plet" / "spec.yml"


# ---------------------------------------------------------------------------
# Pipe / buoyancy / clamp overrides reach run_jumper_analysis results
# ---------------------------------------------------------------------------


class TestPipeOverridesPropagate:
    def test_pipe_od_and_bend_radius_override_reach_results(self):
        """spec jumper: {pipe_od_inch: 12.75, pipe_bend_radius_inch: 60}
        must NOT produce the 10.75\"/50\" default pipe."""
        spec = {
            "jumper": {
                "config_name": "ballymore_mf_plet",
                "pipe_od_inch": 12.75,
                "pipe_bend_radius_inch": 60.0,
            }
        }
        config = parse_jumper_config(spec)
        results = run_jumper_analysis(config)

        pipe = results["pipe_properties"]
        assert pipe.od_inch == pytest.approx(12.75)
        assert pipe.od_m == pytest.approx(12.75 * INCH_TO_M)
        assert pipe.bend_radius_inch == pytest.approx(60.0)
        assert pipe.bend_radius_m == pytest.approx(60.0 * INCH_TO_M)

        # geometry uses the overridden bend radius (GA!C18 = 336 - R)
        geom = results["pipe_geometry"]
        assert geom["straight_lengths_inch"][0] == pytest.approx(336.0 - 60.0)

        # generated sections differ from the default model (no silent no-op)
        default_results = run_jumper_analysis()
        assert results["orcaflex_sections"] != default_results["orcaflex_sections"]
        assert (
            results["orcaflex_sections_yaml"]
            != default_results["orcaflex_sections_yaml"]
        )

    def test_buoyancy_and_clamp_overrides_reach_results(self):
        spec = {
            "jumper": {
                "config_name": "ballymore_mf_plet",
                "buoyancy_dry_lbs": 1500.0,
                "clamp_weight_kg": 300.0,
            }
        }
        results = run_jumper_analysis(parse_jumper_config(spec))
        assert results["buoyancy_module"].dry_weight_lbs == pytest.approx(1500.0)
        # clamp weight reaches the weight check (5 clamps, GA clamp count)
        assert results["weight_check"]["total_clamp_kg"] == pytest.approx(5 * 300.0)

    def test_connector_length_override_reaches_sections(self):
        spec = {
            "jumper": {
                "config_name": "ballymore_mf_plet",
                "connector_length_m": 2.0,
            }
        }
        results = run_jumper_analysis(parse_jumper_config(spec))
        sections = results["orcaflex_sections"]
        assert sections[0]["length_m"] == pytest.approx(2.0)
        assert sections[-1]["length_m"] == pytest.approx(2.0)


# ---------------------------------------------------------------------------
# Module counts drive sections and weight check
# ---------------------------------------------------------------------------


class TestModuleCountsPropagate:
    def test_num_buoy_modules_in_d_reaches_sections(self):
        """num_buoy_modules_in_d: 12 -> two D-buoy groups of 6 x 1.016 m,
        not the hardcoded 10 + 10."""
        config = JumperConfig(num_buoy_modules_in_d=12)
        pipe = BarePipeProperties()
        geom = compute_pipe_geometry(PipeSectionLengths(), pipe.bend_radius_inch)
        sections = {s["name"]: s for s in compute_orcaflex_sections(geom, pipe, config)}
        assert sections["D-buoy-10a"]["length_m"] == pytest.approx(6 * 1.016)
        assert sections["D-buoy-10b"]["length_m"] == pytest.approx(6 * 1.016)

    def test_odd_buoy_count_split(self):
        config = JumperConfig(num_buoy_modules_in_d=5)
        pipe = BarePipeProperties()
        geom = compute_pipe_geometry(PipeSectionLengths(), pipe.bend_radius_inch)
        sections = {s["name"]: s for s in compute_orcaflex_sections(geom, pipe, config)}
        assert sections["D-buoy-10a"]["length_m"] == pytest.approx(3 * 1.016)
        assert sections["D-buoy-10b"]["length_m"] == pytest.approx(2 * 1.016)

    def test_weight_check_uses_config_counts_and_kit_weights(self):
        config = JumperConfig(
            num_buoy_modules_in_c=2,
            num_buoy_modules_in_d=12,
            num_buoy_modules_in_e=2,
            kit1_wi_insul_kg=9000.0,
        )
        wc = compute_weight_check(config=config)
        assert wc["num_buoy_modules"] == 16
        assert wc["total_buoy_kg"] == pytest.approx(16 * 583.772904, abs=1e-2)
        assert wc["kit_weights_kg"]["KIT1"] == pytest.approx(9000.0)

    def test_weight_check_without_config_keeps_workbook_values(self):
        """Workbook lock (Weight Check!C24 = 22 modules, C27 = 46032.486 kg)."""
        wc = compute_weight_check()
        assert wc["num_buoy_modules"] == 22
        assert wc["grand_total_kg"] == pytest.approx(46032.48595184, abs=1e-1)


# ---------------------------------------------------------------------------
# Negative section lengths fail loud
# ---------------------------------------------------------------------------


class TestNegativeLengthValidation:
    def test_short_segment_d_raises_instead_of_negative_sections(self):
        """seg_d_inch=800 -> D straight (17.78 m) cannot hold the 20-module
        stack (21.49 m); previously emitted length_m: -1.854 silently."""
        config = JumperConfig(seg_d_inch=800.0)
        with pytest.raises(ValueError, match="[Nn]egative"):
            run_jumper_analysis(config)

    def test_segment_shorter_than_bend_deduction_raises(self):
        with pytest.raises(ValueError, match="[Nn]egative"):
            compute_pipe_geometry(PipeSectionLengths(B_inch=80.0), 50.0)

    def test_defaults_still_pass_validation(self):
        results = run_jumper_analysis()
        assert all(s["length_m"] > 0 for s in results["orcaflex_sections"])


# ---------------------------------------------------------------------------
# parse_jumper_config: no silent drops
# ---------------------------------------------------------------------------


class TestParseJumperConfig:
    def test_schema_advertised_fields_are_config_fields(self):
        """num_strakes / clamp_ocs_offset_m were dropped by the hasattr
        filter because JumperConfig lacked them."""
        spec = {
            "jumper": {
                "config_name": "ballymore_mf_plet",
                "num_strakes": 3,
                "clamp_ocs_offset_m": 1.5,
            }
        }
        config = parse_jumper_config(spec)
        assert config.num_strakes == 3
        assert config.clamp_ocs_offset_m == pytest.approx(1.5)
        # num_strakes is consumed by the weight check
        wc = compute_weight_check(config=config)
        assert wc["num_strake_modules"] == 3

    def test_unknown_jumper_key_fails_loud(self):
        spec = {"jumper": {"config_name": "ballymore_mf_plet", "not_a_field": 1.0}}
        with pytest.raises(ValueError, match="not_a_field"):
            parse_jumper_config(spec)

    def test_shipped_specs_parse_clean(self):
        for name in ("ballymore_mf_plet", "ballymore_plet_plem"):
            raw = yaml.safe_load((SPEC_DIR / name / "spec.yml").read_text())
            config = parse_jumper_config(raw)
            assert config.name == name


# ---------------------------------------------------------------------------
# run_pipeline threads the spec's metocean Hs into Go/No-Go
# ---------------------------------------------------------------------------


class TestPipelineMetoceanConditions:
    def test_extract_metocean_conditions(self):
        spec = yaml.safe_load(MF_PLET_SPEC.read_text())
        assert extract_metocean_conditions(spec) == {"hs_m": 4.0}
        assert extract_metocean_conditions({"environment": {}}) == {}
        assert extract_metocean_conditions({}) == {}

    def test_pipeline_evaluates_splash_zone_at_spec_hs(self, tmp_path):
        """ballymore_mf_plet carries Hs 4.0 m (4x the DNV-ST-N001 1.0 m
        limit): the splash-zone criterion must be evaluated at 4.0 m and
        FAIL, driving an overall NO_GO — not report 0.00 m PASS."""
        output = run_pipeline(
            str(MF_PLET_SPEC), output_dir=str(tmp_path / "out")
        )
        decision = output.go_no_go
        assert decision is not None
        splash = [c for c in decision.criteria if c.name == "Splash zone Hs"]
        assert len(splash) == 1
        assert splash[0].value == pytest.approx(4.0)
        assert splash[0].state.name == "FAIL"
        assert decision.overall_state.name == "NO_GO"

    def test_pipeline_custom_criteria_forwarded(self, tmp_path):
        """go_no_go_criteria overrides (PipelineConfig.go_no_go_criteria
        contract) reach evaluate_go_no_go."""
        output = run_pipeline(
            str(MF_PLET_SPEC),
            output_dir=str(tmp_path / "out"),
            go_no_go_criteria={"splash_zone_hs_limit_m": 5.0},
        )
        splash = [
            c for c in output.go_no_go.criteria if c.name == "Splash zone Hs"
        ][0]
        assert splash.limit == pytest.approx(5.0)
        assert splash.state.name != "FAIL"


# ---------------------------------------------------------------------------
# build_modular_spec: connector override consistency (#602)
# ---------------------------------------------------------------------------


class TestModularSpecConnectorConsistency:
    def _spec_with_connector(self, tmp_path, length_m: float) -> Path:
        raw = yaml.safe_load(MF_PLET_SPEC.read_text())
        raw["jumper"]["connector_length_m"] = length_m
        path = tmp_path / "spec.yml"
        path.write_text(yaml.safe_dump(raw, sort_keys=False))
        return path

    def test_connector_rows_and_mass_follow_override(self, tmp_path):
        """connector_length_m: 2.0 -> section rows carry 2.0 m OCS 200-V
        sections and mass_per_length x length recovers the full 1678.5 kg
        connector mass (previously rows stayed 1.3 m: 35% mass under-count
        and section total inconsistent with the walked path)."""
        data = build_modular_spec(
            str(self._spec_with_connector(tmp_path, 2.0))
        )
        props = data["generic"]["lines"][0]["properties"]
        key = next(k for k in props if k.startswith("LineType,"))
        rows = props[key]
        connector_rows = [r for r in rows if r[0] == LT_CONNECTOR]
        assert len(connector_rows) == 2
        assert all(r[1] == pytest.approx(2.0) for r in connector_rows)

        lt = {t["name"]: t for t in data["generic"]["line_types"]}
        mass_te = lt[LT_CONNECTOR]["mass_per_length"] * connector_rows[0][1]
        assert mass_te == pytest.approx(1678.5 / 1000.0, abs=1e-3)
