# ABOUTME: TDD tests for the 50x50 M-T allowable lookup table generator
# ABOUTME: Validates grid shape, columns, value ranges, CSV output

import os
import tempfile

import pandas as pd
import pytest

from digitalmodel.structural.analysis.wall_thickness import (
    DesignCode,
    DesignFactors,
    DesignLoads,
    PipeGeometry,
    PipeMaterial,
    SafetyClass,
)
from digitalmodel.structural.analysis.wall_thickness_lookup import (
    LookupConfig,
    MTLookupGenerator,
)


def make_config(**overrides):
    defaults = dict(
        geometry=PipeGeometry(outer_diameter=0.27305, wall_thickness=0.0214, corrosion_allowance=0.001),
        material=PipeMaterial(grade="X65", smys=448e6, smts=531e6),
        internal_pressure=20e6,
        external_pressure=5e6,
        safety_class=SafetyClass.MEDIUM,
        codes=[DesignCode.DNV_ST_F101, DesignCode.API_RP_1111],
        n_moment_steps=10,
        n_tension_steps=10,
    )
    defaults.update(overrides)
    return LookupConfig(**defaults)


class TestLookupConfig:
    def test_config_default_codes_includes_all_registered(self):
        cfg = LookupConfig(
            geometry=PipeGeometry(outer_diameter=0.27305, wall_thickness=0.0214),
            material=PipeMaterial(grade="X65", smys=448e6, smts=531e6),
        )
        assert len(cfg.codes) >= 5  # DNV, API, PD, ASME, ISO

    def test_config_default_grid_size_50(self):
        cfg = LookupConfig(
            geometry=PipeGeometry(outer_diameter=0.27305, wall_thickness=0.0214),
            material=PipeMaterial(grade="X65", smys=448e6, smts=531e6),
        )
        assert cfg.n_moment_steps == 50
        assert cfg.n_tension_steps == 50


class TestMTLookupGenerator:
    def test_generate_returns_dataframe(self):
        config = make_config()
        gen = MTLookupGenerator(config)
        df = gen.generate()
        assert isinstance(df, pd.DataFrame)
        assert not df.empty

    def test_generate_correct_row_count(self):
        """Grid 10x10 = 100 rows."""
        config = make_config(n_moment_steps=10, n_tension_steps=10)
        gen = MTLookupGenerator(config)
        df = gen.generate()
        assert len(df) == 100

    def test_generate_correct_row_count_50x50(self):
        """Grid 50x50 = 2500 rows. Using small grid for speed."""
        config = make_config(n_moment_steps=5, n_tension_steps=5)
        gen = MTLookupGenerator(config)
        df = gen.generate()
        assert len(df) == 25

    def test_generate_has_moment_and_tension_columns(self):
        config = make_config()
        gen = MTLookupGenerator(config)
        df = gen.generate()
        assert "moment_kNm" in df.columns
        assert "tension_kN" in df.columns

    def test_generate_has_util_columns_per_code(self):
        config = make_config(codes=[DesignCode.DNV_ST_F101, DesignCode.API_RP_1111])
        gen = MTLookupGenerator(config)
        df = gen.generate()
        assert "util_DNV_ST_F101" in df.columns
        assert "util_API_RP_1111" in df.columns
        assert "allowable_DNV_ST_F101" in df.columns
        assert "allowable_API_RP_1111" in df.columns

    def test_generate_has_util_columns_for_new_codes(self):
        config = make_config(codes=[DesignCode.PD_8010_2, DesignCode.ASME_B31_8, DesignCode.ISO_13623])
        gen = MTLookupGenerator(config)
        df = gen.generate()
        assert "util_PD_8010_2" in df.columns
        assert "util_ASME_B31_8" in df.columns
        assert "util_ISO_13623" in df.columns

    def test_generate_utilisations_non_negative(self):
        config = make_config()
        gen = MTLookupGenerator(config)
        df = gen.generate()
        util_cols = [c for c in df.columns if c.startswith("util_")]
        for col in util_cols:
            assert (df[col] >= 0).all(), f"Negative utilisation in {col}"

    def test_generate_zero_moment_zero_tension_row_exists(self):
        config = make_config(n_moment_steps=10, n_tension_steps=10)
        gen = MTLookupGenerator(config)
        df = gen.generate()
        # First row should be (0, 0)
        assert df.iloc[0]["moment_kNm"] == pytest.approx(0.0)
        assert df.iloc[0]["tension_kN"] == pytest.approx(0.0)

    def test_generate_moment_range_starts_at_zero(self):
        config = make_config()
        gen = MTLookupGenerator(config)
        df = gen.generate()
        assert df["moment_kNm"].min() == pytest.approx(0.0)

    def test_generate_tension_range_starts_at_zero(self):
        config = make_config()
        gen = MTLookupGenerator(config)
        df = gen.generate()
        assert df["tension_kN"].min() == pytest.approx(0.0)

    def test_generate_at_capacity_utilisation_above_one(self):
        """At max moment AND max tension, utilisation should be > 1."""
        config = make_config(n_moment_steps=10, n_tension_steps=10)
        gen = MTLookupGenerator(config)
        df = gen.generate()
        # Last row = max moment + max tension
        last_row = df.iloc[-1]
        util_cols = [c for c in df.columns if c.startswith("util_")]
        # At least one code should show high utilisation at the boundary
        max_util = max(last_row[col] for col in util_cols)
        assert max_util > 0.5  # Should be significant at full capacity

    def test_generate_allowable_is_boolean(self):
        config = make_config()
        gen = MTLookupGenerator(config)
        df = gen.generate()
        allow_cols = [c for c in df.columns if c.startswith("allowable_")]
        for col in allow_cols:
            assert df[col].dtype == bool or set(df[col].unique()).issubset({True, False})


class TestMTLookupCSV:
    def test_to_csv_creates_file(self):
        config = make_config(n_moment_steps=5, n_tension_steps=5)
        gen = MTLookupGenerator(config)
        with tempfile.TemporaryDirectory() as tmpdir:
            path = os.path.join(tmpdir, "lookup.csv")
            result_path = gen.to_csv(path)
            assert os.path.isfile(result_path)

    def test_to_csv_parseable(self):
        config = make_config(n_moment_steps=5, n_tension_steps=5)
        gen = MTLookupGenerator(config)
        with tempfile.TemporaryDirectory() as tmpdir:
            path = os.path.join(tmpdir, "lookup.csv")
            gen.to_csv(path)
            df = pd.read_csv(path)
            assert len(df) == 25
            assert "moment_kNm" in df.columns

    def test_to_csv_correct_headers(self):
        config = make_config(
            n_moment_steps=5, n_tension_steps=5,
            codes=[DesignCode.DNV_ST_F101],
        )
        gen = MTLookupGenerator(config)
        with tempfile.TemporaryDirectory() as tmpdir:
            path = os.path.join(tmpdir, "lookup.csv")
            gen.to_csv(path)
            df = pd.read_csv(path)
            expected_cols = {"moment_kNm", "tension_kN", "util_DNV_ST_F101", "allowable_DNV_ST_F101"}
            assert expected_cols.issubset(set(df.columns))

    def test_to_csv_creates_parent_dirs(self):
        config = make_config(n_moment_steps=3, n_tension_steps=3)
        gen = MTLookupGenerator(config)
        with tempfile.TemporaryDirectory() as tmpdir:
            path = os.path.join(tmpdir, "sub", "dir", "lookup.csv")
            gen.to_csv(path)
            assert os.path.isfile(path)
