"""
Tests for CSVParser service.

The source module lives under a hyphenated directory (orcaflex-dashboard),
which is not a valid Python identifier. We use importlib to load it directly
from the file path instead of a dotted import.
"""

import importlib.util
import math
import os
import sys
import warnings
from io import StringIO
from pathlib import Path

import numpy as np
import pandas as pd
import pytest

# ---------------------------------------------------------------------------
# Dynamic import -- the directory "orcaflex-dashboard" contains a hyphen,
# so a normal dotted import is impossible. Load from absolute file path.
# ---------------------------------------------------------------------------
_MODULE_PATH = (
    Path(__file__).resolve().parents[2]
    / "src"
    / "digitalmodel"
    / "visualization"
    / "orcaflex-dashboard"
    / "backend"
    / "app"
    / "services"
    / "csv_parser.py"
)

_spec = importlib.util.spec_from_file_location("csv_parser", _MODULE_PATH)
_mod = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(_mod)

CSVParser = _mod.CSVParser
FileType = _mod.FileType
ParsedColumn = _mod.ParsedColumn
PolarData = _mod.PolarData
quick_parse = _mod.quick_parse
extract_time_series_stats = _mod.extract_time_series_stats


# ---------------------------------------------------------------------------
# Helpers -- reusable factories
# ---------------------------------------------------------------------------

def _make_parser(chunk_size: int = 10000) -> CSVParser:
    """Return a default CSVParser instance."""
    return CSVParser(chunk_size=chunk_size)


def _write_csv(tmp_path: Path, filename: str, content: str) -> Path:
    """Write CSV text to a temp file and return its path."""
    fp = tmp_path / filename
    fp.write_text(content, encoding="utf-8")
    return fp


def _polar_columns(param_prefix: str = "Tension",
                   headings=None) -> list:
    """Return column names like 'Tension 0 deg', 'Tension 15 deg', ..."""
    if headings is None:
        headings = range(0, 360, 15)
    return [f"{param_prefix} {h} deg" for h in headings]


def _polar_df(param_prefix: str = "Tension",
              n_rows: int = 3,
              headings=None) -> pd.DataFrame:
    """Build a DataFrame with polar heading columns plus dummy values."""
    if headings is None:
        headings = list(range(0, 360, 15))
    cols = _polar_columns(param_prefix, headings)
    rng = np.random.default_rng(42)
    data = {c: rng.uniform(100, 200, n_rows) for c in cols}
    return pd.DataFrame(data)


def _time_series_df(n_rows: int = 100,
                    time_col: str = "time") -> pd.DataFrame:
    """Build a simple time-series DataFrame."""
    rng = np.random.default_rng(42)
    t = np.linspace(0, 10, n_rows)
    return pd.DataFrame({
        time_col: t,
        "Fx_kN": rng.normal(0, 50, n_rows),
        "Fy_kN": rng.normal(0, 30, n_rows),
        "displacement_m": rng.normal(1.0, 0.5, n_rows),
    })


# ===========================================================================
# FileType enum
# ===========================================================================

class TestFileType:
    """Tests for the FileType enum."""

    def test_dm_summary_value(self):
        assert FileType.DM_SUMMARY.value == "dm_summary"

    def test_dm_inputs_value(self):
        assert FileType.DM_INPUTS.value == "dm_inputs"

    def test_time_trace_value(self):
        assert FileType.TIME_TRACE.value == "time_trace"

    def test_unknown_value(self):
        assert FileType.UNKNOWN.value == "unknown"


# ===========================================================================
# ParsedColumn dataclass
# ===========================================================================

class TestParsedColumn:
    """Tests for the ParsedColumn dataclass."""

    def test_all_fields_populated(self):
        pc = ParsedColumn(
            name="Fx kN",
            unit="kN",
            component="fst1",
            parameter="force_x",
            original_name="Fx_kN [fst1]",
        )
        assert pc.name == "Fx kN"
        assert pc.unit == "kN"
        assert pc.component == "fst1"
        assert pc.parameter == "force_x"
        assert pc.original_name == "Fx_kN [fst1]"

    def test_optional_fields_none(self):
        pc = ParsedColumn(
            name="col", unit=None, component=None, parameter=None,
            original_name="col",
        )
        assert pc.unit is None
        assert pc.component is None
        assert pc.parameter is None


# ===========================================================================
# PolarData dataclass
# ===========================================================================

class TestPolarData:
    """Tests for PolarData dataclass."""

    def test_creation_minimal(self):
        pd_obj = PolarData(
            headings=np.array([0, 15, 30]),
            values=np.array([1.0, 2.0, 3.0]),
        )
        assert pd_obj.units is None
        assert pd_obj.parameter is None

    def test_creation_full(self):
        pd_obj = PolarData(
            headings=np.array([0, 15]),
            values=np.array([5.0, 6.0]),
            units="kN",
            parameter="force_x",
        )
        assert pd_obj.units == "kN"
        assert pd_obj.parameter == "force_x"


# ===========================================================================
# CSVParser.__init__
# ===========================================================================

class TestCSVParserInit:
    """Tests for CSVParser constructor."""

    def test_default_chunk_size(self):
        parser = CSVParser()
        assert parser.chunk_size == 10000

    def test_custom_chunk_size(self):
        parser = CSVParser(chunk_size=500)
        assert parser.chunk_size == 500

    def test_logger_created(self):
        parser = CSVParser()
        assert parser.logger is not None


# ===========================================================================
# CSVParser.identify_file_type
# ===========================================================================

class TestIdentifyFileType:
    """Tests for file type identification."""

    def test_dm_summary_basic(self, tmp_path):
        fp = _write_csv(tmp_path, "dm_results.csv", "a,b\n1,2\n")
        parser = _make_parser()
        assert parser.identify_file_type(fp) == FileType.DM_SUMMARY

    def test_dm_inputs_file(self, tmp_path):
        fp = _write_csv(tmp_path, "dm_inputs_v2.csv", "a,b\n1,2\n")
        parser = _make_parser()
        assert parser.identify_file_type(fp) == FileType.DM_INPUTS

    def test_dm_inputs_mixed_case(self, tmp_path):
        fp = _write_csv(tmp_path, "DM_INPUTS.CSV", "a,b\n1,2\n")
        parser = _make_parser()
        # .name.lower() converts to dm_inputs.csv
        assert parser.identify_file_type(fp) == FileType.DM_INPUTS

    def test_time_trace_by_name(self, tmp_path):
        fp = _write_csv(tmp_path, "trace_results.csv", "a,b\n1,2\n")
        parser = _make_parser()
        assert parser.identify_file_type(fp) == FileType.TIME_TRACE

    def test_ts_prefix_is_time_trace(self, tmp_path):
        fp = _write_csv(tmp_path, "ts_output.csv", "a,b\n1,2\n")
        parser = _make_parser()
        assert parser.identify_file_type(fp) == FileType.TIME_TRACE

    def test_time_keyword_is_time_trace(self, tmp_path):
        fp = _write_csv(tmp_path, "output_time.csv", "a,b\n1,2\n")
        parser = _make_parser()
        assert parser.identify_file_type(fp) == FileType.TIME_TRACE

    def test_unknown_file_falls_to_structure(self, tmp_path):
        fp = _write_csv(tmp_path, "random.csv", "a,b\n1,2\n")
        parser = _make_parser()
        # No heading columns, no time column -- unknown
        assert parser.identify_file_type(fp) == FileType.UNKNOWN

    def test_structure_detects_time_column(self, tmp_path):
        fp = _write_csv(tmp_path, "results.csv", "time,Fx\n0,1.0\n1,2.0\n")
        parser = _make_parser()
        assert parser.identify_file_type(fp) == FileType.TIME_TRACE

    def test_structure_detects_polar_headings(self, tmp_path):
        # Build CSV with 24 heading columns
        cols = [f"param {h} deg" for h in range(0, 360, 15)]
        header = ",".join(cols)
        values = ",".join(["1.0"] * 24)
        fp = _write_csv(tmp_path, "output.csv", f"{header}\n{values}\n")
        parser = _make_parser()
        assert parser.identify_file_type(fp) == FileType.DM_SUMMARY

    def test_nonexistent_file_returns_unknown(self, tmp_path):
        fp = tmp_path / "nonexistent.csv"
        parser = _make_parser()
        # _identify_by_structure will fail, returning UNKNOWN
        assert parser.identify_file_type(fp) == FileType.UNKNOWN


# ===========================================================================
# CSVParser.parse_column_metadata
# ===========================================================================

class TestParseColumnMetadata:
    """Tests for column metadata extraction."""

    def test_extracts_kn_unit(self):
        parser = _make_parser()
        pc = parser.parse_column_metadata("Tension kN")
        assert pc.unit == "kN"

    def test_extracts_mn_unit(self):
        parser = _make_parser()
        pc = parser.parse_column_metadata("Moment MN")
        assert pc.unit == "MN"

    def test_extracts_meter_unit(self):
        parser = _make_parser()
        pc = parser.parse_column_metadata("Displacement m")
        assert pc.unit == "m"

    def test_extracts_deg_unit(self):
        parser = _make_parser()
        pc = parser.parse_column_metadata("Heading deg")
        assert pc.unit == "deg"

    def test_velocity_string_matches_displacement_unit_first(self):
        # UNIT_PATTERNS iterates force->moment->displacement->angle->velocity.
        # "m/s" contains "m" which matches displacement's \bm\b before velocity.
        parser = _make_parser()
        pc = parser.parse_column_metadata("Velocity m/s")
        assert pc.unit == "m"

    def test_no_unit_found(self):
        parser = _make_parser()
        pc = parser.parse_column_metadata("some_random_column")
        assert pc.unit is None

    def test_extracts_fst1_component(self):
        parser = _make_parser()
        pc = parser.parse_column_metadata("fst1_Tension")
        assert pc.component == "fst1"

    def test_extracts_floater_component(self):
        parser = _make_parser()
        pc = parser.parse_column_metadata("Floater 2 drift")
        assert pc.component == "fst2"

    def test_extracts_strut_component(self):
        parser = _make_parser()
        pc = parser.parse_column_metadata("strut tension")
        assert pc.component == "strut"

    def test_extracts_jacket_component(self):
        parser = _make_parser()
        pc = parser.parse_column_metadata("jacket_base_shear")
        assert pc.component == "jacket"

    def test_extracts_lngc_component(self):
        parser = _make_parser()
        pc = parser.parse_column_metadata("lngc_heading")
        assert pc.component == "lngc"

    def test_vessel_maps_to_lngc(self):
        parser = _make_parser()
        pc = parser.parse_column_metadata("vessel drift")
        assert pc.component == "lngc"

    def test_no_component(self):
        parser = _make_parser()
        pc = parser.parse_column_metadata("temperature_C")
        assert pc.component is None

    def test_extracts_force_x_parameter(self):
        parser = _make_parser()
        pc = parser.parse_column_metadata("Fx total")
        assert pc.parameter == "force_x"

    def test_extracts_force_y_parameter(self):
        parser = _make_parser()
        pc = parser.parse_column_metadata("Fy total")
        assert pc.parameter == "force_y"

    def test_extracts_force_z_parameter(self):
        parser = _make_parser()
        pc = parser.parse_column_metadata("Fz total")
        assert pc.parameter == "force_z"

    def test_extracts_moment_x_parameter(self):
        parser = _make_parser()
        pc = parser.parse_column_metadata("Mx total")
        assert pc.parameter == "moment_x"

    def test_extracts_moment_y_parameter(self):
        parser = _make_parser()
        pc = parser.parse_column_metadata("My total")
        assert pc.parameter == "moment_y"

    def test_extracts_moment_z_parameter(self):
        parser = _make_parser()
        pc = parser.parse_column_metadata("Mz total")
        assert pc.parameter == "moment_z"

    def test_clean_name_strips_special_chars(self):
        parser = _make_parser()
        pc = parser.parse_column_metadata("Fx[kN]@fst1")
        assert "[" not in pc.name
        assert "@" not in pc.name

    def test_clean_name_collapses_whitespace(self):
        parser = _make_parser()
        pc = parser.parse_column_metadata("Fx   kN    fst1")
        assert "  " not in pc.name

    def test_original_name_preserved(self):
        parser = _make_parser()
        original = "Fx[kN]@fst1"
        pc = parser.parse_column_metadata(original)
        assert pc.original_name == original

    def test_knm_moment_unit(self):
        parser = _make_parser()
        pc = parser.parse_column_metadata("Moment kNm")
        assert pc.unit == "kNm"

    def test_acceleration_string_matches_displacement_unit_first(self):
        # Same precedence issue: "m/s2" contains "m" matching displacement first.
        parser = _make_parser()
        pc = parser.parse_column_metadata("accel m/s2")
        assert pc.unit == "m"

    def test_velocity_unit_standalone_knots(self):
        # "knots" does not contain a displacement match, so velocity matches.
        parser = _make_parser()
        pc = parser.parse_column_metadata("Speed knots")
        assert pc.unit == "knots"

    def test_angle_unit_rad(self):
        parser = _make_parser()
        pc = parser.parse_column_metadata("Phase rad")
        assert pc.unit == "rad"


# ===========================================================================
# CSVParser.extract_polar_data
# ===========================================================================

class TestExtractPolarData:
    """Tests for polar data extraction from DataFrames."""

    def test_full_24_point_extraction(self):
        parser = _make_parser()
        df = _polar_df("Tension", n_rows=5)
        result = parser.extract_polar_data(df)
        assert "Tension" in result
        assert len(result["Tension"].headings) == 24

    def test_values_are_last_row(self):
        parser = _make_parser()
        cols = _polar_columns("T")
        data = {c: [10.0, 20.0, 30.0] for c in cols}
        df = pd.DataFrame(data)
        result = parser.extract_polar_data(df)
        # Last non-NaN value should be 30.0 for each column
        np.testing.assert_array_equal(result["T"].values, np.full(24, 30.0))

    def test_headings_sorted(self):
        parser = _make_parser()
        df = _polar_df("Force")
        result = parser.extract_polar_data(df)
        headings = result["Force"].headings
        assert np.all(np.diff(headings) >= 0), "Headings should be sorted"

    def test_no_polar_columns_returns_empty(self):
        parser = _make_parser()
        df = pd.DataFrame({"a": [1, 2], "b": [3, 4]})
        result = parser.extract_polar_data(df)
        assert result == {}

    def test_too_few_columns_skipped(self):
        # Only 10 heading columns -- below the threshold of 12
        parser = _make_parser()
        headings = list(range(0, 150, 15))  # 10 points
        df = _polar_df("Short", headings=headings)
        result = parser.extract_polar_data(df)
        assert "Short" not in result

    def test_exactly_12_columns_accepted(self):
        parser = _make_parser()
        headings = list(range(0, 180, 15))  # 12 points
        df = _polar_df("Twelve", headings=headings)
        result = parser.extract_polar_data(df)
        assert "Twelve" in result
        assert len(result["Twelve"].headings) == 12

    def test_multiple_parameters_extracted(self):
        parser = _make_parser()
        cols_a = _polar_columns("Tension")
        cols_b = _polar_columns("Moment")
        rng = np.random.default_rng(42)
        data = {}
        for c in cols_a + cols_b:
            data[c] = rng.uniform(0, 100, 5)
        df = pd.DataFrame(data)
        result = parser.extract_polar_data(df)
        assert "Tension" in result
        assert "Moment" in result

    def test_nan_values_handled(self):
        parser = _make_parser()
        cols = _polar_columns("Val")
        data = {c: [np.nan, np.nan, 42.0] for c in cols}
        df = pd.DataFrame(data)
        result = parser.extract_polar_data(df)
        assert "Val" in result
        # Last non-NaN is 42.0
        np.testing.assert_array_equal(result["Val"].values, np.full(24, 42.0))

    def test_degree_symbol_columns(self):
        parser = _make_parser()
        headings = list(range(0, 360, 15))
        cols = [f"Param {h}\u00b0" for h in headings]
        data = {c: [1.0, 2.0] for c in cols}
        df = pd.DataFrame(data)
        result = parser.extract_polar_data(df)
        assert "Param" in result

    def test_decimal_heading_values(self):
        parser = _make_parser()
        headings = [float(h) for h in range(0, 360, 15)]
        cols = [f"Force {h:.1f} deg" for h in headings]
        data = {c: [5.0] for c in cols}
        df = pd.DataFrame(data)
        result = parser.extract_polar_data(df)
        assert "Force" in result

    def test_all_nan_column_skipped_gracefully(self):
        parser = _make_parser()
        cols = _polar_columns("AllNan")
        data = {c: [np.nan, np.nan] for c in cols}
        df = pd.DataFrame(data)
        result = parser.extract_polar_data(df)
        # All columns are NaN, so no valid values -> parameter group skipped
        assert "AllNan" not in result

    def test_polar_data_units_extracted(self):
        parser = _make_parser()
        headings = list(range(0, 360, 15))
        cols = [f"Tension kN {h} deg" for h in headings]
        data = {c: [100.0] for c in cols}
        df = pd.DataFrame(data)
        result = parser.extract_polar_data(df)
        assert "Tension kN" in result
        assert result["Tension kN"].units == "kN"

    def test_heading_out_of_range_ignored(self):
        parser = _make_parser()
        # Include a heading >= 360 which should be excluded
        headings = list(range(0, 360, 15)) + [400]
        cols = [f"Force {h} deg" for h in headings]
        data = {c: [1.0] for c in cols}
        df = pd.DataFrame(data)
        result = parser.extract_polar_data(df)
        if "Force" in result:
            assert 400 not in result["Force"].headings


# ===========================================================================
# CSVParser.parse_file
# ===========================================================================

class TestParseFile:
    """Tests for full file parsing."""

    def test_basic_csv_parse(self, tmp_path):
        fp = _write_csv(tmp_path, "dm_test.csv", "a,b,c\n1,2,3\n4,5,6\n")
        parser = _make_parser()
        result = parser.parse_file(fp)
        assert result["shape"] == (2, 3)
        assert result["file_type"] == FileType.DM_SUMMARY

    def test_result_contains_dataframe(self, tmp_path):
        fp = _write_csv(tmp_path, "dm_out.csv", "x,y\n1.0,2.0\n")
        parser = _make_parser()
        result = parser.parse_file(fp)
        assert isinstance(result["dataframe"], pd.DataFrame)

    def test_result_contains_column_metadata(self, tmp_path):
        fp = _write_csv(tmp_path, "dm_out.csv", "Fx kN,Fy kN\n1,2\n")
        parser = _make_parser()
        result = parser.parse_file(fp)
        assert "Fx kN" in result["column_metadata"]
        assert isinstance(result["column_metadata"]["Fx kN"], ParsedColumn)

    def test_memory_usage_positive(self, tmp_path):
        fp = _write_csv(tmp_path, "dm_out.csv", "a,b\n1,2\n")
        parser = _make_parser()
        result = parser.parse_file(fp)
        assert result["memory_usage_mb"] > 0

    def test_file_not_found_raises(self, tmp_path):
        fp = tmp_path / "no_such_file.csv"
        parser = _make_parser()
        with pytest.raises(FileNotFoundError):
            parser.parse_file(fp)

    def test_dm_summary_includes_polar_data(self, tmp_path):
        # Build a dm_summary with polar heading columns
        headings = list(range(0, 360, 15))
        cols = [f"Force {h} deg" for h in headings]
        header = ",".join(cols)
        values = ",".join(["100.0"] * 24)
        fp = _write_csv(tmp_path, "dm_results.csv",
                        f"{header}\n{values}\n{values}\n")
        parser = _make_parser()
        result = parser.parse_file(fp)
        assert "polar_data" in result

    def test_time_trace_no_polar_data_key(self, tmp_path):
        fp = _write_csv(tmp_path, "trace_data.csv", "time,Fx\n0,1\n1,2\n")
        parser = _make_parser()
        result = parser.parse_file(fp)
        assert "polar_data" not in result

    def test_statistics_for_numeric_columns(self, tmp_path):
        fp = _write_csv(tmp_path, "dm_out.csv", "a,b\n1.0,2.0\n3.0,4.0\n")
        parser = _make_parser()
        result = parser.parse_file(fp)
        assert "statistics" in result
        assert result["statistics"]["numeric_columns"] == 2
        assert result["statistics"]["total_rows"] == 2

    def test_na_values_treated_as_nan(self, tmp_path):
        fp = _write_csv(tmp_path, "dm_out.csv", "a,b\n1,N/A\n2,#N/A\n")
        parser = _make_parser()
        result = parser.parse_file(fp)
        df = result["dataframe"]
        assert df["b"].isna().all()

    def test_kwargs_forwarded_to_pandas(self, tmp_path):
        content = "a;b;c\n1;2;3\n"
        fp = _write_csv(tmp_path, "dm_out.csv", content)
        parser = _make_parser()
        result = parser.parse_file(fp, sep=";")
        assert result["shape"] == (1, 3)

    def test_latin1_fallback(self, tmp_path):
        fp = tmp_path / "dm_latin.csv"
        fp.write_bytes(b"a,b\n1,caf\xe9\n")
        parser = _make_parser()
        result = parser.parse_file(fp)
        assert result["shape"][0] == 1


# ===========================================================================
# CSVParser.parse_chunked
# ===========================================================================

class TestParseChunked:
    """Tests for chunked file parsing."""

    def test_single_chunk(self, tmp_path):
        rows = "\n".join([f"{i},{i*2}" for i in range(5)])
        fp = _write_csv(tmp_path, "dm_small.csv", f"a,b\n{rows}\n")
        parser = CSVParser(chunk_size=100)
        chunks = list(parser.parse_chunked(fp))
        assert len(chunks) == 1

    def test_multiple_chunks(self, tmp_path):
        rows = "\n".join([f"{i},{i*2}" for i in range(50)])
        fp = _write_csv(tmp_path, "dm_big.csv", f"a,b\n{rows}\n")
        parser = CSVParser(chunk_size=10)
        chunks = list(parser.parse_chunked(fp))
        assert len(chunks) >= 2

    def test_chunk_metadata_present(self, tmp_path):
        fp = _write_csv(tmp_path, "dm_c.csv", "a,b\n1,2\n3,4\n")
        parser = CSVParser(chunk_size=100)
        chunks = list(parser.parse_chunked(fp))
        chunk = chunks[0]
        assert "chunk_number" in chunk
        assert "column_metadata" in chunk
        assert "file_type" in chunk
        assert "dataframe" in chunk

    def test_chunk_numbers_sequential(self, tmp_path):
        rows = "\n".join([f"{i},{i*2}" for i in range(50)])
        fp = _write_csv(tmp_path, "dm_seq.csv", f"a,b\n{rows}\n")
        parser = CSVParser(chunk_size=10)
        chunks = list(parser.parse_chunked(fp))
        for idx, chunk in enumerate(chunks):
            assert chunk["chunk_number"] == idx

    def test_file_not_found_raises(self, tmp_path):
        fp = tmp_path / "nonexistent.csv"
        parser = _make_parser()
        with pytest.raises(FileNotFoundError):
            list(parser.parse_chunked(fp))

    def test_latin1_chunked_raises_unicode_error(self, tmp_path):
        # In chunked mode the UnicodeDecodeError occurs during chunk iteration,
        # not during the initial pd.read_csv call, so the latin-1 fallback in
        # parse_chunked does not catch it. This is a known source limitation.
        fp = tmp_path / "dm_latin_c.csv"
        fp.write_bytes(b"a,b\n1,caf\xe9\n2,ol\xe9\n")
        parser = CSVParser(chunk_size=100)
        with pytest.raises(UnicodeDecodeError):
            list(parser.parse_chunked(fp))


# ===========================================================================
# CSVParser.validate_polar_data
# ===========================================================================

class TestValidatePolarData:
    """Tests for polar data validation."""

    def test_valid_complete_data(self):
        parser = _make_parser()
        pd_obj = PolarData(
            headings=np.arange(0, 360, 15, dtype=float),
            values=np.ones(24) * 100.0,
        )
        result = parser.validate_polar_data(pd_obj)
        assert result["is_valid"] is True
        assert result["completeness"] == 1.0
        assert len(result["issues"]) == 0

    def test_missing_headings_flagged(self):
        parser = _make_parser()
        # Only 12 headings out of 24
        headings = np.arange(0, 180, 15, dtype=float)
        pd_obj = PolarData(headings=headings, values=np.ones(12))
        result = parser.validate_polar_data(pd_obj)
        assert result["is_valid"] is False
        assert result["completeness"] == 0.5
        assert any("Missing headings" in iss for iss in result["issues"])

    def test_extra_headings_noted(self):
        parser = _make_parser()
        headings = np.append(np.arange(0, 360, 15, dtype=float), [5.0])
        values = np.ones(25)
        pd_obj = PolarData(headings=headings, values=values)
        result = parser.validate_polar_data(pd_obj)
        assert any("Extra headings" in iss for iss in result["issues"])

    def test_nan_values_invalid(self):
        parser = _make_parser()
        headings = np.arange(0, 360, 15, dtype=float)
        values = np.ones(24)
        values[5] = np.nan
        pd_obj = PolarData(headings=headings, values=values)
        result = parser.validate_polar_data(pd_obj)
        assert result["is_valid"] is False
        # np.isnan().any() returns np.bool_, use == for comparison
        assert result["data_quality"]["has_nan"] == True  # noqa: E712

    def test_inf_values_invalid(self):
        parser = _make_parser()
        headings = np.arange(0, 360, 15, dtype=float)
        values = np.ones(24)
        values[0] = np.inf
        pd_obj = PolarData(headings=headings, values=values)
        # Under pytest's filterwarnings=error, np.std on inf values raises
        # RuntimeWarning as an error. The try/except in validate_polar_data
        # catches it, so data_quality may be incomplete. Either way, is_valid
        # should be False (from inf detection or from the caught exception).
        result = parser.validate_polar_data(pd_obj)
        assert result["is_valid"] is False
        # Check that the issue was recorded (either via inf detection or error)
        assert len(result["issues"]) > 0

    def test_data_quality_stats(self):
        parser = _make_parser()
        headings = np.arange(0, 360, 15, dtype=float)
        values = np.arange(1, 25, dtype=float)
        pd_obj = PolarData(headings=headings, values=values)
        result = parser.validate_polar_data(pd_obj)
        dq = result["data_quality"]
        assert dq["range"] == [1.0, 24.0]
        assert dq["mean"] == pytest.approx(12.5)

    def test_custom_expected_headings(self):
        parser = _make_parser()
        custom = np.array([0, 90, 180, 270], dtype=float)
        pd_obj = PolarData(
            headings=np.array([0, 90, 180, 270], dtype=float),
            values=np.ones(4),
        )
        result = parser.validate_polar_data(pd_obj, expected_headings=custom)
        assert result["is_valid"] is True
        assert result["completeness"] == 1.0

    def test_heading_coverage_ratio(self):
        parser = _make_parser()
        headings = np.arange(0, 180, 15, dtype=float)  # 12 of 24
        pd_obj = PolarData(headings=headings, values=np.ones(12))
        result = parser.validate_polar_data(pd_obj)
        assert result["heading_coverage"] == pytest.approx(0.5)


# ===========================================================================
# extract_time_series_stats (module-level function)
# ===========================================================================

class TestExtractTimeSeriesStats:
    """Tests for the extract_time_series_stats utility function.

    NOTE: The source code uses np.trapz for time_rms, which was removed in
    numpy >= 2.0 (replaced by np.trapezoid). Under numpy 2.x the try/except
    in the source silently swallows the AttributeError for each column,
    resulting in empty stats. Tests that need column stats must suppress
    RuntimeWarning (from pytest filterwarnings=error) and monkeypatch np.trapz
    if missing.
    """

    @pytest.fixture(autouse=True)
    def _patch_trapz(self):
        """Shim np.trapz to np.trapezoid on numpy >= 2.0 so the source works."""
        if not hasattr(np, "trapz") and hasattr(np, "trapezoid"):
            np.trapz = np.trapezoid
            yield
            del np.trapz
        else:
            yield

    def test_basic_stats(self):
        df = _time_series_df(100)
        stats = extract_time_series_stats(df)
        assert "Fx_kN" in stats
        assert "Fy_kN" in stats
        assert "displacement_m" in stats

    def test_stat_keys_present(self):
        df = _time_series_df(100)
        stats = extract_time_series_stats(df)
        for key in ("mean", "std", "min", "max", "rms", "range"):
            assert key in stats["Fx_kN"], f"Missing key: {key}"

    def test_rms_calculation(self):
        # Constant signal: RMS should equal absolute value
        df = pd.DataFrame({"time": [0, 1, 2], "val": [3.0, 3.0, 3.0]})
        stats = extract_time_series_stats(df)
        assert stats["val"]["rms"] == pytest.approx(3.0)

    def test_rms_sine_wave(self):
        t = np.linspace(0, 2 * np.pi, 1000)
        df = pd.DataFrame({"time": t, "sine": np.sin(t)})
        stats = extract_time_series_stats(df)
        # RMS of sine wave ~ 1/sqrt(2) ~ 0.7071
        assert stats["sine"]["rms"] == pytest.approx(1 / math.sqrt(2), abs=0.02)

    def test_range_calculation(self):
        df = pd.DataFrame({"time": [0, 1, 2], "val": [5.0, 10.0, 15.0]})
        stats = extract_time_series_stats(df)
        assert stats["val"]["range"] == pytest.approx(10.0)

    def test_no_time_column_error(self):
        df = pd.DataFrame({"a": [1, 2], "b": [3, 4]})
        stats = extract_time_series_stats(df)
        assert "error" in stats

    def test_auto_detect_time_column(self):
        df = pd.DataFrame({"Time_s": [0, 1, 2], "val": [1.0, 2.0, 3.0]})
        stats = extract_time_series_stats(df)
        assert "val" in stats

    def test_auto_detect_t_column(self):
        df = pd.DataFrame({"t": [0, 1, 2], "val": [1.0, 2.0, 3.0]})
        stats = extract_time_series_stats(df)
        assert "val" in stats

    def test_insufficient_time_data(self):
        df = pd.DataFrame({"time": [0.0], "val": [1.0]})
        stats = extract_time_series_stats(df)
        assert "error" in stats

    def test_time_rms_present(self):
        df = _time_series_df(100)
        stats = extract_time_series_stats(df)
        # time_rms should be computed when data length matches time length
        assert "time_rms" in stats["Fx_kN"]

    def test_non_numeric_columns_excluded(self):
        df = pd.DataFrame({
            "time": [0, 1, 2],
            "val": [1.0, 2.0, 3.0],
            "label": ["a", "b", "c"],
        })
        stats = extract_time_series_stats(df)
        assert "label" not in stats
        assert "val" in stats

    def test_empty_df_with_time(self):
        df = pd.DataFrame({"time": pd.Series(dtype=float),
                           "val": pd.Series(dtype=float)})
        stats = extract_time_series_stats(df)
        assert "error" in stats

    def test_time_column_not_in_stats(self):
        """The time column itself should not appear in the stats output."""
        df = _time_series_df(50)
        stats = extract_time_series_stats(df)
        assert "time" not in stats

    def test_mean_of_constant_signal(self):
        df = pd.DataFrame({"time": [0, 1, 2], "val": [7.0, 7.0, 7.0]})
        stats = extract_time_series_stats(df)
        assert stats["val"]["mean"] == pytest.approx(7.0)

    def test_std_of_constant_signal_is_zero(self):
        df = pd.DataFrame({"time": [0, 1, 2], "val": [5.0, 5.0, 5.0]})
        stats = extract_time_series_stats(df)
        assert stats["val"]["std"] == pytest.approx(0.0)

    def test_min_max_values(self):
        df = pd.DataFrame({"time": [0, 1, 2, 3], "val": [10.0, 20.0, 5.0, 15.0]})
        stats = extract_time_series_stats(df)
        assert stats["val"]["min"] == pytest.approx(5.0)
        assert stats["val"]["max"] == pytest.approx(20.0)

    def test_multiple_numeric_columns(self):
        df = pd.DataFrame({
            "time": [0, 1, 2],
            "a": [1.0, 2.0, 3.0],
            "b": [4.0, 5.0, 6.0],
        })
        stats = extract_time_series_stats(df)
        assert "a" in stats
        assert "b" in stats


# ===========================================================================
# quick_parse (module-level function)
# ===========================================================================

class TestQuickParse:
    """Tests for the quick_parse convenience function."""

    def test_returns_dict(self, tmp_path):
        fp = _write_csv(tmp_path, "dm_quick.csv", "a,b\n1,2\n")
        result = quick_parse(str(fp))
        assert isinstance(result, dict)
        assert "dataframe" in result

    def test_accepts_string_path(self, tmp_path):
        fp = _write_csv(tmp_path, "dm_quick.csv", "a,b\n1,2\n")
        result = quick_parse(str(fp))
        assert result["shape"] == (1, 2)

    def test_accepts_path_object(self, tmp_path):
        fp = _write_csv(tmp_path, "dm_quick.csv", "a,b\n1,2\n")
        result = quick_parse(fp)
        assert result["shape"] == (1, 2)

    def test_file_not_found(self, tmp_path):
        with pytest.raises(FileNotFoundError):
            quick_parse(tmp_path / "nonexistent.csv")


# ===========================================================================
# STANDARD_HEADINGS class attribute
# ===========================================================================

class TestStandardHeadings:
    """Tests for the STANDARD_HEADINGS class attribute."""

    def test_length_is_24(self):
        assert len(CSVParser.STANDARD_HEADINGS) == 24

    def test_starts_at_0(self):
        assert CSVParser.STANDARD_HEADINGS[0] == 0

    def test_ends_at_345(self):
        assert CSVParser.STANDARD_HEADINGS[-1] == 345

    def test_increment_is_15(self):
        diffs = np.diff(CSVParser.STANDARD_HEADINGS)
        np.testing.assert_array_equal(diffs, np.full(23, 15))
