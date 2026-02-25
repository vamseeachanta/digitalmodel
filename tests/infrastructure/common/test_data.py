"""Comprehensive unit tests for digitalmodel.infrastructure.common.data module.

Covers all pure-logic classes and functions:
  - ReadData: DataFrame filtering, column deletion, file reading, dictionary extraction
  - FromString: regex, fraction conversion, string manipulation, delimited parsing
  - SaveData: JSON/YAML/CSV/ASCII file writing
  - DefineData: empty DataFrames, nested dictionary mutation
  - AttributeDict / objdict: dict subclasses with attribute access
  - DateTimeUtility: last-day-of-month calculation
  - Transform: DataFrame transformations, JSON/HTML conversion, list uniquification,
                numpy type conversion, column addition, transposition
  - TransformData: linear transform dispatch
  - PandasChainedAssignent: context manager for chained-assignment setting
  - transform_df_datetime_to_str: datetime column serialisation
"""

import datetime
import importlib
import json
import os
import warnings

import numpy as np
import pandas as pd
import pytest

# The source module contains invalid escape sequences in sample_cfg strings
# (e.g. '\d' in Windows paths). Python 3.12 raises SyntaxWarning for these,
# and pytest's assertion rewriter converts SyntaxWarning to SyntaxError during
# collection.  We import via importlib to bypass assertion rewriting.
with warnings.catch_warnings():
    warnings.simplefilter("ignore", SyntaxWarning)
    _mod = importlib.import_module("digitalmodel.infrastructure.common.data")

AttributeDict = _mod.AttributeDict
DateTimeUtility = _mod.DateTimeUtility
DefineData = _mod.DefineData
FromString = _mod.FromString
PandasChainedAssignent = _mod.PandasChainedAssignent
ReadData = _mod.ReadData
SaveData = _mod.SaveData
Transform = _mod.Transform
TransformData = _mod.TransformData
objdict = _mod.objdict
transform_df_datetime_to_str = _mod.transform_df_datetime_to_str


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------

@pytest.fixture
def read_data():
    return ReadData()


@pytest.fixture
def from_string():
    return FromString()


@pytest.fixture
def save_data():
    return SaveData()


@pytest.fixture
def define_data():
    return DefineData()


@pytest.fixture
def transform():
    return Transform()


@pytest.fixture
def transform_data():
    return TransformData()


@pytest.fixture
def sample_df():
    return pd.DataFrame({
        "name": ["Alice", "Bob", "Charlie", "Diana"],
        "age": [30, 25, 35, 28],
        "city": ["NYC", "LA", "NYC", "LA"],
    })


# ===========================================================================
# ReadData tests
# ===========================================================================

class TestReadDataDfFilterByColumnValues:

    def test_filter_single_value_flat_config(self, read_data, sample_df):
        cfg = {"filter": [{"column": "city", "value": "NYC"}]}
        result = read_data.df_filter_by_column_values(cfg, sample_df)
        assert len(result) == 2
        assert list(result["city"]) == ["NYC", "NYC"]

    def test_filter_none_returns_original(self, read_data, sample_df):
        cfg = {"filter": None}
        result = read_data.df_filter_by_column_values(cfg, sample_df)
        assert len(result) == len(sample_df)

    def test_filter_multiple_values(self, read_data):
        df = pd.DataFrame({
            "status": ["active", "inactive", "active", "archived"],
            "score": [10, 20, 30, 40],
        })
        cfg = {"filter": [{"column": "status", "value": "active"}]}
        result = read_data.df_filter_by_column_values(cfg, df)
        assert len(result) == 2
        assert list(result["score"]) == [10, 30]

    def test_filter_nested_files_config(self, read_data, sample_df):
        cfg = {
            "files": {
                "from_xlsx": [
                    {"filter": [{"column": "age", "value": 25}]}
                ]
            }
        }
        result = read_data.df_filter_by_column_values(cfg, sample_df, file_index=0)
        assert len(result) == 1
        assert result.iloc[0]["name"] == "Bob"

    def test_filter_resets_index(self, read_data):
        df = pd.DataFrame({"x": [1, 2, 3], "keep": ["no", "yes", "no"]})
        cfg = {"filter": [{"column": "keep", "value": "yes"}]}
        result = read_data.df_filter_by_column_values(cfg, df)
        assert list(result.index) == [0]

    def test_filter_no_match_returns_empty(self, read_data, sample_df):
        cfg = {"filter": [{"column": "city", "value": "Chicago"}]}
        result = read_data.df_filter_by_column_values(cfg, sample_df)
        assert len(result) == 0

    def test_filter_chained_filters(self, read_data):
        df = pd.DataFrame({
            "color": ["red", "red", "blue", "red"],
            "size": ["S", "M", "S", "S"],
        })
        cfg = {"filter": [
            {"column": "color", "value": "red"},
            {"column": "size", "value": "S"},
        ]}
        result = read_data.df_filter_by_column_values(cfg, df)
        assert len(result) == 2


class TestReadDataFromDfDeleteUnwantedColumns:

    def test_drop_columns_by_index(self, read_data):
        df = pd.DataFrame({"a": [1], "b": [2], "c": [3], "d": [4], "e": [5]})
        result = read_data.from_df_delete_unwanted_columns(df, [1, 3])
        assert list(result.columns) == ["a", "c", "e"]

    def test_drop_single_column(self, read_data):
        df = pd.DataFrame({"x": [10], "y": [20]})
        result = read_data.from_df_delete_unwanted_columns(df, [0])
        assert list(result.columns) == ["y"]


class TestReadDataExtractFromDictionary:

    def test_extract_nested_value(self, read_data):
        d = {"a": {"b": {"c": 42}}}
        assert read_data.extract_from_dictionary(d, ["a", "b", "c"]) == 42

    def test_extract_top_level(self, read_data):
        d = {"key": "value"}
        assert read_data.extract_from_dictionary(d, ["key"]) == "value"

    def test_extract_list_element(self, read_data):
        d = {"items": [10, 20, 30]}
        assert read_data.extract_from_dictionary(d, ["items", 1]) == 20


class TestReadDataGetArrayRowsContainingKeywords:

    def test_keyword_found_first_row(self, read_data):
        array = ["header line", "data line 1", "data line 2"]
        result = read_data.get_array_rows_containing_keywords(array, ["header"])
        assert result == 1  # 1-indexed

    def test_keyword_found_middle(self, read_data):
        array = ["line A", "line B target", "line C"]
        result = read_data.get_array_rows_containing_keywords(array, ["target"])
        assert result == 2

    def test_keyword_not_found_returns_none(self, read_data):
        array = ["line 1", "line 2"]
        result = read_data.get_array_rows_containing_keywords(array, ["missing"])
        assert result is None

    def test_multiple_keywords_any_match(self, read_data):
        array = ["no match", "has alpha", "has beta"]
        result = read_data.get_array_rows_containing_keywords(array, ["alpha", "beta"])
        assert result == 2  # first row with any keyword


class TestReadDataAsciiFileOperations:

    def test_read_all_lines(self, read_data, tmp_path):
        f = tmp_path / "test.txt"
        f.write_text("line1\nline2\nline3\n")
        cfg = {"io": str(f)}
        result = read_data.from_ascii_file_get_lines_as_string_arrays(cfg)
        assert len(result) == 3
        assert "line1\n" in result[0]

    def test_read_with_start_and_end_line(self, read_data, tmp_path):
        f = tmp_path / "test.txt"
        f.write_text("a\nb\nc\nd\ne\n")
        cfg = {"io": str(f), "start_line": 2, "end_line": 4}
        result = read_data.from_ascii_file_get_lines_as_string_arrays(cfg)
        assert len(result) == 3
        assert result[0].strip() == "b"
        assert result[2].strip() == "d"

    def test_read_single_line_with_end_none(self, read_data, tmp_path):
        f = tmp_path / "test.txt"
        f.write_text("first\nsecond\nthird\n")
        cfg = {"io": str(f), "start_line": 2, "end_line": None}
        result = read_data.from_ascii_file_get_lines_as_string_arrays(cfg)
        assert result.strip() == "second"

    def test_read_lines_from_end(self, read_data, tmp_path):
        """lines_from_end=N sets start_line = total_lines - N,
        so the slice is all_lines[start-1:end] which yields N+1 lines
        when end=total_lines. This tests the actual implementation."""
        f = tmp_path / "test.txt"
        f.write_text("1\n2\n3\n4\n5\n")
        cfg = {"io": str(f), "lines_from_end": 2}
        result = read_data.from_ascii_file_get_lines_as_string_arrays(cfg)
        # start_line = 5-2 = 3, slice = all_lines[2:5] -> 3 lines
        assert len(result) == 3
        assert result[0].strip() == "3"
        assert result[1].strip() == "4"
        assert result[2].strip() == "5"

    def test_get_line_number_containing_keywords(self, read_data, tmp_path):
        f = tmp_path / "data.txt"
        f.write_text("header\nfrequency = 1.5\ndata\n")
        cfg = {
            "io": str(f),
            "line": {
                "key_words": ["frequency"],
                "transform": {"scale": 1, "shift": 0},
            },
        }
        result = read_data.from_ascii_file_get_line_number_containing_keywords(cfg)
        assert result == 2  # 1-indexed, found on line 2

    def test_get_line_number_with_transform(self, read_data, tmp_path):
        f = tmp_path / "data.txt"
        f.write_text("header\nfrequency = 1.5\ndata\n")
        cfg = {
            "io": str(f),
            "line": {
                "key_words": ["frequency"],
                "transform": {"scale": 2, "shift": 3},
            },
        }
        result = read_data.from_ascii_file_get_line_number_containing_keywords(cfg)
        assert result == 2 * 2 + 3  # 7


class TestReadDataGetFileListFromFolder:

    def test_with_path_and_extension(self, read_data, tmp_path):
        (tmp_path / "a.log").touch()
        (tmp_path / "b.log").touch()
        (tmp_path / "c.txt").touch()
        pattern = str(tmp_path / "*.log")
        result = read_data.get_file_list_from_folder(pattern, with_path=True, with_extension=True)
        assert len(result) == 2
        assert all(f.endswith(".log") for f in result)

    def test_without_path(self, read_data, tmp_path):
        (tmp_path / "file.dat").touch()
        pattern = str(tmp_path / "*.dat")
        result = read_data.get_file_list_from_folder(pattern, with_path=False, with_extension=True)
        assert result == ["file.dat"]

    def test_without_extension(self, read_data, tmp_path):
        (tmp_path / "report.csv").touch()
        pattern = str(tmp_path / "*.csv")
        result = read_data.get_file_list_from_folder(pattern, with_path=False, with_extension=False)
        assert result == ["report"]

    def test_with_path_without_extension(self, read_data, tmp_path):
        (tmp_path / "data.bin").touch()
        pattern = str(tmp_path / "*.bin")
        result = read_data.get_file_list_from_folder(pattern, with_path=True, with_extension=False)
        assert len(result) == 1
        assert not result[0].endswith(".bin")

    def test_empty_folder_returns_empty(self, read_data, tmp_path):
        pattern = str(tmp_path / "*.xyz")
        result = read_data.get_file_list_from_folder(pattern)
        assert result == []


class TestReadDataReadYmlFile:

    def test_read_yml_file(self, read_data, tmp_path):
        f = tmp_path / "config.yml"
        f.write_text("key: value\nnested:\n  a: 1\n  b: 2\n")
        result = read_data.read_yml_file({"io": str(f)})
        assert result["key"] == "value"
        assert result["nested"]["a"] == 1


# ===========================================================================
# FromString tests
# ===========================================================================

class TestFromStringUsingRegex:

    def test_simple_match(self, from_string):
        result = from_string.using_regex(r"\d+", "abc123def")
        assert result == "123"

    def test_case_insensitive(self, from_string):
        result = from_string.using_regex(r"hello", "Say HELLO World")
        assert result == "HELLO"

    def test_no_match_returns_none(self, from_string):
        result = from_string.using_regex(r"\d+", "no digits here")
        assert result is None

    def test_group_capture(self, from_string):
        result = from_string.using_regex(r"(\d+\.\d+)", "freq=3.14Hz")
        assert result == "3.14"


class TestFromStringConvertFractionToFloat:

    def test_plain_float(self, from_string):
        assert from_string.convert_fraction_to_float("3.14") == pytest.approx(3.14)

    def test_plain_integer(self, from_string):
        assert from_string.convert_fraction_to_float("7") == 7.0

    def test_simple_fraction(self, from_string):
        assert from_string.convert_fraction_to_float("1/2") == pytest.approx(0.5)

    def test_mixed_number_positive(self, from_string):
        assert from_string.convert_fraction_to_float("2 1/4") == pytest.approx(2.25)

    def test_mixed_number_negative(self, from_string):
        assert from_string.convert_fraction_to_float("-2 1/4") == pytest.approx(-2.25)

    def test_fraction_three_quarters(self, from_string):
        assert from_string.convert_fraction_to_float("3/4") == pytest.approx(0.75)

    def test_whole_zero_fraction(self, from_string):
        assert from_string.convert_fraction_to_float("0 1/3") == pytest.approx(1.0 / 3.0)


class TestFromStringRemoveStrings:

    def test_remove_single_string(self, from_string):
        result = from_string.remove_strings("hello world", ["world"])
        assert result == "hello "

    def test_remove_multiple_strings(self, from_string):
        result = from_string.remove_strings("a-b-c-d", ["-b", "-d"])
        assert result == "a-c"

    def test_remove_from_none_returns_none(self, from_string):
        result = from_string.remove_strings(None, ["x"])
        assert result is None

    def test_remove_empty_array_returns_original(self, from_string):
        result = from_string.remove_strings("test", [])
        assert result == "test"


class TestFromStringRemoveString:

    def test_basic_removal(self, from_string):
        assert from_string.remove_string("foobar", "bar") == "foo"

    def test_no_occurrence(self, from_string):
        assert from_string.remove_string("hello", "xyz") == "hello"

    def test_multiple_occurrences(self, from_string):
        assert from_string.remove_string("abcabc", "abc") == ""


class TestFromStringGetValueByDelimiter:

    def test_space_delimiter_float(self, from_string):
        cfg = {"text": "   97   2001", "delimiter": " ", "column": 2, "data_type": "float"}
        assert from_string.get_value_by_delimiter(cfg) == 2001.0

    def test_space_delimiter_first_column(self, from_string):
        cfg = {"text": "10 20 30", "delimiter": " ", "column": 1, "data_type": "float"}
        assert from_string.get_value_by_delimiter(cfg) == 10.0

    def test_custom_delimiter(self, from_string):
        cfg = {"text": "0.017300(Hz)", "delimiter": "(", "column": 1, "data_type": "float"}
        assert from_string.get_value_by_delimiter(cfg) == pytest.approx(0.0173)

    def test_string_data_type(self, from_string):
        cfg = {"text": "key=value", "delimiter": "=", "column": 2, "data_type": "string"}
        assert from_string.get_value_by_delimiter(cfg) == "value"


class TestFromStringRemoveNextLineValues:

    def test_remove_newlines(self, from_string):
        assert from_string.remove_next_line_values("a\nb\nc") == "abc"

    def test_no_newlines(self, from_string):
        assert from_string.remove_next_line_values("abc") == "abc"

    def test_empty_string(self, from_string):
        assert from_string.remove_next_line_values("") == ""


# ===========================================================================
# SaveData tests
# ===========================================================================

class TestSaveDataJson:

    def test_save_json(self, save_data, tmp_path):
        data = {"key": "value", "num": 42}
        file_base = str(tmp_path / "output")
        save_data.saveDataJson(data, file_base)
        with open(file_base + ".json") as f:
            loaded = json.load(f)
        assert loaded == data


class TestSaveDataYaml:

    def test_save_yaml_default(self, save_data, tmp_path):
        import yaml
        data = {"greeting": "hello", "count": 5}
        file_base = str(tmp_path / "config")
        save_data.saveDataYaml(data, file_base, default_flow_style=False)
        with open(file_base + ".yml") as f:
            loaded = yaml.safe_load(f)
        assert loaded == data

    def test_save_yaml_none_flow_style(self, save_data, tmp_path):
        import yaml
        data = {"a": 1}
        file_base = str(tmp_path / "cfg")
        save_data.saveDataYaml(data, file_base, default_flow_style=None)
        with open(file_base + ".yml") as f:
            loaded = yaml.safe_load(f)
        assert loaded == data


class TestSaveDataFrame:

    def test_save_csv(self, save_data, tmp_path):
        df = pd.DataFrame({"x": [1, 2], "y": [3, 4]})
        file_base = str(tmp_path / "data")
        save_data.saveDataFrame(df, file_base)
        loaded = pd.read_csv(file_base + ".csv", index_col=0)
        pd.testing.assert_frame_equal(loaded, df)


class TestSaveDataAsciiFile:

    def test_write_ascii_from_text(self, save_data, tmp_path):
        text = "Hello World\nLine 2"
        file_name = str(tmp_path / "output.txt")
        save_data.write_ascii_file_from_text(text, file_name)
        with open(file_name) as f:
            content = f.read()
        assert content == text


class TestSaveDataAsciiArray:

    def test_save_array_with_newlines(self, save_data, tmp_path):
        array = ["line1\n", "line2\n", "line3\n"]
        file_name = str(tmp_path / "out")
        save_data.save_ascii_file_from_array(array, file_name, extension=".txt")
        with open(file_name + ".txt") as f:
            lines = f.readlines()
        assert len(lines) == 3
        assert lines[0] == "line1\n"

    def test_save_array_without_newlines_appends_them(self, save_data, tmp_path):
        array = ["alpha", "beta"]
        file_name = str(tmp_path / "out")
        save_data.save_ascii_file_from_array(array, file_name, extension=".dat")
        with open(file_name + ".dat") as f:
            lines = f.readlines()
        assert len(lines) == 2
        assert lines[0] == "alpha\n"
        assert lines[1] == "beta\n"


# ===========================================================================
# DefineData tests
# ===========================================================================

class TestDefineDataEmptyDataFrame:

    def test_empty_df_no_columns(self, define_data):
        df = define_data.empty_data_frame()
        assert isinstance(df, pd.DataFrame)
        assert len(df) == 0
        assert len(df.columns) == 0

    def test_empty_df_with_columns(self, define_data):
        df = define_data.empty_data_frame(columns=["a", "b", "c"])
        assert list(df.columns) == ["a", "b", "c"]
        assert len(df) == 0


class TestDefineDataSetValueInDictionary:

    def test_set_nested_value(self, define_data):
        d = {"level1": {"level2": {"level3": "old"}}}
        result = define_data.set_value_in_dictionary(d, "new", ["level1", "level2", "level3"])
        assert result["level1"]["level2"]["level3"] == "new"

    def test_set_top_level_value(self, define_data):
        d = {"key": "old"}
        result = define_data.set_value_in_dictionary(d, "new", ["key"])
        assert result["key"] == "new"


# ===========================================================================
# AttributeDict tests
# ===========================================================================

class TestAttributeDict:

    def test_attribute_access(self):
        d = AttributeDict({"foo": 1, "bar": "baz"})
        assert d.foo == 1
        assert d.bar == "baz"

    def test_dict_access(self):
        d = AttributeDict({"x": 42})
        assert d["x"] == 42

    def test_set_via_attribute(self):
        d = AttributeDict()
        d.new_key = 99
        assert d["new_key"] == 99

    def test_set_via_dict(self):
        d = AttributeDict()
        d["another"] = "value"
        assert d.another == "value"


# ===========================================================================
# objdict tests
# ===========================================================================

class TestObjdict:

    def test_attribute_get(self):
        d = objdict({"name": "test"})
        assert d.name == "test"

    def test_attribute_set(self):
        d = objdict()
        d.value = 100
        assert d["value"] == 100

    def test_attribute_delete(self):
        d = objdict({"temp": True})
        del d.temp
        assert "temp" not in d

    def test_missing_attribute_raises(self):
        d = objdict()
        with pytest.raises(AttributeError, match="No such attribute"):
            _ = d.nonexistent

    def test_delete_missing_attribute_raises(self):
        d = objdict()
        with pytest.raises(AttributeError, match="No such attribute"):
            del d.nonexistent

    def test_dict_operations(self):
        d = objdict({"a": 1, "b": 2})
        assert len(d) == 2
        assert set(d.keys()) == {"a", "b"}


# ===========================================================================
# DateTimeUtility tests
# ===========================================================================

class TestDateTimeUtility:

    def test_last_day_january(self):
        dt = DateTimeUtility()
        result = dt.last_day_of_month(datetime.date(2024, 1, 15))
        assert result == datetime.date(2024, 1, 31)

    def test_last_day_february_leap(self):
        dt = DateTimeUtility()
        result = dt.last_day_of_month(datetime.date(2024, 2, 1))
        assert result == datetime.date(2024, 2, 29)

    def test_last_day_february_non_leap(self):
        dt = DateTimeUtility()
        result = dt.last_day_of_month(datetime.date(2023, 2, 10))
        assert result == datetime.date(2023, 2, 28)

    def test_last_day_april(self):
        dt = DateTimeUtility()
        result = dt.last_day_of_month(datetime.date(2024, 4, 5))
        assert result == datetime.date(2024, 4, 30)

    def test_last_day_december(self):
        dt = DateTimeUtility()
        result = dt.last_day_of_month(datetime.date(2024, 12, 1))
        assert result == datetime.date(2024, 12, 31)

    def test_already_last_day(self):
        dt = DateTimeUtility()
        result = dt.last_day_of_month(datetime.date(2024, 3, 31))
        assert result == datetime.date(2024, 3, 31)


# ===========================================================================
# Transform tests
# ===========================================================================

class TestTransformDataframeToDict:

    def test_basic_conversion(self, transform):
        df = pd.DataFrame({"a": [1, 2], "b": [3, 4]})
        result = transform.dataframe_to_dict(df)
        assert isinstance(result, list)
        assert len(result) == 2
        assert result[0] == {"a": 1, "b": 3}

    def test_none_df_returns_empty_dict(self, transform):
        result = transform.dataframe_to_dict(None)
        assert result == {}

    def test_empty_df(self, transform):
        df = pd.DataFrame({"x": []})
        result = transform.dataframe_to_dict(df)
        assert result == []

    def test_custom_orient(self, transform):
        df = pd.DataFrame({"col": [10]})
        result = transform.dataframe_to_dict(df, cfg={"orient": "records"})
        assert result == [{"col": 10}]

    def test_duplicate_columns_returns_empty(self, transform):
        df = pd.DataFrame([[1, 2]], columns=["x", "x"])
        result = transform.dataframe_to_dict(df)
        assert result == {}


class TestTransformListToUniqueList:

    def test_trailing_alphabet(self, transform):
        cfg = {"list": ["a", "a", "b"], "transform_character": "trailing_alphabet"}
        result = transform.transform_list_to_unique_list(cfg)
        assert len(result) == len(set(result))  # all unique
        assert result[2] == "b"

    def test_no_duplicates_unchanged(self, transform):
        cfg = {"list": ["x", "y", "z"], "transform_character": "trailing_alphabet"}
        result = transform.transform_list_to_unique_list(cfg)
        assert result == ["x", "y", "z"]

    def test_trailing_space(self, transform):
        cfg = {"list": ["col", "col"], "transform_character": "trailing_space"}
        result = transform.transform_list_to_unique_list(cfg)
        assert len(set(result)) == 2

    def test_leading_space(self, transform):
        cfg = {"list": ["dup", "dup", "dup"], "transform_character": "leading_space"}
        result = transform.transform_list_to_unique_list(cfg)
        assert len(set(result)) == 3

    def test_leading_alphabet(self, transform):
        cfg = {"list": ["q", "q"], "transform_character": "leading_alphabet"}
        result = transform.transform_list_to_unique_list(cfg)
        assert len(set(result)) == 2

    def test_triple_duplicates(self, transform):
        cfg = {"list": ["m", "m", "m"], "transform_character": "trailing_alphabet"}
        result = transform.transform_list_to_unique_list(cfg)
        assert len(set(result)) == 3


class TestTransformAddColumnToDf:

    def test_add_column(self, transform):
        df = pd.DataFrame({"a": [1, 2, 3]})
        cfg = {
            "add_column_to_transposed_df": {
                "location": 0,
                "header": "idx",
                "values": [10, 20, 30],
            }
        }
        result = transform.add_column_to_df(df, cfg)
        assert list(result.columns) == ["idx", "a"]
        assert list(result["idx"]) == [10, 20, 30]

    def test_no_add_column_config(self, transform):
        df = pd.DataFrame({"x": [1]})
        cfg = {}
        result = transform.add_column_to_df(df, cfg)
        pd.testing.assert_frame_equal(result, df)

    def test_none_df(self, transform):
        cfg = {"add_column_to_transposed_df": {"location": 0, "header": "h", "values": []}}
        result = transform.add_column_to_df(None, cfg)
        assert result is None


class TestTransformTransposeDf:

    def test_transpose_with_column_name(self, transform):
        df = pd.DataFrame({"label": ["row1", "row2"], "val1": [10, 20], "val2": [30, 40]})
        cfg = {"transposed_df_column_name": {"column": "label", "drop": True}}
        result = transform.transpose_df(df, cfg)
        assert list(result.columns) == ["row1", "row2"]
        assert result.shape == (2, 2)

    def test_no_transpose_config(self, transform):
        df = pd.DataFrame({"a": [1]})
        cfg = {}
        result = transform.transpose_df(df, cfg)
        pd.testing.assert_frame_equal(result, df)

    def test_none_df_returns_none(self, transform):
        result = transform.transpose_df(None, {"transposed_df_column_name": None})
        assert result is None


class TestTransformDfTransformRepeatColumnsToUniqueColumns:

    def test_duplicate_columns_made_unique(self, transform):
        df = pd.DataFrame([[1, 2, 3]], columns=["a", "a", "b"])
        result = transform.df_transform_repeat_columns_to_unique_columns(df)
        assert len(result.columns) == len(set(result.columns))

    def test_no_duplicates_unchanged(self, transform):
        df = pd.DataFrame([[1, 2]], columns=["x", "y"])
        result = transform.df_transform_repeat_columns_to_unique_columns(df)
        assert list(result.columns) == ["x", "y"]


class TestTransformConvertNumpyTypes:

    def test_convert_int64(self, transform):
        data = {"a": np.int64(5), "b": np.float64(3.14)}
        cfg = {"datatype": dict, "data": data}
        result = transform.convert_numpy_types_to_native_python_types(cfg)
        assert type(result["a"]) is int or type(result["a"]) is float
        assert type(result["b"]) is float

    def test_native_types_unchanged(self, transform):
        data = {"s": "hello", "i": 42, "f": 1.5}
        cfg = {"datatype": dict, "data": data}
        result = transform.convert_numpy_types_to_native_python_types(cfg)
        assert result == data

    def test_unsupported_datatype_returns_none(self, transform, capsys):
        cfg = {"datatype": list, "data": [1, 2]}
        result = transform.convert_numpy_types_to_native_python_types(cfg)
        assert result is None
        captured = capsys.readouterr()
        assert "not supported" in captured.out


class TestTransformDataframeToHtml:

    def test_basic_html(self, transform):
        df = pd.DataFrame({"x": [1, 2], "y": [3, 4]})
        html = transform.dataframe_to_html(df=df)
        assert "<table" in html
        assert "table-striped" in html

    def test_custom_settings(self, transform):
        df = pd.DataFrame({"a": [10]})
        cfg = {"index": False, "justify": "left", "classes": "custom-class"}
        html = transform.dataframe_to_html(df=df, cfg_settings=cfg)
        assert "custom-class" in html

    def test_none_df_uses_default(self, transform):
        html = transform.dataframe_to_html()
        assert "<table" in html
        assert "Somu" in html  # default sample data


class TestTransformDataframeToJson:

    def test_basic_json_output(self, transform):
        df = pd.DataFrame({"col": [1, 2, 3]})
        result = transform.dataframe_to_json(df, cfg={"index": False})
        parsed = json.loads(result)
        assert len(parsed) == 3

    def test_none_df_returns_empty_string(self, transform):
        result = transform.dataframe_to_json(None)
        assert result == ""


# ===========================================================================
# TransformData tests
# ===========================================================================

class TestTransformDataLinear:

    def test_list_data(self, transform_data):
        cfg = {"scale": 2, "shift": 1, "data": [1, 2, 3], "type": "linear"}
        result = transform_data.linear(cfg)
        assert result["data"] == [3, 5, 7]

    def test_scalar_data(self, transform_data):
        cfg = {"scale": 3, "shift": -1, "data": 10, "type": "linear"}
        result = transform_data.linear(cfg)
        assert result["data"] == 29

    def test_zero_scale(self, transform_data):
        cfg = {"scale": 0, "shift": 5, "data": [10, 20], "type": "linear"}
        result = transform_data.linear(cfg)
        assert result["data"] == [5, 5]

    def test_identity_transform(self, transform_data):
        cfg = {"scale": 1, "shift": 0, "data": [7], "type": "linear"}
        result = transform_data.linear(cfg)
        assert result["data"] == [7]

    def test_get_transformed_data_dispatch(self, transform_data):
        cfg = {"scale": 1, "shift": 10, "data": 5, "type": "linear"}
        transform_data.get_transformed_data(cfg)
        assert cfg["data"] == 15


# ===========================================================================
# PandasChainedAssignent context manager tests
# ===========================================================================

class TestPandasChainedAssignment:

    def test_context_sets_and_restores(self):
        original = pd.options.mode.chained_assignment
        with PandasChainedAssignent("raise"):
            assert pd.options.mode.chained_assignment == "raise"
        assert pd.options.mode.chained_assignment == original

    def test_context_none(self):
        original = pd.options.mode.chained_assignment
        with PandasChainedAssignent(None):
            assert pd.options.mode.chained_assignment is None
        assert pd.options.mode.chained_assignment == original

    def test_context_warn(self):
        original = pd.options.mode.chained_assignment
        with PandasChainedAssignent("warn"):
            assert pd.options.mode.chained_assignment == "warn"
        assert pd.options.mode.chained_assignment == original

    def test_invalid_value_raises(self):
        with pytest.raises(AssertionError):
            PandasChainedAssignent("invalid")


# ===========================================================================
# transform_df_datetime_to_str tests
# ===========================================================================

class TestTransformDfDatetimeToStr:

    def test_datetime_column_converted(self):
        df = pd.DataFrame({
            "ts": [datetime.datetime(2024, 1, 15, 10, 30, 0), datetime.datetime(2024, 6, 20, 14, 0, 0)],
            "val": [1, 2],
        })
        result = transform_df_datetime_to_str(df)
        assert result["ts"].iloc[0] == "2024-01-15 10:30:00"
        assert result["ts"].iloc[1] == "2024-06-20 14:00:00"

    def test_date_column_converted(self):
        df = pd.DataFrame({
            "dt": [datetime.date(2023, 12, 25), datetime.date(2024, 1, 1)],
            "x": [10, 20],
        })
        result = transform_df_datetime_to_str(df)
        assert result["dt"].iloc[0] == "2023-12-25 00:00:00"

    def test_custom_format(self):
        df = pd.DataFrame({"ts": [datetime.datetime(2024, 3, 5, 8, 0, 0)]})
        result = transform_df_datetime_to_str(df, date_format="%Y/%m/%d")
        assert result["ts"].iloc[0] == "2024/03/05"

    def test_empty_df(self):
        df = pd.DataFrame({"ts": pd.Series([], dtype="object"), "val": pd.Series([], dtype="float64")})
        result = transform_df_datetime_to_str(df)
        assert len(result) == 0

    def test_no_datetime_columns_unchanged(self):
        df = pd.DataFrame({"a": [1, 2], "b": ["x", "y"]})
        result = transform_df_datetime_to_str(df)
        pd.testing.assert_frame_equal(result, df)

    def test_does_not_mutate_original(self):
        df = pd.DataFrame({"ts": [datetime.datetime(2024, 1, 1)]})
        original_val = df["ts"].iloc[0]
        _ = transform_df_datetime_to_str(df)
        assert df["ts"].iloc[0] == original_val


# ===========================================================================
# ReadData: structured data from ASCII (space-delimited)
# ===========================================================================

class TestReadDataStructuredAsciiData:

    def test_space_delimited_to_dataframe(self, read_data, tmp_path):
        f = tmp_path / "data.txt"
        f.write_text("1.0 2.0 3.0\n4.0 5.0 6.0\n7.0 8.0 9.0\n")
        cfg = {
            "io": str(f),
            "delimiter": "space",
            "DataFrame": True,
            "columns": ["col_a", "col_b", "col_c"],
        }
        result = read_data.from_ascii_file_get_structured_data_delimited_white_space(cfg)
        assert isinstance(result, pd.DataFrame)
        assert list(result.columns) == ["col_a", "col_b", "col_c"]
        assert len(result) == 3
        assert result["col_a"].iloc[0] == pytest.approx(1.0)
        assert result["col_c"].iloc[2] == pytest.approx(9.0)

    def test_space_delimited_to_list(self, read_data, tmp_path):
        f = tmp_path / "data.txt"
        f.write_text("10 20\n30 40\n")
        cfg = {
            "io": str(f),
            "delimiter": "space",
            "DataFrame": False,
            "columns": ["x", "y"],
        }
        result = read_data.from_ascii_file_get_structured_data_delimited_white_space(cfg)
        assert isinstance(result, list)
        assert result[0] == [10.0, 30.0]
        assert result[1] == [20.0, 40.0]
