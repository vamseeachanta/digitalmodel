"""ISA-5.1 tag and line-number grammar."""

from __future__ import annotations

import pytest

from digitalmodel.process_diagrams.tags import (
    LineNumber,
    TagError,
    parse_line_number,
    parse_tag,
)


class TestInstrumentTags:
    def test_splits_variable_modifier_functions_and_loop(self):
        tag = parse_tag("PDIC-1204")
        assert (tag.variable, tag.variable_modifier) == ("P", "D")
        assert tag.functions == "IC"
        assert tag.loop == "1204"
        assert tag.letters == "PDIC"

    def test_describes_each_letter(self):
        assert parse_tag("PDIC-1204").describe() == (
            "pressure, differential, indicate, control"
        )
        assert parse_tag("FQI-416").describe() == (
            "flow, integrate or totalise, indicate"
        )

    def test_z_in_second_position_is_the_sis_modifier(self):
        # As a succeeding letter Z means "actuator" and never appears second;
        # position tags put Z first (ZV, ZC, ZT).
        tag = parse_tag("SZ-404")
        assert tag.variable_modifier == "Z"
        assert tag.is_safety_function

        position_tag = parse_tag("ZV-403")
        assert position_tag.variable == "Z"
        assert position_tag.variable_modifier is None
        assert not position_tag.is_safety_function

    def test_high_high_and_low_low_are_trips(self):
        assert parse_tag("PZHH-401").is_trip
        assert parse_tag("LSLL-102").is_trip
        assert not parse_tag("LAH-102").is_trip
        assert parse_tag("LAH-102").function_modifier == "H"

    @pytest.mark.parametrize(
        "tag,expected",
        [("PZHH-401", True), ("AZHH-305", True), ("SZ-404", True),
         ("PSHH-401", False), ("LAHH-401", False), ("TIC-409", False)],
    )
    def test_safety_function_detection(self, tag, expected):
        assert parse_tag(tag).is_safety_function is expected

    def test_sis_suffix_form_is_also_accepted(self):
        assert parse_tag("PSHH-401 (SIS)").is_safety_function

    def test_same_loop_groups_controller_and_final_element(self):
        assert parse_tag("LIC-401").same_loop(parse_tag("LV-401"))
        assert not parse_tag("LIC-401").same_loop(parse_tag("LV-408"))

    def test_case_and_whitespace_are_normalised(self):
        assert parse_tag("  pdic-1204 ").letters == "PDIC"

    @pytest.mark.parametrize(
        "bad",
        ["P-101",          # no succeeding function letter
         "PDIC1204",       # no separator
         "1AB-101",        # digit as first letter
         "PDIC-",          # no loop number
         "P#C-101"],       # not a letter
    )
    def test_rejects_malformed_tags(self, bad):
        with pytest.raises(TagError):
            parse_tag(bad)


class TestLineNumbers:
    def test_splits_all_five_fields(self):
        line = parse_line_number('12"-PG-1001-A1A-IH')
        assert line == LineNumber(
            raw='12"-PG-1001-A1A-IH', size_in=12.0, service="PG",
            sequence="1001", piping_class="A1A", insulation="IH",
        )
        assert line.is_insulated

    def test_insulation_code_is_optional(self):
        line = parse_line_number('6"-NGL-6003-F1A')
        assert line.insulation is None
        assert not line.is_insulated

    @pytest.mark.parametrize(
        "text,size",
        [('1-1/2"-FG-2001-C1A', 1.5), ('3/4"-VT-9001-A1A', 0.75), ('24"-FL-7002-A1A', 24.0)],
    )
    def test_fractional_and_whole_sizes(self, text, size):
        assert parse_line_number(text).size_in == pytest.approx(size)

    def test_cryogenic_line_carries_a_low_temperature_class(self):
        # 304L cryogenic class with cold conservation — the cold flare header
        # must be a distinct system from the warm carbon-steel one.
        cold = parse_line_number('12"-FLC-7003-S1D-C')
        warm = parse_line_number('24"-FL-7002-A1A')
        assert cold.piping_class != warm.piping_class
        assert cold.service != warm.service

    @pytest.mark.parametrize("bad", ["12-PG-1001-A1A", '12"-PG-A1A', "PG-1001-A1A"])
    def test_rejects_malformed_line_numbers(self, bad):
        with pytest.raises(TagError):
            parse_line_number(bad)
