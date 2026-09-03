"""Sheet assembly, rendering, and drafting lint."""

from __future__ import annotations

import xml.etree.ElementTree as ET

import pytest

from digitalmodel.process_diagrams import Sheet, TitleBlock, symbols
from digitalmodel.process_diagrams.sheet import (
    ZONE_LETTERS,
    note_flag,
    off_page_connector,
)
from digitalmodel.process_diagrams.tags import TagError


@pytest.fixture
def sheet() -> Sheet:
    return Sheet(
        width=800, height=600,
        title_block=TitleBlock(
            drawing_number="AE-0000-PID-1001", title="INLET SEPARATION",
            originator="ACEENGINEER", revision="A", sheet="1 OF 3",
            date="2026-09-03",
        ),
        aria_label="Test sheet",
    )


class TestRendering:
    def test_renders_well_formed_svg(self, sheet):
        sheet.add(symbols.vessel_vertical(200, 300, 30, 70))
        root = ET.fromstring(sheet.render())
        assert root.tag == "svg"
        assert root.get("viewBox") == "0 0 800 600"

    def test_carries_an_aria_label_for_screen_readers(self, sheet):
        assert ET.fromstring(sheet.render()).get("aria-label") == "Test sheet"

    def test_zone_grid_letters_run_bottom_to_top(self, sheet):
        out = sheet.render()
        # Four rows: A at the bottom of the sheet, D at the top.
        for letter in ZONE_LETTERS[-4:]:
            assert f">{letter}</text>" in out

    def test_title_block_carries_the_controlled_document_fields(self, sheet):
        out = sheet.render()
        assert "AE-0000-PID-1001" in out
        assert "INLET SEPARATION" in out
        assert "A · 1 OF 3" in out
        assert "SCALE: NONE" in out          # P&IDs are never to scale
        assert "NOT FOR CONSTRUCTION" in out  # default status stamp

    def test_status_stamp_can_be_changed_on_issue(self, sheet):
        sheet.title_block.status = "ISSUED FOR REVIEW"
        assert "ISSUED FOR REVIEW" in sheet.render()

    def test_notes_column_numbers_notes_from_one(self, sheet):
        assert sheet.note("First note.") == 1
        assert sheet.note("Second note.") == 2
        out = sheet.render()
        assert "1. First note." in out and "2. Second note." in out

    def test_no_notes_column_when_there_are_no_notes(self, sheet):
        assert "NOTES" not in sheet.render()


class TestTagValidation:
    def test_instruments_are_validated_as_they_are_placed(self, sheet):
        sheet.add_instrument(100, 100, "LIC-401", kind="bpcs")
        assert sheet.loops() == {"401": ["LIC-401"]}

    def test_a_malformed_tag_fails_at_build_time(self, sheet):
        with pytest.raises(TagError):
            sheet.add_instrument(100, 100, "NOTATAG", kind="field")

    def test_loops_group_controller_with_final_element(self, sheet):
        sheet.add_instrument(100, 100, "LIC-401", kind="bpcs")
        sheet.add_control_valve(200, 100, "LV-401")
        assert sheet.loops()["401"] == ["LIC-401", "LV-401"]


class TestLint:
    def test_clean_sheet_reports_nothing(self, sheet):
        sheet.add_control_valve(200, 100, "LV-401", fail="FC")
        assert sheet.lint() == []

    def test_control_valve_without_a_fail_action_is_a_defect(self, sheet):
        sheet.add_control_valve(200, 100, "LV-401", fail=None)
        assert sheet.lint() == ["control valve LV-401 has no fail action annotated"]

    def test_duplicate_tags_are_reported(self, sheet):
        sheet.add_instrument(100, 100, "LIC-401", kind="bpcs")
        sheet.add_instrument(300, 100, "LIC-401", kind="bpcs")
        assert sheet.lint() == ["tag LIC-401 is used 2 times"]

    def test_fail_open_is_accepted_and_annotated(self, sheet):
        # The anti-surge recycle valve is normally the only FO on a
        # compressor sheet; it must survive lint and appear on the drawing.
        sheet.add_control_valve(200, 100, "UV-501", fail="FO", actuator="piston")
        assert sheet.lint() == []
        assert ">FO</text>" in sheet.render()


class TestAnnotation:
    def test_off_page_connector_points_the_way_it_is_told(self):
        right = off_page_connector(0, 100, ["TO PID-1003"], direction="right")
        left = off_page_connector(0, 100, ["FROM PID-1001"], direction="left")
        assert "TO PID-1003" in right and "FROM PID-1001" in left
        assert right != left

    def test_connector_carries_line_number_and_destination(self):
        out = off_page_connector(0, 100, ['16"-RG-5003-F1A', "PID-1004 / D-2"])
        # The inch mark is escaped, or the document stops parsing.
        assert "16&quot;-RG-5003-F1A" in out and "PID-1004 / D-2" in out
        ET.fromstring(f'<svg xmlns="http://www.w3.org/2000/svg">{out}</svg>')

    def test_ampersands_and_quotes_in_labels_are_escaped(self, sheet):
        # "SHELL & TUBE" and a 12" line number both break a raw document.
        sheet.note('EXCHANGER, SHELL & TUBE on 12"-PG-1001-A1A')
        sheet.title_block.title = "AMINE & DEHYDRATION"
        out = sheet.render()
        ET.fromstring(out)
        assert "SHELL &amp; TUBE" in out and "AMINE &amp; DEHYDRATION" in out

    def test_note_flag_shows_its_number(self):
        assert ">3</text>" in note_flag(100, 100, 3)


class TestSymbols:
    @pytest.mark.parametrize(
        "kind,expected",
        [("field", "circle"), ("bpcs", "rect"), ("sis", "rect"),
         ("computer", "path"), ("interlock", "path")],
    )
    def test_bubble_kind_selects_the_geometry(self, kind, expected):
        assert f"<{expected}" in symbols.bubble(50, 50, "PT", "101", kind=kind)

    def test_bpcs_is_a_circle_in_a_square_and_sis_a_diamond_in_a_square(self):
        # ISA-5.1-2009: the square means shared display / shared control; the
        # inner shape selects BPCS vs SIS. It does NOT mean DCS vs PLC.
        bpcs = symbols.bubble(50, 50, "PIC", "101", kind="bpcs")
        sis = symbols.bubble(50, 50, "PZHH", "101", kind="sis")
        assert "<circle" in bpcs and "<rect" in bpcs
        assert "<rect" in sis and "<circle" not in sis

    def test_every_symbol_is_well_formed_xml(self):
        fragments = [
            symbols.bubble(50, 50, "PT", "101"),
            symbols.gate(100, 100), symbols.globe(150, 100), symbols.ball(200, 100),
            symbols.check(250, 100), symbols.relief_valve(300, 100),
            symbols.orifice(350, 100),
            symbols.control_valve(400, 100, actuator="solenoid", fail="FC"),
            symbols.vessel_vertical(100, 200, 20, 40),
            symbols.vessel_horizontal(200, 200, 40, 20),
            symbols.column(300, 250, 30, 100, packed_beds=[(180, 220)]),
            symbols.exchanger_shell_tube(400, 200, 17),
            symbols.exchanger_plate_fin(450, 200, 20, 15),
            symbols.air_cooler(500, 200, 30),
            symbols.turbomachine(560, 200, 25, 20, expanding=True),
            symbols.pump(620, 200), symbols.pump(660, 200, flip=True),
            symbols.generator(700, 200), symbols.transformer(740, 200),
            symbols.breaker(780, 200), symbols.busbar(0, 800, 260),
        ]
        ET.fromstring('<svg xmlns="http://www.w3.org/2000/svg">'
                      + "".join(fragments) + "</svg>")

    def test_pump_impeller_flips_for_a_right_to_left_run(self):
        assert symbols.pump(100, 100) != symbols.pump(100, 100, flip=True)
