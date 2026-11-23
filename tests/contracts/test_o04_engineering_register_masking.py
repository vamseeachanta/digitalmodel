"""URL and inline-code masking contracts for the engineering register."""

import importlib.util
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[2]
SPEC = importlib.util.spec_from_file_location(
    "masking_checker", ROOT / "scripts/enforcement/check-engineering-register.py"
)
CHECKER = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(CHECKER)

ADJECTIVE = "R5-unsupported-adj"
SUBJECT = "R1-subject"


@pytest.mark.parametrize(
    "source",
    [
        "http://example.org/conservative-design",
        "https://example.org/conservative-design",
        "www.example.org/conservative-design",
        "[source](https://example.org/conservative-design)",
        "[source](conservative-design)",
        "![diagram](https://example.org/conservative-design)",
        "![diagram](conservative-design)",
        "[id]: https://example.org/conservative-design",
    ],
    ids=[
        "http",
        "https",
        "www",
        "link",
        "relative-link",
        "image",
        "relative-image",
        "reference",
    ],
)
def test_new_url_and_target_mask_leaves_next_line_prose(source):
    assert CHECKER.check_file_text(source + "\nCONSERVATIVE prose.") == [
        (2, ADJECTIVE, "'CONSERVATIVE' without governing criterion")
    ]


@pytest.mark.parametrize("width", [2, 4, 6, 8])
def test_new_even_backtick_span_leaves_next_line_prose(width):
    ticks = "`" * width
    assert CHECKER.check_file_text(
        f"Text {ticks}conservative{ticks}.\nCONSERVATIVE prose."
    ) == [(2, ADJECTIVE, "'CONSERVATIVE' without governing criterion")]


@pytest.mark.parametrize("source", ["https://example.org/our", "www.example.org/our"])
def test_new_url_first_person_leaves_prose(source):
    assert CHECKER.check_file_text(source + " notes\nOur analysis converges.") == [
        (2, SUBJECT, "First-person 'Our' — use an analysis/component subject")
    ]


def test_new_double_backtick_first_person_leaves_prose():
    assert CHECKER.check_file_text(
        "Text ``our analysis``.\nOur analysis converges."
    ) == [(2, SUBJECT, "First-person 'Our' — use an analysis/component subject")]


@pytest.mark.parametrize("punctuation", ["", ".", ",", ";", ")"])
def test_new_url_and_same_word_prose_on_one_line(punctuation):
    text = f"https://example.org/conservative-design{punctuation} CONSERVATIVE prose."
    assert CHECKER.check_file_text(text) == [
        (1, ADJECTIVE, "'CONSERVATIVE' without governing criterion")
    ]


@pytest.mark.parametrize(
    "text",
    [
        "[source](https://example.org/conservative-design) CONSERVATIVE prose.",
        "[id]: https://example.org/conservative-design CONSERVATIVE prose.",
        "Text ``conservative`` CONSERVATIVE prose.",
    ],
    ids=["link-prose", "reference-prose", "code-prose"],
)
def test_new_source_mask_selects_visible_prose(text):
    assert CHECKER.check_file_text(text) == [
        (1, ADJECTIVE, "'CONSERVATIVE' without governing criterion")
    ]


def test_new_two_double_backtick_runs_form_one_span():
    assert CHECKER.check_file_text("Text ``conservative between safe`` end.") == []


def test_new_double_backtick_span_can_contain_single_backtick():
    assert CHECKER.check_file_text("Text ``conservative ` safe`` end.") == []


@pytest.mark.parametrize(
    "text",
    [
        "[source](https://example.org/conservative-design_(draft))",
        "[source](conservative-design_(draft))",
        "[source](conservative-design_(draft_(v2)))",
    ],
)
def test_new_nested_link_target_is_masked(text):
    assert CHECKER.check_file_text(text) == []


@pytest.mark.parametrize("width", [1, 3, 5])
def test_regression_pin_odd_backtick_spans_and_prose(width):
    """BASE already masks these inline spans."""
    ticks = "`" * width
    assert CHECKER.check_file_text(
        f"Text {ticks}conservative{ticks}.\nCONSERVATIVE prose."
    ) == [(2, ADJECTIVE, "'CONSERVATIVE' without governing criterion")]


def test_regression_pin_visible_link_text_is_prose():
    assert CHECKER.check_file_text("[CONSERVATIVE](https://example.org/notes)") == [
        (1, ADJECTIVE, "'CONSERVATIVE' without governing criterion")
    ]


def test_regression_pin_single_backtick_first_person():
    assert CHECKER.check_file_text("Text `our analysis`.\nOur analysis converges.") == [
        (2, SUBJECT, "First-person 'Our' — use an analysis/component subject")
    ]


def test_regression_pin_angle_url_criterion_detection():
    """BASE reports zero through its criterion detector, although it does not mask."""
    assert CHECKER.check_file_text("<https://example.org/conservative-design>") == []


@pytest.mark.parametrize(
    "text",
    [
        "Text ` conservative prose.",
        "Text `` conservative prose.",
        "Text `` conservative ` end.",
    ],
    ids=["lone", "unclosed-double", "mismatched"],
)
def test_regression_pin_unbalanced_backticks_leave_prose(text):
    assert CHECKER.check_file_text(text) == [
        (1, ADJECTIVE, "'conservative' without governing criterion")
    ]


def test_regression_pin_prose_between_two_single_backtick_spans():
    assert CHECKER.check_file_text("Text `safe` conservative `adequate` end.") == [
        (1, ADJECTIVE, "'conservative' without governing criterion")
    ]


@pytest.mark.parametrize(
    "text",
    [
        "The coating shall be recommended for use.",
        "Text ``shall be recommended`` end.",
        "https://example.org/mandatory-should",
        "[source](shall be recommended)",
    ],
)
def test_regression_pin_unrelated_rule_keeps_its_input(text):
    assert CHECKER.check_file_text(text) == [
        (1, "R4-shall-should", "Requirement and advisory language are mixed")
    ]


def test_regression_pin_other_constructs_and_rules(tmp_path):
    text = (
        "---\nOur analysis converges.\n---\n"
        "> We use safe values.\n"
        "```text\nWe use safe values.\n```\n"
        "    We use safe values.\n"
        "<!-- safe -->\n"
        "Table 1. Values\n| Item | Value |\n"
        "The thickness is 12.5 mm.\n"
    )
    expected = [
        (2, SUBJECT, "First-person 'Our' — use an analysis/component subject"),
        (12, "R7-thickness-decimals", "Thickness '12.5 mm' requires 3 decimals"),
        (10, "R6-caption-position", "Caption appears above table; place it below"),
    ]
    path = tmp_path / "report.md"
    path.write_text(text, encoding="utf-8")
    assert CHECKER.check_file_text(text) == expected
    assert CHECKER.check_file(path) == expected


def test_regression_pin_single_span_keeps_existing_word_join():
    assert CHECKER.check_file_text("Text sa`identifier`fe.") == [
        (1, ADJECTIVE, "'safe' without governing criterion")
    ]


@pytest.mark.parametrize(
    "text",
    [
        "Text ](safe prose).",
        "Text [source](safe prose).",
        r"Text \[source](safe-design).",
        "Text [source](safe-design without a close.",
    ],
    ids=["missing-label", "invalid-spaces", "escaped-label", "unclosed-target"],
)
def test_regression_pin_malformed_links_leave_prose(text):
    assert CHECKER.check_file_text(text) == [
        (1, ADJECTIVE, "'safe' without governing criterion")
    ]


@pytest.mark.parametrize(
    "text",
    [
        '[source](conservative-design "our safe note")',
        "[source](conservative-design 'our safe note')",
    ],
    ids=["double-quoted-title", "single-quoted-title"],
)
def test_fix_link_destination_mask_leaves_title(text):
    assert CHECKER.check_file_text(text) == CHECKER.check_file_text("our safe note")


def test_regression_pin_angle_link_target_with_title():
    assert CHECKER.check_file_text('[source](<conservative-design> "note")') == []


def test_new_angle_link_destination_first_person_is_masked():
    assert CHECKER.check_file_text("[source](<our notes>)") == []


def test_extended_self_test():
    assert CHECKER.run_self_test()


@pytest.mark.parametrize(
    "text, prose",
    [
        ("|https://example.org/a|conservative|", "conservative"),
        ("| https://example.org/a|conservative estimate |", "conservative estimate"),
        ("www.example.org/a|safe|", "safe"),
        ("**https://example.org/a**conservative", "conservative"),
        ("(https://example.org/a)conservative", "conservative"),
        ("https://example.org/a,conservative design", "conservative design"),
        ("|https://example.org/a|our notes|", "our notes"),
        ("|https://example.org/safe-design|conservative|", "conservative"),
        ("**https://example.org/safe-design**conservative", "conservative"),
        ("(https://example.org/safe-design)conservative", "conservative"),
        ("_https://example.org/a_our notes", ""),
        ("https://example.org/a;safe value", "safe value"),
        ("https://example.org/a'safe value", "safe value"),
    ],
    ids=[
        "table",
        "table-spaces",
        "www",
        "bold",
        "parenthesis",
        "comma",
        "subject",
        "table-slug",
        "bold-slug",
        "parenthesis-slug",
        "pin-italic",
        "semicolon",
        "quote",
    ],
)
def test_fix_url_boundary_preserves_adjacent_prose(text, prose):
    """The italic case pins MAIN's lack of a word boundary after an underscore."""
    assert CHECKER.check_file_text(text) == CHECKER.check_file_text(prose)


@pytest.mark.parametrize("image", [False, True], ids=["link", "image"])
@pytest.mark.parametrize(
    "target, title, prose",
    [
        ("conservative-design", '"our safe note"', "our safe note"),
        ("safe-design", "'we use conservative values'", "we use conservative values"),
        ("<our notes>", '"we use safe values"', "we use values"),
    ],
    ids=["double-title", "single-title", "angle-title"],
)
def test_fix_link_and_image_titles_remain_prose(image, target, title, prose):
    text = ("!" if image else "") + f"[source]({target} {title})"
    assert CHECKER.check_file_text(text) == CHECKER.check_file_text(prose)


@pytest.mark.parametrize(
    "text",
    [
        "[id]: docs/conservative-design.md",
        "   [id]: docs/conservative-design.md",
        "[id]: <our notes>",
    ],
    ids=["relative", "indent-three", "angle-subject"],
)
def test_fix_reference_destination_is_masked(text):
    assert CHECKER.check_file_text(text) == []


@pytest.mark.parametrize(
    "text, prose",
    [
        ('[id]: docs/conservative-design.md "our safe note"', "our safe note"),
        (
            "[id]: docs/safe-design.md 'we use conservative values'",
            "we use conservative values",
        ),
        ('[id]: <our notes> "we use safe values"', "we use values"),
    ],
    ids=["double-title", "single-title", "angle-title"],
)
def test_fix_reference_relative_destination_leaves_title(text, prose):
    assert CHECKER.check_file_text(text) == CHECKER.check_file_text(prose)


@pytest.mark.parametrize(
    "text, prose",
    [
        ("https://example.org/conservative-design_(draft) SAFE prose.", "SAFE prose."),
        ("https://example.org/conservative_design*value SAFE prose.", "SAFE prose."),
        ("[id]: https://example.org/conservative-design", ""),
        ('[id]: https://example.org/a "our safe note"', "our safe note"),
        ('[id]: <https://example.org/a> "we use safe values"', "we use values"),
        ("Text [id]: docs/conservative-design.md", "conservative"),
        ("Text [id]: docs/safe-design.md", "safe"),
    ],
    ids=[
        "balanced-url",
        "internal-emphasis",
        "absolute-definition",
        "absolute-title",
        "angle-absolute-title",
        "embedded-definition",
        "embedded-safe",
    ],
)
def test_regression_pin_boundary_and_reference_forms(text, prose):
    assert CHECKER.check_file_text(text) == CHECKER.check_file_text(prose)
