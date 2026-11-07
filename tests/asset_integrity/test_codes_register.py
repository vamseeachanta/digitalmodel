# ABOUTME: Keeps docs/domains/codes-register.md in sync with codes.REGISTER —
# ABOUTME: every registered CodeReference standard must be documented.
import ast
import io
import tokenize
from pathlib import Path, PureWindowsPath

import pytest

from digitalmodel import codes

_REGISTER_DOC = (
    Path(__file__).resolve().parents[2] / "docs" / "domains" / "codes-register.md"
)


def test_register_doc_exists():
    assert _REGISTER_DOC.is_file(), f"missing {_REGISTER_DOC}"


def test_every_registered_code_is_documented():
    text = _REGISTER_DOC.read_text(encoding="utf-8")
    missing = [
        ref.standard
        for ref in codes.REGISTER.values()
        if ref.standard not in text
    ]
    assert not missing, f"codes missing from codes-register.md: {missing}"


def test_register_is_nonempty_and_well_formed():
    assert codes.REGISTER
    assert all(isinstance(r, codes.CodeReference) for r in codes.REGISTER.values())
    assert all(r.standard for r in codes.REGISTER.values())


def _screening_reference_uses(root):
    sources = sorted([
        *(root / "src" / "digitalmodel").rglob("*.py"),
        *(root / "scripts").rglob("*.py"),
    ])
    uses = []
    for source in sources:
        if source == root / "src" / "digitalmodel" / "codes.py":
            continue  # The reference definition is allowed in its own module.
        tokens = tokenize.generate_tokens(
            io.StringIO(source.read_text(encoding="utf-8")).readline
        )
        for token in tokens:
            value = token.string if token.type == tokenize.NAME else None
            if token.type == tokenize.STRING:
                try:
                    value = ast.literal_eval(token.string)
                except (ValueError, SyntaxError):
                    value = None  # Dynamic strings are outside this static guard.
            if value == "ASME_B31_8_SCREENING":
                uses.append(f"{source.relative_to(root).as_posix()}:{token.start[0]}")
    return uses


def test_b318_screening_reference_remains_citation_only():
    root = Path(__file__).resolve().parents[2]
    assert (root / "src/digitalmodel/asset_integrity/assessment/level1_screener.py").is_file()
    assert not (uses := _screening_reference_uses(root)), (
        f"Citation-only screening reference used by production code: {uses}"
    )


@pytest.mark.parametrize("folder", [
    "src/digitalmodel/asset_integrity", "src/digitalmodel/structural", "scripts",
])
@pytest.mark.parametrize("expression", [
    "codes.ASME_B31_8_SCREENING.label",
    'codes.REGISTER["ASME_B31_8_SCREENING"].label',
    'getattr(codes, "ASME_B31_8_SCREENING").label',
])
def test_citation_guard_detects_direct_and_quoted_lookups(tmp_path, folder, expression):
    source = tmp_path / folder / "consumer.py"
    source.parent.mkdir(parents=True)
    source.write_text(f"result = {expression}\n", encoding="utf-8")
    assert _screening_reference_uses(tmp_path) == [f"{folder}/consumer.py:1"]


def test_citation_guard_reports_posix_paths(tmp_path, monkeypatch):
    source = tmp_path / "scripts" / "consumer.py"
    source.parent.mkdir()
    source.write_text("result = codes.ASME_B31_8_SCREENING.label\n", encoding="utf-8")
    relative_to = type(source).relative_to

    def windows_relative_to(path, *args, **kwargs):
        return PureWindowsPath(relative_to(path, *args, **kwargs))

    # Exercise Windows path rendering without requiring a Windows filesystem.
    with monkeypatch.context() as patch:
        patch.setattr(type(source), "relative_to", windows_relative_to)
        uses = _screening_reference_uses(tmp_path)
    assert uses == ["scripts/consumer.py:1"]


def test_citation_guard_ignores_comments_and_definition(tmp_path):
    package = tmp_path / "src" / "digitalmodel"
    package.mkdir(parents=True)
    (package / "codes.py").write_text(
        'ASME_B31_8_SCREENING = CodeReference("ASME B31.8", "2022")\n',
        encoding="utf-8",
    )
    (package / "consumer.py").write_text(
        '# ASME_B31_8_SCREENING is a citation record.\n'
        'text = "See ASME_B31_8_SCREENING in the register."\n',
        encoding="utf-8",
    )
    assert not _screening_reference_uses(tmp_path)


def test_session_codes_present():
    # The tubular-product and ship-structural codes added with the register.
    for name in ("API_5CT", "API_5C3", "API_11B", "IACS_S11", "IACS_S11A",
                 "DNV_RU_SHIP", "IACS_CSR"):
        assert name in codes.REGISTER, f"{name} not registered"
