# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Offline header-only repository-reference guard; synthetic scanner examples.

Run directly with Python as well as through the hosted pytest suite. No model
parser, package import, solver, network, or temporary directory is required.
"""

import re
import runpy
import unittest
from pathlib import Path

# Public repository URL recorded in src/digitalmodel/__init__.py:2.
PUBLIC_REPOSITORIES = frozenset({"vamseeachanta/digitalmodel"})
REPOSITORY_REFERENCE = re.compile(
    r"(?:https?://github\.com/)([\w.-]+/[\w.-]+)"
    r"|(?<![\w/.-])([\w.-]+/[\w.-]+)(?:/[\w./-]+)?"
    r"(?=\s+(?:issues?\b|repository\b|repo\b)|#\d)",
    re.IGNORECASE,
)


def header_repository_findings(text: str) -> list[int]:
    """Return header line numbers only, withholding potentially private text.

    Recognize GitHub URLs and owner/repository references followed by issue or
    repository context. Ordinary slash-separated prose is not a repository ID.
    YAML directives, document markers and blank lines may precede comments.
    Scanning stops at the first data line.
    """
    findings = []
    for number, line in enumerate(text.lstrip("\ufeff").splitlines(), 1):
        stripped = line.strip()
        if not stripped or stripped.startswith("%YAML") or stripped == "---":
            continue
        if not stripped.startswith("#"):
            break
        for match in REPOSITORY_REFERENCE.finditer(stripped):
            reference = (match.group(1) or match.group(2)).rstrip(".")
            if reference.casefold() not in PUBLIC_REPOSITORIES:
                findings.append(number)
                break
    return findings


class TestModelHeaderGuard(unittest.TestCase):
    def test_synthetic_private_style_header(self) -> None:
        text = "# Synthetic input\n# example-owner/restricted-repo issues #2\nx: 1\n"
        self.assertEqual(header_repository_findings(text), [2])
        nested = "# example-owner/restricted-repo/models issues #2\nx: 1\n"
        self.assertEqual(header_repository_findings(nested), [1])
        url = "# https://github.com/example-owner/restricted-repo/tree/main\n"
        self.assertEqual(header_repository_findings(url), [1])

    def test_public_issue_header(self) -> None:
        text = "# https://github.com/vamseeachanta/digitalmodel/issues/2425\nx: 1\n"
        self.assertEqual(header_repository_findings(text), [])

    def test_header_boundary_and_directives(self) -> None:
        text = "%YAML 1.1\n---\n# installed/licensed\nx: 1\n"
        text += "# example-owner/restricted-repo issues #2\n"
        self.assertEqual(header_repository_findings(text), [])

    def test_model_headers(self) -> None:
        root = Path(__file__).resolve().parents[3]
        models = root / "docs/domains/orcaflex"
        findings: list[str] = []
        count = 0
        for path in sorted(models.rglob("*")):
            if path.suffix.lower() not in {".yml", ".yaml"} or not path.is_file():
                continue
            count += 1
            # Preserve non-UTF-8 bytes; repository identifiers are ASCII.
            text = path.read_text(encoding="utf-8-sig", errors="surrogateescape")
            lines = header_repository_findings(text)
            findings.extend(f"{path.relative_to(root).as_posix()}:{n}" for n in lines)
        self.assertGreater(count, 0)
        self.assertEqual(findings, [], "Header repository references (text withheld)")


class TestDuplicateInventory(unittest.TestCase):
    def setUp(self) -> None:
        root = Path(__file__).resolve().parents[3]
        self.tool = runpy.run_path(
            str(root / "docs/domains/orcaflex/library/duplicate_inventory.py")
        )

    def test_comment_digest_preserves_model_bytes(self) -> None:
        digest = self.tool["content_digest"]
        self.assertEqual(digest(b"# synthetic\nx: 1\r\n"), digest(b"x: 1\n"))
        self.assertNotEqual(digest(b"x: 1\n"), digest(b"x: 2\n"))
        self.assertNotEqual(digest(b"x: 1\n"), digest(b"x: 1\n\n"))
        self.assertNotEqual(digest(b"x: 1\n"), digest(b"x: 1 # inline\n"))

    def test_family_and_canonical_proposal(self) -> None:
        prefix = "docs/domains/orcaflex/"
        paths = [prefix + "examples/yml/sample.yml", prefix + "examples/raw/sample.yml"]
        family = self.tool["family"](paths)
        self.assertEqual(family, "examples_and_model_library")
        chosen = min(paths, key=lambda p: self.tool["canonical_key"](p, family))
        self.assertEqual(chosen, paths[1])

    def test_buoy_proposal_prefers_non_modular_source(self) -> None:
        prefix = "docs/domains/orcaflex/mooring/"
        paths = [prefix + "buoy_converted/sample.yml", prefix + "buoy/sample.yml"]
        family = self.tool["family"](paths)
        self.assertEqual(family, "buoy")
        chosen = min(paths, key=lambda p: self.tool["canonical_key"](p, family))
        self.assertEqual(chosen, paths[1])



class TestProvenanceNotes(unittest.TestCase):
    """Unrecorded redistribution terms will remain explicit in all notes."""

    def test_redistribution_terms_not_stated(self) -> None:
        """Every added note will state the same unverified redistribution terms."""
        root = Path(__file__).resolve().parents[3]
        paths = (
            "docs/domains/orcaflex/aqwa/to_orcaflex/PROVENANCE.md",
            "docs/domains/orcaflex/mooring/buoy/modular/spm-proj-1/PROVENANCE.md",
            "docs/domains/orcaflex/mooring/buoy/modular/spm-proj-2/PROVENANCE.md",
            "docs/domains/orcaflex/mooring/buoy/spm-proj-1/PROVENANCE.md",
            "docs/domains/orcaflex/mooring/buoy/spm-proj-2/PROVENANCE.md",
            "docs/domains/orcaflex/mooring/buoy_converted/spm-proj-1/PROVENANCE.md",
            "docs/domains/orcaflex/mooring/buoy_converted/spm-proj-2/PROVENANCE.md",
            "docs/domains/orcaflex/mooring/drillship-riser/PROVENANCE.md",
            "docs/domains/orcaflex/mooring/reference/PROVENANCE.md",
            "docs/domains/orcaflex/mooring/riser/PROVENANCE.md",
            "docs/domains/orcaflex/mooring/semi/PROVENANCE.md",
            "docs/domains/orcaflex/mooring_fender/PROVENANCE.md",
            "docs/domains/orcaflex/reference/PROVENANCE.md",
            "docs/domains/orcaflex/regional/PROVENANCE.md",
            "docs/domains/orcaflex/risers/completion/PROVENANCE.md",
            "docs/domains/orcaflex/risers/disconnect/PROVENANCE.md",
            "docs/domains/orcaflex/risers/drilling/PROVENANCE.md",
            "docs/domains/orcaflex/risers/intervention/PROVENANCE.md",
            "docs/domains/orcaflex/risers/production/PROVENANCE.md",
            "tests/solvers/orcaflex/analysis/moorings/PROVENANCE.md",
        )
        expected = (
            "not stated; redistribution terms to be checked "
            "before any model file is shared outside"
        )
        self.assertEqual(len(paths), 20)
        for name in paths:
            with self.subTest(path=name):
                text = (root / name).read_text(encoding="utf-8")
                self.assertIn(expected, text)
                self.assertNotIn("internal use only", text.casefold())


if __name__ == "__main__":
    unittest.main()
