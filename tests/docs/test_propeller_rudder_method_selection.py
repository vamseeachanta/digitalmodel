"""TDD tests for WRK-1149: propeller-rudder method selection document.

Validates acceptance criteria against the deliverable document.
"""
import pathlib
import re

import pytest

DOC = pathlib.Path(__file__).resolve().parents[2] / "docs" / "domains" / "hydrodynamics" / "propeller-rudder-method-selection.md"


@pytest.fixture(scope="module")
def doc_text():
    assert DOC.exists(), f"Deliverable not found: {DOC}"
    return DOC.read_text()


# --- AC1: Comparison table with five criteria scored/ranked ---

class TestAC1ComparisonTable:
    """Each method scored on accuracy, input requirements, J range, cost, complexity."""

    def test_comparison_matrix_exists(self, doc_text):
        assert "## 3. Comparison Matrix" in doc_text or "Comparison Matrix" in doc_text

    @pytest.mark.parametrize("criterion", [
        "Accuracy", "Input requirements", "J range", "Computational cost",
        "Implementation complexity",
    ])
    def test_criterion_present(self, doc_text, criterion):
        assert criterion.lower() in doc_text.lower(), f"Missing criterion: {criterion}"

    def test_scoring_table_exists(self, doc_text):
        """Numerical scores (1-5) must appear for ranking."""
        assert "Scoring" in doc_text or "Weight" in doc_text

    @pytest.mark.parametrize("method", [
        "Actuator-Disk", "Holtrop", "Söding", "Molland", "Hybrid",
    ])
    def test_method_in_comparison(self, doc_text, method):
        assert method.lower() in doc_text.lower(), f"Method missing from comparison: {method}"

    def test_weighted_totals(self, doc_text):
        """Weighted total scores must be present for ranking."""
        assert "Weighted total" in doc_text or "weighted total" in doc_text


# --- AC2: Primary method selected with written rationale ---

class TestAC2PrimarySelection:

    def test_primary_method_section(self, doc_text):
        assert "Primary Method Selected" in doc_text or "primary method" in doc_text.lower()

    def test_rationale_section(self, doc_text):
        assert "Rationale" in doc_text

    def test_rationale_has_numbered_points(self, doc_text):
        """Rationale should have multiple justification points."""
        rationale_match = re.search(r"### Rationale(.*?)(?=\n## |\n---|\Z)", doc_text, re.DOTALL)
        assert rationale_match, "No rationale section found"
        points = re.findall(r"^\d+\.", rationale_match.group(1), re.MULTILINE)
        assert len(points) >= 3, f"Expected >=3 rationale points, got {len(points)}"

    def test_selected_method_named(self, doc_text):
        """The selected method must be explicitly named."""
        # Söding/Brix is the expected selection based on literature
        assert "Söding" in doc_text or "Soding" in doc_text


# --- AC3: Fallback method for edge cases ---

class TestAC3FallbackMethod:

    def test_fallback_section(self, doc_text):
        assert "Fallback" in doc_text

    def test_engine_off_covered(self, doc_text):
        lower = doc_text.lower()
        assert "engine-off" in lower or "engine off" in lower or "n = 0" in lower

    def test_free_wheeling_covered(self, doc_text):
        lower = doc_text.lower()
        assert "free-wheeling" in lower or "windmill" in lower or "free wheeling" in lower

    def test_low_j_covered(self, doc_text):
        lower = doc_text.lower()
        assert "low j" in lower or "j < 0.3" in lower or "j → 0" in lower or "low-speed" in lower


# --- AC4: Known limitations and validation gaps ---

class TestAC4Limitations:

    def test_limitations_section(self, doc_text):
        assert "Limitations" in doc_text or "limitations" in doc_text

    def test_validation_gaps_section(self, doc_text):
        assert "Validation" in doc_text and "gap" in doc_text.lower()

    def test_crp_uncertainty_noted(self, doc_text):
        """C^(rp) uncertainty is the principal limitation — must be documented."""
        assert "C^(rp)" in doc_text or "C_rp" in doc_text or "interaction coefficient" in doc_text.lower()

    def test_twin_screw_limitation(self, doc_text):
        assert "twin-screw" in doc_text.lower() or "twin screw" in doc_text.lower()

    def test_stall_limitation(self, doc_text):
        assert "stall" in doc_text.lower()


# --- AC5: Output file location ---

class TestAC5OutputFile:

    def test_file_exists(self):
        assert DOC.exists(), f"Output file missing: {DOC}"

    def test_file_not_empty(self, doc_text):
        assert len(doc_text) > 500, "Document too short to be substantive"

    def test_wrk_reference(self, doc_text):
        assert "WRK-1149" in doc_text

    def test_references_literature(self, doc_text):
        """Must reference the WRK-1148 literature survey."""
        assert "WRK-1148" in doc_text or "propeller-rudder-literature" in doc_text


# --- Structural quality checks ---

class TestDocumentQuality:

    def test_decision_summary(self, doc_text):
        assert "Decision Summary" in doc_text or "decision summary" in doc_text.lower()

    def test_implementation_equations(self, doc_text):
        """Selected method's equations must be listed for implementer reference."""
        assert "F_lift" in doc_text or "F_prop" in doc_text

    def test_next_wrk_reference(self, doc_text):
        """Should point to the implementation WRK."""
        assert "WRK-1150" in doc_text or "implementation" in doc_text.lower()
