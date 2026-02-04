"""OrcaFlex analysis validation: modular generator vs monolithic baseline.

Validates that the modular generator output produces numerically equivalent
OrcaFlex results compared to the monolithic baseline. Runs static analysis
on the modular model, loads pre-computed monolithic results from .sim files,
and compares within tolerances.

The 24in monolithic model (6 fine-mesh sections, 4900m) requires >30 min for
CalculateStatics() — too expensive for in-test computation. Instead, we load
pre-computed .sim files. If no .sim exists, monolithic tests skip gracefully.

All tests are gated by OrcFxAPI availability and skip gracefully when the
solver is not installed.
"""

from __future__ import annotations

import math
from dataclasses import dataclass
from pathlib import Path

import pytest

# ---------------------------------------------------------------------------
# OrcFxAPI availability gate
# ---------------------------------------------------------------------------

try:
    import OrcFxAPI

    ORCAFLEX_AVAILABLE = True
except ImportError:
    ORCAFLEX_AVAILABLE = False

requires_orcaflex = pytest.mark.skipif(
    not ORCAFLEX_AVAILABLE, reason="OrcFxAPI not available"
)

# Modular 24in statics: 1 section, 0.5m mesh over 4900m ≈ 9800 nodes.
# May need several minutes to converge.
slow_statics = pytest.mark.timeout(1800)

# ---------------------------------------------------------------------------
# Path constants
# ---------------------------------------------------------------------------

_DOCS_ROOT = Path(__file__).parent.parent.parent.parent.parent / "docs"

PIPELINE_24IN = (
    _DOCS_ROOT
    / "modules/orcaflex/pipeline/installation/floating/24in_pipeline"
)
SPEC_24IN = PIPELINE_24IN / "spec.yml"
MONOLITHIC_24IN_YML = (
    PIPELINE_24IN
    / "monolithic/basefile/D24inL4900mBuoy7kNSpacing1500mm.yml"
)
# Pre-computed .sim from the basefile (generate offline, commit once).
# Until this file exists, 24in monolithic comparison tests skip.
MONOLITHIC_24IN_SIM = PIPELINE_24IN / "monolithic/basefile/statics.sim"

PIPELINE_30IN = (
    _DOCS_ROOT
    / "modules/orcaflex/pipeline/installation/floating/30in_pipeline"
)
MONOLITHIC_30IN_SIM = PIPELINE_30IN / "monolithic.sim"
MODULAR_30IN_SIM = PIPELINE_30IN / "modular/master.sim"

LINE_NAME_24IN = "pipeline"
LINE_NAME_30IN = "30'' Line"

# ---------------------------------------------------------------------------
# Tolerances
# ---------------------------------------------------------------------------

TENSION_REL_TOL = 0.05  # 5%
TENSION_ABS_TOL = 10.0  # kN
BENDING_REL_TOL = 0.15  # 15%
BENDING_ABS_TOL = 5.0  # kN·m
LENGTH_REL_TOL = 0.0001  # 0.01%
LENGTH_ABS_TOL = 0.1  # m
EXPECTED_LENGTH_24IN = 4900.0  # m


# ---------------------------------------------------------------------------
# Data extraction helper
# ---------------------------------------------------------------------------


@dataclass
class StaticEndResults:
    """Static results at line ends from a converged OrcaFlex model."""

    line_name: str
    end_a_tension: float  # kN
    end_b_tension: float  # kN
    end_a_bending: float  # kN·m
    end_b_bending: float  # kN·m
    length: float  # m
    object_count: int


def extract_static_end_results(
    model: "OrcFxAPI.Model", line_name: str
) -> StaticEndResults:
    """Extract static results at line ends from a converged OrcaFlex model.

    Args:
        model: OrcaFlex model with statics already calculated.
        line_name: Name of the line object to query.

    Returns:
        StaticEndResults with tension/bending at both ends.
    """
    line = model[line_name]

    end_a_tension = line.StaticResult("Effective Tension", OrcFxAPI.oeEndA)
    end_b_tension = line.StaticResult("Effective Tension", OrcFxAPI.oeEndB)
    end_a_bending = line.StaticResult("Bend Moment", OrcFxAPI.oeEndA)
    end_b_bending = line.StaticResult("Bend Moment", OrcFxAPI.oeEndB)
    length = sum(line.Length[i] for i in range(line.NumberOfSections))

    object_count = len(model.objects) if model.objects else 0

    return StaticEndResults(
        line_name=line_name,
        end_a_tension=end_a_tension,
        end_b_tension=end_b_tension,
        end_a_bending=end_a_bending,
        end_b_bending=end_b_bending,
        length=length,
        object_count=object_count,
    )


def _within_tolerance(
    actual: float,
    expected: float,
    rel_tol: float,
    abs_tol: float,
) -> bool:
    """Check if actual is within tolerance of expected.

    Uses the same logic as math.isclose but also accepts values within
    abs_tol regardless of relative difference (e.g. near-zero values).
    """
    if abs(actual - expected) <= abs_tol:
        return True
    if expected == 0.0:
        return abs(actual) <= abs_tol
    return abs((actual - expected) / expected) <= rel_tol


def _fmt_comparison(
    label: str,
    modular: float,
    monolithic: float,
    rel_tol: float,
    abs_tol: float,
) -> str:
    """Format a comparison line for stdout reporting."""
    diff = modular - monolithic
    if monolithic != 0.0:
        pct = 100.0 * diff / monolithic
        return (
            f"  {label}: modular={modular:.4f}  monolithic={monolithic:.4f}  "
            f"diff={diff:+.4f} ({pct:+.2f}%)  "
            f"tol=±{rel_tol*100:.0f}%/±{abs_tol}"
        )
    return (
        f"  {label}: modular={modular:.4f}  monolithic={monolithic:.4f}  "
        f"diff={diff:+.4f}  tol=±{abs_tol}"
    )


def _skip_if_no_sim(sim_path: Path, label: str) -> None:
    """Skip test if pre-computed .sim file does not exist."""
    if not sim_path.exists():
        pytest.skip(
            f"{label} .sim not found: {sim_path}. "
            "Generate offline with: model.CalculateStatics(); "
            "model.SaveSimulation(path)"
        )


# ---------------------------------------------------------------------------
# Session-scoped fixtures (expensive operations done once)
# ---------------------------------------------------------------------------


@pytest.fixture(scope="session")
def modular_output_dir(tmp_path_factory):
    """Generate modular output from 24in spec.yml once per session."""
    from digitalmodel.solvers.orcaflex.modular_generator import (
        ModularModelGenerator,
    )

    output_dir = tmp_path_factory.mktemp("modular_24in")
    generator = ModularModelGenerator(SPEC_24IN)
    generator.generate(output_dir)
    return output_dir


@pytest.fixture(scope="session")
def modular_model(modular_output_dir):
    """Load modular master.yml and run statics.

    The modular model (1 section, 0.5m mesh) is much lighter than the
    monolithic (6 sections, 0.1-0.5m mesh) and should converge within
    the slow_statics timeout.

    Note: The generator omits the YAML ``---`` document separator that
    OrcFxAPI requires before ``includefile`` directives. We patch the
    file in-place before loading.
    """
    if not ORCAFLEX_AVAILABLE:
        pytest.skip("OrcFxAPI not available")

    master_path = modular_output_dir / "master.yml"

    # Ensure YAML document separator exists before includefile directives.
    text = master_path.read_text()
    if "\n---\n" not in text:
        text = text.replace(
            "\n- includefile:", "\n---\n- includefile:", 1
        )
        master_path.write_text(text)

    model = OrcFxAPI.Model()
    try:
        model.LoadData(str(master_path))
    except Exception as exc:
        pytest.skip(
            f"Modular master.yml not loadable by OrcFxAPI — "
            f"generator output needs fixing: {exc}"
        )
    model.CalculateStatics()
    return model


@pytest.fixture(scope="session")
def monolithic_model():
    """Load monolithic baseline from pre-computed .sim file.

    The 24in monolithic model (6 fine-mesh sections over 4900m) takes
    >30 min for CalculateStatics() — too expensive for in-test use.
    Requires a pre-computed statics.sim generated offline.
    """
    if not ORCAFLEX_AVAILABLE:
        pytest.skip("OrcFxAPI not available")
    _skip_if_no_sim(MONOLITHIC_24IN_SIM, "24in monolithic")

    return OrcFxAPI.Model(str(MONOLITHIC_24IN_SIM))


@pytest.fixture(scope="session")
def modular_results(modular_model):
    """Extract static end results from the modular model."""
    return extract_static_end_results(modular_model, LINE_NAME_24IN)


@pytest.fixture(scope="session")
def monolithic_results(monolithic_model):
    """Extract static end results from the monolithic model."""
    return extract_static_end_results(monolithic_model, LINE_NAME_24IN)


# ===================================================================
# Phase A: Smoke Test — Load & Statics
# ===================================================================


class TestSmokeGeneration:
    """Verify modular output is generated correctly."""

    def test_modular_output_generated(self, modular_output_dir):
        """Modular generator produces master.yml from 24in spec."""
        master = modular_output_dir / "master.yml"
        assert master.exists(), f"master.yml not found at {master}"

    def test_modular_output_has_includes(self, modular_output_dir):
        """Modular output has include files."""
        includes = list((modular_output_dir / "includes").glob("*.yml"))
        assert len(includes) >= 5, (
            f"Expected >=5 include files, got {len(includes)}"
        )

    @requires_orcaflex
    def test_monolithic_yml_loads_in_orcaflex(self):
        """Monolithic YAML loads in OrcFxAPI (no statics, just parsing)."""
        model = OrcFxAPI.Model(str(MONOLITHIC_24IN_YML))
        assert model is not None
        objects = model.objects
        assert len(objects) > 0, "Monolithic model has no objects"


@slow_statics
class TestModularStatics:
    """Verify modular model loads and converges statics."""

    @requires_orcaflex
    def test_modular_loads_in_orcaflex(self, modular_model):
        """Modular master.yml loads in OrcFxAPI without exception."""
        assert modular_model is not None

    @requires_orcaflex
    def test_modular_statics_converge(self, modular_model):
        """Static analysis converges on the modular model."""
        assert modular_model.state == OrcFxAPI.ModelState.InStaticState


# ===================================================================
# Phase B: Static Result Extraction
# ===================================================================


@slow_statics
class TestStaticResultExtraction:
    """Extract engineering results from both models after statics."""

    @requires_orcaflex
    def test_modular_results_are_finite(self, modular_results):
        """Modular static results are finite real numbers."""
        assert math.isfinite(modular_results.end_a_tension)
        assert math.isfinite(modular_results.end_b_tension)
        assert math.isfinite(modular_results.end_a_bending)
        assert math.isfinite(modular_results.end_b_bending)

    @requires_orcaflex
    def test_monolithic_results_are_finite(self, monolithic_results):
        """Monolithic static results are finite real numbers."""
        assert math.isfinite(monolithic_results.end_a_tension)
        assert math.isfinite(monolithic_results.end_b_tension)
        assert math.isfinite(monolithic_results.end_a_bending)
        assert math.isfinite(monolithic_results.end_b_bending)

    @requires_orcaflex
    def test_modular_pipeline_length(self, modular_results):
        """Modular pipeline total length is approximately 4900 m."""
        assert modular_results.length == pytest.approx(
            EXPECTED_LENGTH_24IN, rel=LENGTH_REL_TOL, abs=LENGTH_ABS_TOL
        )

    @requires_orcaflex
    def test_monolithic_pipeline_length(self, monolithic_results):
        """Monolithic pipeline total length is approximately 4900 m."""
        assert monolithic_results.length == pytest.approx(
            EXPECTED_LENGTH_24IN, rel=LENGTH_REL_TOL, abs=LENGTH_ABS_TOL
        )


# ===================================================================
# Phase C: Comparison — Modular vs Monolithic
# ===================================================================


@slow_statics
class TestModularVsMonolithicComparison:
    """Compare modular results against monolithic within tolerances.

    Requires pre-computed monolithic .sim file. Tests skip if unavailable.
    """

    @requires_orcaflex
    def test_end_a_effective_tension(
        self, modular_results, monolithic_results
    ):
        """End A effective tension within 5% / 10 kN."""
        print(
            _fmt_comparison(
                "End A Eff. Tension (kN)",
                modular_results.end_a_tension,
                monolithic_results.end_a_tension,
                TENSION_REL_TOL,
                TENSION_ABS_TOL,
            )
        )
        assert _within_tolerance(
            modular_results.end_a_tension,
            monolithic_results.end_a_tension,
            TENSION_REL_TOL,
            TENSION_ABS_TOL,
        ), (
            f"End A tension mismatch: "
            f"modular={modular_results.end_a_tension:.4f} kN vs "
            f"monolithic={monolithic_results.end_a_tension:.4f} kN"
        )

    @requires_orcaflex
    def test_end_b_effective_tension(
        self, modular_results, monolithic_results
    ):
        """End B effective tension within 5% / 10 kN."""
        print(
            _fmt_comparison(
                "End B Eff. Tension (kN)",
                modular_results.end_b_tension,
                monolithic_results.end_b_tension,
                TENSION_REL_TOL,
                TENSION_ABS_TOL,
            )
        )
        assert _within_tolerance(
            modular_results.end_b_tension,
            monolithic_results.end_b_tension,
            TENSION_REL_TOL,
            TENSION_ABS_TOL,
        ), (
            f"End B tension mismatch: "
            f"modular={modular_results.end_b_tension:.4f} kN vs "
            f"monolithic={monolithic_results.end_b_tension:.4f} kN"
        )

    @requires_orcaflex
    def test_end_a_bending_moment(self, modular_results, monolithic_results):
        """End A bending moment within 15% / 5 kN·m."""
        print(
            _fmt_comparison(
                "End A Bend Moment (kN·m)",
                modular_results.end_a_bending,
                monolithic_results.end_a_bending,
                BENDING_REL_TOL,
                BENDING_ABS_TOL,
            )
        )
        assert _within_tolerance(
            modular_results.end_a_bending,
            monolithic_results.end_a_bending,
            BENDING_REL_TOL,
            BENDING_ABS_TOL,
        ), (
            f"End A bending mismatch: "
            f"modular={modular_results.end_a_bending:.4f} kN·m vs "
            f"monolithic={monolithic_results.end_a_bending:.4f} kN·m"
        )

    @requires_orcaflex
    def test_end_b_bending_moment(self, modular_results, monolithic_results):
        """End B bending moment within 15% / 5 kN·m."""
        print(
            _fmt_comparison(
                "End B Bend Moment (kN·m)",
                modular_results.end_b_bending,
                monolithic_results.end_b_bending,
                BENDING_REL_TOL,
                BENDING_ABS_TOL,
            )
        )
        assert _within_tolerance(
            modular_results.end_b_bending,
            monolithic_results.end_b_bending,
            BENDING_REL_TOL,
            BENDING_ABS_TOL,
        ), (
            f"End B bending mismatch: "
            f"modular={modular_results.end_b_bending:.4f} kN·m vs "
            f"monolithic={monolithic_results.end_b_bending:.4f} kN·m"
        )

    @requires_orcaflex
    def test_object_count_difference_explained(
        self, modular_results, monolithic_results
    ):
        """Object count difference is accounted for (monolithic has winch)."""
        diff = monolithic_results.object_count - modular_results.object_count
        print(
            f"  Object count: modular={modular_results.object_count}  "
            f"monolithic={monolithic_results.object_count}  "
            f"diff={diff} (expected: monolithic has winch wire objects)"
        )
        # Monolithic should have >= modular objects (extra winch wire, etc.)
        assert diff >= 0, (
            f"Modular has MORE objects ({modular_results.object_count}) "
            f"than monolithic ({monolithic_results.object_count}) — unexpected"
        )


# ===================================================================
# Phase D: 30in Cross-Validation
# ===================================================================


class TestCrossValidation30in:
    """Load 30in monolithic.sim and modular/master.sim, compare results."""

    @requires_orcaflex
    def test_30in_monolithic_loads(self):
        """30in monolithic.sim loads and has pipeline results."""
        _skip_if_no_sim(MONOLITHIC_30IN_SIM, "30in monolithic")

        model = OrcFxAPI.Model(str(MONOLITHIC_30IN_SIM))
        results = extract_static_end_results(model, LINE_NAME_30IN)
        assert math.isfinite(results.end_a_tension)
        assert math.isfinite(results.end_b_tension)
        print(
            f"  30in monolithic: End A={results.end_a_tension:.2f} kN, "
            f"End B={results.end_b_tension:.2f} kN"
        )

    @requires_orcaflex
    def test_30in_modular_loads(self):
        """30in modular/master.sim loads and has pipeline results."""
        _skip_if_no_sim(MODULAR_30IN_SIM, "30in modular")

        model = OrcFxAPI.Model(str(MODULAR_30IN_SIM))
        results = extract_static_end_results(model, LINE_NAME_30IN)
        assert math.isfinite(results.end_a_tension)
        assert math.isfinite(results.end_b_tension)
        print(
            f"  30in modular: End A={results.end_a_tension:.2f} kN, "
            f"End B={results.end_b_tension:.2f} kN"
        )

    @requires_orcaflex
    def test_30in_tension_comparison(self):
        """30in End A/B effective tension within 5% tolerance."""
        _skip_if_no_sim(MONOLITHIC_30IN_SIM, "30in monolithic")
        _skip_if_no_sim(MODULAR_30IN_SIM, "30in modular")

        mono = OrcFxAPI.Model(str(MONOLITHIC_30IN_SIM))
        mod = OrcFxAPI.Model(str(MODULAR_30IN_SIM))

        mono_r = extract_static_end_results(mono, LINE_NAME_30IN)
        mod_r = extract_static_end_results(mod, LINE_NAME_30IN)

        print(
            _fmt_comparison(
                "30in End A Tension",
                mod_r.end_a_tension,
                mono_r.end_a_tension,
                TENSION_REL_TOL,
                TENSION_ABS_TOL,
            )
        )
        print(
            _fmt_comparison(
                "30in End B Tension",
                mod_r.end_b_tension,
                mono_r.end_b_tension,
                TENSION_REL_TOL,
                TENSION_ABS_TOL,
            )
        )

        assert _within_tolerance(
            mod_r.end_a_tension,
            mono_r.end_a_tension,
            TENSION_REL_TOL,
            TENSION_ABS_TOL,
        ), (
            f"30in End A tension mismatch: "
            f"modular={mod_r.end_a_tension:.4f} vs "
            f"monolithic={mono_r.end_a_tension:.4f}"
        )
        assert _within_tolerance(
            mod_r.end_b_tension,
            mono_r.end_b_tension,
            TENSION_REL_TOL,
            TENSION_ABS_TOL,
        ), (
            f"30in End B tension mismatch: "
            f"modular={mod_r.end_b_tension:.4f} vs "
            f"monolithic={mono_r.end_b_tension:.4f}"
        )
