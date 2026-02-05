# WRK-031: 3-Way Benchmark Comparison (AQWA, OrcaWave, BEMRosetta)

## Summary

Build N-way solver comparison framework and benchmark AQWA, OrcaWave, and BEMRosetta using the ship spec (`spec_ship_raos.yml`). Produces pairwise statistics, consensus metrics, overlay RAO plots (Plotly HTML), and JSON benchmark reports.

## Key Decisions

- **Keep existing `DiffractionComparator` intact** — new `MultiSolverComparator` accepts `Dict[str, DiffractionResults]`
- **BEMRosetta as solver** — wraps Nemoh/HAMS via `BEMRosetta_cl.exe`; gracefully degrades when unavailable
- **All tests use synthetic data** — no real solver executables needed in CI
- **Plotly HTML only** — interactive overlay plots per project mandate
- **CLI via `add_command()`** — avoids growing `cli.py` further (already >400 lines)

## Data Flow

```
Dict[str, DiffractionResults]  (3 solver results, same vessel/frequencies/headings)
  │
  → MultiSolverComparator
  │   → PairwiseResult for each pair (AQWA-OrcaWave, AQWA-BEMRosetta, OrcaWave-BEMRosetta)
  │   → ConsensusMetrics per DOF (FULL / MAJORITY / SPLIT)
  │   → BenchmarkReport
  │
  → BenchmarkPlotter
  │   → Amplitude overlay (6-DOF subplot, all solvers)
  │   → Phase overlay
  │   → Combined overlay (6×2)
  │   → Difference plot (solver - reference)
  │   → Pairwise correlation heatmap
  │
  → BenchmarkRunner (orchestrator)
      → JSON report
      → HTML report with embedded plot links
```

## Files to Create (3 source + 3 test)

### 1. `src/.../diffraction/multi_solver_comparator.py` (~350 lines)

N-way comparison framework. Reuses `DeviationStatistics` from `comparison_framework.py`.

```python
@dataclass
class PairwiseRAOComparison:
    dof: DOF
    solver_a: str
    solver_b: str
    magnitude_stats: DeviationStatistics
    phase_stats: DeviationStatistics
    max_magnitude_diff: float
    max_phase_diff: float

@dataclass
class PairwiseResult:
    solver_a: str
    solver_b: str
    rao_comparisons: Dict[str, PairwiseRAOComparison]
    added_mass_correlations: Dict[Tuple[int,int], float]
    damping_correlations: Dict[Tuple[int,int], float]
    overall_agreement: str  # EXCELLENT, GOOD, FAIR, POOR

@dataclass
class ConsensusMetrics:
    dof: DOF
    solver_names: List[str]
    agreement_pairs: List[Tuple[str, str]]
    outlier_solver: Optional[str] = None
    consensus_level: str = "UNKNOWN"  # FULL, MAJORITY, SPLIT, NO_CONSENSUS
    mean_pairwise_correlation: float = 0.0

@dataclass
class BenchmarkReport:
    vessel_name: str
    solver_names: List[str]
    comparison_date: str
    pairwise_results: Dict[str, PairwiseResult]   # key = "AQWA-vs-OrcaWave"
    consensus_by_dof: Dict[str, ConsensusMetrics]
    overall_consensus: str
    notes: List[str]

class MultiSolverComparator:
    def __init__(self, solver_results: Dict[str, DiffractionResults], tolerance: float = 0.05) -> None
    def compare_raos(self) -> Dict[str, PairwiseRAOComparison]
    def compare_added_mass(self) -> Dict[str, Dict[Tuple[int,int], DeviationStatistics]]
    def compare_damping(self) -> Dict[str, Dict[Tuple[int,int], DeviationStatistics]]
    def compute_consensus(self) -> Dict[str, ConsensusMetrics]
    def generate_report(self) -> BenchmarkReport
    def export_report_json(self, output_file: Path) -> None
    # internal:
    def _calculate_deviation_stats(self, v1, v2, freqs) -> DeviationStatistics
    def _validate_inputs(self) -> None
    def _assess_pairwise_agreement(self, pair: PairwiseResult) -> str
    def _identify_outlier(self, correlations, solver_names) -> Optional[str]
    @staticmethod
    def _pair_key(a: str, b: str) -> str  # alphabetical: "AQWA-vs-OrcaWave"
```

Consensus levels:
- **FULL**: all solvers agree within tolerance (min pairwise correlation > 0.99)
- **MAJORITY**: 2 of 3 agree (2 pairs above threshold, 1 below)
- **SPLIT**: no clear majority
- **NO_CONSENSUS**: all pairs below threshold

### 2. `src/.../diffraction/benchmark_plotter.py` (~380 lines)

Multi-solver overlay RAO plots. Solvers distinguished by line dash style; headings by color.

```python
DOF_ORDER = [DOF.SURGE, DOF.SWAY, DOF.HEAVE, DOF.ROLL, DOF.PITCH, DOF.YAW]

_SOLVER_STYLES = {
    0: {"dash": "solid",   "color_base": "#1f77b4"},  # blue
    1: {"dash": "dash",    "color_base": "#ff7f0e"},  # orange
    2: {"dash": "dot",     "color_base": "#2ca02c"},  # green
    3: {"dash": "dashdot", "color_base": "#d62728"},  # red
}

class BenchmarkPlotter:
    def __init__(self, solver_results: Dict[str, DiffractionResults],
                 output_dir: Path, x_axis: Literal["period","frequency"] = "period") -> None
    def plot_all(self, headings: list[float] | None = None) -> list[Path]
    def plot_amplitude_overlay(self, headings: list[float] | None = None) -> Path
    def plot_phase_overlay(self, headings: list[float] | None = None) -> Path
    def plot_combined_overlay(self, headings: list[float] | None = None) -> Path
    def plot_difference(self, reference_solver: str, headings: list[float] | None = None) -> Path
    def plot_pairwise_correlation_heatmap(self, report: BenchmarkReport) -> Path
    # internal:
    def _get_x_values(self, component: RAOComponent) -> np.ndarray
    def _get_heading_indices(self, component, headings) -> list[int]
    def _get_solver_style(self, solver_idx: int) -> dict
    def _apply_layout(self, fig, title: str) -> None
    def _save_figure(self, fig, filename: str) -> Path
```

Plot details:
- Layout: 6 rows (Surge→Yaw), shared x-axis, white background
- Solver line styles: solid/dash/dot/dashdot
- X-axis: period (s) or frequency (rad/s), switchable
- Output: standalone HTML with `include_plotlyjs='cdn'`

### 3. `src/.../diffraction/benchmark_runner.py` (~350 lines)

Orchestrator: load spec → run solvers → compare → plot → report.

```python
class SolverType(str, Enum):
    AQWA = "aqwa"
    ORCAWAVE = "orcawave"
    BEMROSETTA = "bemrosetta"

class BenchmarkConfig(BaseModel):
    spec_path: Path
    solvers: list[SolverType] = [SolverType.AQWA, SolverType.ORCAWAVE, SolverType.BEMROSETTA]
    output_dir: Path = Path("benchmark_output")
    dry_run: bool = False
    tolerance: float = 0.05
    x_axis: str = "period"
    headings: list[float] | None = None
    timeout_seconds: int = 7200
    reference_solver: str | None = None
    @classmethod
    def from_yaml(cls, path: Path) -> BenchmarkConfig

@dataclass
class BenchmarkRunResult:
    config: BenchmarkConfig | None = None
    solver_results: Dict[str, DiffractionResults] = field(default_factory=dict)
    report: BenchmarkReport | None = None
    plot_paths: list[Path] = field(default_factory=list)
    report_json_path: Path | None = None
    report_html_path: Path | None = None
    error_message: str | None = None
    success: bool = False

class BenchmarkRunner:
    def __init__(self, config: BenchmarkConfig) -> None
    def run_from_spec(self, spec: DiffractionSpec) -> BenchmarkRunResult
    def run_from_results(self, solver_results: Dict[str, DiffractionResults]) -> BenchmarkRunResult
    def _run_solver(self, solver_type: SolverType, spec: DiffractionSpec) -> DiffractionResults | None
    def _compare(self, solver_results) -> BenchmarkReport
    def _generate_plots(self, solver_results, report) -> list[Path]
    def _generate_html_report(self, report, plot_paths) -> Path
    def _generate_json_report(self, report) -> Path

def run_benchmark(solver_results: Dict[str, DiffractionResults],
                  output_dir: Path = Path("benchmark_output"),
                  tolerance: float = 0.05) -> BenchmarkRunResult
```

Key: `run_from_results` is the primary API for testing. `run_from_spec` drives the full pipeline but gracefully handles unavailable solvers.

### 4–6. Test Files

| # | File | Tests | Description |
|---|------|-------|-------------|
| 4 | `test_multi_solver_comparator.py` | ~20 | Init validation, pairwise stats, consensus levels, outlier detection, report generation, JSON export |
| 5 | `test_benchmark_plotter.py` | ~12 | All plot types create HTML, legend contains solver names, heading filtering, plot_all returns multiple paths |
| 6 | `test_benchmark_runner.py` | ~10 | Config defaults, run_from_results produces report/plots/JSON, dry_run mode, convenience function |

Test fixtures in conftest.py:
```python
def _make_solver_results(solver_name, seed_offset=0, magnitude_scale=1.0) -> DiffractionResults
    """Synthetic results with controllable differences."""

@pytest.fixture
def three_solver_results() -> Dict[str, DiffractionResults]:
    """AQWA (baseline), OrcaWave (close match), BEMRosetta (slight heave bias)."""
```

## Files to Modify (3 files)

### 7. `__init__.py` — Register WRK-031 exports

```python
# WRK-031: Multi-solver benchmark
from digitalmodel.hydrodynamics.diffraction.multi_solver_comparator import (
    MultiSolverComparator, BenchmarkReport, PairwiseResult, ConsensusMetrics,
)
from digitalmodel.hydrodynamics.diffraction.benchmark_plotter import BenchmarkPlotter
from digitalmodel.hydrodynamics.diffraction.benchmark_runner import (
    BenchmarkRunner, BenchmarkConfig, BenchmarkRunResult, SolverType, run_benchmark,
)
```

### 8. `cli.py` — Add `benchmark-solvers` command via `add_command()` pattern

The CLI is already >400 lines. The benchmark command is defined in `benchmark_runner.py` as a standalone Click group and registered via `cli.add_command()`.

```python
# In benchmark_runner.py:
@click.command("benchmark-solvers")
@click.argument("spec_path", type=click.Path(exists=True))
@click.option("--solvers", "-s", multiple=True, default=["aqwa","orcawave","bemrosetta"])
@click.option("--output", "-o", type=click.Path(), default="benchmark_output")
@click.option("--dry-run", is_flag=True)
@click.option("--tolerance", "-t", type=float, default=0.05)
@click.option("--x-axis", type=click.Choice(["period","frequency"]), default="period")
@click.option("--headings", type=str, default=None)
@click.option("--reference", type=str, default=None)
def benchmark_solvers_cmd(spec_path, solvers, output, dry_run, tolerance, x_axis, headings, reference): ...

# In cli.py:
from digitalmodel.hydrodynamics.diffraction.benchmark_runner import benchmark_solvers_cmd
cli.add_command(benchmark_solvers_cmd)
```

### 9. `conftest.py` — Add benchmark fixtures (~40 lines)

## TDD Implementation Order

| Step | Phase | Action | Files |
|------|-------|--------|-------|
| 1 | Red | Write benchmark fixtures in conftest | conftest.py |
| 2 | Red | Write MultiSolverComparator tests | test_multi_solver_comparator.py |
| 3 | Green | Implement MultiSolverComparator | multi_solver_comparator.py |
| 4 | Red | Write BenchmarkPlotter tests | test_benchmark_plotter.py |
| 5 | Green | Implement BenchmarkPlotter | benchmark_plotter.py |
| 6 | Red | Write BenchmarkRunner tests | test_benchmark_runner.py |
| 7 | Green | Implement BenchmarkRunner | benchmark_runner.py |
| 8 | — | Add CLI command | benchmark_runner.py + cli.py |
| 9 | — | Register exports | __init__.py |

## Verification

```bash
# New tests
uv run pytest tests/hydrodynamics/diffraction/test_multi_solver_comparator.py -v
uv run pytest tests/hydrodynamics/diffraction/test_benchmark_plotter.py -v
uv run pytest tests/hydrodynamics/diffraction/test_benchmark_runner.py -v

# Regression
uv run pytest tests/hydrodynamics/diffraction/ -v

# CLI dry-run
uv run python -m digitalmodel.hydrodynamics.diffraction.cli benchmark-solvers \
  tests/hydrodynamics/diffraction/fixtures/spec_ship_raos.yml --dry-run
```

## Acceptance Criteria Mapping

| Criterion | Delivered By |
|-----------|-------------|
| Select hull form for benchmark | Ship spec (`spec_ship_raos.yml`) |
| Run solvers with equivalent inputs | `BenchmarkRunner.run_from_spec()` |
| Compare RAOs across all 6 DOFs | `MultiSolverComparator.compare_raos()` |
| Compare added mass and damping | `compare_added_mass()` / `compare_damping()` |
| Generate overlay plots | `BenchmarkPlotter.plot_all()` |
| 3-way consensus analysis | `ConsensusMetrics` (FULL/MAJORITY/SPLIT) |
| Summary report with discrepancy analysis | `BenchmarkReport` + JSON + HTML export |
| Pairwise statistics for all 3 pairs | `PairwiseResult` for each combination |

## Phase Status

### Phase 1: Framework ✅ COMPLETE
- `MultiSolverComparator`, `BenchmarkPlotter`, `BenchmarkRunner` implemented
- 43 tests passing
- Commit: `bacd8c1d`

### Phase 2: Trial Run ✅ COMPLETE
- Script: `scripts/run_benchmark_ship_raos.py`
- Ran with existing L01_aqwa_benchmark CSV data
- Result: **NO_CONSENSUS** (expected — source data had mismatched parameters)
- Output: `docs/modules/orcawave/L01_aqwa_benchmark/benchmark_results/`

### Phase 3: Proper 3-Way Benchmark 🔄 PENDING

**Available Hull Geometries (all exist, no coarsening needed):**

| Hull | File | Panels | Dimensions | Symmetry |
|------|------|--------|------------|----------|
| **Unit Box** | `tests/hydrodynamics/bemrosetta/fixtures/sample_box.gdf` | 5 | 1m × 1m × 1m | None |
| **Barge** | `specs/modules/orcawave/test-configs/geometry/barge.gdf` | 912 | 100m × 20m × 8m | Y-sym |
| **Spar** | `specs/modules/orcawave/test-configs/geometry/spar.gdf` | 1512 | R=10m, T=50m | None |
| **Cylinder** | `docs/.../L00_validation_wamit/3.3/.../Cylinder.gdf` | 420 | R=1m (unit) | X+Y sym |

**Solver Requirements:**

| Solver | Executable | License | Status |
|--------|------------|---------|--------|
| AQWA | `aqwa_solve` | ANSYS license | Need to verify |
| OrcaWave | via OrcFxAPI | Orcina license | Need to verify |
| BEMRosetta | `BEMRosetta_cl.exe` | Open source (GPL) | Need to install |

**Next Steps for Phase 3:**
1. Verify solver installations on target machine
2. Create unified DiffractionSpec for each hull with identical parameters:
   - Same water depth (e.g., 500m or infinite)
   - Same frequency range (e.g., 0.2–1.5 rad/s)
   - Same heading range (e.g., 0°, 45°, 90°, 135°, 180°)
3. Run benchmark for **Unit Box** first (fastest, good for validation)
4. Run benchmark for **Barge** (practical offshore case)
5. Run benchmark for **Spar/Cylinder** (axisymmetric case)
6. Document solver-specific settings and quirks

## Not in Scope

- Modifying existing `DiffractionComparator` (preserved for backward compat)
- Nemoh/HAMS input generation backend (future WRK item)
- BEMRosetta solver adapter beyond executable wrapper
- Wave excitation force comparison (not in unified schema yet)
- Multi-hull benchmark (ship spec only per user request)
