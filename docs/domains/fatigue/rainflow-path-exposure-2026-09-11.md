# Rainflow counting paths — behaviour record and exposure assessment

**Issue:** https://github.com/vamseeachanta/workspace-hub/issues/3839
**Plan:** workspace-hub `docs/plans/2026-09-11-issue-3839-rainflow-divergence.md`
**Measured:** 2026-09-11, `digitalmodel` at `90bcff85`
**Gate:** `src/digitalmodel/fatigue/counting_contract.py`, `tests/fatigue/test_rainflow_invariants.py`

This record exists so the evidence the deferral argument rests on survives the
session. Consolidation onto one counting implementation is deferred to a
successor issue; retiring an implementation before this record existed would
have removed the evidence needed to judge what the retired implementation
produced.

---

## 1. Measured behaviour

Each path was invoked through its public entry point on four deterministic
signals. The invariant is that a retain-residual cycle count extracts a maximum
range equal to the signal's peak-to-valley span, since the largest excursion is
necessarily a reversal pair and therefore appears as a full or half cycle.

| Signal | Definition |
|---|---|
| `astm_example` | `[-2, 1, -3, 5, -1, 3, -4, 4, -2]` |
| `narrow_band_sine` | `50·sin(t)`, `t = linspace(0, 10·2π, 640, endpoint=False)` |
| `broadband_random` | `default_rng(20260911).normal(0, 25, 4096)` |
| `residual_dominated` | `linspace(0, 200, 512) + 8·sin(linspace(0, 40π, 512))` |

Maximum extracted range per path; `FAIL` marks a maximum below the span.

| signal | span | pylife_4pt | pypi_rainflow | sigproc_astm | calm_buoy | fapps_counting | fapps_counter | struct_fatigue |
|---|---|---|---|---|---|---|---|---|
| astm_example | 9.0000 | PASS | PASS | FAIL 6.0 | FAIL 7.0 | PASS | PASS | PASS |
| narrow_band_sine | 100.0000 | PASS | PASS | PASS | FAIL 50.0 | PASS | PASS | PASS |
| broadband_random | 177.1224 | PASS | PASS | FAIL 125.2664 | FAIL 106.3779 | FAIL 169.556 | PASS | FAIL 169.556 |
| residual_dominated | 201.2606 | PASS | PASS | FAIL 190.6564 | FAIL 200.6303 | FAIL 200.6303 | PASS | FAIL 200.6303 |

Table 1 — maximum extracted range against the peak-to-valley span, by path and signal.

Table 1 reports the maximum-range invariant alone. Applying the whole contract —
clause 1 (span extracted), clause 2 (nothing counted the signal does not
contain), clause 3 (exact count conservation, positive half-integer counts, no
zero-range row) — gives the verdict matrix below, which is the form committed as
`RECORDED_VERDICTS` in `tests/fatigue/test_rainflow_invariants.py` so that any
movement in a cell fails a test rather than going stale.

| signal | pylife_4pt | pypi_rainflow | sigproc_astm | calm_buoy | fapps_counting | fapps_counter | struct_fatigue |
|---|---|---|---|---|---|---|---|
| astm_example | PASS | PASS | FAIL c1 | FAIL c1, c3 | PASS | PASS | PASS |
| narrow_band_sine | PASS | PASS | FAIL c3 | FAIL c1 | PASS | PASS | PASS |
| broadband_random | PASS | PASS | FAIL c1 | FAIL c1 | FAIL c1 | PASS | FAIL c1 |
| residual_dominated | PASS | PASS | FAIL c1 | FAIL c1 | FAIL c1 | PASS | FAIL c1 |

Table 2 — contract verdict by path and signal; `c1`, `c3` name the failing clause.

Two cells diverge in **count** as well as in range, correcting the claim in the
issue body that total counted cycles agree across all seven paths on every
signal:

- `sigproc_astm` on `narrow_band_sine` extracts the correct maximum range of
  100.0 and would pass Table 1, but emits only 6.0 cycles of non-zero range
  against the 10.5 the reversal count requires, with the remaining 4.5 emitted
  as **zero-range rows**. Equal-valued extrema become adjacent on the stack
  after the implementation pops the wrong element, and the pair is counted at
  range zero. A zero-range cycle carries no damage, so 43 % of the counted
  cycles on this signal contribute nothing.
- `calm_buoy` on `astm_example` returns 3.5 cycles against 4.0 (§3.2).

On the remaining twenty-six cells the totals do agree (4.0, 10.5, 1370.0, 20.5),
so outside those two the divergence is confined to the ranges assigned.

Fatigue damage under a Miner summation with an S-N slope of 3 scales as range
cubed. A maximum range understated by 29 % (`sigproc_astm`, broadband)
understates that cycle's damage contribution by a factor of 2.8; understated by
40 % (`calm_buoy`, broadband) by a factor of 4.6. The direction is
non-conservative in every failing case.

**Standards-evidence limitation.** The contract is scoped to this project's
declared convention, not to a standard. ASTM E1049-85(2023) is paywalled and was
not read, so no clause is presented as a standards requirement.

---

## 2. Conditionality of the invariant

The invariant is **not** universal. An implementation that discards residual
turning points rather than counting them as half cycles can correctly omit the
global span. The contract therefore states clause 1 against a **declared**
residual policy, and `tests/fatigue/test_rainflow_invariants.py::test_10_*`
proves the contract accepts a declared discard-residual counter that omits the
span.

Every path in Table 1 declares `RETAIN`, and the evidence is that total counts
agree across all seven and end in `.5` (10.5, 20.5). The divergence is not a
convention difference.

---

## 3. Per-path verdict, mechanism, consumers and refusal state

Consumer inventory is call-graph derived, not import-scan derived: an import
scan over `src/` alone misses documentation examples, standalone scripts, and
modules reachable only through a public entry point.

### 3.1 `sigproc_astm` — NON-CONSERVATIVE

`src/digitalmodel/signal_processing/signal_analysis/core/rainflow.py:16`
`RainflowCounter.count_cycles`

**Mechanism, read from source.** `core/rainflow.py:162-164` reads
`if range_XY >= range_YZ: cycle_range = range_YZ`. Both halves are inverted
relative to ASTM §5.4.4: the rule fires when the *newer* range is the larger,
and it extracts the *older* (inner) pair. This implementation fires when the
older range is the larger and extracts the newer. It also draws no distinction
between the starting-point case and the interior case — every extraction emits
`0.5` and discards `stack[-2]` (`:168-176`), so ASTM's full-cycle rule is never
applied, and the residual loop (`:181-195`) emits every remaining adjacent pair
as a half cycle.

**Limit of this reading.** Correcting the inversion alone does not restore the
span: a controlled pair over the ASTM fixture that flips only the comparison
and the extracted range still reports a maximum of 7.0 against a span of 9.0.
The defect is therefore structural rather than a sign error, and the specific
arithmetic that produces 6.0 on the fixture and 125.2664 on broadband is **not
established** beyond the source reading above. Establishing it is not required
to act: the measured shortfall in Table 1 is the finding, and the repair is
governed by the contract regardless of which line is changed.

**Second failure mode.** On `narrow_band_sine` this path extracts the correct
maximum range and fails clause 3 instead: 6.0 cycles of non-zero range against
the 10.5 the reversal count requires, with 4.5 emitted at range zero. Popping
`stack[-2]` makes two equal-valued extrema adjacent, and the resulting pair is
counted at range zero, so it carries no damage. A maximum-range check alone
does not see this.

| Consumer | Kind | Verdict |
|---|---|---|
| `signal_analysis/orcaflex/analyzer.py:16,46` | src | diagnostic — statistics, histogram, CSV, plots; no S-N in the file |
| `signal_analysis/cli.py:14,48` | src | diagnostic |
| `signal_analysis/adapters.py:14,39,69,284,312,367,396` | src | diagnostic |
| `signal_analysis/orcaflex/error_handling.py:191` | src | diagnostic |
| `signal_analysis/orcaflex/__main__.py:27,388` | src | diagnostic |
| `solvers/orcaflex/time_trace_processor.py:253,266` | src | **damage** — `FatigueDamageCalculator(method='miners')`, `damage_per_hour`, `damage_design_life`, `life_years` |
| `solvers/orcaflex/time_trace_processor.py:435,438` | src | **damage** — parallel twin of the above |
| `solvers/orcaflex/opp_time_series_v2.py:253,259` | src | diagnostic — histogram and statistics |
| `solvers/orcaflex/opp_time_series_v2.py:159-200` | src | **damage**, indirect — delegates to `OrcaFlexTimeTraceProcessor.process()` |
| `tests/signal_processing/signal_analysis/test_signal_integration.py:13,58,304` | test | damage, but constructs counter and damage calculator directly |
| `tests/signal_processing/signal_analysis/test_integration_simple.py:30` | test | damage, direct construction |
| `tests/signal_processing/signal_analysis/test_generate_outputs.py:23,159` | test | damage, direct construction |
| `tests/signal_processing/signal_analysis/test_orcaflex_tension_analysis.py:26,39` | test | diagnostic |
| `docs/domains/examples/signal_analysis_usage_example.py:22,41,213` | docs | diagnostic and damage |
| `docs/domains/examples/orcaflex_signal_analysis_example.py:21,104,178,330` | docs | diagnostic and damage, indirect |
| `scripts/python/digitalmodel/tools/process_fatigue_simplified_rainflow.py:16,77` | script | diagnostic; import path stale (see §5) |

**Refusal applied at:**
`OrcaFlexTimeTraceProcessor._perform_fatigue_analysis` and
`OrcaFlexTimeTraceProcessor._analyze_single_trace`. Blocking those two blocks
`OPPTimeSeriesV2.process_fatigue_analysis` and the documentation examples that
route through them.

`FatigueDamageCalculator.calculate_damage`
(`signal_analysis/fatigue/damage.py:15`) is **not** blocked: it never
constructs a counter, always consuming a cycles DataFrame, so it is not bound
to this path and blocking it would be over-broad.

The engine's live OrcaFlex post-processing route is unaffected.
`solvers/orcaflex/opp_time_series.py:458` calls
`OrcFXAPIObject.RainflowHalfCycles(...)` — OrcaFlex's own rainflow — and
consumes none of the seven paths. The blast radius asserted in the first
comment on #3839 was published from imports without tracing reachability and
was corrected; that correction is the reason this record is call-graph derived.

### 3.2 `calm_buoy` — NON-CONSERVATIVE

`src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py:196`
`RainflowFatigue.count_cycles`

**Mechanism, read from source, confirmed by a controlled pair.**
`calm_buoy_fatigue.py:262-270` makes the three-point comparison correctly
(`if x_range >= y_range: cycles.append((y_range, …))`) and correctly counts the
starting-point case as a half cycle, which is why its failure is milder and
differently shaped than `sigproc_astm`'s. Two lines are wrong:

- `:267` `stack.pop(-2)` discards the **second** point of the counted pair.
  ASTM rule 2 discards the **first** — the starting point — and retains the
  second, which then becomes the new start.
- `:268` `break` exits the extraction loop instead of re-checking the shortened
  stack.

Controlled pair, holding the reversal extraction and everything else fixed and
changing only those two lines to `stack.pop(0)` with no `break`:

| signal | span | as committed | rule 2 restored |
|---|---|---|---|
| `narrow_band_sine` | 100.0 | max 50.0, total 10.5 | max 100.0, total 10.5 |
| `astm_example` | 9.0 | max 7.0, total 3.5 | max 9.0, total 4.0 |

Table 3 — `calm_buoy` maximum extracted range and total counts, as committed
against ASTM rule 2 restored.

The as-committed column reproduces Table 1 exactly, so the reimplementation
used for the comparison is faithful. Restoring rule 2 recovers both the span
and the lost half cycle.

**Additional finding, not in the issue as filed.** On `astm_example` this path
also loses a half cycle: total 3.5 against the 4.0 that the other six report.
The issue body's claim that total counts agree across all seven on every signal
therefore does not hold for this pair. The divergence is in both range and
count, not range alone.

| Consumer | Kind | Verdict |
|---|---|---|
| `calm_buoy_fatigue.py:370,415` `ScatterDiagramFatigue` | src | **damage** — probability-weighted Miner damage per line |
| `calm_buoy_fatigue.py:571,576` `compute_fatigue_life` | src | **damage** — Miner damage, fatigue life, inspection interval |
| `tests/marine_ops/marine_engineering/test_calm_buoy_fatigue.py` | test | counting tests diagnostic; `TestScatterDiagramFatigue` and `TestComputeFatigueLife` damage |

No consumer exists outside this module: the class is not exported from any
`__init__.py` and no other file in `src/`, `tests/`, `docs/`, `scripts/` or
`examples/` imports it.

**Refusal applied at:** `ScatterDiagramFatigue.compute` and
`compute_fatigue_life`. `MinersRuleDamage.compute` and `FatigueLifeReport.build`
are damage-producing but consume pre-counted arrays and are not counter-bound;
they remain callable.

### 3.3 `fapps_counting` — NON-CONSERVATIVE

`src/digitalmodel/structural/fatigue_apps/rainflow_counting.py:39`
`RainflowCounter.process_time_series`

**Mechanism, read from source.** `rainflow_counting.py:154-163` extracts only
when the stack already holds four or more points (`if len(stack) >= 4`) and
breaks otherwise, so ASTM rule 2 — count the pair containing the starting point
as a half cycle and discard the starting point — is never applied. The starting
point is therefore never shed, the stack stays four or more deep, and rule 3
(full cycle, discard **both** points of the pair) is applied where rule 2 was
due. A reversal that ASTM retains is discarded. This is the same defect shape
as `struct_fatigue` §3.4, where it is confirmed by a controlled pair; the two
report identical maxima on both failing signals.

| Consumer | Kind | Verdict |
|---|---|---|
| `rainflow_counting.py:465` its own `main()` | src | pipeline — writes `*_cycles.csv`, `rainflow_summary.csv`, `rainflow_report.txt` |

**No importer exists anywhere in the repository.** The class is not in
`fatigue_apps/__init__.py`'s exports — that file exports the different
`RainflowCounter` from `rainflow_counter.py` — and no module in `src/`,
`tests/`, `docs/`, `scripts/` or `examples/` imports it. Two non-code mentions
exist: `fatigue_apps/INPUT_FILE_STRUCTURE.md:77,116,275` documents it as
"Module 2", invoked as `python rainflow_counting.py`; and
`fatigue_apps/LOAD_SCALING_DOCUMENTATION.md:293` gives an import path
(`digitalmodel.structural.fatigue_analysis.rainflow_counting`) that does not
resolve against the current layout.

**Refusal applied at:** `RainflowCounter.process_batch`. There is no in-process
damage consumer to block, so this is the narrowest defensible placement: the
hand-off to damage is by file, and `process_batch` is the documented Module 2
step whose CSV output feeds the Module 3 damage calculator.
`process_time_series` and `rainflow_counting_astm` remain callable, so the
diagnostic use of the counter is preserved.

### 3.4 `struct_fatigue` — NON-CONSERVATIVE

`src/digitalmodel/structural/fatigue/rainflow.py:210` `RainflowCounter.count_cycles`

**Mechanism, read from source, confirmed by a controlled pair.**
`_rainflow_counting_numba` (`rainflow.py:183-196`) extracts only when
`stack_size >= 4` and breaks otherwise, so ASTM rule 2 is never applied. The
starting point is never shed, the stack stays four or more deep, and rule 3 —
full cycle, discard **both** points of the pair — is applied where rule 2 was
due, discarding a reversal ASTM retains. `_rainflow_with_means`
(`rainflow.py:562-581`) repeats the defect independently for the mean-stress
route.

Controlled pair on `broadband_random`, holding the turning-point extraction
(`_find_turning_points_numba`, 2741 points) fixed and changing only the
`else: break` branch to apply rule 2:

| variant | max range | total counts | global valley (−86.3782) |
|---|---|---|---|
| as committed | 169.5560 | 1370.0 | discarded by an extraction |
| rule 2 applied | 177.1224 | 1370.0 | retained to the residual |

Table 4 — `struct_fatigue` on `broadband_random`; span 177.1224,
`len(reversals) − 1 = 2740`.

The as-committed column reproduces Table 1 exactly. The discarded reversal is
the signal's global valley: the counter emits a full cycle of 169.5560 between
the global peak and −78.8119 and then has no valley left to pair the peak with,
so the span is never emitted. Total counts are unchanged between the two
variants, which is why the failing paths still satisfy count conservation.

| Consumer | Kind | Verdict |
|---|---|---|
| `structural/fatigue/analysis.py:50,310,354,358` | src | **damage** — `FatigueAnalysisEngine.analyze_time_series` → `total_damage`, `safety_factor`, `life_fraction_used` |
| `structural/fatigue/analysis.py:962-992` | src | **damage** — `quick_time_domain_analysis`, wrapper over the engine |
| `structural/fatigue/__init__.py:265-277` | src | **damage** — `quick_fatigue_analysis`, an independent counter construction, not routed through the engine |
| `fatigue/__init__.py:75-81` | src | alias import path for the above two engine callables |
| `structural/fatigue/rainflow.py:622,750,769,784` | src | diagnostic — `RainflowBatch`, `rainflow_count`, `rainflow_with_means`, `__main__` demo |
| `tests/structural/fatigue/test_rainflow.py` | test | diagnostic throughout |
| `tests/structural/fatigue/test_fatigue_migration.py:324,345,352,370,402` | test | **damage** via `quick_time_domain_analysis` |
| `examples/domains/fatigue/advanced_examples/complete_fatigue_analysis.py:170,431,446` | example | diagnostic and damage |
| `examples/domains/fatigue/fatigue_analysis_examples.py:170,193,422` | example | **damage** |

`damage_accumulation.py` and `scatter_fatigue.py` import no rainflow module and
are not counter-bound; `analysis.analyze_psd` is spectral. None is blocked.

**Refusal applied at:** `FatigueAnalysisEngine.analyze_time_series`,
`quick_time_domain_analysis`, and `structural.fatigue.quick_fatigue_analysis`.
The third needs its own refusal because it constructs its own counter rather
than routing through the engine.

### 3.5 Conforming paths

| Path | Entry point | Status |
|---|---|---|
| `pylife_4pt` | `digitalmodel.fatigue.rainflow.rainflow_count` | conforming on all four signals; no refusal |
| `pypi_rainflow` | `TimeSeriesComponents.count_cycles`, delegating to the `rainflow` PyPI package | conforming on all four signals; no refusal. One degenerate-case divergence, §4 |
| `fapps_counter` | `structural/fatigue_apps/rainflow_counter.py:33` `RainflowCounter.count_cycles` | conforming on all four signals; no refusal |

`fapps_counter` aggregates on a range key rounded to six decimals, so its
emitted maximum can differ from the span by up to 5·10⁻⁷. The contract's
relative tolerance of 10⁻⁶ admits that and nothing larger; the smallest
recorded failure understates the span by 4.3 %.

---

## 4. Divergences found by the gate, not by the issue

Recorded rather than absorbed, because each is a fact about a path that a later
consolidation decision needs.

1. **`calm_buoy` loses a half cycle on `astm_example`** (3.5 against 4.0), and
   **`sigproc_astm` emits 4.5 of its 10.5 cycles at range zero on
   `narrow_band_sine`**. The issue body's claim that total counted cycles agree
   across all seven paths on every signal does not hold for either pair. The
   second is invisible to a maximum-range check — that cell passes Table 1 —
   which is the argument for clause 3 being an exact equality rather than an
   aggregate sanity check. §1, §3.1, §3.2.
2. **`rainflow` 3.2.0 returns nothing for a two-sample series.** `pylife_4pt`
   and `fapps_counter` both emit the single half cycle at the full span;
   `extract_cycles([0.0, 10.0])` returns `[]`. Third-party behaviour on a
   degenerate input; recorded as a strict expected-fail
   (`test_12b_pypi_rainflow_two_point_signal`).
3. **The conforming paths disagree on a constant signal.** pyLife and the PyPI
   package emit one zero-range half cycle; `fapps_counter` emits nothing. A
   zero range contributes exactly zero damage, so neither is wrong. The
   contract treats zero-range rows as inert but forbids them when the span is
   non-zero, which is the case the sub-clause exists to catch.
4. **Importing `marine_ops.marine_engineering.calm_buoy_fatigue` closes
   `sys.stdout`.** `marine_ops/marine_analysis/extraction/run_extraction.py:11-12`
   replaces `sys.stdout` with a new `TextIOWrapper` over the same buffer at
   import time, and the discarded wrapper closes the buffer. Unrelated to
   cycle counting; it makes any script that imports that package unable to
   print. Not fixed here — out of this plan's scope.

---

## 5. Open items

- **Deliverable exposure is not established.** Whether any issued result was
  produced through a failing path requires access to client repositories, which
  is outside this plan's scope and requires the owner's direction. This item is
  named, not resolved.
- **The exposure cannot be dated.** `git log` for the affected files returns a
  single commit, so the interval over which a failing path was callable for
  damage is not determinable from the repository.
- **Eight scripts under `scripts/python/digitalmodel/tools/` import
  `digitalmodel.modules.signal_analysis.orcaflex`**, a module path that does not
  match the current layout. Whether they ran against an earlier layout is not
  established; the record states that rather than assuming either way. The same
  stale namespace appears at `docs/domains/examples/signal_analysis_usage_example.py:24-25`
  and, wrapped in `try/except ImportError` so it silently no-ops, at
  `structural/fatigue/__init__.py:216-217`.
- **`opp_time_series_v2.py` has no current `src` importer.** It is constructed
  by a committed documentation example, so "orphaned" and "unreachable"
  overstate what the evidence supports.
- **Consolidation onto one implementation is deferred** to a successor issue.
  The refusals above are the interim measure; deletion is the successor's work.
  Each expected-fail marker carries the issue number so it surfaces in triage.

### 5.1 A damage entry point the refusal cannot reach

`src/digitalmodel/ansys/fatigue_postprocessor.py` was raised as a candidate
eighth counting path and is **not** one. The module contains no counting
implementation: `rainflow` occurs exactly once in its 346 lines, at the module
docstring, and no reversal, peak, valley, turning-point or stack logic exists
anywhere in the file. `calculate_fatigue_damage()` at `:208` iterates
`config.load_cases` and reads `lc.stress_range_mpa` (`:232`) and `lc.num_cycles`
(`:247`) as caller-supplied inputs; counting is delegated to ANSYS through the
APDL commands emitted by `generate_stress_range_extraction()` at `:272`. The
seven-path inventory in section 3 stands. Verified independently by two sessions.

It is, however, a **damage entry point that the refusal mechanism cannot
protect**, and this is a limitation of the control rather than an omission from
it. The refusals in section 3 guard entry points that call a failing counter
**in process**. This one receives stress ranges and cycle counts as data — by
CSV, by spreadsheet, or by hand — so a range produced by one of the four
non-conservative paths can reach Miner summation here without any counter being
called, and no in-process guard can observe it.

The risk is **latent rather than live**: the module has no production caller.
A repository-wide search for `fatigue_postprocessor`, `FatiguePostprocessor` and
`FatigueConfig` returns only `tests/ansys/test_fatigue_postprocessor.py` and the
re-export in `src/digitalmodel/ansys/__init__.py`. Nothing currently feeds it
counts from a defective path. It matters because `digitalmodel.ansys` is a
public export surface, so the hand-off is available to any external caller.

A separate documentation defect is recorded here because it caused the false
lead and would mislead the next reader identically: the module docstring at
`:14` advertises "Rainflow cycle counting (simplified)" as a module capability
that the module does not implement. `ansys/__init__.py:24` describes the module
correctly as "fatigue damage evaluation (S-N, Miner)" with no counting claim, so
the misleading advertisement is isolated to that one line. The file sits inside
the scope of digitalmodel issue #2094, and the fix is routed to that issue's
owner rather than made here.

---

## 6. Reproducing this record

```
cd digitalmodel
.venv/Scripts/python -m pytest tests/fatigue/test_rainflow_invariants.py -q
```

The four signals, the seven registered paths and their declared residual
policies are in `src/digitalmodel/fatigue/counting_contract.py`
(`CONTRACT_SIGNALS`, `COUNTING_PATHS`). The contract is callable without pytest
via `check_counting_contract` and `assert_counting_contract`.
