# Shard setup and geotechnical test time

**Measured · sweep 43998853169.** The 22 shard jobs used 145.05 runner-minutes. Pytest reported 91.77 minutes and 7 shards below 60 seconds. The enclosing domain steps used 100.98 minutes. Summing JUnit testcase times gives 87.13 minutes and 11 shards below 60 seconds. JUnit omits time outside individual cases.

## Seven-day measurements

**Measured · run sets S and P.** The creation window is 2026-11-11T14:00:00Z through 2026-11-18T14:00:00Z. S includes every Full Matrix Sweep event. P includes pull requests in Quality Gates by Domain. The index records every run ID and head SHA. Each job record carries the run ID, attempt and job ID. Within each run, records with the same shard name, start time and completion time count as one execution. The earliest attempt supplies the record. A missing start or completion time keeps the record separate. This removed 147 carried-over job records. All indexed runs reached a terminal state by snapshot completion.

Sweep 43998853169 used `68a90cad09e1fe9c07935aaf421b79bcf9ad9526`. The W9 static history audit and the local collection use the later revision identified below. S comprises 99 push, 7 schedule runs. Runs with no named shard jobs remain in the window denominator. Their individual causes were outside the timing capture.

| Run set | Runs in window | Runs with shard jobs | Shard jobs | Full depth / shallow / unknown | Known install / pytest readings |
| --- | --- | --- | --- | --- | --- |
| S | 106 | 101 | 2222 | 2222 / 0 / 0 | 2220 / 2220 |
| P | 611 | 371 | 7175 | 6826 / 0 / 349 | 6736 / 6643 |

Each timing cell gives median / nearest-rank p90. Parentheses in the shard tables give the known sample size for that cell. Step durations come from Actions. The install interval runs from command launch through the last observed install completion before pytest. Pytest supplies its own elapsed time. The domain and solver steps contain these intervals. Upload and teardown consume part of job time. Setup includes runner initialization, checkout, PR merge, Python, uv, conditional Node, formatter and system installs, report directory creation, reading the gate command, register selection and observed package installs. All pytest includes domain and solver pytest. Each percentage uses that job’s elapsed time before aggregation.

Skipped steps have no observation. A missing timestamp, archive, install boundary or unique pytest summary gives an unknown reading. Interrupted steps and cancelled job durations remain unknown. Completed steps on interrupted attempts still contribute to their own distributions. Percentages require a completed success or failure with an executed domain gate. Per-metric known and unknown counts and individual job steps are in the JSON report.

**Measured · per shard, run set S.**

| Shard | Jobs | Job (s) | Checkout (s) | Domain install (s) | Domain pytest (s) | Setup incl. install (%) | All pytest (%) |
| --- | --- | --- | --- | --- | --- | --- | --- |
| asset-integrity | 101 | 182/216 (101) | 64/93 (101) | 21.1/25.3 (101) | 53.8/68.5 (101) | 62.8/67.6 (101) | 28.9/37.7 (101) |
| capabilities | 101 | 154/178 (100) | 62.5/75 (100) | 20.8/26.4 (100) | 30.7/39 (100) | 71.9/75.8 (100) | 19.4/26.4 (100) |
| cathodic-protection | 101 | 187/211 (101) | 66/87 (101) | 19.7/25.1 (101) | 59.6/74.4 (101) | 60.8/66.7 (101) | 31.1/40.4 (101) |
| citations | 101 | 124/151 (101) | 62/83 (101) | 19.7/25.2 (101) | 6.5/8.1 (101) | 85.3/88.1 (101) | 4.8/6.7 (101) |
| contracts | 101 | 923/1027 (101) | 66/104 (101) | 16.6/27.3 (101) | 774.4/888.9 (101) | 13.4/16.6 (101) | 84.8/88.4 (101) |
| field-development | 101 | 143/170 (101) | 66/79 (101) | 20/25.7 (101) | 23/29.5 (101) | 75.4/78.9 (101) | 14.9/21.4 (101) |
| hydrodynamics-diffraction | 101 | 809/1136 (101) | 65/76 (101) | 20.6/26.2 (101) | 657/1016.1 (101) | 14.6/17.3 (101) | 83.1/89.6 (101) |
| hydrodynamics-other | 101 | 567/757 (101) | 62/73 (101) | 20.3/25.5 (101) | 426.9/627.2 (101) | 20.2/23.2 (101) | 75.5/83.7 (101) |
| hydrodynamics-passing-ship | 101 | 152/181 (100) | 65.5/91 (100) | 20.7/25.9 (100) | 25.7/31.3 (100) | 74.1/77.9 (100) | 16.3/21.5 (100) |
| infrastructure-core | 101 | 217/251 (101) | 65/97 (101) | 20.8/24.2 (101) | 85.3/105.4 (101) | 52.5/56.5 (101) | 39.4/47.9 (101) |
| infrastructure-other | 101 | 308/357 (101) | 66/95 (101) | 20.3/26.8 (101) | 175.3/213.4 (101) | 36.6/42.1 (101) | 58.9/62.8 (101) |
| marine-engineering | 101 | 222/260 (101) | 66/93 (101) | 21.1/25.8 (101) | 87.9/111.4 (101) | 52.9/56.9 (101) | 40.4/49.7 (101) |
| marine-ops-other | 101 | 259/289 (101) | 65/75 (101) | 19.7/25.1 (101) | 126.2/169.4 (101) | 44.5/48.5 (101) | 49.1/59.6 (101) |
| misc | 101 | 135/164 (101) | 64/90 (101) | 20.6/26 (101) | 13.9/16.6 (101) | 80.8/84.1 (101) | 9.7/12.8 (101) |
| naval-architecture | 101 | 180/207 (101) | 65/88 (101) | 20.9/26.5 (101) | 52.4/64.8 (101) | 63.1/67.3 (101) | 28.7/36.4 (101) |
| orcaflex | 101 | 196/235 (101) | 62/92 (101) | 20.2/24.8 (101) | 68.6/88.4 (101) | 58/63.1 (101) | 34.5/43.9 (101) |
| orcaflex-solver | 101 | 965/1170 (101) | 68/96 (101) | 19.9/24.8 (101) | 822.4/1026.1 (101) | 12.1/15.3 (101) | 86.2/89.7 (101) |
| solver-smoke | 101 | 425/474 (101) | 65/91 (101) | 20.2/28.6 (101) | 274.9/316.2 (101) | 30.4/35.3 (101) | 65.5/72.2 (101) |
| specialized | 101 | 167/195 (101) | 65/86 (101) | 17/21.9 (101) | 44/55 (101) | 65.1/69.4 (101) | 25.7/33.2 (101) |
| structural | 101 | 330/389 (101) | 64/89 (101) | 20.3/25.4 (101) | 195.2/242.9 (101) | 35.7/41.1 (101) | 59.3/67.9 (101) |
| subsea | 101 | 497/592 (101) | 65/93 (101) | 19.8/25.2 (101) | 350.6/468.4 (101) | 24/29.2 (101) | 72.6/80.1 (101) |
| workflows | 101 | 609/672 (101) | 67/95 (101) | 20.2/24.7 (101) | 473.2/549.6 (101) | 18.9/22.7 (101) | 78.4/82.5 (101) |

**Measured · per shard, run set P.**

| Shard | Jobs | Job (s) | Checkout (s) | Domain install (s) | Domain pytest (s) | Setup incl. install (%) | All pytest (%) |
| --- | --- | --- | --- | --- | --- | --- | --- |
| asset-integrity | 325 | 182/210 (302) | 65/81 (307) | 21/28 (303) | 54.1/68.3 (301) | 62.6/66.9 (300) | 28.9/37.3 (300) |
| capabilities | 329 | 156/192 (308) | 66/88 (310) | 20.5/27.5 (308) | 30.8/39.6 (307) | 72.7/76.3 (307) | 19.3/26.2 (307) |
| cathodic-protection | 325 | 186/218 (306) | 66/85 (309) | 20.7/28.2 (306) | 59/74.3 (305) | 61.5/66.7 (305) | 30.6/39.4 (305) |
| citations | 322 | 130.5/158 (306) | 63.5/87 (308) | 21.4/26.6 (307) | 6.7/8.3 (307) | 85.3/88.2 (305) | 4.9/6.4 (305) |
| contracts | 352 | 890/1010 (322) | 65/88 (333) | 21/28.2 (333) | 744.9/871 (322) | 13.7/17.1 (322) | 84.7/88.1 (322) |
| field-development | 322 | 148/185 (303) | 66/99 (305) | 20.7/28 (303) | 23.4/29.3 (302) | 76.3/79.5 (301) | 14.9/19.7 (301) |
| hydrodynamics-diffraction | 322 | 820.5/1143 (292) | 66/86 (302) | 20.8/28.5 (300) | 662.7/1014.5 (291) | 14.4/17.8 (290) | 83/89.7 (290) |
| hydrodynamics-other | 322 | 588.5/764 (296) | 66/90 (308) | 20.1/27.6 (306) | 429.5/628.4 (295) | 20.6/24.9 (295) | 75.2/83.7 (295) |
| hydrodynamics-passing-ship | 322 | 150/183 (305) | 66/93 (307) | 20.8/27.6 (306) | 26/31.6 (305) | 74.8/78.8 (304) | 16.3/21.9 (304) |
| infrastructure-core | 322 | 220/251 (303) | 66/89 (304) | 20.4/28.2 (302) | 89.2/105.2 (301) | 52.4/56.2 (301) | 39.9/47.6 (301) |
| infrastructure-other | 324 | 307/359 (300) | 66/92 (311) | 20.6/28.4 (308) | 177.1/209.6 (300) | 36.8/42.6 (299) | 58.2/62.5 (299) |
| marine-engineering | 323 | 221/260 (302) | 65/91 (304) | 20.6/27.8 (303) | 87.9/112.1 (301) | 53.1/58.5 (301) | 40/49.6 (301) |
| marine-ops-other | 322 | 273/319 (298) | 66/90 (305) | 20.6/28.4 (301) | 133.1/170.7 (298) | 44.5/50.3 (297) | 49.3/59.9 (297) |
| misc | 322 | 137/175 (301) | 66/96 (303) | 20.6/28.9 (303) | 14.4/16.6 (303) | 80.9/84.5 (301) | 9.6/12.6 (301) |
| naval-architecture | 324 | 180/212 (298) | 66/89 (303) | 20.3/28.3 (301) | 53.3/65.9 (300) | 63.5/68.4 (298) | 28.7/36.8 (298) |
| orcaflex | 322 | 199/227 (299) | 66/81 (305) | 20.8/26.9 (298) | 70.2/88.4 (296) | 57.9/63.8 (295) | 34.7/44 (295) |
| orcaflex-solver | 322 | 946/1153 (292) | 65/85 (307) | 20.2/26.7 (304) | 808.3/1032.8 (292) | 12.5/14.8 (291) | 85.9/89.8 (291) |
| solver-smoke | 327 | 425/467 (299) | 66/90 (306) | 21/31.1 (304) | 269.6/313.3 (298) | 31.3/37 (298) | 64.9/71.5 (298) |
| specialized | 324 | 1025/1468 (290) | 66/89 (304) | 16.3/22.6 (302) | 43.3/55.4 (300) | 13.6/17.5 (289) | 84.2/90.8 (289) |
| structural | 324 | 341/391 (299) | 65/91 (304) | 21.3/29.4 (301) | 202.7/244.9 (298) | 35.2/41.4 (298) | 60/66.9 (298) |
| subsea | 333 | 501.5/616 (306) | 66/90 (313) | 20.4/28.3 (311) | 357.1/476.4 (305) | 23.9/27.8 (305) | 72.7/80.2 (305) |
| workflows | 345 | 616/694 (317) | 66/88 (327) | 20.1/27.8 (326) | 486.5/566.6 (316) | 18.7/21.7 (316) | 78.3/82.4 (316) |

**Measured · per step class, run sets S and P.** Values are seconds. Conditional classes use jobs where the step executed. The full shard-by-step cross-tabulation is in `summary`.

| Step class | S median/p90 | P median/p90 |
| --- | --- | --- |
| Checkout | 65/90 | 66/89 |
| Domain gate (includes install and pytest) | 116/692 | 119/692 |
| Formatter dependency install | 3/4 | 3/4 |
| Merge | n/a | 1/2 |
| Set up Node | 1/3 | 1/4 |
| Result extraction, cleanup and other | 1/1 | 1/1 |
| Set up Python | 0/1 | 0/1 |
| Read gate command | 17/24 | 17/24 |
| Select trusted register | 0/1 | 0/1 |
| Reports dir | 0/0 | 0/0 |
| Runner setup | 1/2 | 1/2 |
| Geotechnical solver step | n/a | 825/1282 |
| System libs | 8/20 | 8/21 |
| Upload | 1/2 | 1/2 |
| Set up uv | 8/12 | 8/12 |
| Domain install (within gate) | 20.1/25.7 | 20.4/28 |
| Domain pytest (within gate) | 88.3/660.3 | 88.7/659.2 |

## W6b decision

**Measured · all 587-second solver steps in run set P.** The two phase clocks leave some command and interpreter overhead outside their sum. Collected and passed counts apply to each run's revision.

| Run ID | Job ID | Step (s) | Install (s) | Pytest (s) | Collected | Passed |
| --- | --- | --- | --- | --- | --- | --- |
| 43812460453 | 133123026146 | 587 | 15.3 | 567.02 | 1318 | 1318 |

**Measured · P, executed solver steps.**

| Reading | Known / executed steps | Median/p90 |
| --- | --- | --- |
| Install (s) | 299/300 | 14.3/18.1 |
| Pytest (s) | 289/300 | 802.1/1269.8 |
| Collected (nodes) | 299/300 | 1318/1408 |
| Passed (nodes) | 289/300 | 1318/1408 |

**Measured · local collection at `c90ef11aa6bc77047215e753f5c3290c73e57635`, Python 3.11.18.** Base package versions matched. The solver environment added xslope 1.0.3 and gmsh 4.15.2. The five sorted lists and their digests are in the job folder.

| Collection | Node IDs |
| --- | --- |
| Without extras | 1389 |
| With extras | 1408 |
| Only with extras | 19 |
| Only without extras | 0 |
| Both | 1389 |

**Measured · same local SHA and environments.** All 1,385 baseline passes also passed with extras. The 4 shared runtime skips became passes. All 1,408 solver-enabled cases passed. Baseline pytest also reported a module-level collection skip. JUnit outcomes and frozen package versions accompany the lists.

The additional nodes come from `test_slope_stability.py`. The controlled local comparison used the editable test extra in both environments. The ordinary specialized CI gate overrides a dependency at its upstream main revision. The separate solver step installs the editable test extra. The sweep runs the ordinary gate alone.

**Measured · P, successful specialized jobs with a successful solver step.** The ordinary gate's contiguous geotechnical progress interval had median/p90 12.6/18.7 s across 289 of 289 paired jobs. The interval runs from the last progress row before the geotechnical cases through their final progress row.

**W6b recommendation and Estimate.** Retain both PR executions. If a future coverage-gated comparison establishes equivalence for both environments, removing the duplicate base execution could save about 13 runner-seconds per comparable PR job across the sampled revisions. The corresponding interval in sweep 43998853169, job 133761278029, was 12.025 s. The estimate excludes collection and the solver execution that would still run.

## W9 decision

**Measured · sweep 43998853169.** Common setup includes runner initialization, checkout, Python and uv. The pilot takes domain-specific shallow-trial candidates below 60 seconds of pytest in this sweep with the same declared uv and pytest arguments as capabilities. The expanded commands agree after removing test paths and per-shard JUnit destinations. The misc engine gate is held out of the first domain trial. Resolved environments and runner compatibility still require qualification.

| Candidate | Common setup (s) | Pytest (s) |
| --- | --- | --- |
| capabilities | 70 | 31.3 |
| citations | 76 | 7.8 |
| field-development | 60 | 17.4 |
| hydrodynamics-passing-ship | 112 | 22.2 |

**W9 recommendation and Estimate.** Trial grouping these four co-dispatched sweep shards. Retaining the slowest common setup would save 206 runner-seconds (3.43 runner-minutes) on the measured sweep. Applying the same assumption to 99 of 101 shard-bearing sweep attempts gives median/p90 savings of 212/237 s. All four jobs succeeded and all four setup clocks were known in each included attempt. Exclusions were 0 missing a shard, 2 with a non-successful job and 0 with an unknown setup clock. Each original command and shard-level result would remain separately accountable. The estimate assumes one runner can serve all four jobs and assigns no saving to dependency reuse, shallow checkout or elapsed critical-path time. PR grouping also needs proof of touched-module closure before merge.

### Checkout depth by shard

Static inspection at `c90ef11aa6bc77047215e753f5c3290c73e57635` covered the 22 commands in `.claude/quality-gates.yaml`, the workflow-added subsea paths, shared setup and matching tests and scripts. Both shard workflows request full history at this revision. These findings identify candidates for a depth trial. They do not measure a checkout saving.

| Shard suffix | Evidence for keeping full history |
| --- | --- |
| asset-integrity | `tests/materials/test_database.py` checks a pinned revision against `origin/main` with `merge-base --is-ancestor` and reads historical blobs. |
| cathodic-protection | `tests/cathodic_protection/test_edition_paths_review.py` resolves every review anchor at its recorded commit. |
| contracts | `tests/docs/test_riser_global_evidence_register.py` reads retained blobs with `git show`. |
| hydrodynamics-diffraction | `tests/solvers/orcawave/diffraction/test_pack_lint.py` reads source and map revisions. |
| hydrodynamics-other | `rao_fixture_population.py` uses path-specific `git log -1` and `git show`. The source-revision test in `tests/hydrodynamics/hull_library/test_rao_fixture_metadata.py` skips on shallow clones. |
| marine-engineering, workflows | Both select `tests/marine_ops/marine_engineering/visualization/test_no_regression_traces.py`, which reads the captured producer's historical blob. |
| naval-architecture | `tests/naval_architecture/test_mesh_pickle_generator.py` populates its temporary repo from the real checkout's pinned baseline when its Python and NumPy conditions match. |
| subsea | `tests/drilling_riser/test_generic_manifest.py` resolves the manifest's pinned commit and spec blob. |

| Shallow-checkout trial candidates | Evidence from selected paths |
| --- | --- |
| capabilities, citations, field-development, hydrodynamics-passing-ship, marine-ops-other, specialized, misc | No checkout-history operation found. Capabilities checks tracked files with `ls-files`; citation resolver tests use local files and fixtures. |
| infrastructure-core, infrastructure-other, orcaflex-solver, solver-smoke | Matched code uses disposable Git fixtures or the current index. The evidence-audit tests call `build_audit` with explicit paths rather than its historical inventory CLI. |
| orcaflex | Model-token tests use `ls-files`. On PRs `scripts/ci/ci_baseline.py:select_register` separately fetches and reads the trusted base tip. Preserve this fetch. |
| structural | Inspected historical blob checks use mocked Git or synthetic roots. The plate study reads HEAD and working-tree status. Exercise the provenance tests in the trial. |

All PR shards also run `merge-base` in the local merge guard before tests. An initially shallow PR checkout needs the existing fetch, deepen and unshallow fallback to produce the same merged tree. Scheduled sweeps have no PR merge guard. Before changing depth, make the literal zero-depth assertions in `tests/materials/test_database.py` and `tests/solvers/orcawave/diffraction/test_pack_lint.py` check effective depth for each protected shard. Verify the same test outcomes, skip reasons and line coverage on the affected paths.

The history audit records selected paths, ignores, file-list hashes, matching lines and references to scripts and modules containing Git reads. Dynamic calls can escape a static scan.

## Reproduction

Run these commands from the repository root with `JOB` set to the retained evidence folder. The inputs are the timing snapshots (including attempt archives and solver logs), reference JUnit archives and index, and geotechnical node lists and outcomes. The static audit reads Git objects at the pinned local-collection revision. The commands run offline.

```sh
set -e
python3 scripts/ci/step_timings.py report "$JOB/snapshots" > "$JOB/report.json"
python3 scripts/ci/step_timings_junit.py "$JOB" > "$JOB/reference-junit.json"
python3 scripts/ci/step_timings_commands.py "$JOB" > "$JOB/gate-commands.json"
python3 scripts/ci/step_timings_history.py "$JOB" > "$JOB/history-audit.json"
python3 scripts/ci/step_timings_render.py "$JOB" > "$JOB/step-timings-20261118.md"
cmp "$JOB/step-timings-20261118.md" docs/ci/step-timings-20261118.md
```

For the exact run IDs behind a shard figure, select its workflow, shard and non-null metric. For example:

```sh
jq '[.jobs[] | select(.workflow=="pr" and .shard=="specialized" and .pytest_s!=null) | .run] | unique' "$JOB/report.json"
```

**Coverage gate.** Measurement only. The sole selection change adds the timing fixture tests to the CI harness. W6b and W9 still require their own before/after JUnit and covered-line evidence on the same base SHA for the applicable PR and daily obligations.

<details><summary>Run IDs contributing shard jobs to measured tables S and P</summary>

**S**

43125970129, 43132379374, 43132844083, 43146169778, 43174607569, 43211553772, 43211566242, 43211577989, 43211591277, 43239442696, 43260676018, 43263376877, 43276527159, 43297102163, 43312066276, 43312122838, 43314479940, 43381720376, 43385060224, 43417523990, 43417536161, 43418545843, 43418566466, 43418587957, 43428858667, 43428889258, 43428914113, 43428936772, 43435254307, 43435308130, 43437468977, 43444499431, 43444535681, 43444562792, 43459521287, 43462074296, 43473494460, 43474666768, 43482022921, 43492464733, 43493895026, 43503279504, 43503320441, 43505395218, 43507831553, 43524708095, 43526640625, 43526669649, 43528478887, 43529512192, 43530250325, 43530303328, 43541676985, 43541708065, 43547336451, 43553830302, 43556324780, 43558769662, 43559979925, 43559996734, 43561216192, 43566025499, 43571082109, 43573109215, 43573123486, 43577855378, 43581605743, 43582598587, 43587343348, 43592104554, 43594438903, 43604425011, 43605825339, 43615249628, 43628999326, 43629012975, 43634437227, 43747801279, 43747816317, 43747836737, 43747856346, 43749996349, 43824707679, 43848236852, 43867164887, 43870493821, 43870520722, 43870534324, 43870558689, 43870584811, 43920309142, 43925516015, 43978733454, 43998853169, 44013234561, 44013268037, 44014952511, 44016731530, 44018466957, 44038078933, 44258266453

**P**

43102508630, 43110319088, 43111878898, 43117662699, 43118582771, 43122165183, 43122398258, 43126018604, 43126040550, 43128235728, 43142637000, 43149612304, 43177808118, 43180843105, 43215180447, 43233269401, 43238339398, 43254151377, 43254621212, 43257625867, 43258168819, 43261493595, 43261889728, 43262504765, 43264051245, 43264868690, 43267296963, 43267385272, 43268286009, 43269528016, 43272015922, 43272654577, 43273231787, 43273236081, 43273301745, 43273368894, 43274488243, 43274548694, 43274568373, 43275326479, 43276074890, 43276969372, 43277358387, 43278422910, 43278695804, 43279517046, 43279581347, 43279847438, 43279999175, 43280168452, 43280513141, 43281355029, 43284856864, 43286557695, 43287459314, 43289503650, 43293290771, 43300157144, 43300158268, 43300158923, 43300919392, 43305329866, 43312107272, 43318341243, 43318448059, 43318490761, 43318684473, 43328881091, 43331863099, 43332512997, 43333497984, 43333502793, 43333509916, 43333520585, 43333916454, 43334008327, 43334021189, 43334148600, 43334162110, 43334175981, 43335645974, 43335650617, 43338584260, 43338590821, 43339218427, 43339284629, 43339357620, 43339903659, 43340301969, 43340758379, 43340941850, 43342367476, 43348698223, 43354064294, 43372659897, 43373165649, 43373763603, 43379692318, 43382197566, 43382386851, 43382983459, 43383166578, 43383266862, 43383417449, 43383451839, 43383724370, 43384621818, 43395604503, 43399750155, 43401263075, 43402665656, 43429068739, 43435086809, 43436295982, 43436991580, 43437660287, 43440709915, 43444893224, 43445823478, 43446469976, 43446478371, 43446755987, 43447965959, 43450390949, 43451051809, 43455417953, 43456754077, 43459090750, 43462573206, 43463479453, 43464909043, 43474979920, 43476528917, 43476895733, 43483207873, 43485659882, 43485922168, 43489268424, 43491388432, 43491496903, 43493058072, 43497653951, 43498498286, 43500874898, 43500880486, 43501287355, 43506986823, 43506990951, 43507001047, 43512108939, 43512114404, 43512120670, 43513241888, 43513850655, 43514497964, 43518654594, 43518661502, 43522347863, 43522688378, 43522692192, 43522697615, 43526811000, 43527158518, 43529072377, 43530412836, 43530418685, 43530573295, 43530811853, 43531244012, 43534026238, 43534094116, 43534166642, 43537091071, 43543088566, 43545933195, 43545942874, 43545945289, 43545948092, 43546070985, 43546073724, 43550235196, 43551301188, 43551310693, 43551333061, 43551343566, 43551354681, 43552954057, 43553746127, 43553751276, 43553768275, 43555913708, 43556241190, 43556247577, 43556257059, 43556279434, 43559947361, 43560526859, 43562082827, 43563268276, 43563520924, 43563527157, 43563566341, 43563790932, 43564083816, 43573456762, 43573504810, 43574464831, 43575457212, 43575467168, 43575477009, 43575483194, 43577046551, 43578143524, 43579218980, 43579221772, 43579224120, 43579226841, 43579229434, 43580168817, 43580174792, 43582533716, 43582543152, 43586569850, 43586828125, 43587464518, 43588171041, 43589427219, 43589433415, 43589436311, 43590529794, 43591720973, 43591906701, 43592229000, 43592439265, 43593897509, 43596204507, 43596802413, 43597370400, 43598295823, 43600285937, 43617053748, 43619218170, 43620699625, 43621876381, 43623640427, 43623797093, 43629712369, 43640807993, 43655397939, 43655966001, 43670518000, 43683119160, 43683292127, 43712186600, 43731750869, 43739327972, 43753777372, 43761348264, 43761583001, 43773721774, 43784359709, 43790875322, 43794313261, 43795636883, 43795801377, 43795904535, 43796091918, 43796414109, 43797715436, 43798353453, 43799007740, 43808747991, 43811549295, 43812460453, 43832699184, 43845986807, 43846766349, 43847395983, 43849201640, 43850130066, 43855864894, 43864448743, 43874599935, 43876484683, 43881104365, 43882566514, 43887904685, 43892823043, 43895736269, 43896292416, 43897785640, 43912378858, 43913251102, 43913428733, 43913442557, 43916004844, 43916134009, 43916394980, 43919220623, 43919289499, 43919588715, 43920976926, 43922794277, 43923319240, 43923602518, 43924365408, 43928402680, 43928601799, 43929771562, 43933926335, 43939381517, 43945815849, 43946824367, 43947073277, 43965651291, 43967795815, 43968307239, 43968382867, 43985285485, 43999399199, 44006218894, 44007297110, 44010359800, 44012068579, 44089495232, 44171285272, 44173866182, 44179137722, 44181773361, 44191911488, 44191919968, 44196557751, 44213324021, 44213380385, 44213386068, 44213391470, 44213968642, 44215567944, 44216928539, 44217791755, 44218099252, 44218196559, 44222822340, 44223608325, 44224401881, 44227097918, 44232447132, 44247247171, 44249003897, 44252702291, 44254573801, 44254586213, 44254597689, 44256472132, 44256477234, 44256482606, 44260371606, 44260377260, 44264353083, 44264358879, 44266524305, 44266575561, 44268707964, 44270881638, 44270897373, 44273125640, 44275437863, 44294267156, 44294273431, 44294280661, 44318283894

</details>
