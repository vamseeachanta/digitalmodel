# Plan: pipeline-end anode banks and flowline attenuation ([#2260](https://github.com/vamseeachanta/digitalmodel/issues/2260))

> **Status:** implemented; code review APPROVE
> **Complexity:** T3
> **Date:** 2026-09-29
> **Issue:** https://github.com/vamseeachanta/digitalmodel/issues/2260
> **Client:** N/A
> **Lane:** lane:codex
> **Execution mode:** parallel-readonly discovery, single-lane implementation
> **Authority:** the 2026-09-29 user instruction explicitly authorizes this implementation scope, local commit, and private comparison; it excludes push, PR creation, and issue creation.
> **Review artifacts:** `scripts/review/results/2026-09-29-plan-2260-claude.md`; `scripts/review/results/2026-09-29-plan-2260-codex.md`; `scripts/review/results/2026-09-29-plan-2260-gemini.md`; `scripts/review/results/2026-09-29-plan-2260-codex-r2.md`
> **Review disposition:** Claude and Gemini were unavailable. Codex r1 returned MAJOR; all ten provider and independent-review defect classes were incorporated, and the bounded r2 re-review returned APPROVE.
> **Code review:** standards r3 APPROVE and API/artifact r2 APPROVE; artifacts are `scripts/review/results/2026-09-29-code-2260-standards-r3.md` and `scripts/review/results/2026-09-29-code-2260-api-r2.md`.

## Resource intelligence summary

### Existing repo code

- `src/digitalmodel/cathodic_protection/_kernels.py` will remain the sole home for validated formula arithmetic. Existing demand, mass, anode-output, coating-breakdown, and individual-anode resistance kernels will be composed rather than duplicated.
- `src/digitalmodel/cathodic_protection/f103_tables.py` will supply cited coating, current-density, anode-potential, capacity, and protection-potential values for editions 2010 and 2019.
- `src/digitalmodel/cathodic_protection/dnv_rp_f103.py` implements uniformly spaced bracelet anodes only. It cannot represent a terminal bank, attached-structure demand, group resistance, or a far-end potential check.
- `src/digitalmodel/cathodic_protection/engine_adapter.py` and `report_adapters.py` provide the route/status/report contracts that the new route will preserve.

### Standards and source correction

| Standard | Verified basis used by this plan | Edition difference |
|---|---|---|
| DNV-RP-F103:2019 | Sec. 6.7 Eq. (9)-(16), Eq. (18)/(20), and Appendix D.8.5-D.8.10 | Appendix B covers seawater resistivity; it is not the attenuation method stated in the issue. Appendix D explicitly addresses isolated anode banks and points to ISO 15589-2 Annex B. |
| DNV-RP-F103:2010 | Sec. 5.6 Eq. (8)-(17) | The same terminal-source resistance/metallic-drop basis is numbered differently and lacks the 2019 Appendix D bank guidance. |
| DNV-RP-B401 | Table 10-7 individual stand-off anode resistance | The ideal group resistance will be `1/R_bank = sum(1/R_a,j)`; interaction or cable resistance will be an explicit input and limitation. |

The conservative F103 terminal-source case will be used. Linepipe and field-joint surface areas and mean/final demands will be calculated separately and summed. For attenuation, the effective final coating factor will be `f'_cf = f_cf,linepipe + r f_cf,FJC` per 2019 Eq. (11), where `r = L_FJC / L_linepipe = g/(1-g)` for total FJC length fraction `g`; total FJC length will be required to remain below total pipe length. The standard attenuation coefficient will be `q_metal=pi D f'_cf i_cm`; the exact area-weighted bank coefficient will be `q_bank=I_side/L_total=pi D(1-g)f'_cf i_cm`. The conservative metallic drop will be `R'_L q_metal L_total^2`, F103:2019 Eq. (15), with protected length from Eq. (16). The combined bank/electrolytic and metallic drops will enforce Eq. (18). The exact isolated-bank Eq. (20) root will use the standard's `I_af=2 q_metal L`, hence a linear coefficient `2 R_bank q_metal`. The actual one-side root with attached-structure/other-side fixed load will use `q_metal` in the metallic term, `q_bank` in the bank-drop term, and only structure/other-side demand in the fixed load; it will remain a separately named circuit extension and will not be represented as Eq. (20).

For a fixed side of length `L`, the conservative profile will hold its full far-point current `I_side = qL` constant along the path: `E(x) = E_bank + R'_L I_side x`. The figure will be labelled a conservative Eq. (15) voltage-drop envelope. It will not recompute `q x` at each plotted position and will not be described as a distributed-current profile.

### Private regression evidence

The authorized private inventory identifies a complete rank-3 flowline calculation and its design-basis companion. It provides geometry, coating factors, terminal-bank resistance, pipeline and structure demand, end potential, protected length, and anode mass. The vendor standard and client calculation are distinct data classes: the vendor-licensed standard will remain off Git; the client original remains under its existing authoritative private custody and will not be copied, moved, or published by this task. The private follow-up will record its opaque owner evidence, existing stable digest, custody/readiness limitation, and source relationship. A coarsened derived fixture will use rounded generic geometry and independently recomputed cited F103 values; only the private follow-up will record the source comparison. This task is consumption of retained evidence, not a new ingest or authority to repair its owner manifest.

The drive-index utility found no configured index in this checkout. The sibling data catalogs contain no cathodic-protection benchmark dataset; this evidence will remain task-scoped and will not be represented as catalog-qualified reusable data.

### Reproduction proof

`rg -n "anode bank|KEY_F103_ANODE_BANK|DNV_RP_F103_anode_bank" src tests` will return no implementation before the RED tests are added. The live issue is open and has no comments or existing plan artifact.

## Deliverable

A typed `pipeline_anode_bank` design API, engine route `DNV_RP_F103_anode_bank`, and standard report layout will calculate line-plus-structure demand, initial/final group resistance, mass and current adequacy, conservative potential attenuation and protected length for every side connected to one remote bank, and a PASS/FAIL verdict with a governing case.

## Design and pseudocode

```text
bank_resistance(anodes):
    calculate each individual Table 10-7 stand-off resistance
    combine non-interacting paths as reciprocal parallel resistance
    R_total = interaction_factor * R_parallel + cable_resistance

design_anode_bank_cp(input, edition):
    resolve cited F103 current density, coating factors, anode potential,
        protection potential, capacity, and edition provenance
    for each connected side:
        map the edition-specific field-joint coating enum
        calculate separate linepipe/FJC areas and mean/final demands
        calculate r and f'_cf for Eq. (11), q_metal = pi*D*f'_cf*i,
            and q_bank = final side demand / total side length
        calculate longitudinal steel resistance per length
    calculate supplied structure initial/mean/final demand with the demand kernel
    function evaluate_count(N):
        calculate fresh and utilised-end individual/group resistance at N
        calculate installed-bank final potential from all final loads
        for each side:
            calculate far potential using F103 conservative Eq. (15)/(18)
            calculate exact isolated-bank Eq. (20) length with 2*R*q coefficient
            solve A*x^2 + R*q*x + Ea + R*I_fixed - Ep = 0 for the actual
                one-side fixed-load protected length and label it as an extension
            calculate hand-auditable linear envelope samples
        return mass/initial-output/final-output/far-potential checks and governing margin
    evaluate the caller's installed count for the route PASS/FAIL verdict
    search monotonically from max(1, mass count, output count) for the least N
        whose recomputed utilised-end resistance and all side checks pass
    test the `R_cable` asymptote and report `cable_limited_impossible` when it fails
    report `search_cap_exhausted` (not physical impossibility) when the asymptote
        passes but no integer N through max_anode_count passes
    persist installed-count checks and a separately recomputed recommended-count summary
```

The group resistance formula will assume electrically parallel, mutually non-interacting anodes. `interaction_factor` and `cable_resistance_ohm` will be explicit, default to 1.0 and 0.0 respectively, and be echoed in results. The order will be `R_total = interaction_factor * R_parallel + R_cable`. No project-specific contingency factor or 0.02-ohm bank assumption will be embedded. Individual-resistance formula labels will be edition keyed: B401:2010 Table 10-7 for F103:2010 and B401:2017 Table A-7 for F103:2019.

Each side schema will carry outer diameter, wall thickness, length, linepipe coating, concrete-weight-coating flag, fluid temperature/exposure, and either field-joint area fraction or field-joint count plus cutback length. `field_joint_coating=None` will be accepted only when FJC area is zero. A nonzero FJC area will require an edition-valid identifier mapped through `FieldJointCoating` (2010) or `FieldJointCoating2019` (2019); the adapter will not reuse the bracelet adapter's 2010-only coercion.

### Frozen input contract

The public model will be a system of one or more independently assessed banks. It will aggregate one overall status but will reject shared/coupled side identifiers; coupled asymmetric multi-bank circuit solving will remain unsupported and will be named in the result limitation.

| Model | Required fields | Optional/default fields |
|---|---|---|
| `AnodeBankDesignInput` | `design_life_years`, non-empty `banks` | `edition="2019"`, `max_anode_count=10000` |
| `BankInput` | `bank_id`, `installed_anode_count`, `structure`, `anode`, non-empty `sides` | none |
| `StructureDemandInput` | `area_m2`, `initial_current_density_A_m2`, `mean_current_density_A_m2`, `final_current_density_A_m2`, `initial_breakdown_factor`, `mean_breakdown_factor`, `final_breakdown_factor` | zero area/densities are permitted; factors remain in `[0,1]`. These are caller-supplied structure-design inputs, not F103 table values. |
| `BankAnodeInput` | `material`, `net_mass_kg`, `length_m`, `density_kg_m3`, `utilisation_factor`, `electrolyte_resistivity_ohm_m` | `surface_temperature_c=10`, `interaction_factor=1`, `cable_resistance_ohm=0` |
| `PipelineSideInput` | `side_id`, `outer_diameter_m`, `wall_thickness_m`, `length_m`, `linepipe_coating`, `exposure`, `fluid_temperature_c` | `steel_resistivity_ohm_m=2e-7`, `concrete_weight_coating=false`, `field_joint_coating=None`, and either `field_joint_area_fraction` or `field_joint_count` plus `field_joint_length_m` |

`0 < utilisation_factor < 1` will be required. Fresh and final equivalent radii will reuse the existing constant-length cylindrical assumption in `anode_sizing.depleted_equivalent_radius`: `r_i=sqrt(m/(pi L rho_a))` and `r_f=sqrt((1-u)m/(pi L rho_a))`. The same formula and assumption will be echoed in results. Unequal anode branches are excluded from this first typed API; every bank will contain identical anodes, and a kernel test will still verify the general reciprocal-resistance helper with unequal branches.

The YAML mapping will be `inputs.design_data`, then `inputs.banks[]` with nested `structure`, `anode`, and `sides[]` blocks using the field names above. `installed_anode_count` will drive the route verdict; the recommended count will be advisory and separately verified.

### Frozen result contract

Top-level results will contain exactly `standard`, `edition`, `provenance`, `design_life_years`, `banks`, `citations`, `formula_references`, `model_limitations`, and `status`. Each bank will contain:

- `bank_id`;
- `current_demand_A` with `structure`, `pipeline`, and `total` initial/mean/final values; pipeline initial demand will use the F103 mean design density with the initial coating factor because F103 provides one design density rather than a separate B401-style initial density;
- `anode_resistance_ohm` with individual, parallel, and total initial/final values plus topology inputs;
- `anode_requirements` with required/installed mass, `count_by_mass`, `count_by_initial_output`, `count_by_final_output`, `count_by_attenuation`, `recommended_anode_count`, `installed_anode_count`, and a recomputed `recommended_count_verification`;
- `sides`, each carrying geometry, linepipe/FJC areas and factors, mean/final demand, `effective_final_breakdown_factor`, longitudinal resistance, exact isolated-bank Eq. (20) protected length, fixed-load extended protected length, far potential/margin/check, and five fixed-length linear-envelope samples;
- `status` with the standard result/governing/reason/checks shape.

The top status will fail if any installed bank fails and will name `bank:<id>/<case>`. A bank will pass only when installed mass, fresh-bank initial output, utilised-end final output, and every final side potential pass. Each check will return a dimensionless adequacy ratio, with `>=1` passing: installed/required mass, available/required initial current, available/required final current, and available driving voltage divided by the side's bank-plus-metallic voltage-drop requirement. A zero requirement will omit that check from governing selection. The smallest ratio will govern on both PASS and FAIL; exact ties will use lexical order of the keys `mass`, `initial_output`, `final_output`, and `attenuation:<side_id>`. System keys will be prefixed `bank:<bank_id>/`.

The nested output skeleton will be:

```yaml
standard: string
edition: string
provenance: string
design_life_years: float
banks:
  - bank_id: string
    current_demand_A:
      structure: {initial: float, mean: float, final: float}
      pipeline: {initial: float, mean: float, final: float}
      total: {initial: float, mean: float, final: float}
    anode_resistance_ohm:
      individual: {initial: float, final: float}
      parallel: {initial: float, final: float}
      total: {initial: float, final: float}
      interaction_factor: float
      cable: float
      group_formula: string
    anode_requirements:
      required_mass_kg: float
      installed_mass_kg: float
      count_by_mass: int
      count_by_initial_output: int
      count_by_final_output: int
      count_by_attenuation: int | null
      recommended_anode_count: int | null
      installed_anode_count: int
      search_outcome: pass | cable_limited_impossible | search_cap_exhausted
      recommended_count_verification: mapping | null
    sides:
      - side_id: string
        geometry_m: mapping
        coating: mapping
        current_demand_A: {initial: float, mean: float, final: float}
        effective_final_breakdown_factor: float
        longitudinal_resistance_ohm_m: float
        f103_eq20_protected_length_m: float
        extended_protected_length_m: float | null
        far_potential_V: float
        protection_margin_V: float
        protection_ok: bool
        potential_envelope: {distance_m: [float], potential_V: [float]}
    check_scores: mapping[str, float]
    status: {result: string, governing_case: string, reason: string, checks: mapping}
citations: list
formula_references: list
model_limitations: list
status: {result: string, governing_case: string, reason: string, checks: mapping,
         use_status: string}
```

Potential-envelope distances will be exactly `[0, 0.25L, 0.5L, 0.75L, L]`.

The sign and root contract will be:

```text
q_metal = pi * D * f'_cf * i_cm
q_bank = I_side_final / L_total = pi * D * (1-g) * f'_cf * i_cm
R'_L = rho_me / (pi * d * (D - d))
E_bank = Ea_closed + I_total_final * R_bank_final
E_far = E_bank + R'_L * (q_metal * L) * L
PASS when E_far <= E_protection

L_fjc = field_joint_area_fraction * L_total
L_linepipe = L_total - L_fjc
r = L_fjc / L_linepipe
I_fixed = I_structure_final + sum(q_j * L_j for j != side)
A = R'_L * q_metal
B = R_bank_final * q_bank
C = Ea_closed + R_bank_final * I_fixed - E_protection
L_protected = (-B + sqrt(B**2 - 4*A*C)) / (2*A)

B_f103 = 2 * R_bank_final * q_metal
C_f103 = Ea_closed - E_protection
L_f103_eq20 = (-B_f103 + sqrt(B_f103**2 - 4*A*C_f103)) / (2*A)
```

`C >= 0`, a negative discriminant, or a non-positive root will be reported as an impossible protection length. `L_f103_eq20` will carry the F103 Eq. (20) citation. `L_protected` will carry the F103 Eq. (18)/(20) basis plus the explicit note `one-side fixed bank load circuit extension`, including when `I_fixed=0`, because its `R q` coefficient is not the standard's `2 R q` topology.

### Citation resolution

`f103_tables.py` will add edition-keyed formula-reference `Citation` factories for the 2010 and 2019 equation/guidance locations. Metadata-only F103:2019 and B401:2017 citation pages (frontmatter plus non-copyrighted locator summaries, no standard text or private locator) will be added beside the existing runtime 2010/2011 pages so default F103 and its contemporaneous individual-resistance citation both resolve without a test override. The bank calculation will call `validate_citation` for every table and formula citation before returning results. Missing/mismatched wiki content will raise `CitationResolutionError`; tests will run an actual default-2019 calculation without an override, resolve both editions against the vendored pages, and exercise the missing-page failure. No filesystem locator will enter YAML or result payloads.

The ideal group formula will be emitted separately from standards citations as `ideal parallel circuit: 1/R_parallel = sum(1/R_a,j)`. Its reference note will state that the individual `R_a` formula is edition-keyed to B401 and that the reciprocal combination is an elementary circuit relation, not a B401 Table 10-7/A-7 equation. F103 Appendix D.8.6 will be cited only for the requirement to account for bank arrangement/interaction, not as the source of the reciprocal identity.

## Files to change

| Action | Path | Purpose |
|---|---|---|
| Create | `src/digitalmodel/cathodic_protection/pipeline_anode_bank.py` | Typed bank/side inputs, result models, design orchestration, citations and governing check |
| Modify | `src/digitalmodel/cathodic_protection/_kernels.py` | Steel area, longitudinal resistance, reciprocal bank resistance, conservative metallic drop, potential and quadratic protected-length formulas |
| Modify | `src/digitalmodel/cathodic_protection/f103_tables.py` | Edition-keyed formula citation factories for Sec. 5.6 / 6.7 / Appendix D guidance |
| Create | `knowledge/wikis/engineering-standards/wiki/standards/dnv-rp-f103-2019.md` | Metadata-only runtime citation target; no licensed standard text |
| Create | `knowledge/wikis/engineering-standards/wiki/standards/dnv-rp-b401-2017.md` | Metadata-only runtime target for the F103:2019 bank's B401 Table A-7 resistance citation |
| Modify | `src/digitalmodel/cathodic_protection/engine_adapter.py` | New route, edition-aware FJC mapping, result schema, and new validation-required use-status token |
| Modify | `src/digitalmodel/cathodic_protection/report_adapters.py` | Bank-specific five-section layout, new use-status wording, and potential envelope figure |
| Modify | `src/digitalmodel/cathodic_protection/__init__.py` | Public bank API and kernel exports |
| Create | `tests/cathodic_protection/test_pipeline_anode_bank.py` | Hand-derived formula/API/regression tests |
| Modify | `tests/cathodic_protection/test_kernels.py` | Formula-level RED/GREEN tests |
| Modify | `tests/cathodic_protection/test_engine_adapter.py` | Route, validation, status and privacy tests |
| Modify | `tests/cathodic_protection/test_report_adapters.py` | Layout, potential figure and governing-status tests |
| Create | `tests/fixtures/cathodic_protection/workflow_inputs/anode_bank.yml` | De-identified, rounded regression input |
| Modify | `docs/domains/cathodic_protection/_index.md` | API/route/equation and use-status documentation |
| Modify | `CHANGELOG.md` | `[Unreleased]` feature line |
| Create | private follow-up outside Git | Source-only comparison and limitations required by the task |

## TDD test list

- Kernel tests will hand-derive steel area, longitudinal resistance, `10 x 0.2 ohm -> 0.02 ohm`, Eq. (15) drop, fixed-length linear potential samples, and the Eq. (20) positive root.
- API tests will hand-derive separate linepipe/FJC and structure initial/mean/final demand, Eq. (11) effective final factor, required mass, initial/final bank resistance, per-side protected length, far potential, counts by mass/initial output/final output/attenuation, and governing PASS/FAIL.
- A one-bank two-side case will verify that the bank drop includes both line sides plus the structure while each side retains its own metallic drop.
- A just-longer side will fail the far-potential criterion and govern; a shorter side will pass.
- Boundary tests will verify `N-1` fails and `N` passes after every resistance, drop, protected-length, and far-potential value is recomputed at each candidate count.
- Edition tests will verify the 2010/2019 equation and B401 resistance labels and cited table differences without changing the physical sign convention. A 2019-only FJC identifier will prove that the new adapter does not coerce it through the 2010 enum.
- The exact isolated-bank quadratic will reproduce F103 Eq. (20) with `2 R q`; the one-side extension will be checked separately with zero and nonzero structure/other-side loads and will always be labelled as an extension.
- Field-joint tests will prove `r=L_FJC/L_linepipe=g/(1-g)` for both area-fraction and count/cutback inputs and will reject `L_FJC >= L_total`.
- FJC tests will accept null coating only at zero area and will require an edition-valid coating identifier at nonzero area, including a 2019-only row.
- Governing tests will exercise the exact adequacy ratios across mass/current/potential units and the lexical tie rule. Result-schema tests will assert the frozen nested keys and the five fixed profile abscissae.
- Search tests will distinguish the `R_cable` asymptotic impossibility from a finite search-cap exhaustion and will not label the latter physically impossible.
- Formula-reference tests will distinguish the elementary reciprocal group relation from the edition-keyed B401 individual-resistance citation.
- Invalid geometry, potential ordering, zero count, invalid interaction factor, and impossible protection length will fail closed.
- Engine/report tests will verify the route key, five-section layout, resistance/demand/profile tables, potential figure, citations, the exact `engineering-validation-required` token and wording, checks, and governing case.
- A repository privacy test will validate the allowlisted fixture schema and assert that no value is an absolute path or source metadata. Before commit, the standard no-absolute-path checker will scan every touched tracked file, and an external private denylist will scan the complete touched-file set for client/project/document identifiers. The denylist and its matched literals will remain outside Git; only the private follow-up will record its PASS/FAIL. The checker tests will use neutral synthetic sentinels so the enforcement artifacts do not trigger themselves.

Every expected numeric value will carry a hand derivation in its test comment. No expected result will be copied from a program run.

## Acceptance criteria

- The route will report pipeline demand plus structure demand for initial, mean, and final cases.
- The result will expose individual and group resistance at fresh and utilised-end geometry, including cable/interaction assumptions.
- Every side will expose protected length, far potential, potential margin, and a criterion-bound PASS/FAIL check (`E_far <= E_protection`).
- Overall status will combine mass, output, and every far-potential check, and will name the governing case.
- F103/B401 cited values will retain `CitedValue` provenance; formula citations will name exact edition clauses/equations.
- Every new citation will validate at calculation time; a no-override default-2019 calculation will resolve both F103:2019 and B401:2017 runtime metadata pages, vendored fixtures will cover explicit resolver tests, and a missing page will fail closed.
- A new `engineering-validation-required` use-status token will mark the cited route as not for client use pending independent engineering validation; it will not reuse the false `legacy-uncited` label. The calculation verdict will remain distinct from use status.
- Scoped cathodic-protection and reporting tests will pass; ruff and mypy will be clean on touched files.
- The private follow-up will be read back and will remain outside Git.
- Plan review and code/artifact cross-review will have no unresolved MAJOR findings.

## Risks and exclusions

- Closely spaced anode interaction is not solved geometrically. The ideal parallel formula will be qualified and an explicit interaction factor/effective cable resistance will be accepted.
- One system result may contain several independently assessed banks and will aggregate their governing status. A two-terminal flowline will use one side segment per bank (typically half-length per symmetric bank). Shared/coupled asymmetric-bank circuits will be rejected and named as unsupported rather than silently approximated.
- Wet storage will be represented by the caller's design life/coating state; a general phase model is outside this issue.
- The private hyperbolic attenuation method will be comparison evidence only because it is not the F103 simplified terminal-bank equation requested here.

## Adversarial review summary

| Provider | Verdict | Findings |
|---|---|---|
| Claude | UNAVAILABLE | CLI absent on this host |
| Codex | MAJOR (inline r1) | Required executable schemas, edition-aware FJC demand/attenuation, an executable use-status token, count-coupled recomputation, exact sign/root semantics, citation validation, fixed topology/depletion assumptions, and explicit multi-bank aggregation limits. |
| Gemini | pending | pending |

**Overall:** r1 MAJOR; the plan has been revised and focused re-review remains required before implementation.
