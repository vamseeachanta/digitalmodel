# Stability function claims and evidence

This register records claims, not standard compliance. The original knowledge
pack is [agent-pack.md](agent-pack.md); its edition discrepancies remain open.
The code claims October 2021, the originating request names 2007, and the
publisher currently lists May 2021 amended September 2025. No clause mapping or
safety-class table was obtained from an openly readable standard. Clauses below
label legacy claims explicitly; a populated clause is not evidence of accuracy.
`NOT_EVALUATED` means normative equivalence or external-data validation remains
unestablished, even where independent arithmetic tests pass. Unverified coefficient and safety-factor defaults have been removed; exported
legacy constants are not independently qualified. No private workbook
is used by this validation. Fixture conversion remains inventoried as an API.

## Claim table

Every locally defined public function and public method has one row. Constructors
and private helpers are excluded. The coverage test compares this table to the AST
and fails for missing rows or empty equation, edition/reference, clause or status.

| Function | Equation or contract | Edition/reference | Clause or applicability | Status |
|---|---|---|---|---|
| `dnv_rp_f109.absolute_stability_check` | gamma (abs(FH)+mu FL)/(mu W) | DNV-RP-F109; October 2021 claimed by code, unresolved | §4.3.1, Eq 4.1 (claimed only) | NOT_EVALUATED |
| `dnv_rp_f109.generalized_stability_check` | gamma abs(FH)/(mu (W-FL)+FR) | DNV-RP-F109; October 2021 claimed by code, unresolved | §4.3.2, Eq 4.5 (claimed only) | NOT_EVALUATED |
| `dnv_rp_f109.hydrodynamic_force_per_meter` | FD+FI | DNV-RP-F109; October 2021 claimed by code, unresolved | §3.2.1, Eq 3.1 (claimed only) | NOT_EVALUATED |
| `dnv_rp_f109.lift_force_per_meter` | rho CL D U² / 2 | DNV-RP-F109; October 2021 claimed by code, unresolved | §3.2.1, Eq 3.2 (claimed only) | NOT_EVALUATED |
| `dnv_rp_f109.submerged_weight_per_meter` | g(sum(rho A)-rho_water A_displaced) | DNV-RP-F109; October 2021 claimed by code, unresolved | §2.2 (claimed only) | NOT_EVALUATED |
| `e305_fixture.compare_fixture` | abs(actual-expected)/abs(expected); special zero target | Repository fixture schema v1 | Data contract; no standard clause applies | NOT_EVALUATED |
| `e305_fixture.convert_workbook` | cached cells only; bind read bytes to digests | Repository fixture schema v1 | Data contract; no standard clause applies | NOT_EVALUATED |
| `e305_fixture.load_fixture` | read JSON then validate_fixture | Repository fixture schema v1 | Data contract; no standard clause applies | NOT_EVALUATED |
| `e305_fixture.validate_fixture` | schema, both roles, source digests | Repository fixture schema v1 | Data contract; no standard clause applies | NOT_EVALUATED |
| `e305_simplified.diameter_averaged_current` | integral ln(1+z/z0) dz / (D ln(1+href/z0)) | DNV-RP-E305; 1988 claimed, equivalence unresolved | Appendix A (equivalence unverified) | NOT_EVALUATED |
| `e305_simplified.dimensionless_ratios` | K=Uw T/D; M=Uc/Uw | DNV-RP-E305; 1988 claimed, equivalence unresolved | Simplified Static Stability Analysis (claimed title) | NOT_EVALUATED |
| `e305_simplified.evaluate_case` | max_phase(target Fw (abs(FD+FI)/mu+FL)) | DNV-RP-E305; 1988 claimed, equivalence unresolved | Simplified Static Stability Analysis (claimed title) | NOT_EVALUATED |
| `e305_simplified.inverse_concrete_thickness` | bisect available-required; retain feasible endpoint | DNV-RP-E305; 1988 claimed, equivalence unresolved | Numerical bisection; no standard clause applies | NOT_EVALUATED |
| `e305_simplified.wave_from_sea_state` | spectral integral via free_span.wave_velocity | DNV-RP-E305; 1988 claimed, equivalence unresolved | F105 §3.4, 2006/2017 claimed by delegated helper | NOT_EVALUATED |
| `e305_simplified.weight_build_up` | length-weighted annuli; g(m-rho_water A) | DNV-RP-E305; 1988 claimed, equivalence unresolved | Geometry; no standard clause established | NOT_EVALUATED |
| `e305_tables.TableSet.lookup` | multilinear interpolation, bounded axes | DNV-RP-E305; 1988 claimed, equivalence unresolved | External citation section required; no embedded coefficients | NOT_EVALUATED |
| `e305_tables.load_tables` | schema and source digest validation | DNV-RP-E305; 1988 claimed, equivalence unresolved | External citation section required; E305 1988 only | NOT_EVALUATED |
| `e305_workflow.router` | two distinct roles per segment; evaluate_case | DNV-RP-E305; 1988 claimed, equivalence unresolved | Simplified Static Stability Analysis (inherited claim) | NOT_EVALUATED |
| `simplified.StabilityResult.utilisation` | utilisation = utilization | Repository API; current revision | Result spelling alias; no standard clause applies | CONTRACT_TESTED |
| `simplified.VerticalStabilityResult.utilisation` | utilisation = utilization | Repository API; current revision | Result spelling alias; no standard clause applies | CONTRACT_TESTED |
| `simplified.check_lateral_stability` | kN to N; lateral_stability_check | DNV-RP-F109; October 2021 claimed by code, unresolved | §4.3.2 (inherited claim) | NOT_EVALUATED |
| `simplified.check_vertical_stability` | gamma FL/W | DNV-RP-F109; October 2021 claimed by code, unresolved | §3.4 (manifest claim only) | NOT_EVALUATED |
| `simplified.drag_force_per_meter` | rho CD D U² / 2 | DNV-RP-F109; October 2021 claimed by code, unresolved | §3.2.1 (inherited claim) | NOT_EVALUATED |
| `simplified.hydrodynamic_loads` | signed FD+FI; FL | DNV-RP-F109; October 2021 claimed by code, unresolved | §3.2.1 (inherited claim) | NOT_EVALUATED |
| `simplified.inertia_force_per_meter` | rho CM pi D² a / 4 | DNV-RP-F109; October 2021 claimed by code, unresolved | §3.2.1 (inherited claim) | NOT_EVALUATED |
| `simplified.lateral_stability_check` | gamma abs(FH)/(mu (W-FL)); FR=0 | DNV-RP-F109; October 2021 claimed by code, unresolved | §4.3.2 (inherited claim) | NOT_EVALUATED |
| `simplified.lift_force_per_meter` | rho CL D U² / 2 | DNV-RP-F109; October 2021 claimed by code, unresolved | §3.2.1 (inherited claim) | NOT_EVALUATED |
| `simplified.submerged_weight` | g(sum(rho A)-rho_water A_displaced) | DNV-RP-F109; October 2021 claimed by code, unresolved | §2.2 (inherited claim) | NOT_EVALUATED |
| `assessment.assess_static` | A1–A4 demand/capacity and contact | Independent equilibrium derivation, 2026 | Analysis A1–A4; no normative clause established | ARITHMETIC_TESTED |
| `assessment.solve_required_weight` | Feasible-endpoint bisection with fixed forces | Independent equilibrium derivation, 2026 | Analysis A5; numerical method, no normative clause | ARITHMETIC_TESTED |
| `assessment.render_report` | SI inputs, unrounded verdict and governing case | Repository report contract, 2026 | Analysis report contract; no normative clause applies | CONTRACT_TESTED |
| `example.build_example` | Layer ledger, spectral kinematics, sampled A1 demand and A5 root | Synthetic input basis, 2026; legacy helper unqualified | Analysis A1–A5; F105 helper clause unresolved | ARITHMETIC_TESTED |
| `example.example_report` | Deterministic six-case report | Repository report contract, 2026 | Analysis reproduction contract; no normative clause | CONTRACT_TESTED |

## Evidence boundaries

- Morison drag/inertia algebra: [public theory documentation](https://www.orcina.com/webhelp/OrcaFlex/Content/html/Morison%27sequation.htm),
  “Morison's equation”, fixed-body equation, undated; retrieved 2026-10-22.
  Coefficients are supplied assumptions, not validated table values.
- Coulomb friction: [University Physics Volume 1](https://openstax.org/books/university-physics-volume-1/pages/6-2-friction),
  2016, §6.2, Eq 6.1; retrieved 2026-10-22. The normal force is W-L here;
  no standard safety-factor placement or passive-soil calibration is inferred.
- Geometry, lift definition and application of the Coulomb balance: derivations in the
  [analysis](../../../../../docs/domains/subsea/pipeline/on-bottom-stability-analysis.md).
  A resistance supplied in N/m validates force balance, not a passive-soil law.
- [Publisher metadata](https://www.dnv.com/energy/standards-guidelines/dnv-rp-f109-on-bottom-stability-design-of-submarine-pipelines/),
  retrieved 2026-10-22, identifies the edition but exposes no factor table.
- E305 calibration, standard generalised displacement envelopes, dynamic time
  histories, and safety-class factors: `NOT_EVALUATED`. Supply a reviewed edition,
  clause and factors before a normative design assessment.
