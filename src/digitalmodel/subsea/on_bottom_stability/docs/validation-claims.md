# Stability function claims and evidence

This register records claims, not standard compliance. The original knowledge
pack is [agent-pack.md](agent-pack.md); its edition discrepancies remain open.
The historical implementation claimed October 2021, the originating request
names 2007, and the
publisher currently lists May 2021 amended September 2025. The canonical absolute API now follows the supplied clause-map basis on main;
this change supplies no licensed table data or qualified factor selection.
Clauses below identify that basis; a populated clause is not evidence of
standard qualification.
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
| `dnv_rp_f109.absolute_stability_check` | max(gamma (FH+mu FL)/(mu W+FR), gamma FL/W); strict contact W>FL | DNV-RP-F109; supplied clause-map basis, edition qualification open | §3.6.2 Eqs. (3.38)–(3.39); passive basis §3.6.5; contact is an additional supplied screen | NOT_EVALUATED |
| `dnv_rp_f109.generalized_stability_check` | Deprecated alias of absolute_stability_check; warns | DNV-RP-F109; supplied clause-map basis | §3.6.2; displacement §3.5 remains unimplemented | NOT_EVALUATED |
| `dnv_rp_f109.hydrodynamic_force_per_meter` | Signed instantaneous Morison FD+FI | Public Morison theory, original equation 1950 | Fixed-body equation; does not generate §3.6.4 peaks | ARITHMETIC_TESTED |
| `dnv_rp_f109.lift_force_per_meter` | rho CL D U² / 2 | Explicit coefficient definition; independent arithmetic | Instantaneous lift; does not generate §3.6.4 peaks | ARITHMETIC_TESTED |
| `dnv_rp_f109.submerged_weight_per_meter` | g(sum(rho A)-rho_water A_displaced); g=9.81 | DNV-RP-F109; supplied clause-map basis | §1.5 and §2.3 | NOT_EVALUATED |
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
| `simplified.check_lateral_stability` | kN to N; lateral_stability_check | DNV-RP-F109; supplied clause-map basis | §3.6.2 Eqs. (3.38)–(3.39) | NOT_EVALUATED |
| `simplified.check_vertical_stability` | gamma FL/W | DNV-RP-F109; supplied clause-map basis | §3.6.2 Eq. (3.39) | NOT_EVALUATED |
| `simplified.drag_force_per_meter` | rho CD D U² / 2 | Generic instantaneous force model; explicit coefficients | Independent arithmetic; no standard peak selection | ARITHMETIC_TESTED |
| `simplified.hydrodynamic_loads` | signed FD+FI; FL | Generic instantaneous force model; explicit coefficients | Independent arithmetic; no standard peak selection | ARITHMETIC_TESTED |
| `simplified.inertia_force_per_meter` | rho CM pi D² a / 4 | Generic instantaneous force model; explicit coefficients | Independent arithmetic; no standard peak selection | ARITHMETIC_TESTED |
| `simplified.lateral_stability_check` | absolute_stability_check and absolute_stability_margin; FR=0 | DNV-RP-F109; supplied clause-map basis | §3.6.2 Eqs. (3.38)–(3.39) | NOT_EVALUATED |
| `simplified.lift_force_per_meter` | rho CL D U² / 2 | Generic instantaneous force model; explicit coefficients | Independent arithmetic; no standard peak selection | ARITHMETIC_TESTED |
| `simplified.submerged_weight` | g(sum(rho A)-rho_water A_displaced); g=9.81 | DNV-RP-F109; supplied clause-map basis | §1.5 and §2.3 | NOT_EVALUATED |
| `assessment.assess_static` | Canonical absolute check, supplied passive basis and separate contact | Independent arithmetic; canonical supplied clause-map basis | Analysis A1/A4; standard qualification open | ARITHMETIC_TESTED |
| `assessment.solve_required_weight` | Feasible-endpoint bisection; canonical absolute margin | Independent arithmetic, 2026 | Analysis A5; fixed forces and resistance | ARITHMETIC_TESTED |
| `assessment.render_report` | SI inputs, canonical governor, unrounded verdict and supplied revision description | Repository report contract, 2026 | Analysis report contract; no normative clause applies | CONTRACT_TESTED |
| `example.build_example` | Layer ledger, spectral kinematics, sampled A1 demand and A5 root | Synthetic input basis, 2026; legacy helper unqualified | Analysis A1/A4/A5; F105 helper clause unresolved | ARITHMETIC_TESTED |
| `example.example_report` | Deterministic six-case report | Repository report contract, 2026 | Analysis reproduction contract; no normative clause | CONTRACT_TESTED |

| `dnv_rp_f109.transverse_current_at_height` | Transverse log-profile velocity | DNV-RP-F109; supplied clause-map basis, qualification open | §3.4.2 Eq. (3.2) | NOT_EVALUATED |
| `dnv_rp_f109.diameter_mean_current` | Analytic mean transverse log-profile velocity | DNV-RP-F109; supplied clause-map basis, qualification open | §3.4.2 Eq. (3.3) | NOT_EVALUATED |
| `dnv_rp_f109.load_reduction_factor` | Product of supplied direction-specific reductions | DNV-RP-F109; supplied clause-map basis, qualification open | §3.4.5 Eq. (3.17) | NOT_EVALUATED |
| `dnv_rp_f109.absolute_peak_loads` | Supplied peak coefficients times dynamic pressure and reductions | DNV-RP-F109; supplied clause-map basis, qualification open | §3.6.4 Eqs. (3.40)–(3.41) | NOT_EVALUATED |
| `dnv_rp_f109.low_kc_horizontal_coefficient` | Inverse KC below a supplied anchor | DNV-RP-F109; supplied clause-map basis, qualification open | §3.6.4; explicit Table 3-9 anchor | NOT_EVALUATED |
| `dnv_rp_f109.absolute_stability_margin` | max(0, (gamma (FH+mu FL)-FR)/mu, gamma FL, FL); contact bound is strict | DNV-RP-F109; supplied clause-map basis, qualification open | §3.6.2 Eqs. (3.38)–(3.39); contact is an additional supplied screen | NOT_EVALUATED |
| `e305_evidence.canonical_input_basis` | Canonical explicit parity Pipe and Case inputs | Repository evidence schema v1 | Data contract; no standard clause applies | CONTRACT_TESTED |
| `e305_evidence.basis_digest` | SHA-256 of validated canonical input basis | Repository evidence schema v1 | Data contract; no standard clause applies | CONTRACT_TESTED |
| `e305_evidence.write_evidence` | Validate identities and retained output bytes before exclusive sidecar write | Repository evidence schema v1 | Data contract; no standard clause applies | CONTRACT_TESTED |
| `e305_evidence.verify_evidence` | Offline input identity and retained byte-integrity check | Repository evidence schema v1 | Data contract; no standard clause applies | CONTRACT_TESTED |

## Evidence boundaries

- Morison drag/inertia algebra: [public theory documentation](https://www.orcina.com/webhelp/OrcaFlex/Content/html/Morison%27sequation.htm),
  “Morison's equation”, fixed-body equation, undated; retrieved 2026-10-22.
  Coefficients are supplied assumptions, not validated table values.
- Coulomb friction: [University Physics Volume 1](https://openstax.org/books/university-physics-volume-1/pages/6-2-friction),
  2016, §6.2, Eq 6.1; retrieved 2026-10-22. The normal force is W-L here;
  canonical absolute screening factors both horizontal demand and lift;
  no passive-soil calibration is inferred.
- Geometry, lift definition and application of the Coulomb balance: derivations in the
  [analysis](../../../../../docs/domains/subsea/pipeline/on-bottom-stability-analysis.md).
  A resistance supplied in N/m validates force balance, not a passive-soil law.
- [Publisher metadata](https://www.dnv.com/energy/standards-guidelines/dnv-rp-f109-on-bottom-stability-design-of-submarine-pipelines/),
  retrieved 2026-10-22, identifies the edition but exposes no factor table.
- E305 calibration, standard generalised displacement envelopes, dynamic time
  histories, and safety-class factors: `NOT_EVALUATED`. Supply a reviewed edition,
  clause and factors before a normative design assessment.
