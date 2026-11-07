"""Reproducible synthetic 12 inch OD pipe, three depths and two sea states.

Every dimensional input and coefficient is an assumed example input, based on
previous experience, as documented in the generated Synthetic input basis.
No example value is a recommended design coefficient or safety-class factor.
"""

from __future__ import annotations

from dataclasses import dataclass

from .assessment import StaticCase, assess_static, render_report, solve_required_weight
from .e305_simplified import (
    Case,
    Pipe,
    WeightResult,
    evaluate_case,
    wave_from_sea_state,
    weight_build_up,
)


@dataclass(frozen=True)
class ExampleRow:
    """Input sea state and simultaneously sampled load/assessment results."""

    depth_m: float
    hs_m: float
    tp_s: float
    wave_velocity_m_s: float
    wave_period_s: float
    phase_rad: float
    static_case: StaticCase
    required_weight_n_m: float
    absolute_utilisation: float


@dataclass(frozen=True)
class WorkedExample:
    """Explicit geometry, weight ledger and the six synthetic cases."""

    pipe: Pipe
    weight: WeightResult
    rows: tuple[ExampleRow, ...]


def build_example() -> WorkedExample:
    """Compute the input basis in the analysis, with no licensed table lookup.

    Numerical controls (720 phases, 1e-6 N/m root resolution, 100 iterations)
    define reproducibility, not design acceptance. Source: analysis A5 and
    Synthetic input basis. Gravity matches the legacy spectral helper contract.
    """
    # All physical numbers below: synthetic assumptions in Synthetic input basis.
    pipe = Pipe(
        0.3048,
        0.0127,
        0,
        7850,
        0.003,
        950,
        0.05,
        3000,
        0,
        0.01,
        1300,
        0,
        0,
        0,
        800,
        0,
        1025,
    )
    gravity = 9.80665  # Existing spectral-helper fixed gravity; knowledge pack §2.
    weight = weight_build_up(pipe, g=gravity)
    rows = []
    for depth in (20, 50, 100):
        for hs, tp in ((2, 8), (4, 12)):
            wave, period = wave_from_sea_state(
                Hs_m=hs,
                Tp_s=tp,
                depth_m=depth,
                elevation_m=weight.hydrodynamic_diameter_m / 2,
                g=gravity,
            )
            # R=Fw=1 explicitly disable empirical reductions/calibration. The
            # recorded gamma_st is also set to unity and is not used by E305.
            kinematic_case = Case(
                "extreme_wave",
                wave,
                0.5,
                period,
                1,
                1,
                1,
                2,
                0.5,
                0.6,
                1.5,
                1,
                "wave_velocity",
            )
            loads = evaluate_case(
                kinematic_case,
                diameter_m=weight.hydrodynamic_diameter_m,
                rho_water_kg_m3=pipe.water_density_kg_m3,
                available_weight_n_m=weight.submerged_weight_n_m,
            )
            static = StaticCase(
                f"depth {depth}, Hs {hs}",
                weight.submerged_weight_n_m,
                abs(loads.drag_n_m + loads.inertia_n_m),
                loads.lift_n_m,
                0.6,
                0,
                1.5,
                "assumed, based on previous experience; synthetic input basis",
            )
            design = solve_required_weight(
                static,
                method="absolute",
                lower_n_m=0,
                upper_n_m=10000,
                tolerance_n_m=1e-6,
            )
            if not design.converged:
                raise ValueError("synthetic weight root did not converge")
            rows.append(
                ExampleRow(
                    depth,
                    hs,
                    tp,
                    wave,
                    period,
                    loads.phase_rad,
                    static,
                    design.weight_n_m,
                    assess_static(static, method="absolute").utilisation,
                )
            )
    return WorkedExample(pipe, weight, tuple(rows))


_BASIS = """
## Synthetic input basis

This is a synthetic 12 inch **outside-diameter** pipeline, rather than a nominal
pipe-size specification. All geometry, material properties, sea states, current,
soil friction and factors are **assumed, based on previous experience**. No real
asset or previous design is represented. These values are not sourced standard
recommendations. Lengths are specified directly in metres.

| Input | Value | Basis |
|---|---:|---|
| Steel OD / wall (mm) | 304.8 / 12.7 | Assumed geometry |
| Corrosion coating / concrete / marine growth (mm) | 3.0 / 50.0 / 10.0 | Assumed layers |
| Steel / corrosion coating density (kg/m³) | 7850.0 / 950.0 | Assumed materials |
| Concrete / growth density (kg/m³) | 3000.0 / 1300.0 | Assumed materials |
| Contents / seawater density (kg/m³) | 800.0 / 1025.0 | Assumed fluids |
| Corrosion allowance / absorption / joint fraction / extra mass | 0.0 | Assumed absent |
| Current at pipe (m/s) | 0.5 | Assumed perpendicular current, no profile reduction |
| Drag / inertia / lift coefficient | 1.0 / 2.0 / 0.5 | Assumed coefficients; no table qualification |
| Friction / safety multiplier | 0.6 / 1.5 | Assumed inputs; no safety-class assignment |
| Passive resistance (N/m) | 0.0 | No passive credit |
| Wave / calibration multipliers | 1.0 / 1.0 | Explicitly no empirical reduction |
| Water depths (m) | 20.0 / 50.0 / 100.0 | Assumed flat seabed at each depth |
| Sea states Hs (m), Tp (s) | 2.0, 8.0 and 4.0, 12.0 | Assumed wave conditions |

Gravity uses the existing spectral helper's fixed conventional value (recorded
in the code and knowledge pack); it is a numerical convention, not an F109
coefficient. The pipe centre is half the grown diameter above the bed.
The existing spectral helper returns significant orbital velocity Uw and
zero-crossing period Tu. This example uses Uw as the deterministic cosine-wave
amplitude in the E305 arithmetic screen. That conversion is an explicit example
convention, **not** a qualified design-wave or return-period transformation.
Period belongs to each sea state separately. The maximum simultaneous absolute
weight demand is sampled over 720 equally spaced phases. The independent
half-step sampling test bounds its numerical error for these examples only.

## Equations and evidence

The following are independent equilibrium identities. Legacy code attributes
similar expressions to F109, but the claimed edition and clauses have not been
verified against an openly readable standard. These equations therefore do not
establish standard compliance.

- **A1 — absolute force screen:** demand = gamma*(H+mu*L), capacity = mu*W+FR;
  H and L are nonnegative peak demands. This follows the current canonical
  absolute check's clause-map basis, with lift factored together with horizontal
  demand. Nonzero FR requires explicit initial_penetration or prior_loading
  provenance. No passive-soil equation is implemented or qualified here.
- **A4 — uplift and contact:** uplift utilisation = gamma*L/W; require W>L for
  bed contact. An arithmetic pass means unrounded demand <= capacity and
  positive contact. Unity is this ratio definition, not an empirical factor.
- **A5 — required fixed-geometry weight:** solve A1/A4 in a supplied bracket
  by bisection using the canonical absolute margin and retaining a feasible
  endpoint. For mu>0, W_required=max(0, (gamma*(H+mu*L)-FR)/mu, gamma*L, L).
  Bed contact W>L is an additional supplied screen, not a clause criterion.
  When contact governs, required weight is a strict lower bound, and zero
  weight margin does not pass. The residual uses this same boundary.
  A precision or iteration limit returns
  the last feasible weight with converged=False. Coating design must rebuild
  diameter, buoyancy and hydrodynamics; this is not a thickness solver.
- **Hydrodynamics:** FD=rho*CD*D*U*abs(U)/2;
  FI=rho*CM*pi*D²*a/4; FL=rho*CL*D*U²/2. Drag and inertia retain direction;
  lift is defined by the supplied coefficient. For the deterministic screen,
  U=Uc+Uw*cos(theta) and a=-Uw*(2*pi/Tu)*sin(theta). Forces are evaluated at the
  same phase; independent maxima of drag, inertia and lift are never added.
  The absolute value of the simultaneous horizontal resultant is constructed
  explicitly as a synthetic demand before calling the peak-only assessment.
  This phase screen does not establish standard extreme-cycle peaks.
- **Weight:** concentric annulus area=pi*(OD²-ID²)/4;
  W=g*(sum(component masses)-rho_water*displaced area). Growth contributes both
  mass and displaced volume; its outer diameter also sets hydrodynamic demand.

Independent hand cases in the tests give FD=1600.0 N/m, FI=30*pi N/m and
FL=400.0 N/m for the stated synthetic input ledger. A separate layered pipe
gives W=153*pi/16 N/m at supplied gravity. The static case W=200.0, H=40.0,
L=10.0, mu=0.5 and gamma=2.0 gives a root of 180.0 N/m without passive
credit, or 160.0 N/m with qualified initial-state FR=10.0 N/m. Every expected
value is derived in the test docstring or independent ledger, without reading implementation constants.

## Interpretation

The assessment exposes only the canonical absolute expression. The former
passive-balance and simplified alternatives used unfactored lift and have been
removed. The compatibility function named generalized now warns and delegates
to the absolute check; it does not implement a displacement envelope.
The six synthetic cases have no passive credit. Their deterministic
phase demands validate arithmetic and do not qualify standard peak selection.

## Source register

| Source | Edition / section | Evidence used and limit |
|---|---|---|
| [Public Morison theory](https://www.orcina.com/webhelp/OrcaFlex/Content/html/Morison%27sequation.htm) | Undated theory page, fixed-body equation; original equation 1950 | Drag and inertia algebra only; no coefficient calibration |
| [University Physics Volume 1, buoyancy](https://openstax.org/books/university-physics-volume-1/pages/14-4-archimedes-principle-and-buoyancy) | 2016, §14.4, Archimedes' Principle and Buoyancy | Displaced-fluid buoyancy; annular geometry derived here |
| [University Physics Volume 1, friction](https://openstax.org/books/university-physics-volume-1/pages/6-2-friction) | 2016, §6.2, Eq 6.1 | Static friction bound mu*N; N=W-L and the chosen demand factors are applied explicitly here, without standard calibration |
| [F109 publisher metadata](https://www.dnv.com/energy/standards-guidelines/dnv-rp-f109-on-bottom-stability-design-of-submarine-pipelines/) | 2021-05, amended 2025-09 | Edition metadata only; clauses and factor tables not exposed |
| [Function claim inventory](../../../../src/digitalmodel/subsea/on_bottom_stability/docs/validation-claims.md) | Current API register; legacy code claims October 2021 | Every public function, equation, claimed clause and qualification status |
| [Knowledge pack](../../../../src/digitalmodel/subsea/on_bottom_stability/docs/agent-pack.md) | Historical pinned inventory with current amendment | Existing spectral-helper and E305 conventions; standard equivalence unresolved |

The four public pages were retrieved on 2026-10-22 with successful HTTP
responses. The originating issue names F109 2007; the code claims October 2021;
the publisher metadata differs. No edition has been silently substituted. No
licensed tables or private spreadsheets were read or reproduced. All load
coefficients, safety multipliers, friction and passive resistance must be supplied
explicitly to the scalar calculation APIs. Exported legacy constants remain
unqualified compatibility symbols and are not automatically consumed.

## Required follow-up and limits of validity

F109 edition/clauses, safety-class factors, standard absolute-method calibration,
generalised displacement envelopes, E305 calibration and full dynamic analysis
are **NOT_EVALUATED**. A positive arithmetic screen is not design acceptance.
Supply a reviewed, applicable edition and source-qualified inputs, reconcile its
actual load and resistance factors, and perform the required displacement and
soil assessments before accepting a design. Failed arithmetic cases require
additional submerged weight, lower environmental demand or a reviewed restraint
design; the reported fixed-force weight is an initial sizing result only.

Scour, embedment evolution, cyclic soil response, sloping beds, trenching,
oblique flow, nonuniform growth, installation transients and wave nonlinearity
are outside this screen. Depth and wave sensitivity here use the existing
linear spectral helper, whose edition-specific calibration is not independently
qualified by this work. A sampled phase grid is not an exact continuous maximum.

## Reproduction

The report is the exact text returned by
`digitalmodel.subsea.on_bottom_stability.example.example_report()`.
The reproducibility test compares that text with this committed file and checks
the subsea documentation link. Report verdicts and governing-case selection use
full precision; dimensional values use one decimal place, dimensionless inputs
retain their supplied precision and utilisation ratios use three decimals.
"""


def example_report() -> str:
    """Render the complete synthetic input basis, six-case table and answer."""
    example = build_example()
    report = render_report(
        [row.static_case for row in example.rows],
        method="absolute",
        report_date="2026-11-07",
        revision="4",
        revision_description="Rank contact-loss cases by severity and explain infinite utilisation with consistent symbols",
        previous_revisions=(("0", "2026-10-22",
                             "Calculation issue with explicit input provenance"),
                            ("1", "2026-11-06",
                             "Align peak checks and report precision with supplied inputs"),
                            ("2", "2026-11-06",
                             "Record strict bed contact and canonical report governors"),
                            ("3", "2026-11-07",
                             "Show governing utilisation and clarify rounded contact displays")),
    )
    report = report.replace(
        "Results are static arithmetic screens.",
        "Synthetic 12 inch OD pipe at three depths and two sea states.\n"
        "Results are static arithmetic screens.",
    )
    sea = [
        "## Sea-state inputs and sampled phase",
        "",
        "Depth, Hs in m; periods in s; Uw in m/s; phase in radians.",
        "",
        "| Depth | Hs | Tp | Uw | Tu | Phase |",
        "|---:|---:|---:|---:|---:|---:|",
    ]
    for row in example.rows:
        sea.append(
            "| "
            + " | ".join(
                f"{v:.1f}"
                for v in (
                    row.depth_m,
                    row.hs_m,
                    row.tp_s,
                    row.wave_velocity_m_s,
                    row.wave_period_s,
                    row.phase_rad,
                )
            )
            + " |"
        )
    report = report.replace("## Inputs", "\n".join(sea) + "\n\n## Inputs", 1)
    comparison = [
        "",
        "## Fixed-force design weights",
        "",
        "Required weight is the fixed-force A1/A4 root in N/m; loads and diameter stay fixed.",
        "",
        "| Case | Required weight | Absolute |",
        "|---|---:|---:|",
    ]
    for row in example.rows:
        comparison.append(
            f"| {row.static_case.label} | {row.required_weight_n_m:.1f} | "
            f"{row.absolute_utilisation:.3f} |"
        )
    return report + "\n".join(comparison) + "\n" + _BASIS
