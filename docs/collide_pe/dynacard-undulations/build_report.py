# ABOUTME: Builds the rod-pump surface-card report from the standard CalcReport model.
# ABOUTME: Supplies structured data only -- section order and markup come from the house standard.
"""Rod-pump surface-card analysis report, AceEngineer standard format.

This script contains **no HTML**. It fills the typed slots of
:class:`digitalmodel.reporting.CalcReport`, which owns the house section order,
equation-card markup and provenance colouring. Any other calculation reports the
same way, which is the point.

Usage:
    python build_report.py        # writes HTML + PDF beside this script
"""

import subprocess
import sys
from pathlib import Path

from digitalmodel.reporting import (
    CalcReport, Confidence, DataRow, DesignDataGroup, Equation, KPI,
    MethodBlock, Objective, Reference, ResultBlock, ValidationItem,
    VariableDef, WayForwardStage,
)

HERE = Path(__file__).resolve().parent
OUT_HTML = HERE / "dynacard-rod-pump-report.html"
OUT_PDF = HERE / "AceEngineer-dynacard-rod-pump-report.pdf"

FRAC = '<span class="frac"><span class="n">{n}</span><span class="d">{d}</span></span>'


def rows(*triples):
    return [DataRow(label=a, value=b, unit=c) for a, b, c in triples]


report = CalcReport(
    report_id="AE-AL-2026-001",
    title="Rod-pump surface-card analysis",
    discipline="Artificial Lift",
    revision="A",
    preliminary=True,
    lede=(
        "Why a 4,200 ft rod string produces load undulations on the surface card, "
        "what the card confirms about the well, and what is required to progress to "
        "a downhole pump diagnosis. <strong>Preliminary results</strong> &mdash; "
        "kinematics and card integrity are complete; pump diagnosis is pending data."
    ),
    kpis=[
        KPI(value="4.56", unit="predicted", confidence=Confidence.VALIDATED,
            caption="Undulations per half stroke &mdash; 4 observed"),
        KPI(value="1.03", unit="s", confidence=Confidence.VALIDATED,
            caption="Load-peak interval, 60/N&#8338;&prime;"),
        KPI(value="38.8", unit="bfpd", confidence=Confidence.ANALYTICAL,
            caption="Displacement on plunger stroke"),
        KPI(value="2,412", unit="lb",
            caption="Unexplained load datum excess"),
    ],

    objective=Objective(
        purpose=(
            "Quantify the rod-string free-vibration response visible on the surface "
            "dynamometer card, verify the card against first principles, and identify "
            "the data required to progress to a downhole pump diagnosis."
        ),
        scope=[
            "Scope: surface-card kinematics and card integrity",
            "Method: API RP 11L; SPE 18189",
            "Status: preliminary",
        ],
        preliminary_note=(
            "These are preliminary results. The kinematic and card-metric findings are "
            "complete and stand on their own. The pump diagnosis does not: it requires "
            "inputs not yet available, and the diagnostic classifier behind it has not "
            "been calibrated against field cards with known outcomes. Sections are "
            "colour-coded by provenance throughout."
        ),
    ),

    design_data=[
        DesignDataGroup(caption="Pumping unit", rows=rows(
            ("Surface stroke <var>S</var>", "41", "in"),
            ("Pumping speed <var>N</var>", "6.4", "SPM"),
            ("Prime mover", "Arrow C-66 (13 hp)", ""),
            ("Gearbox rating", "not supplied", ""))),
        DesignDataGroup(caption="Downhole", rows=rows(
            ("Pump setting depth <var>D</var>", "4,300", "ft"),
            ("Plunger diameter", "1.25", "in"),
            ("Pump type", "top-hold-down insert", ""),
            ("Rod string <var>L</var>", "4,200 ft of 3/4 in steel", ""),
            ("Taper", "single, <var>F<sub>c</sub></var> = 1.000", ""))),
        DesignDataGroup(caption="Operating", rows=rows(
            ("Tubing pressure", "150", "psi"),
            ("Casing pressure", "25", "psi"),
            ("Production, previous day", "23 oil / 0 water", "bbl"),
            ("Runtime", "not supplied", "h/d"))),
    ],

    assumptions=[
        DesignDataGroup(caption="Assumed values", rows=rows(
            ("Fluid specific gravity", "0.85", "-"),
            ("Fluid level above pump", "4,300 (submerged)", "ft"),
            ("Plunger overtravel", "0 (conservative)", "in"),
            ("Formation volume factor <var>B<sub>o</sub></var>", "unknown", "-"))),
        DesignDataGroup(caption="Card digitisation", rows=rows(
            ("Source", "screenshot from thread", ""),
            ("Load resolution", "&plusmn;40", "lb"),
            ("Position resolution", "&plusmn;0.3", "in"),
            ("Samples", "80 per branch", ""))),
    ],

    methodology=[
        MethodBlock(
            heading="Rod-string free vibration",
            prose=("The rod string is an elastic bar fixed at the pump and free at "
                   "the polished rod. It rings at its own natural frequency, "
                   "independently of pumping speed."),
            equations=[
                Equation(
                    markup='<var>N<sub>o</sub></var> <span class="op">=</span> '
                           + FRAC.format(n="245,000", d="<var>L</var>"),
                    variables=[
                        VariableDef(symbol="N<sub>o</sub>",
                                    description="Undamped natural frequency", unit="SPM"),
                        VariableDef(symbol="L", description="Rod string length", unit="ft"),
                    ],
                    note=("The constant 245,000 is 15c for steel. Computing from wave "
                          "speed directly gives 58.21 SPM against 58.33 SPM from the API "
                          "constant &mdash; 0.20% agreement, which the implementation "
                          "asserts rather than assumes. A non-steel rod trips that check."),
                ),
                Equation(
                    markup='<var>&Delta;t</var> <span class="op">=</span> '
                           + FRAC.format(n="60", d="<var>N<sub>o</sub>&prime;</var>"),
                    variables=[
                        VariableDef(symbol="&Delta;t",
                                    description="Interval between load peaks", unit="s"),
                        VariableDef(symbol="N<sub>o</sub>&prime;",
                                    description="Taper-adjusted natural frequency",
                                    unit="SPM"),
                    ]),
                Equation(
                    markup='<var>n</var> <span class="op">=</span> '
                           + FRAC.format(n="0.5",
                                         d="<var>N</var> / <var>N<sub>o</sub>&prime;</var>"),
                    variables=[
                        VariableDef(symbol="n",
                                    description="Undulations per half stroke", unit="-"),
                        VariableDef(symbol="N", description="Pumping speed", unit="SPM"),
                    ]),
            ]),
        MethodBlock(
            heading="Loads and plunger travel",
            equations=[
                Equation(
                    markup='<var>F<sub>o</sub></var> <span class="op">=</span> 0.433 '
                           '<var>SG</var> <var>D</var> <var>A<sub>p</sub></var> '
                           '<span class="op">+</span> (<var>P<sub>t</sub></var> '
                           '<span class="op">&minus;</span> <var>P<sub>c</sub></var>) '
                           '<var>A<sub>p</sub></var>',
                    variables=[
                        VariableDef(symbol="F<sub>o</sub>",
                                    description="Fluid load on plunger", unit="lb"),
                        VariableDef(symbol="A<sub>p</sub>",
                                    description="Plunger area", unit="in&sup2;"),
                    ]),
                Equation(
                    markup='<var>K<sub>r</sub></var> <span class="op">=</span> '
                           + FRAC.format(n="1", d="<var>E<sub>r</sub></var> <var>L</var>")
                           + ' <span class="op">,</span> <var>E<sub>r</sub></var> '
                             '<span class="op">=</span> '
                           + FRAC.format(n="12",
                                         d="<var>A<sub>r</sub></var> <var>E</var>"),
                    variables=[
                        VariableDef(symbol="K<sub>r</sub>",
                                    description="Rod string spring rate", unit="lb/in"),
                        VariableDef(symbol="E<sub>r</sub>",
                                    description="Rod elastic constant", unit="in/lb/ft"),
                    ]),
                Equation(
                    markup='<var>S<sub>p</sub></var> <span class="op">=</span> '
                           '<var>S</var> <span class="op">&minus;</span> '
                           + FRAC.format(n="<var>F<sub>o</sub></var>",
                                         d="<var>K<sub>r</sub></var>")
                           + ' <span class="op">+</span> <var>S<sub>ot</sub></var>',
                    variables=[
                        VariableDef(symbol="S<sub>p</sub>",
                                    description="Plunger stroke", unit="in"),
                        VariableDef(symbol="S<sub>ot</sub>",
                                    description="Overtravel", unit="in"),
                    ],
                    note=("Rod stretch under fluid load shortens plunger travel. This is "
                          "the step most often skipped, and skipping it biases "
                          "displacement high.")),
                Equation(
                    markup='<var>PD</var> <span class="op">=</span> 0.1484 '
                           '<var>A<sub>p</sub></var> <var>S<sub>p</sub></var> <var>N</var>',
                    variables=[
                        VariableDef(symbol="PD",
                                    description="Theoretical pump displacement",
                                    unit="bfpd"),
                    ]),
            ]),
        MethodBlock(
            heading="Surface-to-downhole conversion",
            prose=("The downhole card is obtained by marching the damped wave equation "
                   "down the rod string and rebuilding load from the strain field. Load "
                   "is never inherited from the surface: an affine rescale would preserve "
                   "every vibration harmonic and could not diagnose anything."),
            equations=[
                Equation(
                    markup='<var>F</var> <span class="op">=</span> <var>EA</var> '
                           + FRAC.format(n="&part;<var>u</var>", d="&part;<var>x</var>"),
                    variables=[
                        VariableDef(symbol="F", description="Load at depth", unit="lb"),
                        VariableDef(symbol="u",
                                    description="Axial displacement field", unit="in"),
                        VariableDef(symbol="EA",
                                    description="Local axial stiffness", unit="lb"),
                    ]),
            ]),
    ],

    results=[
        ResultBlock(
            heading="Rod-string response &mdash; the undulation question",
            confidence=Confidence.VALIDATED,
            groups=[
                DesignDataGroup(caption="Computed", rows=rows(
                    ("Natural frequency <var>N<sub>o</sub></var>", "58.33", "SPM"),
                    ("Peak interval <var>&Delta;t</var>", "1.029", "s"),
                    ("Speed ratio <var>N</var>/<var>N<sub>o</sub>&prime;</var>", "0.110", "-"),
                    ("Predicted undulations <var>n</var>", "4.56", "per half stroke"))),
                DesignDataGroup(caption="Observed on card", rows=rows(
                    ("Clear load humps, upstroke", "4 + 1 partial", ""),
                    ("Agreement", "within digitisation error", ""),
                    ("Regime", "wave-dominated", ""),
                    ("Interpretation", "expected behaviour", ""))),
            ],
            caption=("The undulations are the rod string ringing at its own frequency. "
                     "They crowd toward the top of the card because polished-rod velocity "
                     "approaches zero there, compressing a fixed time interval into a "
                     "shorter distance &mdash; the card plots position, the ringing is "
                     "periodic in time. At this speed their absence would be surprising.")),
        ResultBlock(
            heading="Card metrics",
            confidence=Confidence.VALIDATED,
            groups=[
                DesignDataGroup(caption="Loads", rows=rows(
                    ("PPRL", "12,438", "lb @ 7.5 in"),
                    ("MPRL", "9,274", "lb @ 35.5 in"),
                    ("Load range", "3,164", "lb"),
                    ("Rod weight in air <var>W<sub>r</sub></var>", "6,863", "lb"))),
                DesignDataGroup(caption="Work and power", rows=rows(
                    ("Card area", "69,950", "lb-in"),
                    ("Work per stroke", "5,829", "ft-lb"),
                    ("Polished-rod power", "1.13", "hp"),
                    ("Spring rate <var>K<sub>r</sub></var>", "271.7", "lb/in"))),
            ],
            caption="Independent of every assumption in section 2."),
        ResultBlock(
            heading="Load datum anomaly",
            confidence=Confidence.ANALYTICAL,
            prose=("The minimum polished-rod load exceeds the weight of the entire rod "
                   "string hanging in air by 2,412 lb. This has no mechanical "
                   "explanation. On the downstroke friction acts upward, resisting the "
                   "descending rods, which lowers polished-rod load &mdash; friction "
                   "widens a card, it cannot lift one."),
            groups=[
                DesignDataGroup(caption="Comparison", rows=rows(
                    ("MPRL measured", "9,274", "lb"),
                    ("Rod weight in air", "6,863", "lb"),
                    ("Buoyed rod weight", "6,116", "lb"),
                    ("Unexplained excess", "2,412", "lb"))),
                DesignDataGroup(caption="Candidate causes", rows=rows(
                    ("Rod string heavier than recorded", "eliminated", ""),
                    ("Load-cell zero or scale offset", "remaining candidate", ""),
                    ("Effect on load differences", "none &mdash; offset cancels", ""),
                    ("Effect on absolute loads", "not usable until resolved", ""))),
            ],
            caption=("Recommend checking whether the cell was zeroed with the rods "
                     "hanging rather than unloaded.")),
        ResultBlock(
            heading="Displacement and efficiency",
            confidence=Confidence.ANALYTICAL,
            groups=[
                DesignDataGroup(caption="Carrying assumed SG and fluid level", rows=rows(
                    ("Fluid load <var>F<sub>o</sub></var>", "2,096", "lb"),
                    ("Rod stretch", "7.7", "in"),
                    ("Plunger stroke <var>S<sub>p</sub></var>", "33.3", "in"),
                    ("Displacement <var>PD</var>", "38.8", "bfpd"))),
                DesignDataGroup(caption="Sensitivity", rows=rows(
                    ("SG 0.80&ndash;0.90, level 3,000&ndash;4,300 ft", "38.3 &ndash; 41.7", "bfpd"),
                    ("Spread", "&plusmn;4", "%"),
                    ("Conclusion", "assumptions not limiting", ""),
                    ("Volumetric efficiency", "not determinable", ""))),
            ],
            caption=("Efficiency is not reported. A unit cycling on a pump-off controller "
                     "produces reduced volume with a healthy pump, indistinguishable from "
                     "low fillage on daily volume alone. Using the 41 in surface stroke "
                     "instead of the 33.3 in plunger stroke would overstate displacement "
                     "at 47.8 bfpd.")),
    ],

    validation=[
        ValidationItem(
            claim="Rod-string kinematics and undulation count",
            basis=("API RP 11L; two independent routes to N&#8338; agree to 0.20%. "
                   "Depends only on stroke, depth, speed and the card."),
            confidence=Confidence.VALIDATED),
        ValidationItem(
            claim="Card metrics &mdash; PPRL, MPRL, area, polished-rod power",
            basis="Read directly from the digitised card; independent of all assumptions.",
            confidence=Confidence.VALIDATED),
        ValidationItem(
            claim="Surface-to-downhole solver",
            basis=("SPE 18189 implementation reproduces reference downhole cards at 0.9% "
                   "median normalised RMSE, correlation 1.000, across five validation "
                   "wells including deviated hole."),
            confidence=Confidence.VALIDATED),
        ValidationItem(
            claim="Plunger stroke and theoretical displacement",
            basis=("Carries assumed fluid SG and level. Sensitivity sweep bounds the "
                   "effect at &plusmn;4%."),
            confidence=Confidence.ANALYTICAL),
        ValidationItem(
            claim="Absolute polished-rod loads",
            basis=("Suspended pending resolution of the 2,412 lb datum anomaly. Load "
                   "differences remain usable."),
            confidence=Confidence.PENDING),
        ValidationItem(
            claim="Volumetric efficiency",
            basis="Requires runtime hours per day and formation volume factor. Not assumed.",
            confidence=Confidence.PENDING),
        ValidationItem(
            claim="Pump condition diagnosis",
            basis=("Requires Tier 2 data for a downhole card. Separately, the diagnostic "
                   "classifier is calibrated on synthetic cards only and has not been "
                   "scored against field cards with known outcomes &mdash; its labels are "
                   "not yet reportable."),
            confidence=Confidence.PENDING),
    ],

    way_forward=[
        WayForwardStage(
            heading="Stage A &mdash; card integrity",
            required=rows(
                ("Load-cell make, model, calibration date", "outstanding", ""),
                ("Runtime, hours per day", "outstanding", ""),
                ("Rod string as run", "answered", "&#10003;")),
            returns=rows(
                ("Resolved load datum", "absolute loads usable", ""),
                ("Honest production check", "efficiency with runtime", ""),
                ("Effort", "two questions", ""))),
        WayForwardStage(
            heading="Stage B &mdash; downhole card and diagnosis",
            required=rows(
                ("Tubing ID and anchor status", "outstanding", ""),
                ("Fluid level above pump", "outstanding", ""),
                ("Oil gravity and <var>B<sub>o</sub></var>", "outstanding", ""),
                ("Viscosity or bottomhole temperature", "outstanding", "")),
            returns=rows(
                ("Calculated downhole pump card", "the open question", ""),
                ("Pump fillage", "measured, not assumed", ""),
                ("Condition separation", "pump-off / gas / wear", ""),
                ("Solver status", "ready &mdash; not blocked on code", ""))),
        WayForwardStage(
            heading="Stage C &mdash; mechanical loading",
            prose=("Requires the pumping unit's API designation and its API 11E geometry "
                   "sheet, plus the motor nameplate. Returns gearbox torque against the "
                   "unit's rating, counterbalance condition, rod loading and buckling, and "
                   "power consumption. Note the C-66 is an Arrow Engine natural-gas prime "
                   "mover, not a gearbox rating, so no torque limit is available.")),
        WayForwardStage(
            heading="Better data beats more data",
            prose=("A raw controller export &mdash; .dyn or timestamped CSV &mdash; "
                   "outranks several Stage B items. Timing read off a position-axis card "
                   "carries &plusmn;0.11 to &plusmn;0.15 s at &plusmn;1.5 in digitisation "
                   "error, because rod velocity vanishes at both stroke ends. At that "
                   "uncertainty the apparent peak spacings of 1.24 s and 0.95 s are not "
                   "distinguishable from each other, nor from the predicted 1.03 s.")),
        WayForwardStage(
            heading="Diagnostic calibration &mdash; open initiative",
            prose=("The diagnostic layer requires field cards with known outcomes to "
                   "become trustworthy. A card alone contributes little; the pulling or "
                   "workover report is the informative half. Highest value: cards paired "
                   "with what was found on the pull, before-and-after pairs on one well, "
                   "and confirmed healthy cards &mdash; the last being scarcer than "
                   "failures and the baseline everything else is measured against. "
                   "Contributed data is analysed and returned at no charge, the work is "
                   "open source with no licence dependency, and early contributors are "
                   "supported for life.")),
    ],

    references=[
        Reference(text="API RP 11L &mdash; Recommended Practice for Design Calculations "
                       "for Sucker Rod Pumping Systems."),
        Reference(text="API 11E &mdash; Specification for Pumping Units."),
        Reference(text="Everitt, T.A. and Jennings, J.W., &ldquo;An Improved "
                       "Finite-Difference Calculation of Downhole Dynamometer Cards for "
                       "Sucker-Rod Pumps&rdquo;, SPE 18189, SPE Production Engineering."),
        Reference(text="Gibbs, S.G., &ldquo;Predicting the Behavior of Sucker-Rod Pumping "
                       "Systems&rdquo;, JPT, 1963."),
        Reference(text="Rowlan, O.L. (Echometer), &ldquo;Over Travel Occurs on Both the "
                       "Upstroke and Down Stroke&rdquo;, Sucker Rod Pumping Workshop / "
                       "SWPSC."),
        Reference(text="Source data: operator-reported well parameters and surface "
                       "dynamometer card, Collide &ldquo;Dynamometer Discussions&rdquo; "
                       "thread, July 2026. Rod string confirmed by the operator "
                       "27 July 2026."),
        Reference(text="Implementation and validation data, public: "
                       "github.com/vamseeachanta/digitalmodel &mdash; API RP 11L rod-pump "
                       "module and SPE 18189 solver, with regression tests against five "
                       "anonymised field wells."),
    ],
)


def main() -> None:
    status = report.completeness()
    print("skeleton:", status.summary())
    if not status.complete:
        sys.exit(f"report incomplete: {status.missing_blocks}")

    report.write(OUT_HTML)
    print(f"html: {OUT_HTML} ({OUT_HTML.stat().st_size:,} bytes)")

    cmd = [
        "google-chrome", "--headless=new", "--disable-gpu", "--no-sandbox",
        "--password-store=basic",   # else Chrome blocks on the login keyring
        "--no-pdf-header-footer",   # else Chrome stamps a timestamp + file:// path
        "--virtual-time-budget=15000",
        f"--print-to-pdf={OUT_PDF}", str(OUT_HTML),
    ]
    res = subprocess.run(cmd, capture_output=True, timeout=240)
    if not OUT_PDF.exists():
        sys.exit(f"PDF not created: {res.stderr.decode()[:400]}")
    print(f"pdf:  {OUT_PDF} ({OUT_PDF.stat().st_size:,} bytes)")


if __name__ == "__main__":
    main()
