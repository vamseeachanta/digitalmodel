# ABOUTME: Builds the calm-water hull resistance analysis-basis report (#1173).
# ABOUTME: Fills the house CalcReport slots only -- no hand-written HTML anywhere.
"""Calm-water hull resistance -- analysis basis and method, AceEngineer standard format.

This script contains **no HTML**. It fills the typed slots of
:class:`digitalmodel.reporting.CalcReport`, which owns the house section order,
equation-card markup and provenance colouring.

The defining constraint on this report: **no resistance result exists.** No CFD
has been executed and nothing has been launched on the CFD host, so every field
in the Results section carries :data:`Confidence.PENDING`. What the report
establishes is the validation referent -- pinned field by field from the
workshop specifications that published it -- the governing relations, and the
acceptance gates the eventual result must pass.

Usage:
    PYTHONPATH=src python docs/reports/2026-08-12-calm-water-hull-resistance-basis.py
"""

from pathlib import Path

from digitalmodel.reporting import (
    CalcReport,
    Confidence,
    DataRow,
    DesignDataGroup,
    Equation,
    KPI,
    MethodBlock,
    Objective,
    Reference,
    ResultBlock,
    ValidationItem,
    VariableDef,
    WayForwardStage,
)

HERE = Path(__file__).resolve().parent
OUT_HTML = HERE / "2026-08-12-calm-water-hull-resistance-basis.html"

FRAC = '<span class="frac"><span class="n">{n}</span><span class="d">{d}</span></span>'
OP = '<span class="op">{}</span>'
EQ = OP.format("=")
PLUS = OP.format("+")
MINUS = OP.format("&minus;")


def rows(*triples):
    return [DataRow(label=a, value=b, unit=c) for a, b, c in triples]


NOT_RUN = "&mdash;"


report = CalcReport(
    report_id="AE-MAR-2026-1173",
    # The house renderer escapes the title for <title> and <h1>, so this field
    # must be literal text -- an HTML entity here would render as its own source.
    title="Calm-water hull resistance — analysis basis and method",
    discipline="Marine Hydrodynamics",
    revision="A",
    preliminary=True,
    lede=(
        "Analysis basis, governing relations and acceptance gates for a calm-water "
        "resistance prediction of the KCS hull at <var>Fr</var> = 0.26, towed fixed "
        "and even-keel through a free surface by <code>interFoam</code>. "
        "<strong>No resistance result is reported.</strong> No CFD has been executed "
        "and nothing has been launched on the CFD host, so every field in Section 4 "
        "is marked pending. What this report fixes, before a solver hour is spent, is "
        "the reference condition &mdash; now pinned field by field to the workshop "
        "specifications that published it &mdash; the method, and the gates the "
        "eventual number must pass."
    ),
    kpis=[
        KPI(value="3.560", unit="&times;10&#8315;&#179;",
            confidence=Confidence.VALIDATED,
            caption="Reference <var>C</var><sub>t</sub> &mdash; KCS EFD, fixed even keel, bare hull"),
        KPI(value="2.8320", unit="&times;10&#8315;&#179;",
            confidence=Confidence.ANALYTICAL,
            caption="<var>C</var><sub>f</sub> from the ITTC-57 line at <var>Re</var> = 1.4&times;10&#8311;"),
        KPI(value="7.280", unit="&times;10&#8315;&#8308;",
            confidence=Confidence.ANALYTICAL,
            caption="<var>C</var><sub>r</sub> by subtraction &mdash; derived, never measured"),
        KPI(value="&mdash;", unit="",
            caption="Computed <var>C</var><sub>t</sub> &mdash; no solve has been run"),
    ],

    objective=Objective(
        purpose=(
            "Calm-water resistance is the towing power a hull demands at a given speed "
            "in still water, and it is the base on which every added-resistance, "
            "self-propulsion and powering estimate is built. This calculation "
            "establishes the analysis basis for predicting it by free-surface CFD: it "
            "pins the reference condition the eventual computed coefficient will be "
            "judged against, states the governing relations explicitly, and fixes the "
            "acceptance gates and their failure modes in advance of any solver hour."
        ),
        scope=[
            "Hull: KCS at 1/31.6, bare, fixed even keel",
            "Condition: Fr = 0.26, Re = 1.4&times;10&#8311;, still water",
            "Method: interFoam VOF towing, LTS pseudo-steady",
            "Deliverable: model-scale coefficient only",
            "Status: basis and method &mdash; no results",
        ],
        preliminary_note=(
            "<strong>This report states no resistance result, and the omission is the "
            "point.</strong> No CFD run has been executed for this analysis; nothing "
            "has ever been launched on the CFD host. An analysis basis is a legitimate "
            "and standard deliverable in its own right &mdash; it is what makes the "
            "eventual result reviewable rather than merely reported &mdash; but it "
            "must not be read as a preliminary answer. Every field in Section 4 is "
            "pending, and no number anywhere in this document is a prediction of "
            "resistance. "
            "<br><br>"
            "<strong>The basis is now primary-sourced, and was not always.</strong> "
            "Two earlier plan revisions for this work were rejected in adversarial "
            "review, both for the same root cause: reference-condition provenance. "
            "Revision 1 gated a body fixed in heave and pitch against a free-to-sink-"
            "and-trim measurement, a 4.5&nbsp;% condition offset spent before a cell "
            "was meshed. Revision 2 corrected the referent, then re-scored a "
            "with-rudder study against a bare-hull reference inside the very table "
            "rebutting the first finding. The referent used here was subsequently "
            "retrieved field by field from the workshops that published the data, and "
            "it moves the gate centre again &mdash; from 3.55&times;10&#8315;&#179; to "
            "3.56&times;10&#8315;&#179;, and the residuary by 1.4&nbsp;%. Where a "
            "number in this report replaces an earlier one, Section 5 says so."
        ),
    ),

    design_data=[
        DesignDataGroup(caption="Hull &mdash; KCS at model scale", rows=rows(
            ("Hull", "KRISO Container Ship (KCS)", ""),
            ("Scale ratio", "1 / 31.6", ""),
            ("Length between perpendiculars <var>L</var><sub>pp</sub>", "7.2786", "m"),
            ("Wetted surface <var>S</var><sub>DWL</sub>", "9.4379", "m&sup2;"),
            ("<var>S</var> / <var>L</var><sub>pp</sub>&sup2;",
             "0.1781 (0.178147 recomputed)", ""),
            ("Appendages", "none &mdash; bare hull, no rudder", ""),
            ("Attitude", "fixed, even keel (restrained in heave and pitch)", ""),
            ("Domain", "half hull about the centreplane; forces doubled", ""),
        )),
        DesignDataGroup(caption="Tow condition and environment", rows=rows(
            ("Model speed <var>V</var><sub>m</sub>", "2.1962", "m/s"),
            ("Froude number <var>Fr</var>", "0.26 (0.25990 recomputed)", ""),
            ("Reynolds number <var>Re</var>", "1.4&times;10&#8311;", ""),
            ("Kinematic viscosity <var>&nu;</var>",
             "1.1418&times;10&#8315;&#8310; (derived)", "m&sup2;/s"),
            ("Water surface", "still water, no incident wave", ""),
            ("Gravity <var>g</var>", "9.81", "m/s&sup2;"),
        )),
        DesignDataGroup(caption="Validation referent &mdash; every field stated by the source",
                        rows=rows(
            ("Total resistance coefficient <var>C</var><sub>t</sub>",
             "3.56&times;10&#8315;&#179;", ""),
            ("Attitude",
             "&ldquo;Fixed(even keel)&rdquo; &mdash; Tokyo 2005 Case 1.1, verbatim", ""),
            ("Appendage",
             "&ldquo;Without rudder&rdquo; &mdash; Tokyo 2005 Case 1.1, verbatim", ""),
            ("Normalising area",
             "9.4379 m&sup2;, &ldquo;hull only (no rudder) &hellip; static orientation "
             "without waves&rdquo;", ""),
            ("Reynolds number", "1.4&times;10&#8311; &mdash; stated by both workshops", ""),
            ("Speed and length",
             "<var>V</var><sub>m</sub> = 2.1962 m/s, <var>L</var><sub>pp</sub> = 7.2786 m "
             "&mdash; Gothenburg 2000", ""),
            ("EFD uncertainty <var>U</var><sub>D</sub>",
             "1.0 % of <var>C</var><sub>t</sub> (Case 1.1 page); 0.64 % (EFD table) "
             "&mdash; the conservative figure is carried", ""),
            ("Workshop <var>C</var><sub>F0</sub>",
             "2.83&times;10&#8315;&#179; &mdash; the workshop&rsquo;s own ITTC-57 reduction", ""),
            ("Workshop <var>C</var><sub>R</sub>",
             "0.731&times;10&#8315;&#179;, defined by the workshop as "
             "<var>C</var><sub>T</sub> &minus; <var>C</var><sub>F0</sub>", ""),
            ("Source", "CFD Workshop Tokyo 2005 Case 1.1; Gothenburg 2000 KCS pages "
                       "(R1&ndash;R4)", ""),
        )),
        DesignDataGroup(caption="Derived quantities &mdash; computed here, not measured",
                        rows=rows(
            ("<var>C</var><sub>f</sub> (ITTC-57) at <var>Re</var> = 1.4&times;10&#8311;",
             "2.832045&times;10&#8315;&#179;", ""),
            ("<var>C</var><sub>r</sub> = <var>C</var><sub>t</sub> &minus; "
             "<var>C</var><sub>f</sub>", "7.2796&times;10&#8315;&#8308;", ""),
            ("<var>&nu;</var> = <var>V</var><sub>m</sub><var>L</var><sub>pp</sub> / "
             "<var>Re</var>", "1.14180&times;10&#8315;&#8310;", "m&sup2;/s"),
            ("<var>Fr</var> = <var>V</var><sub>m</sub> / &radic;(<var>g L</var><sub>pp</sub>)",
             "0.25990", ""),
            ("<var>C</var><sub>r</sub> as a fraction of <var>C</var><sub>t</sub>",
             "20.4 %", ""),
        )),
        DesignDataGroup(caption="Solver configuration &mdash; OpenFOAM v2312", rows=rows(
            ("Solver", "<code>interFoam</code> &mdash; two-phase incompressible VOF", ""),
            ("Case origin",
             "tutorial <code>multiphase/interFoam/laminar/DTCHull</code>, ported as "
             "frozen literal templates", ""),
            ("Turbulence", "RAS, <code>kOmegaSST</code>", ""),
            ("Time integration",
             "<code>ddtSchemes localEuler</code> (LTS); <code>maxCo 10</code>, "
             "<code>maxAlphaCo 5</code>, <code>maxDeltaT 1</code>", ""),
            ("Hull wall treatment",
             "<code>nutkWallFunction</code> (smooth) &mdash; replaces the tutorial&rsquo;s "
             "<code>nutkRoughWallFunction</code>, <code>Ks 100e-6</code>", ""),
            ("Force reporting",
             "<code>forces</code> function object on patch <code>(hull)</code>, "
             "<code>rhoInf 998.8</code>, <code>CofR (2.929541 0 0.2)</code>; pressure and "
             "viscous contributions parsed separately", ""),
            ("Decomposition", "<code>numberOfSubdomains 8</code>, hierarchical "
                              "<code>n (2 2 2)</code>", ""),
            ("Iteration budget",
             "25&thinsp;000 LTS iterations; force mean over the final 4&thinsp;000", ""),
            ("Meshes",
             "production ~1.5&times;10&#8310; cells half-domain; companion coarser by "
             "<var>r</var><sub>G</sub> = &radic;2 (~0.53&times;10&#8310;)", ""),
        )),
        DesignDataGroup(caption="Sources deliberately NOT used as the referent", rows=rows(
            ("Tokyo 2015 Case 2.1 / Gothenburg 2010 2.2b",
             "<var>C</var><sub>t</sub> = 3.711&times;10&#8315;&#179;, free to heave and "
             "pitch, <strong>with rudder</strong>, <var>S</var><sub>0</sub>/"
             "<var>L</var><sub>pp</sub>&sup2; = 0.1803, <var>&nu;</var> = "
             "1.27&times;10&#8315;&#8310;, <var>Re</var> = 1.26&times;10&#8311; "
             "&mdash; a different campaign", ""),
            ("Wu (2025) KCS grids",
             "<strong>with rudder</strong> at <var>Re</var> = 1.46&times;10&#8311; "
             "&mdash; not condition-matched to the referent; used only for "
             "solution-convergence behaviour and timing", ""),
            ("Holtrop&ndash;Mennen (this repository)",
             "implementation defective and reproduced &mdash; issue #2020. "
             "<strong>No number in this report comes from it.</strong>", ""),
            ("Value 3.557&times;10&#8315;&#179;",
             "provenance never established; not used", ""),
        )),
    ],

    assumptions=[
        DesignDataGroup(caption="Modelling and numerics", rows=rows(
            ("Water density <var>&rho;</var> = 998.8 kg/m&sup3;",
             "Case value. The 2000 campaign does not publish a tank density; "
             "<var>C</var><sub>t</sub> normalises on the same <var>&rho;</var> used to "
             "form the force, so the coefficient is insensitive to it", ""),
            ("Water temperature and exact <var>&nu;</var>",
             "Not published. <var>&nu;</var> is set to reproduce the stated "
             "<var>Re</var> = 1.4&times;10&#8311;, which is the basis of the "
             "workshop&rsquo;s own data reduction, so the match is by construction", ""),
            ("Turbulence closure",
             "<var>k</var>-<var>&omega;</var> SST, matching the closest published "
             "<code>interFoam</code> analogue. Not itself validated by this work", ""),
            ("Centreplane symmetry",
             "Assumed valid for a fixed hull towed straight in calm water; forces "
             "doubled. A factor-of-two error here is silent, so a dimensional check "
             "against the reference row is carried as a test", ""),
            ("Production mesh ~1.5&times;10&#8310; half-domain",
             "Sized from Wu&rsquo;s 1.64&times;10&#8310; and Shen&rsquo;s "
             "1.68&times;10&#8310; full-domain grids. A <code>snappyHexMesh</code> "
             "hex-dominant cell is not equivalent to an unstructured or overset cell, "
             "and the half-domain doubling is an assumption, not an identity", ""),
        )),
        DesignDataGroup(caption="Uncertainty budget behind the &plusmn;3 % gate", rows=rows(
            ("<var>U</var><sub>D</sub> = 1.00 %",
             "EFD uncertainty, workshop-stated; the conservative of the two published "
             "figures", ""),
            ("<var>U</var><sub>SN</sub> = 1.39 %",
             "Numerical and validation uncertainty <strong>borrowed</strong> from Wu "
             "(2025) KCS static mesh &mdash; not measured on this mesh, and Wu&rsquo;s "
             "case is with-rudder at a different Reynolds number", ""),
            ("<var>U</var><sub>i</sub> = 0.24 %",
             "Iterative uncertainty over a 4&thinsp;000-iteration window, also borrowed "
             "from Wu", ""),
            ("RSS = 1.73 %",
             "The floor. <var>V</var>1 is set at 3.00 %, 1.74&times; the floor. "
             "Tightening a gate on a borrowed uncertainty buys nothing, which is why it "
             "is not tightened further", ""),
            ("Wu&rsquo;s own verdict on his KCS case",
             "&ldquo;the KCS result does not qualify for validation&rdquo;. The borrowed "
             "terms carry that caveat and it is stated rather than absorbed", ""),
        )),
        DesignDataGroup(caption="Cost inputs &mdash; none of them measured here", rows=rows(
            ("Per-cell-iteration solver rate",
             "Not measured on this host. Published: 18.4 &micro;s at 12 ranks wall-clock "
             "(Wu 2025). A repository bracket of 12.25&ndash;27.85 &micro;s exists but is "
             "2-D, few-thousand-cell, single-threaded on a different host and does not "
             "corroborate a 3-D VOF+RAS rate", ""),
            ("8-rank parallel efficiency 0.454",
             "From a repository sloshing benchmark on a different case. A wall-clock "
             "rate measured on <var>N</var> ranks already contains its own parallel "
             "overhead and must not be divided by an efficiency a second time", ""),
            ("Exclusive solver time",
             "<strong>Pending and unreconciled.</strong> Three mutually inconsistent "
             "figures are in play across the planning record (~4.5 d, 9&ndash;12 d, "
             "~19 d). Reconciling them is out of this report&rsquo;s scope; no duration "
             "here should be treated as settled", ""),
        )),
    ],

    methodology=[
        MethodBlock(
            heading="Resistance decomposition and the coefficient definition",
            prose=(
                "What a towing tank and a CFD run both produce is a force. What is "
                "gated is a dimensionless coefficient, formed on the wetted surface "
                "and the tow speed. Because the coefficient carries "
                "<var>S</var> in its denominator, a reference "
                "<var>C</var><sub>t</sub> quoted without the area, attitude and "
                "appendage state it was reduced on is not a number that can be gated "
                "against at any tolerance &mdash; which is precisely how the two "
                "earlier revisions of this work went wrong. "
                "Two decompositions appear below and they are not the same thing. "
                "Equation (2) is a bookkeeping split: <var>C</var><sub>f</sub> is "
                "supplied by a correlation line, so <var>C</var><sub>r</sub> is "
                "whatever remains. Equation (3) is a physical split the solver "
                "actually computes, integrating pressure and viscous traction over the "
                "hull patch separately. The validation strategy in Section 3.8 turns on "
                "the difference."
            ),
            equations=[
                Equation(
                    markup='<var>C</var><sub>t</sub> ' + EQ + ' '
                           + FRAC.format(
                               n='<var>R</var><sub>T</sub>',
                               d='&frac12; <var>&rho; S U</var>&sup2;'),
                    variables=[
                        VariableDef(symbol="<var>R</var><sub>T</sub>",
                                    description="Total resistance, streamwise force on the hull",
                                    unit="N"),
                        VariableDef(symbol="<var>&rho;</var>",
                                    description="Water density", unit="kg/m&sup3;"),
                        VariableDef(symbol="<var>S</var>",
                                    description="Wetted surface, full hull, static orientation",
                                    unit="m&sup2;"),
                        VariableDef(symbol="<var>U</var>",
                                    description="Tow speed", unit="m/s"),
                    ],
                    note=(
                        "On a half domain the integrated force is doubled before this "
                        "is formed, while <var>S</var> remains the full-hull area. "
                        "Omitting the doubling is a clean factor of two and it does not "
                        "announce itself."
                    ),
                ),
                Equation(
                    markup='<var>C</var><sub>t</sub> ' + EQ
                           + ' <var>C</var><sub>f</sub> ' + PLUS
                           + ' <var>C</var><sub>r</sub>',
                    variables=[
                        VariableDef(symbol="<var>C</var><sub>f</sub>",
                                    description="Frictional coefficient from a correlation line",
                                    unit="&ndash;"),
                        VariableDef(symbol="<var>C</var><sub>r</sub>",
                                    description="Residuary coefficient &mdash; the remainder",
                                    unit="&ndash;"),
                    ],
                ),
                Equation(
                    markup='<var>C</var><sub>t</sub> ' + EQ
                           + ' <var>C</var><sub>p</sub> ' + PLUS
                           + ' <var>C</var><sub>v</sub>',
                    variables=[
                        VariableDef(symbol="<var>C</var><sub>p</sub>",
                                    description="Computed pressure-force coefficient",
                                    unit="&ndash;"),
                        VariableDef(symbol="<var>C</var><sub>v</sub>",
                                    description="Computed viscous-force coefficient",
                                    unit="&ndash;"),
                    ],
                    note=(
                        "Equation (3) holds exactly and identically, which is why the "
                        "component gates cannot be fully independent of the total. "
                        "Section 3.8 states the resulting detection floor rather than "
                        "leaving it to be discovered after a run."
                    ),
                ),
            ],
        ),
        MethodBlock(
            heading="The ITTC-57 model&ndash;ship correlation line",
            prose=(
                "<var>C</var><sub>f</sub> is not a measurement. It is a correlation "
                "line adopted by the ITTC in 1957, and the workshop reduced its own "
                "experimental data with it &mdash; the published "
                "<var>C</var><sub>F0</sub> = 2.83&times;10&#8315;&#179; agrees with "
                "2.832045&times;10&#8315;&#179; recomputed here. Two consequences "
                "follow, and both are stated because an earlier revision of this "
                "analysis got them wrong. "
                "First, <var>C</var><sub>r</sub> = <var>C</var><sub>t</sub> &minus; "
                "<var>C</var><sub>f</sub> is a <em>derived</em> quantity carrying the "
                "line&rsquo;s assumption; it is analytical, never validated. "
                "Second, there is no published, independently measured KCS pressure or "
                "residuary coefficient at all. The workshop defines its "
                "<var>C</var><sub>R</sub> as <var>C</var><sub>T</sub> &minus; "
                "<var>C</var><sub>F0</sub>, and Shen&rsquo;s tabulated "
                "<var>C</var><sub>P</sub> is footnoted as <var>C</var><sub>T</sub> "
                "&minus; <var>C</var><sub>F</sub>. A previous revision cited their "
                "agreement as independent corroboration of the arithmetic. It is not "
                "corroboration &mdash; it is a definition reproducing itself. The only "
                "measured resistance datum in the whole referent is "
                "<var>C</var><sub>t</sub>."
            ),
            equations=[
                Equation(
                    markup='<var>C</var><sub>f</sub> ' + EQ + ' '
                           + FRAC.format(
                               n='0.075',
                               d='(log<sub>10</sub> <var>Re</var> ' + MINUS
                                 + ' 2)&sup2;'),
                    variables=[
                        VariableDef(symbol="<var>Re</var>",
                                    description="Reynolds number of the model",
                                    unit="&ndash;"),
                    ],
                    note=(
                        "At <var>Re</var> = 1.4&times;10&#8311; this returns "
                        "2.832045&times;10&#8315;&#179;, so the referent&rsquo;s "
                        "residuary is 7.2796&times;10&#8315;&#8308; &mdash; 20.4 % of "
                        "the total. That fraction is the reason the free surface must "
                        "be resolved rather than approximated."
                    ),
                ),
            ],
        ),
        MethodBlock(
            heading="Froude and Reynolds scaling",
            prose=(
                "A model cannot satisfy Froude and Reynolds similitude at once: "
                "matching <var>Fr</var> fixes the speed, which then fixes "
                "<var>Re</var> far below full scale. The decomposition of Section 3.1 "
                "exists to work around that, by scaling the wave-making part on "
                "<var>Fr</var> and correcting the friction part on <var>Re</var>. "
                "This analysis stops at the model-scale coefficient &mdash; there is "
                "no full-scale extrapolation and no powering estimate here. The "
                "viscosity is set so that the stated speed and length reproduce the "
                "stated <var>Re</var> = 1.4&times;10&#8311;, which reproduces the "
                "workshop&rsquo;s own data reduction by construction."
            ),
            equations=[
                Equation(
                    markup='<var>Fr</var> ' + EQ + ' '
                           + FRAC.format(
                               n='<var>U</var>',
                               d='<span class="rad">&radic;<span class="rc">'
                                 '<var>g L</var><sub>pp</sub></span></span>'),
                    variables=[
                        VariableDef(symbol="<var>g</var>",
                                    description="Gravitational acceleration",
                                    unit="m/s&sup2;"),
                        VariableDef(symbol="<var>L</var><sub>pp</sub>",
                                    description="Length between perpendiculars", unit="m"),
                    ],
                ),
                Equation(
                    markup='<var>Re</var> ' + EQ + ' '
                           + FRAC.format(
                               n='<var>U L</var><sub>pp</sub>',
                               d='<var>&nu;</var>'),
                    variables=[
                        VariableDef(symbol="<var>&nu;</var>",
                                    description="Kinematic viscosity", unit="m&sup2;/s"),
                    ],
                    note=(
                        "<var>U</var> = 2.1962 m/s and <var>L</var><sub>pp</sub> = "
                        "7.2786 m at <var>Re</var> = 1.4&times;10&#8311; give "
                        "<var>&nu;</var> = 1.14180&times;10&#8315;&#8310; m&sup2;/s. "
                        "The tank temperature is not published, so whether "
                        "1.4&times;10&#8311; is exact or nominally rounded is unknown; "
                        "it does not matter, because the reference reduction used the "
                        "same figure."
                    ),
                ),
            ],
        ),
        MethodBlock(
            heading="Free-surface capture &mdash; interFoam VOF",
            prose=(
                "<code>interFoam</code> solves one momentum equation for a mixture of "
                "two incompressible phases and transports a phase fraction "
                "<var>&alpha;</var> with an artificial compression term that keeps the "
                "interface from smearing across the mesh. The hull is towed by "
                "imposing a uniform inlet velocity on a fixed body, so the wave system "
                "develops in the frame of the hull. "
                "At <var>Fr</var> = 0.26 the residuary is 20.4 % of the total, so the "
                "free surface is not a refinement &mdash; a solution that computes no "
                "wave field returns pure friction, "
                "<var>C</var><sub>t</sub> = <var>C</var><sub>f</sub> = "
                "2.832&times;10&#8315;&#179;, which is 20.4 % below the referent and "
                "fails the gate by nearly sevenfold. That degenerate case is the "
                "known-negative control the test suite is required to reject."
            ),
            equations=[
                Equation(
                    markup=FRAC.format(n='&part;<var>&alpha;</var>',
                                       d='&part;<var>t</var>')
                           + ' ' + PLUS + ' &nabla;&middot;(<var>&alpha;</var>'
                           '<strong>U</strong>) ' + PLUS
                           + ' &nabla;&middot;[<var>&alpha;</var>(1 ' + MINUS
                           + ' <var>&alpha;</var>)<strong>U</strong><sub>r</sub>] '
                           + EQ + ' 0',
                    variables=[
                        VariableDef(symbol="<var>&alpha;</var>",
                                    description="Water volume fraction, 1 in water, 0 in air",
                                    unit="&ndash;"),
                        VariableDef(symbol="<strong>U</strong>",
                                    description="Mixture velocity field", unit="m/s"),
                        VariableDef(symbol="<strong>U</strong><sub>r</sub>",
                                    description="Interface compression velocity",
                                    unit="m/s"),
                    ],
                ),
                Equation(
                    markup='<var>&rho;</var> ' + EQ + ' <var>&alpha; &rho;</var><sub>w</sub> '
                           + PLUS + ' (1 ' + MINUS + ' <var>&alpha;</var>)'
                           '<var>&rho;</var><sub>a</sub>',
                    variables=[
                        VariableDef(symbol="<var>&rho;</var><sub>w</sub>",
                                    description="Water density", unit="kg/m&sup3;"),
                        VariableDef(symbol="<var>&rho;</var><sub>a</sub>",
                                    description="Air density", unit="kg/m&sup3;"),
                    ],
                    note=(
                        "Because density is a field rather than a constant, the "
                        "<code>forces</code> function object must be told which "
                        "<var>&rho;</var> to normalise on. Leaving the density source "
                        "implicit in a two-phase run is how a factor slips in "
                        "unnoticed, so <code>rhoInf</code> is stated explicitly."
                    ),
                ),
            ],
        ),
        MethodBlock(
            heading="Local time stepping &mdash; why not a Courant-limited transient",
            prose=(
                "This is the choice that makes the run feasible, so it is argued "
                "rather than asserted. "
                "A time-accurate <code>interFoam</code> run advances the whole domain "
                "on one step, and that step must satisfy the Courant condition in "
                "<em>every</em> cell simultaneously &mdash; so it is set by the "
                "smallest cell anywhere in the mesh. In a resistance mesh the "
                "wall-normal cells near the hull are three to four orders of magnitude "
                "smaller than the free-stream cells, while the tow speed is the same "
                "2.2 m/s everywhere, which drives the global step to order "
                "10&#8315;&#8308; s. The physics being waited on is slow by "
                "comparison: the Kelvin wave system has to establish itself over "
                "several hull lengths, order 10 s of physical time. The product is "
                "10&#8309; to 10&#8310; global steps, and almost every one of them is "
                "rate-limited by boundary-layer cells that contribute nothing to the "
                "timescale being resolved. "
                "<br><br>"
                "Local time stepping breaks that coupling by discarding the one thing "
                "not needed here &mdash; time accuracy. Calm-water resistance is a "
                "<em>steady</em> quantity; the transient path to it is of no interest. "
                "Under <code>ddtSchemes localEuler</code> each cell advances on its own "
                "pseudo-time step sized by its own local Courant limit, so small cells "
                "relax quickly in pseudo-time without holding the rest of the domain "
                "back, and the field marches to the steady state in tens of thousands "
                "of iterations rather than hundreds of thousands of time-accurate "
                "steps. Published practice for exactly this configuration "
                "(<code>interFoam</code>, LTS, static mesh, SST) is "
                "20&thinsp;000&ndash;40&thinsp;000 iterations; this analysis budgets "
                "25&thinsp;000 with the force mean taken over the final 4&thinsp;000. "
                "<br><br>"
                "Two consequences are carried deliberately. The iteration history is "
                "not a physical time series, so nothing may be read from it except "
                "convergence &mdash; the averaging window and its iterative scatter are "
                "recorded for that reason. And an accidental regression to a transient "
                "<code>ddt</code> scheme would not fail loudly; it would simply cost an "
                "order of magnitude more, which is why <code>localEuler</code> is "
                "pinned by a test rather than left to the case files."
            ),
            equations=[
                Equation(
                    markup='&Delta;<var>t</var><sub>i</sub> ' + EQ
                           + ' <var>Co</var><sub>max</sub> '
                           + FRAC.format(n='<var>d</var><sub>i</sub>',
                                         d='|<strong>U</strong><sub>i</sub>|'),
                    variables=[
                        VariableDef(symbol="&Delta;<var>t</var><sub>i</sub>",
                                    description="Pseudo-time step, local to cell i",
                                    unit="s"),
                        VariableDef(symbol="<var>d</var><sub>i</sub>",
                                    description="Cell size along the local flow direction",
                                    unit="m"),
                        VariableDef(symbol="<var>Co</var><sub>max</sub>",
                                    description="Courant limit: maxCo 10, maxAlphaCo 5",
                                    unit="&ndash;"),
                    ],
                    note=(
                        "In a global-step transient the same expression is evaluated "
                        "over the whole mesh and the minimum is taken. LTS keeps the "
                        "per-cell value; that single change is the difference between "
                        "a run of hours and a run of weeks."
                    ),
                ),
            ],
        ),
        MethodBlock(
            heading="Force extraction and the computed decomposition",
            prose=(
                "The OpenFOAM <code>forces</code> function object integrates pressure "
                "and viscous contributions over the named patch and writes them "
                "separately. Keeping them separate is what turns one gated number into "
                "two constrained by different references: the pressure coefficient "
                "carries the wave-making, the viscous coefficient carries the skin "
                "friction, and a wall-treatment defect or an unresolved wave field "
                "shows up in one of them even when the total happens to look right."
            ),
            equations=[
                Equation(
                    markup='<var>C</var><sub>p</sub> ' + EQ + ' '
                           + FRAC.format(
                               n='<var>F</var><sub>p,x</sub>',
                               d='&frac12; <var>&rho; S U</var>&sup2;')
                           + ' <span class="op">,</span> <var>C</var><sub>v</sub> '
                           + EQ + ' '
                           + FRAC.format(
                               n='<var>F</var><sub>v,x</sub>',
                               d='&frac12; <var>&rho; S U</var>&sup2;'),
                    variables=[
                        VariableDef(symbol="<var>F</var><sub>p,x</sub>",
                                    description="Streamwise pressure force on the hull patch",
                                    unit="N"),
                        VariableDef(symbol="<var>F</var><sub>v,x</sub>",
                                    description="Streamwise viscous force on the hull patch",
                                    unit="N"),
                    ],
                    note=(
                        "Both are doubled for the half domain before normalisation, on "
                        "the full-hull <var>S</var> = 9.4379 m&sup2;."
                    ),
                ),
            ],
        ),
        MethodBlock(
            heading="Decomposition, execution and the host",
            prose=(
                "The case decomposes onto 8 ranks, <code>hierarchical</code> with "
                "<code>n (2 2 2)</code>. The repository&rsquo;s own scaling benchmark "
                "measures 1.00 / 0.886 / 0.607 / 0.454 efficiency at 1 / 2 / 4 / 8 "
                "ranks and a regression to 0.193 at 16, so 8 is the practical ceiling "
                "&mdash; but that measurement is from a different (sloshing) case and "
                "is carried in Section 2 as an assumption, not as a property of this "
                "one. "
                "<br><br>"
                "Two execution facts belong in the method because they bound it. "
                "First, the mesh pipeline is not a fixed short sequence: the DTCHull "
                "<code>Allrun</code> needs <code>surfaceFeatureExtract</code>, "
                "<code>blockMesh</code>, six <code>topoSet</code> / "
                "<code>refineMesh</code> pairs, <code>snappyHexMesh</code>, "
                "<code>restore0Dir</code>, and <code>redistributePar</code> / "
                "<code>renumberMesh</code> around the parallel solve. A runner that "
                "emits only mesh-then-solve cannot produce this case on 8 ranks at all. "
                "Second, the existing runner drives each stage as a single subprocess "
                "with a two-hour default timeout and contains no <code>mpirun</code>, "
                "so an execution-capability change necessarily precedes any production "
                "solve. Note also that a fail-closed wall-clock preflight and a "
                "detached long run are in tension: once the command is wrapped in "
                "<code>setsid nohup</code> the timeout no longer bounds the solve, so "
                "the preflight is a configuration-consistency check, not a safety "
                "property, and should be described as one."
            ),
        ),
        MethodBlock(
            heading="Validation criteria &mdash; the gates, and how each can fail",
            prose=(
                "Four criteria are pre-committed here, before any result exists, "
                "because a gate chosen after seeing the number is not a gate. Each is "
                "stated with the way it can fail; a criterion nothing can fail is the "
                "defect that rejected an earlier revision of this work, where the "
                "residuary check was an algebraic re-expression of the total and could "
                "not fail while the total passed."
            ),
            equations=[
                Equation(
                    markup='<var>V</var>1<span class="op">:</span> '
                           + FRAC.format(
                               n='|<var>C</var><sub>t,CFD</sub> ' + MINUS
                                 + ' 3.56&times;10&#8315;&#179;|',
                               d='3.56&times;10&#8315;&#179;')
                           + ' &le; 0.03',
                    note=(
                        "<strong>How it fails.</strong> An under-resolved or absent "
                        "wave field &mdash; the degenerate no-wave solution sits at "
                        "&minus;20.4 %, failing by 6.8&times;. A mesh too coarse to "
                        "resolve the bow and stern wave systems. Or a run whose "
                        "attitude, appendage state or normalising area does not match "
                        "the referent tuple, which is a provenance failure rather than "
                        "a physics one and is guarded by a fixture-row assertion. "
                        "<strong>Why 3 % and not 5 or 10.</strong> 3 % of "
                        "<var>C</var><sub>t</sub> is 1.07&times;10&#8315;&#8308;, "
                        "already 14.7 % of the residuary the case exists to compute; "
                        "the issue&rsquo;s original 10 % would let that quantity be "
                        "wrong by half. Below the 1.73 % uncertainty floor the gate "
                        "would start failing correct work."
                    ),
                ),
                Equation(
                    markup='<var>V</var>2a<span class="op">:</span> '
                           + FRAC.format(
                               n='|<var>C</var><sub>p,CFD</sub> ' + MINUS
                                 + ' <var>C</var><sub>r,ref</sub>|',
                               d='<var>C</var><sub>r,ref</sub>')
                           + ' &le; 0.15',
                    note=(
                        "<strong>What makes it independent.</strong> "
                        "<var>C</var><sub>p,CFD</sub> is integrated by the solver, not "
                        "obtained by subtracting a correlation line from the gated "
                        "total &mdash; so unlike a "
                        "<var>C</var><sub>r</sub> = <var>C</var><sub>t</sub> &minus; "
                        "<var>C</var><sub>f</sub> check, this one can fail while "
                        "<var>V</var>1 passes. "
                        "<strong>How it fails.</strong> Compensating errors: an "
                        "over-predicted friction offsetting an under-predicted wave "
                        "field. A worked pair &mdash; "
                        "<var>C</var><sub>v</sub> at +4.0 % and "
                        "<var>C</var><sub>p</sub> at &minus;15.7 % &mdash; reproduces "
                        "the referent <var>C</var><sub>t</sub> to within 0.05 % while "
                        "failing <var>V</var>2a. "
                        "<strong>What is contested.</strong> "
                        "<var>C</var><sub>r,ref</sub> = 7.2796&times;10&#8315;&#8308; "
                        "is itself derived, so the independence is on the CFD side "
                        "only. And the published deviations of computed pressure "
                        "coefficients from this reference span &minus;7.9 % to "
                        "&minus;2.7 % &mdash; entirely one-sided &mdash; so a symmetric "
                        "&plusmn;15 % band is not symmetric evidence. That asymmetry is "
                        "an open item, recorded rather than resolved."
                    ),
                ),
                Equation(
                    markup='<var>V</var>2b<span class="op">:</span> '
                           + FRAC.format(
                               n='|<var>C</var><sub>v,CFD</sub> ' + MINUS
                                 + ' <var>C</var><sub>f</sub>(<var>Re</var>)|',
                               d='<var>C</var><sub>f</sub>(<var>Re</var>)')
                           + ' &le; 0.05',
                    note=(
                        "<strong>How it fails.</strong> Wall treatment. The DTCHull "
                        "tutorial ships <code>nutkRoughWallFunction</code> with "
                        "<code>Ks uniform 100e-6</code> &mdash; a 100 &micro;m "
                        "sand-grain roughness. Inherited unexamined onto a smooth "
                        "towing-tank model it inflates skin friction, and "
                        "<var>V</var>2b is the criterion that catches it without "
                        "depending on whether the pressure side happens to compensate. "
                        "Published condition-matched grids give "
                        "<var>C</var><sub>v</sub>/<var>C</var><sub>f</sub> within "
                        "1.2 %, so 5 % leaves margin while still catching the defect. "
                        "Note <var>C</var><sub>v,CFD</sub> and the ITTC-57 line are not "
                        "the same decomposition, so the tolerance absorbs some "
                        "definitional slack as well as physical error."
                    ),
                ),
                Equation(
                    markup='<var>V</var>3<span class="op">:</span> '
                           + FRAC.format(
                               n='|<var>C</var><sub>t,fine</sub> ' + MINUS
                                 + ' <var>C</var><sub>t,coarse</sub>|',
                               d='<var>C</var><sub>t,fine</sub>')
                           + ' &le; 0.03',
                    note=(
                        "<strong>What it tests.</strong> Self-consistency between two "
                        "mesh levels separated by <var>r</var><sub>G</sub> = &radic;2, "
                        "refining downward from the production mesh. "
                        "<var>V</var>1 is deliberately <em>not</em> required on both "
                        "levels: on a two-point study the coarse level&rsquo;s "
                        "agreement with the experiment is not the property under test. "
                        "<strong>How it fails.</strong> The answer is mesh-dependent, "
                        "in which case no validation claim stands regardless of which "
                        "level happens to match. <strong>What is contested.</strong> "
                        "3 % here is several times the grid-uncertainty scale the "
                        "closest published analogue produces, so a pair differing by "
                        "2.9 % would pass while implying a fine-grid error larger than "
                        "the budget <var>V</var>1 was built from. Either the threshold "
                        "belongs nearer the grid-uncertainty scale, or a near-miss must "
                        "re-open the <var>V</var>1 tolerance. Recorded as open."
                    ),
                ),
                Equation(
                    markup='<var>C</var><sub>v</sub> at ' + PLUS
                           + '5.0 % <span class="op">,</span> <var>C</var><sub>p</sub> at '
                           + MINUS + '15.0 % <span class="op">&rArr;</span> '
                           '<var>C</var><sub>t</sub> at ' + PLUS + '0.91 %',
                    note=(
                        "<strong>The detection floor, stated rather than discovered.</strong> "
                        "Because <var>C</var><sub>t</sub> = <var>C</var><sub>p</sub> + "
                        "<var>C</var><sub>v</sub> holds identically, a compensating "
                        "pair sitting exactly on both component boundaries lands at "
                        "+0.91 % on the total &mdash; inside <var>V</var>1, and passing "
                        "all three criteria at once. So the decomposition gate cannot "
                        "detect a compensating pair whose net effect on "
                        "<var>C</var><sub>t</sub> is below roughly 1 %. That is the "
                        "limit of what these criteria can prove, and it is a property "
                        "of the arithmetic, not of the mesh."
                    ),
                ),
            ],
        ),
    ],

    results=[
        ResultBlock(
            heading="Total resistance coefficient",
            confidence=Confidence.PENDING,
            prose=(
                "<strong>No CFD run has been executed.</strong> Nothing has been "
                "launched on the CFD host for this analysis. The fields below are "
                "empty because the computation has not happened &mdash; not because a "
                "value is being withheld, rounded, or deferred to a later revision. No "
                "estimate is offered in their place; an estimate here would be "
                "indistinguishable in a table from a result."
            ),
            groups=[DesignDataGroup(caption="Gated quantity", rows=rows(
                ("<var>C</var><sub>t,CFD</sub> at <var>Fr</var> = 0.26", NOT_RUN, ""),
                ("Deviation from the 3.56&times;10&#8315;&#179; referent", NOT_RUN, ""),
                ("<var>V</var>1 verdict (&plusmn;3 %)", "Not evaluated", ""),
                ("Converged?", "No run", ""),
            ))],
            caption="No solve executed &mdash; the value cannot be stated",
        ),
        ResultBlock(
            heading="Force decomposition &mdash; pressure and viscous",
            confidence=Confidence.PENDING,
            prose=(
                "These are the quantities that make the residuary check independent of "
                "the total. They come from the <code>forces</code> function object of a "
                "run that has not been performed."
            ),
            groups=[DesignDataGroup(caption="Computed components", rows=rows(
                ("<var>C</var><sub>p,CFD</sub>", NOT_RUN, ""),
                ("<var>C</var><sub>v,CFD</sub>", NOT_RUN, ""),
                ("<var>V</var>2a verdict (&plusmn;15 %)", "Not evaluated", ""),
                ("<var>V</var>2b verdict (&plusmn;5 %)", "Not evaluated", ""),
            ))],
            caption="No solve executed &mdash; components cannot be stated",
        ),
        ResultBlock(
            heading="Mesh self-consistency between two levels",
            confidence=Confidence.PENDING,
            groups=[DesignDataGroup(caption="Two-level check", rows=rows(
                ("Production mesh cell count", NOT_RUN, ""),
                ("Companion mesh cell count (<var>r</var><sub>G</sub> = &radic;2 coarser)",
                 NOT_RUN, ""),
                ("|&Delta;<var>C</var><sub>t</sub>| / <var>C</var><sub>t,fine</sub>",
                 NOT_RUN, ""),
                ("<var>V</var>3 verdict", "Not evaluated", ""),
            ))],
            caption="Neither mesh has been generated or solved",
        ),
        ResultBlock(
            heading="Free-surface field and Kelvin wedge",
            confidence=Confidence.PENDING,
            prose=(
                "A qualitative check only, never a gate: the rendered free-surface "
                "elevation with the 19.47&deg; Kelvin half-angle overlaid. It is "
                "listed here so that its absence is visible alongside the quantitative "
                "fields rather than quietly omitted."
            ),
            groups=[DesignDataGroup(caption="Qualitative check", rows=rows(
                ("Free-surface elevation field", NOT_RUN, ""),
                ("Kelvin half-angle recovered", "Not evaluated", ""),
            ))],
            caption="No field data exists to render",
        ),
        ResultBlock(
            heading="Empirical cross-check &mdash; unavailable",
            confidence=Confidence.PENDING,
            prose=(
                "An independent empirical prediction would ordinarily sit here as a "
                "sanity bound on the CFD result. This repository&rsquo;s "
                "Holtrop&ndash;Mennen implementation cannot supply one and is not used "
                "to produce any number in this report. "
                "The defect is filed and reproduced as issue #2020: a Series 60 form "
                "and a tanker &mdash; hulls differing by 68 % in waterline length and "
                "33 % in block coefficient &mdash; return total resistance "
                "coefficients agreeing to <strong>4 parts in 10&#8309;</strong>. "
                "Hull-form dependence has collapsed out of the total somewhere in the "
                "chain. Its own declared fixture vectors fail by &minus;31.5 % and "
                "+50.6 % against a &plusmn;15 % tolerance, and the form-factor row "
                "fails its band as well; no test loaded the fixture, which is why none "
                "of this was caught. "
                "Using an unvalidated empirical method to corroborate a CFD result "
                "would only move the unverified claim one level down, so the "
                "cross-check is reported as unavailable and carries no weight until "
                "#2020 is repaired (the implementation is owned by #1682)."
            ),
            groups=[DesignDataGroup(caption="Cross-check status", rows=rows(
                ("Holtrop&ndash;Mennen <var>C</var><sub>t</sub>",
                 "Unavailable &mdash; implementation defective (#2020)", ""),
                ("Used in this report", "No &mdash; explicitly excluded", ""),
            ))],
            caption="Method unavailable &mdash; defect reproduced, not used",
        ),
        ResultBlock(
            heading="Measured solver rate and schedule",
            confidence=Confidence.PENDING,
            prose=(
                "No rate has been measured on the target host, so no duration in this "
                "analysis rests on a measurement. Three mutually inconsistent estimates "
                "of exclusive solver time exist in the planning record and they span a "
                "factor of four; reconciling them is out of scope for this report and "
                "is flagged as unreconciled rather than averaged into a single "
                "reassuring figure."
            ),
            groups=[DesignDataGroup(caption="Cost", rows=rows(
                ("Per-cell-iteration rate on this host", NOT_RUN, ""),
                ("Achieved 8-rank efficiency on this case", NOT_RUN, ""),
                ("Exclusive solver time",
                 "Pending &mdash; unreconciled (~4.5 d, 9&ndash;12 d, ~19 d in play)", ""),
            ))],
            caption="Nothing measured; the schedule is not settled",
        ),
    ],

    validation=[
        ValidationItem(
            claim="Referent <var>C</var><sub>t</sub> = 3.56&times;10&#8315;&#179; at "
                  "<var>Fr</var> = 0.26",
            basis="CFD Workshop Tokyo 2005 Case 1.1 and the Gothenburg 2000 EFD table, "
                  "both retrieved directly. Two self-consistent tables; Shen&rsquo;s "
                  "3.55&times;10&#8315;&#179; is the same measurement rounded once more",
            confidence=Confidence.VALIDATED),
        ValidationItem(
            claim="Attitude: fixed, even keel",
            basis="Tokyo 2005 Case 1.1, verbatim &mdash; &ldquo;Fixed(even keel)&rdquo;. "
                  "The workshop test matrix runs a free-to-sink-and-trim variant on a "
                  "different hull, never on KCS",
            confidence=Confidence.VALIDATED),
        ValidationItem(
            claim="Appendage: bare hull, no rudder",
            basis="Tokyo 2005 Case 1.1 (&ldquo;Without rudder&rdquo;); Gothenburg 2000 "
                  "description (&ldquo;bare hull and fixed model&rdquo;); EFD-table "
                  "note (&ldquo;hull only (no rudder)&rdquo;)",
            confidence=Confidence.VALIDATED),
        ValidationItem(
            claim="Normalising area <var>S</var><sub>DWL</sub> = 9.4379 m&sup2; "
                  "(<var>S</var>/<var>L</var><sub>pp</sub>&sup2; = 0.1781)",
            basis="Gothenburg 2000 geometry and conditions, stated. Recomputes to "
                  "0.178147 against the stated 0.1781. An earlier revision carried this "
                  "as a 1.22 % unresolved systematic; it is now stated, and the "
                  "systematic leaves the uncertainty budget",
            confidence=Confidence.VALIDATED),
        ValidationItem(
            claim="<var>Re</var> = 1.4&times;10&#8311;, <var>V</var><sub>m</sub> = "
                  "2.1962 m/s, <var>L</var><sub>pp</sub> = 7.2786 m",
            basis="Stated by both workshops. The workshop reduced its own experimental "
                  "data at this <var>Re</var>, so it is the case&rsquo;s defining "
                  "condition rather than a round number attached afterwards",
            confidence=Confidence.VALIDATED),
        ValidationItem(
            claim="Analysis basis is primary-sourced, superseding two rejected revisions",
            basis="Referent retrieved field by field from the publishing workshops "
                  "rather than inherited from secondary literature. Both earlier "
                  "rejections traced to reference-condition provenance; the gate centre "
                  "moved twice as a result and its current value is the workshop&rsquo;s",
            confidence=Confidence.VALIDATED),
        ValidationItem(
            claim="<var>&nu;</var> = 1.14180&times;10&#8315;&#8310; m&sup2;/s",
            basis="Derived from three stated quantities. Tank temperature is not "
                  "published, so whether <var>Re</var> = 1.4&times;10&#8311; is exact or "
                  "nominally rounded is unknown &mdash; immaterial, since the reference "
                  "reduction used the same figure",
            confidence=Confidence.ANALYTICAL),
        ValidationItem(
            claim="<var>C</var><sub>f</sub>(ITTC-57) = 2.832045&times;10&#8315;&#179;",
            basis="Correlation line evaluated at the stated <var>Re</var>; agrees with "
                  "the workshop&rsquo;s published <var>C</var><sub>F0</sub> = "
                  "2.83&times;10&#8315;&#179;. A line, not a measurement",
            confidence=Confidence.ANALYTICAL),
        ValidationItem(
            claim="<var>C</var><sub>r</sub> = 7.2796&times;10&#8315;&#8308; &mdash; "
                  "derived, not measured",
            basis="Obtained by subtracting a correlation line from the measured total. "
                  "The workshop&rsquo;s own <var>C</var><sub>R</sub> and Shen&rsquo;s "
                  "<var>C</var><sub>P</sub> are defined the same way, so their agreement "
                  "is a definition reproducing itself, not independent corroboration. "
                  "An earlier revision claimed otherwise; that claim is withdrawn",
            confidence=Confidence.ANALYTICAL),
        ValidationItem(
            claim="<var>V</var>1 tolerance of 3 %",
            basis="RSS(<var>U</var><sub>D</sub> 1.00, <var>U</var><sub>SN</sub> 1.39, "
                  "<var>U</var><sub>i</sub> 0.24) = 1.73 %, giving 1.74&times; margin. "
                  "<var>U</var><sub>SN</sub> and <var>U</var><sub>i</sub> are borrowed "
                  "from a published grid, not measured on this one",
            confidence=Confidence.ANALYTICAL),
        ValidationItem(
            claim="Achievability of a 3 % gate",
            basis="Rests on four condition-matched published results only &mdash; "
                  "bare-hull, fixed even-keel, <var>Re</var> = 1.4&times;10&#8311; "
                  "&mdash; landing at &minus;0.90 %, &minus;0.96 %, &minus;1.12 % and "
                  "&minus;1.24 % against the pinned referent. An earlier claim of "
                  "&ldquo;six of seven inside 1 %&rdquo; pooled condition-mismatched "
                  "grids and is withdrawn",
            confidence=Confidence.ANALYTICAL),
        ValidationItem(
            claim="Detection floor of the decomposition gate is about 1 % on "
                  "<var>C</var><sub>t</sub>",
            basis="A compensating pair on both component boundaries "
                  "(<var>C</var><sub>v</sub> +5.0 %, <var>C</var><sub>p</sub> "
                  "&minus;15.0 %) yields +0.91 % on the total and passes every "
                  "criterion. Arithmetic, recomputed against the pinned referent",
            confidence=Confidence.ANALYTICAL),
        ValidationItem(
            claim="Wu (2025) is not condition-matched to the referent",
            basis="KCS appended with a rudder, at <var>Re</var> = 1.46&times;10&#8311; "
                  "against the referent&rsquo;s 1.4&times;10&#8311; &mdash; a friction "
                  "bias of 0.56 % of <var>C</var><sub>t</sub> before any other "
                  "difference. Wu also states his KCS result &ldquo;does not qualify "
                  "for validation&rdquo;. Scoring his grids against a bare-hull "
                  "referent caused a review rejection; the work is cited only for "
                  "solution-convergence behaviour and timing",
            confidence=Confidence.ANALYTICAL),
        ValidationItem(
            claim="Computed <var>C</var><sub>t</sub>, <var>C</var><sub>p</sub>, "
                  "<var>C</var><sub>v</sub>",
            basis="No CFD has been executed and nothing has been launched on the CFD "
                  "host. No value exists at any confidence",
            confidence=Confidence.PENDING),
        ValidationItem(
            claim="Mesh independence of the eventual result",
            basis="Requires two solved levels. Neither mesh has been generated",
            confidence=Confidence.PENDING),
        ValidationItem(
            claim="KCS watertight geometry at 1/31.6",
            basis="The workshop publishes IGES/STP and no import path exists here. "
                  "Tessellation to a closed STL, and verification against published "
                  "particulars, are both outstanding",
            confidence=Confidence.PENDING),
        ValidationItem(
            claim="Wigley smoke geometry closure",
            basis="The shipped STL reports 4 illegal triangles <em>and</em> 392 edges "
                  "connected to a single face. De-duplicating the 4 triangles does not "
                  "close it &mdash; the 392 open edges require capping an open lid, so "
                  "a repair scoped as de-duplication alone cannot pass a closure gate",
            confidence=Confidence.PENDING),
        ValidationItem(
            claim="Execution capability on the target host",
            basis="The current runner is serial, has no <code>mpirun</code>, drives a "
                  "fixed short stage sequence rather than the pipeline this case needs, "
                  "and defaults to a two-hour timeout. Unproven end to end; nothing has "
                  "been run",
            confidence=Confidence.PENDING),
        ValidationItem(
            claim="Solver cost and schedule",
            basis="No rate measured on this host. Three inconsistent estimates in the "
                  "record (~4.5 d, 9&ndash;12 d, ~19 d); explicitly unreconciled and out "
                  "of scope here",
            confidence=Confidence.PENDING),
        ValidationItem(
            claim="Holtrop&ndash;Mennen empirical cross-check",
            basis="Implementation defective and reproduced (#2020): two hulls differing "
                  "68 % in length and 33 % in block coefficient return "
                  "<var>C</var><sub>t</sub> agreeing to 4 parts in 10&#8309;. "
                  "Unavailable; contributes no number here",
            confidence=Confidence.PENDING),
    ],

    way_forward=[
        WayForwardStage(
            heading="Stage 0 &mdash; commit the referent as a machine-readable fixture",
            prose=(
                "No CFD. The referent tuple is transcribed into a committed fixture "
                "with a per-field provenance marker and its workshop citation, together "
                "with the free-condition series from the other lineage so that a future "
                "free-to-sink-and-trim criterion has something correct to gate against. "
                "The fixture schema requires attitude, appendage state and wetted "
                "surface on every row, which is what makes it structurally impossible "
                "to load a free-condition value into a fixed-condition gate &mdash; the "
                "failure that rejected the first revision."
            ),
            required=rows(
                ("Input", "The retrieved referent evidence &mdash; already committed", ""),
                ("Effort", "Transcription only; nothing left to retrieve", ""),
            ),
            returns=rows(
                ("Unlocks", "Gate centres fixed and testable before any solve", ""),
                ("Guards", "Condition-mismatch regression, by schema rather than by care", ""),
            )),
        WayForwardStage(
            heading="Stage 1 &mdash; execution capability, then a tutorial reproduction",
            prose=(
                "The first thing to establish is that a long parallel solve can run at "
                "all: MPI ranks, detached execution that survives a deliberate "
                "mid-solve disconnect, a staged mesh pipeline that can express the "
                "six-pass refinement loop, and a preflight that refuses a solve whose "
                "estimated wall-clock exceeds its configured budget. Then the DTCHull "
                "tutorial is reproduced unmodified &mdash; no repository code in the "
                "loop &mdash; to prove the toolchain end to end and to take the first "
                "measurement of the per-cell-iteration rate on this box. That "
                "measurement is expected to change the schedule, possibly upward."
            ),
            required=rows(
                ("Host", "CFD host reachable, OpenFOAM v2312, 8 ranks", ""),
                ("Change", "Runner gains ranks, detachment, declared stages, preflight", ""),
            ),
            returns=rows(
                ("Unlocks", "The first cost figure grounded in measurement", ""),
                ("Retires", "&ldquo;Nothing has ever been launched on the host&rdquo;", ""),
            )),
        WayForwardStage(
            heading="Stage 2 &mdash; the emitter, test-first",
            prose=(
                "A resistance case emitted from a typed configuration, ported from the "
                "tutorial as frozen literal templates. The test that matters asserts a "
                "declared-deviation diff in both directions: every intended deviation "
                "from the tutorial is present, and no unintended one exists. An "
                "accidental drop during the port then fails a test in seconds rather "
                "than producing a divergent solve days later."
            ),
            required=rows(
                ("Input", "Stage-1 case as the reference tree", ""),
            ),
            returns=rows(
                ("Unlocks", "Re-parameterisation to any hull and speed", ""),
                ("Guards", "Silent loss of a tutorial setting during the port", ""),
            )),
        WayForwardStage(
            heading="Stage 3 &mdash; Wigley smoke case, after a real geometry repair",
            prose=(
                "A cheap re-parameterisation exercise with no validation claim. The "
                "shipped Wigley STL is not closed, and the closure work is larger than "
                "it first appears: beyond 4 illegal triangles there are 392 edges "
                "connected to a single face, so de-duplication alone leaves the surface "
                "open and a closure gate would still fail. Capping the open lid is part "
                "of the stage or the stage does not pass."
            ),
            required=rows(
                ("Input", "Repaired, verified-closed STL", ""),
            ),
            returns=rows(
                ("Unlocks", "Confidence that the emitter re-targets cleanly", ""),
                ("Claims", "None &mdash; explicitly not a validation case", ""),
            )),
        WayForwardStage(
            heading="Stage 4 &mdash; KCS geometry and mesh",
            prose=(
                "The largest schedule risk, and deliberately late so that it is not "
                "paid for twice. The workshop publishes IGES/STP; a watertight STL at "
                "1/31.6 must be produced and verified against the published particulars "
                "&mdash; waterline length, beam, draught, displacement and wetted "
                "surface &mdash; before any cell is generated. Verification before "
                "meshing is what makes a bad tessellation cost minutes instead of a "
                "multi-day solve."
            ),
            required=rows(
                ("Input", "KCS hull geometry from the workshop distribution", ""),
                ("Gate", "Closed surface; strict checkMesh verdict read from output text", ""),
            ),
            returns=rows(
                ("Unlocks", "The production and companion meshes", ""),
            )),
        WayForwardStage(
            heading="Stage 5 &mdash; solve, and populate Section 4",
            prose=(
                "One Froude number, two mesh levels, 25&thinsp;000 LTS iterations each "
                "with the force mean over the final 4&thinsp;000 and its iterative "
                "scatter recorded. Extract <var>C</var><sub>t</sub>, "
                "<var>C</var><sub>p</sub> and <var>C</var><sub>v</sub>; evaluate "
                "<var>V</var>1, <var>V</var>2a, <var>V</var>2b and the two-level "
                "criterion. This is the stage that turns every pending field in this "
                "report into a result &mdash; and the only one that can."
            ),
            required=rows(
                ("Input", "Stages 1&ndash;4 complete", ""),
                ("Cost", "Pending &mdash; unreconciled; see Section 2 assumptions", ""),
            ),
            returns=rows(
                ("Unlocks", "The first stated resistance coefficient", ""),
                ("Decides", "Whether the capability is validated or the gate fails", ""),
            )),
        WayForwardStage(
            heading="Stage 6 &mdash; evidence and closeout",
            prose=(
                "A committed verification artifact carrying the coefficient table "
                "against the referent, the two-level result, the convergence history "
                "with its averaging window marked, the free-surface figure with the "
                "Kelvin wedge overlaid, and a provenance manifest naming the hull "
                "source, scale, attitude, appendage state, wetted surface, "
                "<var>Fr</var>, <var>Re</var>, water properties, solver build and "
                "citation. Host paths redacted. Deferred scope &mdash; the Froude "
                "sweep, the fine-grid convergence study &mdash; stated on the issue "
                "where the issue is read, not buried in a document."
            ),
            required=rows(
                ("Input", "Stage-5 run manifest", ""),
            ),
            returns=rows(
                ("Unlocks", "A reviewable claim rather than a reported number", ""),
            )),
    ],

    references=[
        Reference(text="CFD Workshop Tokyo 2005, Test Case 1.1 (KCS, towed). "
                       "National Maritime Research Institute. States the condition "
                       "verbatim: &ldquo;Towing condition in still water / Fixed(even "
                       "keel) / Without rudder&rdquo;, Fn 0.26, Rn 1.4&times;10&#8311;, "
                       "and points its reference data at the Gothenburg 2000 table. "
                       "This is the case Shen et al. cite as Hino (2005)."),
        Reference(text="CFD Workshop Tokyo 2005, &ldquo;Data and Data "
                       "Uncertainty for Unpropelled KCS Integral Variables&rdquo;. "
                       "<var>S</var>/<var>L</var>&sup2; = 0.1781, "
                       "<var>C</var><sub>T</sub> = 3.56&times;10&#8315;&#179;, "
                       "<var>U</var><sub>D</sub> = 0.64 %, with the note that the "
                       "wetted surface is &ldquo;for hull only (no rudder) &hellip; and "
                       "for static orientation without waves&rdquo;."),
        Reference(text="CFD Workshop Gothenburg 2000, KCS geometry and "
                       "conditions. <var>Re</var> = 1.4&times;10&#8311;, "
                       "<var>Fr</var> = 0.26, <var>L</var><sub>pp</sub> = 7.2786 m, "
                       "<var>S</var><sub>DWL</sub> = 9.4379 m&sup2;, "
                       "<var>V</var><sub>m</sub> = 2.1962 m/s, "
                       "&ldquo;full-scale bare-hull geometry in fixed static "
                       "orientation&rdquo;."),
        Reference(text="CFD Workshop Gothenburg 2000, KCS EFD comparison "
                       "table and case description. <var>C</var><sub>F0</sub> = "
                       "2.83&times;10&#8315;&#179;, <var>C</var><sub>R</sub> = "
                       "0.731&times;10&#8315;&#179;, &ldquo;EFD "
                       "<var>C</var><sub>R</sub> is defined as "
                       "<var>C</var><sub>R</sub> = <var>C</var><sub>T</sub> &minus; "
                       "<var>C</var><sub>F0</sub>&rdquo;; &ldquo;the conditions include "
                       "bare hull and fixed model&rdquo;. Experiments by KRISO."),
        Reference(text="ITTC (1957) model&ndash;ship correlation line, "
                       "<var>C</var><sub>F</sub> = 0.075 / (log<sub>10</sub> "
                       "<var>Re</var> &minus; 2)&sup2;. A correlation, not a "
                       "measurement; every residuary in this report inherits its "
                       "assumption."),
        Reference(text="Shen, Z.; Wan, D.; Carrica, P. M. (2015). "
                       "&ldquo;Dynamic overset grids in OpenFOAM with application to "
                       "KCS self-propulsion and maneuvering.&rdquo; "
                       "<em>Ocean Engineering</em> 108:287&ndash;306. Condition-matched "
                       "&mdash; fixed even keel, bare hull, "
                       "<var>Re</var> = 1.4&times;10&#8311;. <strong>Caution:</strong> "
                       "the &ldquo;Experiment&rdquo; column contains exactly one "
                       "measured quantity; <var>C</var><sub>P</sub> and "
                       "<var>C</var><sub>F</sub> are footnoted as derived."),
        Reference(text="Wu, P.-C. (2025). <em>Mathematics</em> 13(11):1788. "
                       "<code>interFoam</code> v6.0, VOF, SST <var>k</var>-"
                       "<var>&omega;</var>, LTS, static mesh &mdash; the closest "
                       "configurational analogue. <strong>NOT condition-matched:</strong> "
                       "the KCS hull is fitted <strong>with a rudder</strong> and run at "
                       "<var>Re</var> = 1.46&times;10&#8311;, against the "
                       "referent&rsquo;s bare hull at 1.4&times;10&#8311;. Wu further "
                       "states that his KCS result &ldquo;does not qualify for "
                       "validation&rdquo;. Cited here only for solution-convergence "
                       "behaviour and timing. Re-scoring these grids against the "
                       "bare-hull referent caused a review rejection and is recorded so "
                       "that it is not repeated."),
        Reference(text="CFD Workshop Tokyo 2015, Case 2.1 (and Gothenburg "
                       "2010 case 2.2b). <var>C</var><sub>t</sub> = "
                       "3.711&times;10&#8315;&#179;, free to heave and pitch, with "
                       "rudder, <var>S</var><sub>0</sub>/<var>L</var><sub>pp</sub>&sup2; "
                       "= 0.1803, <var>&nu;</var> = 1.27&times;10&#8315;&#8310;. "
                       "A separate campaign, listed so it cannot be mistaken for the "
                       "referent &mdash; gating a fixed bare hull against it is the "
                       "error that rejected the first revision of this work."),
        Reference(text="OpenFOAM v2312 tutorial "
                       "<code>multiphase/interFoam/laminar/DTCHull</code> (ESI-OpenCFD). "
                       "The case template: LTS controls, <code>kOmegaSST</code>, "
                       "eight-way hierarchical decomposition, and the "
                       "<code>surfaceFeatureExtract</code> / six-pass "
                       "<code>topoSet</code>&ndash;<code>refineMesh</code> pipeline. Its "
                       "<code>endTime 4000</code> is a demonstration budget, not a "
                       "convergence requirement, and its "
                       "<code>nutkRoughWallFunction</code> is replaced here."),
        Reference(text="Internal evidence: "
                       "<code>docs/plans/evidence/2026-08-11-kcs-referent-resolution.md</code>. "
                       "Primary-source retrieval log for the referent tuple, with the "
                       "retrieval status of every source recorded and superseded claims "
                       "marked. The authority for Section 2 of this report."),
        Reference(text="Issue #1173 (calm-water hull resistance, "
                       "<code>interFoam</code> towing). Scope and original acceptance "
                       "criteria."),
        Reference(text="Issue #2020 (Holtrop&ndash;Mennen returns "
                       "essentially the same <var>C</var><sub>t</sub> for any hull). "
                       "Filed and reproduced; the reason no empirical cross-check "
                       "appears in Section 4. Implementation owned by #1682."),
    ],
)


def main() -> None:
    status = report.completeness()
    if not status.complete:
        raise SystemExit(f"incomplete report: {status.summary()}")
    report.write(OUT_HTML)
    print(f"wrote {OUT_HTML.name} ({OUT_HTML.stat().st_size:,} bytes)")


if __name__ == "__main__":
    main()
