# ABOUTME: Builds the data-request brief -- the ask only, in the house identity.
# ABOUTME: Analysis lives in the engineering report; this links to it rather than restating it.
"""Data request brief for the Collide rod-pump well.

Deliberately scoped to **what is needed and why**. Every result belongs in the
engineering calculation report; duplicating numbers here would guarantee the two
documents drift apart the first time either is revised.

Usage:
    python build_data_ask.py     # writes HTML + PDF beside this script
"""

import subprocess
import sys
from pathlib import Path

from digitalmodel.reporting import (
    Brief, BriefSection, Callout, Confidence, DataRow, DesignDataGroup,
)

HERE = Path(__file__).resolve().parent
OUT_HTML = HERE / "dynacard-data-ask.html"
OUT_PDF = HERE / "AceEngineer-dynacard-data-ask.pdf"

REPORT_URL = ("https://github.com/vamseeachanta/digitalmodel/blob/main/docs/"
              "collide_pe/dynacard-undulations/"
              "AceEngineer-dynacard-rod-pump-report.pdf")


def rows(*triples):
    return [DataRow(label=a, value=b, unit=c) for a, b, c in triples]


brief = Brief(
    brief_id="AE-AL-2026-DR-01",
    title="Data request — supporting wells",
    discipline="Artificial Lift",
    prepared_for="Reed Goodman",
    lede=("Cards from <strong>additional wells</strong> to calibrate rod-pump "
          "diagnostics, and the smaller set of items still outstanding on the well "
          "already analysed. Ordered by how much each changes the answer, not by how "
          "easy it is to ask for."),
    footer_note=(f'Analysis of the well already reviewed: '
                 f'<a href="{REPORT_URL}">engineering report AE-AL-2026-001</a>. '
                 "Prepared by AceEngineer Artificial Lift."),
    sections=[
        BriefSection(
            heading="Why supporting wells, and what makes one useful",
            subtitle="This is the main ask. The physics is done; the diagnostics are not.",
            prose=[
                "Surface-to-downhole card conversion reproduces reference cards to "
                "within about 1%. The diagnostic layer that names a pump condition is "
                "a different matter: it is currently calibrated on synthetic cards, so "
                "it recognises shapes a computer drew rather than shapes a well made. "
                "On a real field card it returns answers that do not survive scrutiny. "
                "That is a data problem, not an algorithm problem, and it is stated "
                "here rather than papered over.",
                "A card on its own contributes almost nothing to fixing that. What "
                "makes one valuable is knowing how it turned out &mdash; the outcome is "
                "the label, and the pulling or workover report is the informative half.",
            ],
            groups=[
                DesignDataGroup(caption="Most valuable", rows=rows(
                    ("Card + what was found on the pull", "the report is the key half", ""),
                    ("Before/after pair on one well", "same string, known change", ""),
                    ("Confirmed healthy cards", "scarcer than sick ones, and the baseline", ""))),
                DesignDataGroup(caption="Also useful", rows=rows(
                    ("Raw .dyn or timestamped CSV", "removes timing error", ""),
                    ("Repeat cards over months", "shows a condition developing", ""),
                    ("Cards you are unsure about", "still useful as unlabelled context", ""))),
            ],
            callouts=[Callout(
                text=("Minimum context per card: rod string, pump size and setting "
                      "depth, stroke, SPM, and roughly what the well makes."),
                confidence=Confidence.VALIDATED)]),

        BriefSection(
            heading="Still outstanding on the well already analysed",
            subtitle="Two questions. Detail and full working are in the engineering report.",
            groups=[
                DesignDataGroup(caption="Load cell", rows=rows(
                    ("Make and model", "needed", ""),
                    ("Last calibration date", "needed", ""),
                    ("Zeroed with rods hanging or unloaded?", "needed", ""))),
                DesignDataGroup(caption="Operation", rows=rows(
                    ("Runtime, hours per day", "needed", ""),
                    ("On a pump-off controller or timer?", "needed", ""),
                    ("Rod string as run", "answered &mdash; 4,200 ft of 3/4 in", "&#10003;"))),
            ],
            prose=[
                "Why the load cell leads: the minimum load on the card is 2,412 lb "
                "heavier than the entire rod string weighs hanging in air. Friction "
                "cannot produce that &mdash; on the downstroke it acts upward and "
                "lowers polished-rod load, widening a card rather than lifting it. "
                "With the string now confirmed as recorded, the load cell is the only "
                "remaining explanation.",
                "Why runtime matters more than it looks: a unit on a 50% duty cycle "
                "produces half the fluid with a perfectly healthy pump. That is "
                "indistinguishable from 50% fillage if all you have is daily volume, "
                "so no efficiency figure is meaningful without it.",
            ],
            callouts=[Callout(
                text=("Until the load datum is resolved, absolute loads from this card "
                      "are not usable. Load differences remain sound, so everything "
                      "above still stands."),
                confidence=Confidence.ANALYTICAL)]),

        BriefSection(
            heading="Tier 2 — unlocks the downhole card",
            subtitle="The piece the thread ended on, and the only way to separate pump-off from gas interference from wear.",
            groups=[
                DesignDataGroup(caption="Well", rows=rows(
                    ("Tubing size / ID", "needed", ""),
                    ("Tubing anchored?", "needed", ""),
                    ("Fluid level above pump", "acoustic shot if available", ""))),
                DesignDataGroup(caption="Fluid", rows=rows(
                    ("Oil gravity, &deg;API", "needed", ""),
                    ("Formation volume factor <var>B<sub>o</sub></var>", "needed", ""),
                    ("Viscosity or bottomhole temperature", "needed", ""))),
            ],
            prose=[
                "Do not over-invest in precision here. Sweeping specific gravity "
                "0.80&ndash;0.90 and fluid level 3,000&ndash;4,300 ft moves theoretical "
                "displacement only from 38.3 to 41.7 bfpd &mdash; about &plusmn;4%. "
                "Reasonable estimates are fine; runtime swings the answer far harder.",
            ]),

        BriefSection(
            heading="Tier 3 — unlocks torque, counterbalance and power",
            groups=[
                DesignDataGroup(caption="Pumping unit", rows=rows(
                    ("Full API designation, e.g. C-228D-200-74", "needed", ""),
                    ("API 11E geometry sheet (A, C, I, K, P, crank radius, phase)", "needed", ""),
                    ("Counterbalance moment and structural imbalance", "needed", ""))),
                DesignDataGroup(caption="Motor", rows=rows(
                    ("Horsepower", "needed", ""),
                    ("Voltage and NEMA class", "needed", ""))),
            ],
            prose=[
                "The C-66 mentioned in the thread is an Arrow Engine natural-gas engine "
                "&mdash; the prime mover &mdash; not a pumping-unit or gearbox rating, "
                "so it gives no torque limit. Without the unit's own designation the "
                "torque calculation cannot run at all.",
            ]),

        BriefSection(
            heading="Better data beats more data",
            prose=[
                "A raw controller export &mdash; .dyn or timestamped CSV &mdash; is "
                "worth more than several Tier 2 items. Time read off a position-axis "
                "card is inherently uncertain because rod velocity vanishes at both "
                "stroke ends: at &plusmn;1.5 in digitising error the peak times carry "
                "&plusmn;0.11 to &plusmn;0.15 s. At that uncertainty the apparent peak "
                "spacings of 1.24 s and 0.95 s cannot be told apart, nor separated from "
                "the predicted 1.03 s. A raw time series removes the error class "
                "entirely.",
                "Also useful: several consecutive strokes rather than one, and a card "
                "from a different date. Stroke-to-stroke variation distinguishes a "
                "steady condition from an intermittent one, and a second date shows "
                "whether anything is trending.",
            ]),

        BriefSection(
            heading="The trade",
            subtitle="What is offered in return, stated plainly.",
            prose=[
                "You send data. We do the analysis and send it back &mdash; your wells, "
                "worked properly, at no charge. That starts immediately, not at the end "
                "of a development programme.",
                "This is an open source effort, not a product built toward a licence "
                "fee. The code is already public. Check the arithmetic, hand it to your "
                "own engineer, or ignore us entirely and still use it. There is nothing "
                "to get locked into and nothing that can be taken away later.",
                "Early contributors are supported for life &mdash; as stated, not an "
                "introductory rate that changes once the work matures. And it does not "
                "stop at dynacards: rod string design, unit sizing, gas interference, "
                "production troubleshooting, anything across your operations.",
            ],
            callouts=[Callout(
                text=("Well identifiers are kept out of anything public. The existing "
                      "validation set is anonymised the same way."),
                confidence=Confidence.VALIDATED)]),

        BriefSection(
            heading="What comes back",
            subtitle="Forward this brief to anyone it might suit — the same terms apply.",
            prose=[
                "Every well sent is analysed and returned: card metrics, the calculated "
                "downhole card where the inputs allow it, and an honest statement of "
                "what each result can and cannot support. Same format as the report "
                "linked below.",
                "Approximately three months from a workable labelled set, a diagnostics "
                "package carrying a measured accuracy figure &mdash; not a marketing "
                "one. Contributors' wells run through it first.",
            ],
            callouts=[Callout(
                text=("Until a real-card accuracy figure exists, any diagnosis from "
                      "this system is a hypothesis to check against the well, not a "
                      "finding. The downhole card and geometric measurements are "
                      "trustworthy today; the labels placed on them are not yet."),
                confidence=Confidence.PENDING)]),
    ],
)


def main() -> None:
    brief.write(OUT_HTML)
    print(f"html: {OUT_HTML} ({OUT_HTML.stat().st_size:,} bytes)")
    cmd = [
        "google-chrome", "--headless=new", "--disable-gpu", "--no-sandbox",
        "--password-store=basic", "--no-pdf-header-footer",
        "--virtual-time-budget=15000",
        f"--print-to-pdf={OUT_PDF}", str(OUT_HTML),
    ]
    res = subprocess.run(cmd, capture_output=True, timeout=240)
    if not OUT_PDF.exists():
        sys.exit(f"PDF not created: {res.stderr.decode()[:400]}")
    print(f"pdf:  {OUT_PDF} ({OUT_PDF.stat().st_size:,} bytes)")


if __name__ == "__main__":
    main()
