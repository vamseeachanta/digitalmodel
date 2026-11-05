# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Public presentation of the released synthetic CP examples; no solver changes."""

from cp_review_document import _findings, _sections, render_bound
from digitalmodel.reporting.spec import RevisionRow, Section, StatusBlock, TableBlock, TextBlock


CASES = {"S02": "monopile", "S03": "subsea manifold", "S06": "bracelet pipeline"}
SOURCE = "https://www.dnv.com/energy/standards-guidelines/dnv-rp-b401-cathodic-protection-design/"
LIMIT = (
    "Worked example on synthetic data, not an installation design. "
    "The geometry, environment and anode inputs are illustrative. "
    "PASS/FAIL describes the stated calculation checks only. Independent engineering "
    "review, qualified product data and a spatial protection assessment are required "
    "before selecting an arrangement for an installation."
)


def public_spec(spec, case_id, results):
    """Preserve numerical blocks and verdicts while changing public presentation."""
    if case_id not in CASES:
        raise ValueError("unsupported worked example")
    public = spec.model_copy(deep=True)
    public.verification_presentation = True
    public.document.title = f"Cathodic protection analysis: {CASES[case_id]} worked example"
    public.document.revision = "01"
    public.document.number = public.document.number.rsplit("-", 1)[0] + "-01"
    public.document.project = "Cathodic protection worked examples"
    public.document.client = "Not applicable — synthetic data"
    public.document.date = "2026-11-05"
    public.document.revision_history = [RevisionRow(
        rev="01", date="2026-11-05", description="First public worked-example issue",
    )]
    for section in [*public.sections, *public.appendices]:
        for block in section.blocks:
            if isinstance(block, StatusBlock):
                block.detail = block.detail.split("; Use status:", 1)[0]
            elif isinstance(block, TextBlock) and block.markdown.startswith("Use status:"):
                block.markdown = LIMIT
    evidence = [*public.sections, *public.appendices]
    sections = _sections(public)
    sections[0].blocks = [TextBlock(markdown=LIMIT + "\n\n" + _findings(public))]
    sections[1].blocks = [TextBlock(markdown=(
        f"This {CASES[case_id]} worked example uses synthetic data from a released "
        "repository fixture. It illustrates the calculation method and its limits; "
        "no installed asset or measured operating condition is represented."
    ))]
    sections[5].blocks = [TextBlock(markdown=_findings(public, numbered=True))]
    sections[6].blocks = [TextBlock(markdown=LIMIT + "\n\n"
        "Remaining mass, elapsed service, measured potentials, shielding and local "
        "current distribution are not established. A survey interval, operating "
        "limit and installed retrofit quantity cannot be inferred from these inputs.")]
    advice = (
        "Retain the reported PASS for the stated mass, current and spacing checks. "
        "Qualify field-joint coating, infill, electrical continuity and anode product "
        "data, then assess local potentials before using an installation layout."
    )
    if case_id in {"S02", "S03"}:
        ver = results["current_output_verification"]
        count = ver["recommended_anode_count"]
        checked = ver["anode_count"]
        advice = (
            f"The assessed {checked}-anode arrangement remains FAIL. For a new design "
            f"on the same synthetic basis, use at least {count} anodes of the stated "
            "size as the next arrangement to assess: final current output governs "
            "the maximum of mass, initial-current and final-current sizing counts. "
            "This is not a retrofit quantity. Re-run the complete calculation for "
            "that count and qualify product geometry, attachment, spacing, shielding "
            "and local potentials before design acceptance. A changed arrangement "
            "has not been spatially assessed here.\n\n"
            "Design basis: DNV-RP-B401, Cathodic protection design, May 2021, "
            "Anode mass; Anode resistance; Anode current output (the calculation's "
            "existing citations). The proposed count is derived from the supplied "
            "calculation, not a value quoted from the standard. The publisher's "
            "public summary establishes the practice's scope for galvanic-anode "
            f"design, manufacture and installation: {SOURCE} (accessed 2026-11-05). "
            "That summary does not independently validate this sizing calculation."
        )
    sections[7].blocks = [TextBlock(markdown=advice)]
    sections[8].blocks = [TextBlock(markdown=(
        "The standards, editions and clause titles in the calculation references "
        "below identify the method. No licensed standard table is reproduced in "
        "the recommendation. The public publisher summary supports scope only."
    ))]
    sections.insert(1, Section(key="revision-history", title="Revision history", blocks=[
        TableBlock(title="Revision history", columns=["Revision", "Date", "What changed and why"],
            rows=[["01", "2026-11-05", "First public worked-example issue; identify synthetic data and state the response to the calculated verdict."]]),
    ]))
    public.sections = sections
    public.appendices = evidence
    return public


def render_worked_example(spec, case_id, results):
    return render_bound(public_spec(spec, case_id, results), case_id,
                        status="Worked example — issued for information")
