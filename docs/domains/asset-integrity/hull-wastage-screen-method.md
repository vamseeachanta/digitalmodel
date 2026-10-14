# Bounded hull-structure wastage screening

The layout follows the neighbouring riser-joint fitness-for-service validation
document: scope, method, applicability, validation and limitations.
The module is `digitalmodel.asset_integrity.hull_wastage_screen`.
The component results do not establish an overall classification disposition.

## Source and scope

The calculation source is IACS Common Structural Rules for Bulk Carriers and
Oil Tankers, the NR606 class-society mirror with footer dated 1 July 2025,
Part 1 Chapter 13 Sections 1 and 2, printed pages 713–717 [1].
The issuer index was opened; direct issuer retrieval was rejected by the web
server. Equivalence to the issuer's consolidated edition is not established.
The date identifies the opened footer, rather than the mirror URL's January date.

The implemented local check uses Section 2 symbols and paragraphs 2.1.1–2.1.3.
The global check uses Section 2 paragraphs 1.2.1–1.2.3 and 2.2.1–2.2.2.
The reused section API corresponds to Chapter 5 Section 1 paragraphs
1.4.2–1.5.1 for a simple steel section without elevated effective members.

## Inputs and calculation

Local inputs are as-built, measured, corrosion-addition and voluntary-addition
thicknesses in millimetres. Renewal thickness is obtained by subtracting both
additions from as-built thickness. Required repair thickness excludes only the
voluntary addition. The reserve input is fixed to 0.500 millimetre [1].
RENEW denotes a reading below renewal; MONITOR_RESERVE denotes the interval
starting at renewal and ending strictly below renewal plus reserve.
ACCEPT_LOCAL denotes a reading at or above that upper boundary.
The reserve band identifies alternative-solution eligibility, not approval.

Global inputs are contributing horizontal or vertical flat plates, their
centroid heights and dimensions in metres, gross offered and measured
thicknesses in millimetres, corrosion additions, and confirmed survey zones.
Gross offered thickness shall come from the approved plans; it is distinct
from as-built thickness when voluntary additions are present.
`SectionElement` factories and `section_properties` are called twice; their
behaviour is unchanged. Required deck and bottom modulus is ninety percent
of the corresponding gross offered modulus. Neutral-zone target area uses
gross offered thickness less half the corrosion addition [1].
Required/current ratios govern; equality passes. The largest ratio determines
PASS_GLOBAL or RENEW. Each result carries values, applicability flags and
Citation objects. Local results carry the governing thickness margin.

## Applicability and evidence

The local criterion applies to operational ships classed under CSR.
The global criterion additionally requires a special survey after ten years
of age. An unsupported case returns NOT_APPLICABLE with no calculation values.
The caller shall confirm complete contributing geometry, approved zone
membership and a simple steel section. Stiffener profiles, inclined plates,
openings, dissimilar materials and elevated effective members are unsupported.
Neutral-zone plates shall be vertical, avoiding an inferred projection factor.
Incomplete or unsupported sections shall not be supplied as complete sections.

Finite dimensions, positive section areas and positive renewal thickness are
checked. The two rule-limit inputs are explicit and fixed to the opened values;
changing the reserve or retained-modulus fraction raises an error.
Caller-supplied generic citation metadata shall identify IACS-CSR, IACS and
revision 2025-07-01. Existing citation validation fails closed when the wiki
target is missing or its metadata differs. Production wiki availability was
not established; tests use explicitly synthetic resolver metadata.

## Validation

The offline tests derive synthetic cases directly from the cited criteria.
For local thicknesses 12.000, 3.000 and 1.000 millimetres, renewal is
8.000 millimetres, repair is 11.000 millimetres and reserve ends at
8.500 millimetres. Both sides and equality of each boundary are checked.
The global fixture has two horizontal flanges and one vertical web, each
two metres wide or high, with gross thickness 10.000 millimetres.
At measured thickness 9.000 millimetres, the neutral-zone area equals its
0.018 square metre target. Gross and measured modulus are respectively
0.046667 and 0.042000243 cubic metre. The unrounded modulus check fails.
Rectangle inertia and parallel-axis hand sums are checked against the opened
online Engineering Statics text, Sections 10.2.1, 10.3 and 10.4 [2].
The test docstring records every arithmetic step; no published vessel is used.

## Gaps

- Percentage-family substantial corrosion is not implemented. Recommendation
  84, Revision 1 November 2017, paragraph 3.2(g), addresses container ships.
- Pitting intensity classes remain unconfirmed; the existing API 579 pitting
  morphology engine does not supply the requested hull classification.
- Thinned-panel buckling is not added. The held DNV-RP-C201 October 2010,
  Section 6.1 and Section 6.2, Equations 6.1–6.4, specifies effective-width
  resistance. The existing plate solver instead applies a Johnson–Ostenfeld
  stress correction. Equivalence was not established; reuse as the published
  method is a gap. The open plate-buckling study is untouched.
- General profile geometry, equivalent-deck correction, alternative flange
  area route and per-item shortcut are not implemented.
- IMO MSC.108(73), adopted 5 December 2000, uses a newbuilding-required reference;
  it is not mixed with the implemented gross-offered reference.
- Decision/report adapters, a durable workflow and catalogue updates are not
  added; an exact applicable sibling integration pattern was not established.
- The chatbot route is not built: concept retired by the owner.
- Numeric gauging uncertainty, published-vessel validation and the production
  wiki target remain unverified. Second-provider review and hand review remain.
- Archive searches did not locate CSR with the research note's 2025 footer,
  DCV-ITS-021 May 2026, MSC.108(73) 2000, Recommendation 84 Revision 1 2017,
  SSC-474 2018 or the 2020 tanker ultimate-strength paper. Public copies opened.
- UR Z7, UR Z10.2, UR Z10.4, UR Z10.5, Recommendation 77 Revision 4 2017,
  PR 19 and IMO A.1049(27) 2011 were not located in the archive search;
  the needed survey clauses remain unqualified. No archive document was unreadable.

## References

[1] [IACS CSR, NR606 public mirror](https://rulesexplorer-docs.bureauveritas.com/documents/nr606/jan2025/606-NR_2025-01.pdf).

[2] Engineering Statics: Open and Interactive, undated online edition, opened
12 October 2026: [rectangle derivation](https://engineeringstatics.org/MOI-common-shapes.html),
[parallel-axis theorem](https://engineeringstatics.org/parallel-axis-theorem-section.html),
[composite-area Equation 10.4.1](https://engineeringstatics.org/Chapter_10-moment-of-inertia-of-composite-shapes.html).
