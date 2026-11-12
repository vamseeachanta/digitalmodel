# Jack-up design basis: gap note against a public rack-chock article

Date: 12 Nov 2026. Status: working note, public-data screening only. This is not
design, operational or incident advice. It states no cause for any rig loss; it
shows which checks our screening method covers and which a disclosed basis
should state.

## Trigger

A public LinkedIn article (Satish Jha, "Fried & Golaman's 260%, 4, 6 And 12: The
Numbers Behind A Jackup Design",
https://www.linkedin.com/pulse/fried-golamans-260-4-6-12-numbers-behind-jackup-design-satish-jha-smucc)
raises these topics: a rack-chock holding claim of 260 % over pinions alone, a
1981 patent statement on pinion holding ratings, rack phase differential,
preload procedure, and what a design basis should disclose. The 260 % figure is a
vendor claim. It is treated here as unverified until checked against a published
baseline and test conditions.

## Gap table

Files below are under `docs/domains/geotechnical/`.

| Item | What our basis says | What a disclosed basis should state | Public source to use |
| --- | --- | --- | --- |
| Rack chock vs pinion holding | One lumped check: leg reaction over an assumed 35.0 MN per-leg "pinion or fixation equivalent capacity" (`jackup-failure-sequence.md:97`, input range 25-60 MN at line 46). Marked an assumption, not an equipment rating (`:32-34`). No separate pinion and chock models. | Holding capacity per leg for brakes set and for jacking, with and without chocks; the baseline the capacity is compared with (new or aged pinions, brakes-set or jacking rating); the test report. | US Patent 4,269,543 (1981) text on pinion holding rating; class rules for jacking systems; ISO 19905-1. |
| The 260 % claim | Not covered. Our holding ratio only ranks late (holding 8.0 in the base storm order, `jackup-failure-sequence.md:183-185`), so it is a sensitivity, not a test of any vendor figure. | Definition of "pinions alone", load case, test conditions; a way to reproduce the ratio. | Vendor paper or test report if published; otherwise state "no public baseline". |
| Rack phase differential (RPD) | Not covered. Rack-tooth mechanics, guide contact and connection flexibility are listed as omitted (`jackup-failure-sequence.md:242-243`). | Allowed RPD per design, how it is measured and corrected, and how it enters the pinion load share. | Drilling Contractor (Sept/Oct 2006) article on JU2000E-class units, which the article cites for RPD tolerances. |
| Preload procedure and hold time | Covered as a screen: four-stage per-leg history, 1.0 h minimum full-load hold, settlement-rate limit, strict equality fails (`jackup-screening.md:316-343`); air-gap mitigation is qualitative only (`:367-374`). Controls are illustrative. | Target preload per leg, hold time and settlement criterion, hull air gap during preload, when chocks are engaged (preload on pinions vs chocks). | SNAME T&RB 5-5A Rev 3; ISO 19905-1; Li et al. 2018 settlement study already cited at `jackup-screening.md:52`. |
| Punch-through / foundation | Covered as a screen: layered penetration, peak vs preload, descending-branch warnings (`jackup-layered-penetration.md:4-5, 114-120`); soil sensitivity range (`jackup-soil-sensitivity.md:4-7`). Soil and preload are assumed. | Site soil profile, penetration curve, peak vs applied preload, leg-position-specific data. | City St George's jack-up conference papers already cited (e.g. Jackup-2015-11, 2015-19). |
| Standards clauses (SNAME 5-5A, ISO 19905-1) | Clause numbers "TO CONFIRM" (`jackup-failure-sequence.md:66-67`, `jackup-layered-penetration.md:28`, `jackup-soil-sensitivity.md:78`). Clause map with OPEN items at `jackup-screening.md:392-420`. | Clause-by-clause check list with each item implemented, not implemented, or out of scope. | SNAME T&RB 5-5A Rev 3 (2008); ISO 19905-1 (2012). |
| Slender legs / chord reserve | Chord buckling check with assumed section and effective length (`jackup-failure-sequence.md:95`, inputs at `:38-40`); chord governs the base elevated storm case. Assumed members. | Leg section, effective length, reserve ratio to the governing load. | Class rules; ISO 19905-1 leg checks. |
| What the basis discloses | Assumption ranges, OPEN lists and omissions are stated (`jackup-failure-sequence.md:236-249`). The wider incident-screening basis (I-0369, db1k) sits outside this repo in the review folder and is held for legal screening. | One page per design: capacity ratings and their baselines, preload and hold criteria, soil data used, standards edition, what was not checked. | The article's own list; ISO 19905-1 reporting clauses. |

## Suggested next steps (method only)

1. Split the lumped holding input into pinion (brakes set, jacking) and chock
   capacities, each as a labelled user input with no vendor value supplied.
2. Add a "baseline test" helper that takes a claimed capacity ratio and its stated
   baseline and reports what extra information is needed to check it.
3. Add an RPD input to the omitted-physics list with a placeholder load-share
   parameter, marked OPEN.
4. Replace "TO CONFIRM" clause markers once the standards are in hand.
