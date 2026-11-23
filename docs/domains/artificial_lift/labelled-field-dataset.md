# Labelled field-card acquisition and intake

The dataset has not been received. Contact, written permission and real-card
accuracy are **NOT_EVALUATED**. The request below is prepared for the maintainers
to send through the publication's contact route; no message has been sent by
this implementation. No field cards, contact details or permission documents
are distributed here.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| Initial | 2026-10-19 | Prepare the request and a permission-gated intake adapter so missing data cannot become an accuracy claim. |
| Correction | 2026-10-24 | Match default export headers and count records in intake errors; check the inventory tables against the open PDFs. |

## Source basis

The following open sources were read for acquisition, representation, labels
and evaluation limits:

- [Diagnostic of Operation Conditions and Sensor Faults Using Machine Learning in Sucker-Rod Pumping Wells, Sensors (2021), 21, 4546](https://doi.org/10.3390/s21134546).
  [Open publisher PDF](https://mdpi-res.com/d_attachment/sensors/sensors-21-04546/article_deploy/sensors-21-04546.pdf):
  section 1 describes data from 38 wells; section 3 describes manual review of
  surface and downhole cards; Table 1 lists the class counts; the Data Availability
  Statement offers data on request. Article access is not a dataset licence.
- [Doctoral thesis (2023), chapter 5, pages 38–40, Table 5.1, and chapter 7](https://repositorio.ufrn.br/server/api/core/bitstreams/61bea1b0-ca08-4a0d-9539-511ed6289171/content).
  The review tool stores labels with downhole cards. Equations (5.1)–(5.2)
  normalize each axis independently. The text warns that normalization can
  obscure a parted-rod pattern. Similar consecutive cards are not independent
  evidence of generalization.
- [Monograph (2025), section 3.1, Tables 3.1–3.2, Appendix A, and conclusion](https://repositorio.ufrn.br/server/api/core/bitstreams/e8346c07-e473-4cb7-ac48-ca06175554a0/content).
  The described export has loads, positions and type; the appendix uses
  `cargas`, `posicoes`, `tipo`. It reports 49,079 downhole cards and five
  classes. Equations (2.6) and (2.8) define accuracy and recall. The conclusion
  limits generalization because training and testing use the same wells.

The publisher PDF and both theses were accessible. The article landing page
returned an access denial and its alternative archive returned a browser
challenge; neither response was used as scientific evidence.

### Reconcile before use

The open PDFs were checked on 2026-10-25 (UTC). Article Table 1 and doctoral
thesis Table 5.1 agree on the following counts and valve associations; the
monograph subset below was checked against its Table 3.1. These checks confirm
the printed tables and their arithmetic, not any received dataset. The valve
association difference between the sources remains unresolved.

Table 1 of the article and Table 5.1 of the doctoral thesis give:

| Published label | Published cards | Current mode |
| --- | ---: | --- |
| Normal | 10,282 | NORMAL |
| Gas interference | 260 | GAS_INTERFERENCE |
| Fluid pound | 38,298 | FLUID_POUND |
| Traveling valve leak | 67 | VALVE_LEAK_TV |
| Standing valve leak | 172 | VALVE_LEAK_SV |
| Rod parted | 29 | ROD_PARTING |
| Gas lock | 6 | GAS_LOCK |
| Tubing anchor malfunction | 30 | Unmapped: mechanism requires review |
| Sensor fault — rotated card | 895 | Unmapped: sensor fault is outside the pump-mode taxonomy |
| Sensor fault — line card | 73 | Unmapped: sensor fault is outside the pump-mode taxonomy |

The printed counts sum to **50,112**, not the 50,012 stated in the issue.
The later monograph's Table 3.1 gives 10,441 normal and 38,139 fluid-pound
cards, plus 260 gas-interference cards, 67 standing-valve cards and 172
traveling-valve cards. Its counts total **49,079**. These are distinct source
inventories, not interchangeable file checksums or acceptance targets. In
particular, the valve counts differ in association. Do not infer integer type
codes from counts: obtain the dictionary for the actual export in writing.

The crosswalk is a proposed semantic correspondence, not validation of a
diagnosis. `acquisition_status()` lists current modes without a proposed match
from the repository's current generator registry; it does not freeze the older
mode count mentioned in the issue. Tubing-anchor malfunction is deliberately
not assumed to be identical to the narrower `TUBING_MOVEMENT` mechanism.

## Request prepared for the maintainers

Subject: Request for labelled dynamometer-card export and use terms

The open implementation would benefit from a labelled real-card benchmark.
The Sensors article (2021), DOI 10.3390/s21134546, Table 1 and its Data
Availability Statement describe the relevant dataset. The doctoral thesis
(2023), chapter 5, and the monograph (2025), section 3.1 and Appendix A,
describe its review process and an existing three-column export.

Could the existing loads/positions/type export be made available, preferably
with both original surface cards and corresponding downhole cards where
permitted? Table 1 reports the class counts reproduced above. Their sum differs
from the issue's total, and the later table associates the valve counts
differently. Please confirm the supplied subset's counts and label dictionary,
including integer codes, omitted classes and any relabelling.

Please also confirm axis units, normalization, point order, resampling, the
downhole reconstruction method and label-review provenance. Physical amplitudes
or the original scaling metadata are needed for this implementation's current
classifier. Normalized cards alone cannot be treated as inches and force.
Anonymous grouping identifiers for wells and acquisition sequences would allow
held-out-well evaluation without publishing well identities. These grouping
identifiers should be supplied separately from the three-column card export.

Before any data use, written terms are requested covering benchmark use,
training if permitted, derivative models, aggregate result publication,
redistribution restrictions and required attribution. Please specify any
embargo, retention or deletion obligations and confirm authority to grant use.
No redistribution or training rights will be inferred from article access.

The repository's open surface-to-downhole implementation and a reproducible
benchmark report can be offered in return. Acknowledgement will follow the
agreed terms; any authorship arrangement requires a separate agreement based
on actual contributions. No solver accuracy claim is made in this request.

## Intake and method

`benchmark.field_dataset.load_three_column_export` reads a local CSV only after
the caller records written benchmark permission, its evidence reference,
licence and attribution. This is a caller attestation, not independent legal
verification. Keep the grant and raw data outside the public repository.
Require the supplier-confirmed SHA-256 digest and an explicit codebook reference.
The digest check identifies bytes, not label correctness or authorization.
The returned `FieldDataset` retains that digest, the codebook reference and the
permission attestation. Reports retain the permission reference and attribution;
these references can also be private and require review before publication.

Each array cell must contain a JSON array of finite numbers. Only the selected
three columns are accepted, to reject accidentally included identity columns.
The defaults are `cargas`, `posicoes`, `tipo`, as in monograph Appendix A.
For other headers, supply `columns` explicitly in load/position/type order,
for example `columns=("loads", "positions", "type")`. Invalid-row errors count
CSV records starting at 2 after the header, including cells with line breaks.
This does not anonymize arbitrary cell contents; review the export before
loading. Unknown types remain counted and unscored. Preserve point traversal
order. No sorting, interpolation, unit conversion or normalization occurs.
The adapter requires pump cards in inches and lbf; normalized-only exports and
surface-only exports remain **NOT_EVALUATED** until a separately reviewed
conversion or inversion is available. No licensed solver is needed.

`evaluate_export` connects the mapped set to the existing `real_accuracy`
measurement path. Both numerical classifier callbacks must be supplied
explicitly; no external service is called. `minimum_count` and `confidence`
are required caller choices, with no new acceptance limit. Per-class
`accuracy` is correct/actual-class count (recall), with confusion counts and
score intervals from the existing measurement module. The interval source is
the [statistical handbook, section 7.2.4.1](https://www.itl.nist.gov/div898/handbook/prc/section2/prc241.htm).
Classes below the chosen count retain counts but omit percentages. Published
class counts never stand in for measured counts.

Run the worked example from the repository root:

```sh
PYTHONPATH=src python examples/domains/artificial_lift/field_dataset_demo.py
```

Its invented square cards and fixed predictions demonstrate the adapter:
two normal cards, one correct prediction, give **50.0 %** normal recall.
One gas-interference card is too thin under the example's chosen count of two.
The field-data section remains **NOT_EVALUATED**. The fixtures and the count
are software examples, not field observations or engineering limits.

## Remaining acquisition and validation work

- The maintainers must send the request, record the response and agree written
  rights and attribution before intake. No request receipt is claimed.
- Confirm the file digest, the integer codebook and the table discrepancies.
- Receive physical pump-card amplitudes or arrange a reviewed reconstruction.
- Keep well grouping and split provenance privately; this adapter does not
  establish independence from classifier training or create a held-out split.
- Benchmark both numerical paths with an explicit reporting policy. Report
  actual per-class counts and name rare/unmapped classes. Do not claim
  cross-well generalization from card-level score intervals: correlated cards
  need a separately designed well-level study.
- Inspect reports for private provenance before publishing aggregate results;
  the existing measurement path retains supplied labelling provenance.
