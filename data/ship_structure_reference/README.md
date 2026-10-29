Ship structure references, issue #2288
=====================================

Existing discovery authorities remain llm-wiki:data/data-source-catalog.yml,
data/domain-database-index.yml (structural-ffs), and data/query_sources.json.
No replacement registry or ingestion pipeline is introduced. Existing
data/aisc_shapes.yaml is referenced without replication or edition upgrade.

Internal source research is staged as source_research.yml with a module catalog
in the existing DataCatalog format. Those local-only files are deliberately
excluded from the public draft: edition/rights evidence remains incomplete.
They include SSAB plate supply observations, two British Steel bulb-flat rows,
two equal-angle rows with a confirmed printed July2026 publication identifier,
and metadata for DNV/IACS; catalog rows are not stock-inventory confirmation.
Section properties retain mm2/mm3/mm4, nominal bare-section basis and publisher
axes; ambiguous warping data is withheld. Products do not prescribe panel spans.

Source pointers:
- SSAB: https://www.ssab.com/en/brands-and-products/ssab-multisteel/product-offer/ssab-multisteel-hs
- British Steel: https://www.britishsteel.co.uk/wp-content/uploads/2026/07/bulb-flats-brochure-20-07-26.pdf
- DNV: https://www.dnv.com/energy/standards-guidelines/dnv-rp-c201-buckling-strength-of-plated-structures/
- IACS: https://iacs.org.uk/resolutions/common-structural-rules/csr-for-bulk-carriers-and-oil-tankers
- Ovako: https://www.ovako.com/globalassets/downloads/products/dimensions_hot-rolled-flat-bar_2025.pdf

[Structured source pointers](source_metadata.yml) are authored discovery metadata,
not a replacement central catalog. [Method audit and results](../../docs/domains/plate-buckling/ship-structure-study.md)
links the private result owner and distinguishes numerical screens from acceptance.

Minimum record contract: issuer/URL/catalog ID (unknown if not issued), printed
edition and source vintage separately from observed date, table/page locator,
dimension kind, units, grade/condition, section axes/basis, source-specific rights,
qualification limits and processing status. Unknown evidence requires a reason.
Supply envelopes never imply every Cartesian combination exists. A stringer is
a structural role; its section/spacing/restraints/loads are project inputs.

Database coverage remains partial: discrete flat-bar/rolled-tee catalogs,
plate width/length combinations, material heat/certification, bulb axis mapping,
current IACS edition and source-specific publication rights need qualification.
Welded tees are fabricated from selected web/flange plates, not universal stock.
No ready database or engineering-acceptance claim is made.

Latest internal staging holds six sources, three products and four catalog
sections. Ovako's flat-bar sheet supplies a graphical width/thickness production
envelope, without discrete stock, grade, length or properties. Its boundaries are
not digitized or treated as a Cartesian matrix; no ideal rectangle properties or
marine certification are substituted. The numerical200x10 study fixture remains
authored geometry. Temporary inspection copies of this public sheet are removed
after review; no source PDF is retained or published.

Staging validation
------------------

The research schema version 2 requires per-source retrieval dates and rights
status, plus explicit metric units and delivery condition for section records.
An unpublished edition, unissued catalog ID or unknown condition remains an
`unknown` object with a reason.
Existing version 1 private records require an explicit migration: add the missing
evidence or reasoned unknowns before changing the version; no automatic upgrade
or inference is performed. `source_metadata.yml` is a discovery pointer document,
not a research record, and is not validated against this schema.

Validate a local JSON research record without running a calculation:

```sh
python scripts/studies/ship_structure_reference.py research.json
```

The command checks JSON Schema 2020-12 structural and date formats, absolute HTTP(S) source URLs, source
references, unique source/product/section identifiers and finite JSON numbers.
Duplicate JSON object keys are rejected at read time. Sections require explicit
axes and property basis. Dimension kinds distinguish a supply envelope, a discrete
catalogue and project geometry; none establishes stock availability. Source URLs
require a hostname and a valid port when supplied; authority parsing enforces
these requirements even without the optional URI format checker. Source URLs
are retained as metadata and are never fetched or resolved by this command.

Success returns `metadata_valid`, `engineering_status: NOT_EVALUATED` and
`publication_allowed: false`. Invalid records return exit code 2 without echoing
record contents. Metadata validity does not verify publisher accuracy, rights,
physical consistency, material certification or engineering applicability.
The existing bulb/angle solver exclusions remain required. The AISC dataset,
central catalogs and production buckling algorithms remain separate authorities.

Validation basis: [JSON Schema 2020-12 Validation](https://json-schema.org/draft/2020-12/json-schema-validation)
(2022), sections 6 and 7; finite JSON numbers follow
[RFC 8259](https://www.rfc-editor.org/rfc/rfc8259#section-6) (2017), section 6.
No engineering equations, coefficients or acceptance limits are introduced.

## Synthetic elastic reference input contract

[elastic_reference.schema.json](elastic_reference.schema.json) defines a separate
strict input contract for authored synthetic analytical fixtures. The worked
[elastic_reference_inputs.json](elastic_reference_inputs.json) contains explicit
metric geometry, material constants, plate/web/flange loss controls and source
provenance. All numeric inputs are author-selected examples; source relationships
identify the equations, not manufacturer or material-property evidence. The
input schema requires units, axes, condition, readiness, retrieval dates and
reasoned unknown catalog identities. No extracted section properties are added.

This reference route uses the existing independent square-plate kernel and the
finite rectangular mode equation in MIT 2.080J (2013), Lecture 11, Eqs. 11.15–11.17,
with Lecture 4, Eq. 4.60. Both official source pointers and retrieval dates are
in the fixture. See the [worked example and qualification limits](../../docs/domains/plate-buckling/ship-structure-study.md).

Validation rejects nonfinite values, booleans as numeric inputs, unknown keys,
duplicate cases/source identities, broken material/source links, ambiguous JSON
keys and incompatible units. Only bare simply-supported plates under fixed
uniaxial stress receive elastic reference values. Panel, local-loss and other
unqualified combinations keep null engineering minima. Saved HTML and JSON
lookup retrieve exact combinations without calculation or interpolation.
Existing research.schema.json remains the staging contract for manufacturer
research; neither contract establishes stock availability or engineering rights.
