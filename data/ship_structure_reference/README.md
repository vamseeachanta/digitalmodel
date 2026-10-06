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
