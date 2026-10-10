# Ship plate and stiffener study

[Study, exact applicability and independent evidence](ship-structure-study.md)
is the entry point for issue2288. [Structured source pointers](../../../data/ship_structure_reference/source_metadata.yml)
link the authoritative manufacturer and class-rule pages; numerical extracted
source records remain internal staging pending rights. Existing AISC and central
source catalogs are reused.

[Private retained results, draftPR44](https://github.com/vamseeachanta/digitalmodel-data/pull/44)
contains self-contained lookup.html, precomputed.json and a provenance manifest
at docs/reviews/ship-plate-study/. Its local-loss-diagnostic subfolder is a separate
18-case patch-length/breadth study. Download the authorized HTML to retrieve saved
combinations; no server computation or interpolation runs from the selectors.

Eight original plate cases are numerical k=4 model screens;32 original panel
cases are illustrative. All local-patch acceptance is INAPPLICABLE. None establishes
current class-rule compliance, minimum accepted asset thickness or certification.
