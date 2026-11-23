# Power cable source catalogue: first slice

This is a fixed-layout transcription of the read material in the
[research note on the issue](https://github.com/vamseeachanta/digitalmodel/issues/1792#issuecomment-6115262341),
checked 2026-10-11. It contains eight source records (1–7 and 10), eleven
indicative static cable rows, two dynamic cable property sets, nine array-cable
layers, ten bonded-hose layers and six derivation statements. The note records
sixteen sources: eleven read (some only as abstract or record) and five located
but not read. Those note totals describe the research pass, not this slice.

The note describes the pass as strong on offshore-wind power cables and thin
on umbilicals and unbonded flexible pipe. Property derivation is recorded in six
statements. Rigid pipelines were not researched. No bending stiffness, torsional stiffness or minimum bend radius
value was read for any family. The tabulated properties are indicative, rather
than qualified design inputs. The bonded-hose build-up has no dimensions.

| Key | Layout |
|---|---|
| Header and `limits` | Schema version, slice, note origin, checked date and note status statements |
| `sources` | Note entry IDs and exact source, type, family, usable fields and access wording; DOI only where printed; null URLs |
| `static_array_cable_33kv` | Descriptor and eleven conductor-size, diameter and weight rows with source ID and table/page locator |
| `dynamic_cable_66kv` | Descriptor, two five-property sets with value/unit/source ID/locator and armour caveat |
| `layer_buildups` | Ordered array-cable and bonded-hose layers; comparison, load-limit ratios and manufacturer-attributed designation |
| `stiffness_derivation_summary` | Five method statements and the read-tools statement, with source IDs and white-paper section locator |

Table 1. Catalogue layout. Numeric units are recorded in field names or the `unit` field.

`source_id` identifies the primary source; `source_ids` records additional sources
named by the note for combined statements and layer descriptions. Locators retain
the printed page, table or section wording. The tools statement includes Helica
and UFLEX version 2.8.9; source 7 retains its record-only access status, and its
unavailable property database is excluded. Source titles and access remarks do
not establish numeric data for every family named in them.

The adjacent JSON Schema closes the top-level and record layouts. The offline
test module validates it with the already available jsonschema package and checks
provenance, numeric constraints, units, ordering and negative cases. No URL or DOI
is fetched. This catalogue is separate from `src/digitalmodel/subsea/cross_sections/`:
that module requires dimensioned geometry for fixtures; this slice supplies source
records and indicative properties without filling those geometry requirements.
No module fixture or existing report is changed.

## Excluded material, by note section

- Section 2.2: umbilical search-result descriptions only, unverified; brochures
  opened but not readable. No layer build-up or ranges are included.
- Section 2.3: unbonded flexible pipe, not read. The handbook layer description
  and standards-history abstract are outside this slice.
- Section 2.1: the voltage-trend and qualification-summary paragraph is outside
  the requested layer/property transcription.
- Section 3: the unverified individual-hose diameter and bend-radius line is
  excluded. No absent stiffness, bend-radius or allowable-tension data is filled.
- Section 4: BFLEX and explicit finite-element approaches appear only in
  unverified search-result descriptions and are excluded.
- Section 5: the designation list and edition-status discussion are excluded.
  API Spec 17K appears only in the manufacturer-attributed bonded-hose statement.
- Section 6: entries 8, 9 and 11–16 are excluded. Entry 7 is used only for the
  tool named in the read record; its inaccessible property tables are excluded.
- Section 7: no numerical comparison/test-case tables are transcribed; pending
  full-text test cases remain excluded. The source 4 properties are retained as
  properties, rather than claimed as executed engineering validation cases.
- Section 8: additional located material remains outside the catalogue.

## What the issue's acceptance criteria still lack

The acceptance criteria are **NOT met by this slice**.

1. **At least twelve entries across four asset families:** only eight source
   records are included. No umbilical, unbonded-flexible or rigid-pipeline
   exemplar entries are delivered. Method-source family names do not supply
   those missing exemplars; bonded hose has layers but no property ranges.
2. **Per-entry provenance, layers, numeric fields and licensing/access:** no URL
   or local path is recorded for any entry because the note prints none. Licensing
   is recorded only as an access remark. Layers and numeric fields are limited
   to what was read; no complete cross-family property sets are delivered.
3. **Deduplication and links to the wiki pages:** deduplication and wiki links
   are not done. A directory listing found none of the three named wiki pages
   in this worktree; no wiki page is updated.
4. **No proprietary or paywalled standard text copied:** no such text is included
   in this slice. This constraint does not supply the missing catalogue coverage
   in criteria 1–3.

The proposed deliverables also remain incomplete:

- **YAML/Markdown catalogue:** this first slice supplies the read cable data and
  bonded-hose layers only; the broader source/exemplar catalogue remains missing.
- **Standards map:** not delivered; section 5 remains in the research note.
- **Open-versus-paywalled gap list:** not delivered; exact access remarks are
  preserved without converting them into a licensing assessment.
- **Wiki updates:** not delivered; no wiki links or updates are added.
