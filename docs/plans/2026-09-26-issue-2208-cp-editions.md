# Plan: make the DNV-RP-B401 / DNV-RP-F103 edition argument functional (#2208)

> Issue: #2208 | Epic: #2206 | Owner decision: D3 (keep editions open)
> Status: approved by owner instruction "continue with tasks" (2026-09-26); implementation in worktree `feat/cp-2208-editions`, stacked on the #2211 branch.
> Owner research note: keep as many edition options for clients as possible.

## Why
`b401_tables.py` and `f103_tables.py` (#2207) accept edition tokens but every
edition returned the 2010/2011 numbers; 2017/2021 (B401) and 2016 (F103) were
flagged `inherited-*-unverified`. The 2017 and 2021 B401 prints and the 2019
F103 print (republished July 2016 edition, amended May 2021) are now on file
(`/mnt/ace/O&G-Standards/DNV/`, pdftotext text layer, hand-verified
2026-09-26), so every edition can be table-real and cited to its own wiki page
revision.

## Source facts (verified against the extracts)
- B401 June 2017: design tables moved to Appendix A (Table A-1 .. A-8); values
  of A-1, A-2, A-3, A-4 (Cat I-III), A-6, A-8 identical to the 2010 Tables
  10-1 .. 10-8. Protective potential [5.4]; buried density [6.3.8].
- B401 May 2021: Sec. 8 (Table 8-1 .. 8-8). 8-1/8-2/8-3/8-8 identical. 8-4 adds
  Category IV (a = 0.02; b = 0.008 for 0-30 m, 0.005 for >30 m). 8-6 is keyed
  by anode surface temperature: Al-Zn-In <=30 °C seawater -1.050 V / 2000
  Ah/kg, sediment -1.000 / 1500; 60 °C -1.050 / 1500 and -1.000 / 680; 80 °C
  -1.000 / 720 and -1.000 / 320; Zn seawater -1.030 / 780 (one cell printed
  across the <=30 and >30-50 rows), sediment <=30 -0.980 / 750, >30-50
  -0.980 / 580. Protective potential [2.4.2]; buried density [3.3.8].
- F103 September 2019 (amended May 2021, editorial only): Table 6-2 mean
  current density with five fluid-temperature bands (<=25 / >25-50 / >50-80 /
  >80-120 / >120 °C): non-buried 0.050 / 0.060 / 0.075 / 0.100 / 0.130,
  buried 0.020 / 0.030 / 0.040 / 0.060 / 0.080. Table 6-3 anode design values
  equal B401 2021 Table 8-6. Table A-1 prints a and b directly (no x100):
  GFR asphalt enamel (CDS 4, concrete) 0.01 / 0.0003; FBE (CDS 1) with
  concrete 0.030 / 0.0003, without 0.030 / 0.0010; 3LPE and 3LPP 0.001 /
  0.00003 (with or without concrete); FBE/PP thermally insulating 0.0003 /
  0.00001; FBE/PU thermally insulating 0.01 / 0.003; polychloroprene (CDS 5)
  0.010 / 0.001; coal-tar enamel absent. Table A-2 uses DNVGL-RP-F102 (2011)
  ids: none + 4E(1) PU 0.30 / 0.030; 1D or 2A + mastic 0.10 / 0.010; 2B(1)
  0.03 / 0.003; 2C(1) 0.03 / 0.003; 3A FBE 0.10 / 0.010 (with 4E(2) infill
  0.03 / 0.003); 2B(2) 0.01 / 0.0003; 5D(1)/5E 0.01 / 0.0003; 2C(2) 0.01 /
  0.0003; 5A/B/C(1) 0.01 / 0.0003; 5C(1) and 5C(2) moulded infill on FBE
  0.01 / 0.0003; 8A polychloroprene 0.03 / 0.001. Bracelet utilisation max
  0.80 [6.4.2]; protective potential -0.80 V CMn steel [6.7.11].

## Scope
1. Fixture CSVs under `tests/fixtures/test_vectors/cathodic_protection/datasets/`
   for `dnv-rp-b401/2017-06/` (A-1, A-2, A-3, A-4, A-6, A-8),
   `dnv-rp-b401/2021-05/` (8-1, 8-2, 8-3, 8-4, 8-6, 8-8) and
   `dnv-rp-f103/2019-09/` (6-2, 6-3, A-1, A-2); caption row, header rows,
   data rows, same layout as the 2005/2010 CSVs; `PROVENANCE.md` extended.
2. `b401_tables.py`: per-edition source record (citation revision, wiki page,
   table prefix, provenance, clause labels). 2005/2010 -> Table 10-x, revision
   "2011"; 2017 -> Table A-x, "2017-06", `dnv-rp-b401-2017.md`; 2021 -> Table
   8-x, "2021-05", `dnv-rp-b401-2021.md`. `PaintCategory.IV` (2021 only,
   `ValueError` otherwise). `anode_capacity` / `anode_closed_circuit_potential`
   / `design_driving_voltage` gain `anode_surface_temperature_c: float = 30.0`;
   2021 selects the Table 8-6 row (next row at or above the requested
   temperature, i.e. no interpolation, conservative), earlier editions raise
   above 30 °C. `edition_provenance` -> `verified-<edition>-tables`.
3. `f103_tables.py`: `F103Edition = Literal["2010", "2019"]`; "2016" is a
   warned alias of "2019" (the 2016 print is not on file; the 2019 print
   republishes it unchanged; owner D3 keeps the option); "2021" alias -> "2019". Five 2019
   temperature bands; Table 6-3 anode lookups (own F103 citation for 2019,
   B401 deferral for 2010); Table A-1 with `concrete_weight_coating: bool`;
   `FieldJointCoating2019` enum with the F102 (2011) ids (amended-2021 names
   recorded on the rows); bracelet utilisation 0.80 cited to [6.4.2] for 2019.
   `DEFAULT_F103_EDITION` stays "2010" so existing results do not change.
4. `_edition.py`: standard strings with the confirmed months; no "inherited"
   wording anywhere in the package.
5. Callers: `dnv_rp_f103.design_bracelet_cp` (new optional inputs
   `concrete_weight_coating`, `anode_surface_temperature_c`), `coating.py`
   (`concrete_weight_coating` threaded), `marine_structure_cp.py`
   (`anode_surface_temperature_c` for the default driving voltage),
   `dnv_rp_b401.py` (F103 companion map 2017/2021 -> 2019), legacy
   `cp_DNV_RP_B401_2021.py` unchanged (token pass-through).
6. Citation fixture pages `dnv-rp-b401-2017.md`, `dnv-rp-b401-2021.md`,
   `dnv-rp-f103-2019.md` under `tests/citations/fixtures/...`;
   `FIXTURE_PROVENANCE.md` updated. Real wiki pages created in parallel in
   llm-wiki with the same frontmatter.
7. Tests: `test_b401_tables.py` / `test_f103_tables.py` parse every edition's
   CSVs; new `test_edition_crosswalk.py`; `test_edition_api_foundation.py`
   "2017 == 2021" assertions replaced by the true statement.

## Assumptions
- Table 8-6 / 6-3 Zn seawater cell (-1.030 V / 780 Ah/kg) is printed once
  across the <=30 and >30-50 °C rows and is read as valid for both rows.
- Al rows at 60 and 80 °C are applied stepwise (30 < T <= 60 uses the 60 °C
  row, 60 < T <= 80 the 80 °C row); above 80 °C (Al) or 50 °C (Zn) the lookup
  raises. Interpolation permitted by F103 [6.4.4] is not implemented.
- `FieldJointCoating.NONE` under F103 2019 maps to the "none + 4E(1) PU" row
  (same a/b, bare steel with infill); every other 2010 id must be re-selected
  from `FieldJointCoating2019`.
- 2019 Table A-1 coatings printed with a single concrete flag return that row
  regardless of the requested flag (the flag is a compatibility column).

## Verification
Run set from the worktree root (see the issue): `tests/cathodic_protection`,
`tests/specialized/cathodic_protection`, `tests/marine_ops/marine_engineering/
test_cathodic_protection_dnv.py`, `tests/benchmarks/test_cp_benchmarks.py`,
`tests/citations` (two pre-existing Windows-only resolver-log failures);
`ruff check` and `mypy` clean on every touched file.
