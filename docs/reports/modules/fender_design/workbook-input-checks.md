# Fender design workbook: input checks

The universal workbook (`fender-design-generic.xlsx`, written by
`examples/hydrodynamics/fender_design/build_fender_workbook.py`) recalculates in a
spreadsheet program, so every value a reader can edit has to meet the same rule the
Python module (`digitalmodel.hydrodynamics.fender_design`, with the case loader
`examples/hydrodynamics/fender_design/fender_case.py`) applies to the argument it
mirrors. This page lists every such input cell, the argument, every rule the module
applies to it, and the workbook cell that applies the rule.

## How the workbook applies a rule

- **One checked cell per input.** Each input on `Inputs` has a checked cell on `Calc`
  (`MC` for `M`, `XC` for `X`, and so on). The checked cell returns the value when the
  module accepts it and `#N/A` otherwise. No other formula reads the input; the
  `Inputs` Check column (column E) is the only other reader, to state the error.
- **One rule text.** The generator holds each rule once (`INPUT_RULES`); the same text
  is the condition of the checked cell and the data-validation formula (stop style) on
  the input cell. The type is tested first (`ISNUMBER` or `ISTEXT`), so text or a
  logical value in a number cell gives `#N/A`, never another error.
- **Blank.** Where the module takes None (DWT, velocity override, reaction and
  hull-pressure limits), a blank cell is accepted and means what None means to the
  module; any non-blank value has to meet the rule.
- **Case refused.** The module refuses the whole case when any input breaks a rule (it
  raises before any result). `Calc` `INPUTS_OK` is 1 when every input and catalogue
  row is accepted and `#N/A` otherwise, and every result reads it: the energy of every
  method (`Methods` H and J, so the selected design energy), and the capacity,
  utilisation, design reaction, design hull pressure and the Energy, Reaction and
  Pressure OK checks on `Fenders`. No result or check is shown while an input is
  refused; the Check column names the value to correct.
- **Method tables.** A rule that belongs to one method's table (a velocity table that
  does not cover the ship, no DWT) gives `#N/A` in that method's row only, as
  `compare_methods` leaves out only that method. The selected design energy is then
  `#N/A` if the selected method is the one left out, as `compute` raises.
- **Catalogue rows.** On `Fenders`, the row check (column V) applies the catalogue
  loader's rules (`FenderRating`) to every value of the row; the rated energy, rated
  reaction and hull pressure are read only through the checked columns S, T and U,
  which are `#N/A` for a refused row. A refused row also clears `INPUTS_OK`, as the
  loader refuses the whole catalogue.

## Named inputs (`Inputs` column B)

| Input | Row | Python argument | Module rule (function that applies it) | Checked cell | Blank means |
|---|---|---|---|---|---|
| `M` | 2 | `Ship.displacement_t` | number above 0 (`Ship`, `_finite`); BS 6349-4 velocity: 1,000 to 500,000 t (`brolsma_velocity`), UFC 4-152-01 velocity: 1,000 to 100,000 LT (`ufc_velocity`), method rows only | `MC` | refused |
| `DWT` | 3 | `Ship.dwt_t` | number above 0 or None (`Ship`); None: PIANC 2002, Ueda and EAU refuse (`_brolsma`, `_m_eau`); PIANC 2002 and Ueda velocity: 1,000 to 500,000 t (`brolsma_velocity`); EAU velocity: 5,000 DWT or more (`eau_velocity`), method rows only | `DWTC` | no DWT: PIANC 2002, Ueda and EAU rows `#N/A` |
| `LBP` | 4 | `Ship.lbp_m` | number above 0 (`Ship`) | `LBPC` | refused |
| `BEAM` | 5 | `Ship.beam_m` | number above 0 (`Ship`) | `BEAMC` | refused |
| `T` | 6 | `Ship.draft_m` | number above 0 (`Ship`) | `TC` | refused |
| `RHO` | 7 | `Ship.water_density_t_m3` | number above 0 (`Ship`) | `RHOC` | refused |
| `DEPTH` | 8 | `Berth.water_depth_m` | number above 0 (`Berth`); above the draught T, keel clearance (`_ukc_ratio`, every method) | `DEPTHC` | refused |
| `X` | 9 | `Berth.contact_x_from_bow_m` | number, not negative (`Berth`); at most LBP, contact on the ship (`contact_offset_m`) | `XC` | refused |
| `ALPHA` | 10 | `Berth.berthing_angle_deg` | finite number, any sign (`Berth`) | `ALPHAC` | refused |
| `STRUCT` | 11 | `Berth.structure` | one of `open`, `semi_closed`, `closed`, exact lower case (`Berth`, `STRUCTURES`) | `STRUCTC` | refused |
| `DEFL` | 12 | `Berth.fender_deflection_m` | number above 0 (`Berth`) | `DEFLC` | refused |
| `TUGS` | 13 | `Case.tugs` | true or false (`Case`); workbook code 1 (true) or 0 (false) | `TUGSC` | refused |
| `NCALLS` | 14 | `Case.berthings_per_year` | number above 0 (`Case`) | `NCALLSC` | refused |
| `CONT` | 15 | `Case.continuous_berth` | true or false (`Case`); workbook code 1 (true) or 0 (false) | `CONTC` | refused |
| `VOVR` | 16 | `energy_by_method(velocity_m_s=...)`, case `design.velocity_m_s` | None or a number above 0 (`_caller_velocity`) | `VOVRC` | each method's own velocity |
| `N` | 17 | case `design.fenders_in_contact` | whole number, 1 or more, not a logical (`check_fenders_in_contact`, called by `compute`, `standard_sweeps` and `parameter_sweep`) | `NC` | refused |
| `ETOL` | 18 | `design_performance(energy_tolerance=...)` | number from 0 up to, not including, 1 (`design_performance`) | `ETOLC` | refused |
| `RTOL` | 19 | `design_performance(reaction_tolerance=...)` | number, not negative (`design_performance`) | `RTOLC` | refused |
| `ANG` | 20 | `design_performance(angular_factor=...)` | number above 0 and at most 1 (`design_performance`) | `ANGC` | refused |
| `RLIM` | 21 | case `design.reaction_limit_kn` | None or a number above 0 (`check_limit`, called by `compute`, `size_checks`, `standard_sweeps` and `parameter_sweep`; the rule `select_fenders` applies) | `RLIMC` | no reaction limit: Reaction OK `-` |
| `PLIM` | 22 | case `design.hull_pressure_limit_kpa` | None or a number above 0 (`check_limit`, as for `RLIM`) | `PLIMC` | no hull-pressure limit: Pressure OK `-` |
| `UFCF` | 23 | `Case.ufc_f_basis` | `berthing_coefficient` or `block_coefficient` (`Case`, `UFC_F_BASES`); workbook code 1 or 0 | `UFCFC` | refused |

Table 1. The 22 named inputs: Python argument, the 29 module rules on them (5 of them
limits of one method's table) and the checked cell. Row is the row on `Inputs`.

## Catalogue values (`Fenders`, one row per fender)

| Column | Field | Module rule (`FenderRating`, read by `load_fender_catalogue`) | Read through |
|---|---|---|---|
| Fenders `D` | `diameter_mm` | blank or a number above 0 | row check V only |
| Fenders `E` | `length_mm` | blank or a number above 0 | row check V only |
| Fenders `F` | `internal_pressure_kpa` | blank or a number above 0 | row check V only |
| Fenders `G` | `rated_energy_knm` | number above 0 | checked column S |
| Fenders `H` | `reaction_kn` | number above 0 | checked column T |
| Fenders `I` | `hull_pressure_kpa` | blank or a number above 0 (blank: no datum; a set pressure limit is not met) | checked column U |
| Fenders `J` | `rated_deflection_pct` | blank or a number above 0 and at most 100 | row check V only |

Table 2. The 7 catalogue value columns and their 7 module rules. Each column also
carries the rule as data validation (stop style) over the catalogue rows.

## Cells that are not inputs

- `Fenders` columns A to C and R (identifier, manufacturer, type, source): text, read
  by no formula. The fender type selects the curve peak factor at generation.
- `Inputs` rows below "Fixed at generation" (method, navigation condition, condition
  class, ship type, factor position, factor override, consequence class, CoV_M,
  multiple contact, base fender and catalogue): read by no formula; they select
  tables and constants when the workbook is generated and need regeneration.
- `Velocity` tables, `Methods` constants (Cm, Ce, Cs, Cc where the method fixes them,
  factors, capacity divisor), `Sensitivity`, `Tornado`, `Sizes` and `Sources`: written
  by the generator from the module; they are not input cells.

## Where the workbook is stricter

A logical TRUE or FALSE typed in `TUGS` or `CONT` is refused (`#N/A`), although the
module takes a boolean: the cells' codes are 1 and 0. Refusing it cannot give a result
the module would not give. `N` is checked as a whole-valued number: a spreadsheet holds
2 and 2.0 alike, while the case file needs the integer 2 (the generator writes it).

## Tests

`tests/hydrodynamics/fender_design/test_workbook_input_checks.py`:

- the generated and the committed workbook: every formula that reads an input is its
  checked cell or its Check cell, no formula addresses `Inputs` cells directly, every
  input cell on `Inputs` is in this inventory, and `Fenders` results read the catalogue
  values only through the checked columns (a test also proves the scan reports an
  unchecked read);
- the checked cell, the data-validation formula and the Check text come from one rule;
- for every input and catalogue column, each value the module refuses (zero, negative,
  out of range, text, a logical, blank where blank is refused) is refused by the module
  in the test and gives `#N/A` in the checked cell, every energy, capacity, reaction and
  check, and an `ERROR` text in the Check column;
- accepted values, boundaries included (contact at 0 and LBP, tolerances 0, angular
  factor 1, one fender, blank limits and override) equal the module per method and per
  fender; ships beyond a method's velocity table give `#N/A` in that method's row
  exactly where the module leaves the method out.
