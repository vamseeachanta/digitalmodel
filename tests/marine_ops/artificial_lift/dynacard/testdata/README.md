# Dynacard test fixtures

## `plate_reference_cards.json` — canonical downhole card shapes

Ten reference dynamometer cards digitised from a published sucker-rod
working-condition diagnosis plate (标准图版). They are the ground truth that
`test_card_generator_shapes.py` holds the synthetic generators in
`card_generators.py` against, so that generator shapes cannot drift away from
the shapes real pumps make without a test going red.

### Provenance

| | |
|---|---|
| DOI | [10.17632/hnfwm9mjs6.1](https://doi.org/10.17632/hnfwm9mjs6.1) |
| Dataset | *Data for: Real time and intelligent diagnosis method of working condition in rod pumping well based on multi-dimensional data coupling of reservoir-wellbore-surface* |
| Author | Zhang, Ruichao |
| Publisher | Mendeley Data, 2020 |
| Licence | **CC0-1.0** — [Creative Commons Zero v1.0 Universal](https://creativecommons.org/publicdomain/zero/1.0/legalcode), public domain dedication |
| Source file | `Working condition diagnosis plate.xls` |
| SHA-256 | `69723091c93269761a2fa79fc774a6ae7bf0eff786af439fb94c40171018058e` |

CC0 is a public-domain dedication, so redistributing the digitised curves here
carries no rights condition. Attribution is recorded because it is good
practice, not because the licence demands it.

### What was extracted

Each sheet of the plate workbook holds **two** curve blocks in adjacent pairs
of numeric columns:

* **block 0 — the dynamometer card.** This is what is reproduced here.
* **block 1 — the motor-current diagram.** Not a card; not reproduced. Reading
  block 1 as a card is the obvious way to get this wrong.

Coordinates are the plate's own arbitrary plotting units. Nothing downstream
depends on their scale: the comparison normalises each axis to 0–1 before
measuring anything, because the plate carries no engineering units.

### Sheets and the modes they map to

| Sheet | Mode |
|---|---|
| 工况正常 | `NORMAL` |
| 供液不足 | `FLUID_POUND` |
| 气体影响 | `GAS_INTERFERENCE` |
| 抽油杆断脱 | `ROD_PARTING` |
| 上碰泵 | `PUMP_TAGGING_UP` |
| 下碰泵 | `PUMP_TAGGING_DOWN` |
| 游动阀漏失 | `VALVE_LEAK_TV` |
| 固定阀漏失 | `VALVE_LEAK_SV` |
| 活塞脱出工作筒 | `PLUNGER_OUT_OF_BARREL` |
| 出砂 | `SAND_ABRASION` |

Three further sheets in the source workbook (过平衡 over-balance, 欠平衡
under-balance, 减速箱故障 gearbox fault) describe surface-unit conditions
rather than pump conditions and have no generator, so they are not carried
here.

The remaining eight modes in `ALL_GENERATORS` have no exemplar on this plate
and are therefore not shape-tested. That gap is real and is listed in the test
module.

### Other files in this directory

The `*.json` files named by numeric id are field card captures used by the
vision-benchmark and data-loader tests; they are unrelated to this fixture.
