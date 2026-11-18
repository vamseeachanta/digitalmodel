---
code_id: ILLUSTRATIVE-IFRT
publisher: Digitalmodel
revision: 1
---

# Synthetic IFRT example input basis

Stable ID: ifrt-gasoline-30m-v1. Created 2026-11-02.
Source: explicitly assumed inputs in `examples/code_checks/ifrt_example.py`.
Readiness: illustrative only; no measured or client data are represented.

Geometry: welded tank shell, 30 m diameter, 15 m shell height, six round
fixed-roof columns each 0.3 m diameter, bolted floating deck with unknown seam
length. Inventory: one bolted gasketed access hatch, one gasketed gauge hatch,
one gasketed vacuum breaker, one unslotted guidepole with gasketed cover and
pole sleeve, six column wells with fabric sleeves and twenty adjustable IFR legs
with sleeve lengths less than 12 inches. This is an assumed inventory, not an
AP-42 typical-count table or an API 650 construction requirement.

Gasoline assumptions: true vapor pressure 35 kPa absolute at the assumed annual
average liquid surface condition, atmospheric pressure 101.325 kPa absolute,
vapor molecular weight 62 kg/kmol, liquid density 740 kg/m3, annual net
withdrawal 100000 m3/year, light-rust shell clingage selection, intact materials,
afloat deck and open fixed-roof vents. These are not generic gasoline properties.

Independent vent illustration: fill 500 m3/h, withdrawal 750 m3/h;
transfer factors 1 and 2 Nm3 air/m3 liquid; thermal in/out duties each
1000 Nm3 air/h. Rated pressure capacity 2200 Nm3/h at 1.5 kPa differential
with 0.2 kPa connected loss; rated vacuum capacity 2000 Nm3/h at 0.3 kPa
differential with 0.1 kPa connected loss. Assumed tank limits are +2 kPag and
0.5 kPa vacuum magnitude. All air flows refer to 0 C and 101.3 kPa absolute.

These vent factors, thermal duties, tank limits and curve points are round
illustrative inputs for arithmetic verification, not certified device curves;
the transfer factors are not taken from API 2000, and
the governing API 2000 edition's factors must be confirmed by the caller. API 2000 edition qualification, tank-specific thermal
duty derivation, open-vent circulation and all emergency cases remain unresolved.
The vent illustration does not establish a protective arrangement for this tank.
