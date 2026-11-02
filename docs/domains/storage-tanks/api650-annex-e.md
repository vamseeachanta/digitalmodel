# API 650 Annex E seismic screen

`digitalmodel.hydrodynamics.sloshing.api650_annex_e` returns effective liquid
weights and force heights, mapped-spectrum force coefficients, base shear,
ringwall/slab moments, sloshing clearance, an anchorage ratio and hoop demands.
`dataclasses.asdict(result)` gives JSON-compatible output including Citation
sidecars and limitations. The existing `screen_tank` and router are unchanged.

This is a **public-source screening basis**, using API 650 11th edition (2007)
load equations and the 12th-edition Addendum 2 (January 2016) freeboard provisions.
It does not establish compliance with a complete later edition. The 2007 wave
coefficient is 0.5; this screen deliberately uses the later 0.42 coefficient.
The older SRSS vertical hoop combination is retained and identified in the API.

## Inputs and interpretation

Every project factor is explicit. Supply design `sds_g`, `sd1_g`, mapped `tl_s`,
importance `importance_factor`, ASD reductions `rwi`/`rwc`, convective damping
factor `k`, and the governing `minimum_ai_g`. The minimum is at least 0.007;
additional site-class and high-hazard minima must be established outside this
routine using the project edition. Mapped Ss and S1 are not design SDS and SD1.
A response spectrum from a single accelerogram is not automatically a qualified
design spectrum. The existing seismic SDOF solver therefore is not invoked.

Lengths and net thickness are **metres**, density kg/m3, weights N, annular
resistance N/m, moments N m and stresses Pa. `_g` values are fractions of gravity,
not m/s2. `evaluation_depth_m` is measured down from the surface; omission checks
the liquid bottom. The required annular resistance input is separately qualified
for annulus width, thickness, strength and vertical effects. Roof reaction on the
shell is supplied separately from total roof inertial weight. Internal gas
pressure is zero in this scope.

`Ac` uses the force transition TL and ASD reduction; `Af` is independent of Rwc.
The wave calculation uses the 4 s transition and project importance for SUG I/II,
and mapped TL with unit importance for SUG III. The Table E.7 minimum is applied
by SUG and SDS. `purchaser_freeboard=True` adopts the advisory clearance in cases
without a mandatory numeric minimum. No containment or impact-resistant-roof
alternative is credited. `freeboard_ok=None` distinguishes those advisory cases
from an evaluated pass; `freeboard_required` preserves the distinction even
when a mandatory calculation gives a true zero wave. The advisory clearance
is always reported.

J is a self-anchorage **screen only**. A no-uplift result is not a completed
self-anchorage design. Uplift below the upper threshold still requires shell
compression, annular plate and piping checks. Above it, annular redesign or
mechanical anchorage is required; mechanical-anchor capacity is not evaluated.
The hoop result is demand only: no material allowable or acceptance is inferred.
Slab equivalent force heights can exceed liquid depth because bottom pressure
is included. The approximate effective weights need not sum to total weight.

## Worked example

All site and tank data in
[`examples/hydrodynamics/api650_annex_e.py`](../../../examples/hydrodynamics/api650_annex_e.py)
are assumed demonstration inputs, not measured or client-supplied data. The case
uses a 20 m diameter, 10 m water depth, 12.000 mm net shell, 1,000 kN shell and
200 kN roof, with CGs 5.5 m and 11 m. All roof weight reacts on the shell. The
assumed design site parameters are SDS=0.6g, SD1=0.3g, TL=8 s, SUG II,
I=1.25, Rwi=3.5, Rwc=2, K=1.5, minimum Ai=0.007g and Av=0. Qualified annular
resistance is assumed to be 100 kN/m; available freeboard is 1.0 m.

```sh
PYTHONPATH=src uv run python examples/hydrodynamics/api650_annex_e.py
```

Independent arithmetic and rounded intermediate values are recorded in
`tests/hydrodynamics/test_api650_annex_e.py`.

| Quantity | Result |
| --- | ---: |
| Tc (s) | 4.771686 |
| Wi / Wc (MN) | 16.707952 / 13.467749 |
| Xi / Xc (m) | 3.750000 / 6.050613 |
| Xis / Xcs (m) | 7.968657 / 7.723285 |
| Ai / Ac / Af (g) | 0.214286 / 0.058941 / 0.098819 |
| Base shear (MN) | 3.918662 |
| Ringwall / slab moment (MN m) | 15.822636 / 30.796401 |
| Wave height / required freeboard (m) | 0.830076 / 0.581053 |
| J (-) | 0.332133 |
| Hydrodynamic / combined hoop stress (MPa) | 14.268071 / 95.990154 |

Table: illustrative SUG II case, evaluated at the liquid bottom with Av=0.

The 1.0 m available clearance exceeds the 0.581053 m minimum for this case.
J=0.332133 is below 0.785, so no uplift is calculated by this screen. Hoop
acceptance is not established because no allowable is supplied.

## Sources and follow-ups

- [API 650, 11th edition (2007), public incorporation-by-reference copy](https://law.resource.org/pub/us/cfr/ibr/002/api.650.2007.pdf):
  E.4.5.2, E.4.6.1, E.6.1 and E.6.2.1.1.1/Table E-6. The function docstring
  identifies each equation and coefficient.
- [API 650, 12th-edition Addendum 2 (January 2016), publisher PDF](https://www.api.org/~/media/files/publications/addenda-and-errata/650_e12%20addendum%202.pdf):
  E.7.2 equations 1-5 and Table E.7, page E-26.
- [NCREE, Seismic Assessment of Steel Liquid Storage Tanks, 103E4](https://www.ncree.niar.org.tw/assets/file/103E4.pdf):
  pp. 85-87, Table 1, independent presentation of the two-mode loads and hoop forces.

No standards PDF, whole section or material table is distributed here. The source
revisions are disclosed rather than silently combined into a current-edition claim.

Floating-roof lift, rim seals, jamming, seal damage and impact pressure at the
shell-roof junction require separate analysis; inertial roof weight alone does
not model them. Also excluded are baffles, bolted/elevated/isolated tanks,
soil-structure interaction, sliding and component/foundation capacities.
The workspace-hub `scripts/capabilities/build_sloshing_explorer.py` update is a
follow-up outside this repository. Integration with the separate floating-roof
tank work remains a follow-up.
