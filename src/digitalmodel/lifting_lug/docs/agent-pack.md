# Lifting lug screening knowledge pack

This pack documents the pinned code contract. Arithmetic evidence does not
qualify physical criteria, equipment selection or a final design. UNVERIFIED
means the numerical implementation is readable but its claimed governing basis
has not been established. NOT_EVALUATED means the required source was unreadable.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| 1 | 2026-10-22 | Initial pinned API inventory and hand anchors for repeatable review. |

| Header | Value |
| --- | --- |
| Code pin (`origin/main`) | `3554b14fde67f44b1c52b81b6c72106282eaff29` |
| Package | `src/digitalmodel/lifting_lug/` |
| QA family | EC — engineering calculation screening |
| Evidence basis | Source lines at code pin; synthetic offline tests |

## 1. Purpose and scope

Circular padeye nominal bearing, net-section tension and two-plane shear screening under factored sling tension. No weld, fatigue, pin bending, empirical lug interaction or final acceptance assessment. [src/digitalmodel/lifting_lug/workflow.py:L1-L15]

## 2. Inputs and outputs

The table includes every package-owned public class, function, method and
property in direct Python files, including definitions not re-exported by
`__init__`. Imported utilities and local closures are excluded. Constructor
fields and function signatures record implementation defaults, not recommended
engineering inputs. Suffixes declare units: `_lpm` L/min, `_l` L, `_m` m,
`_mm` mm, `_m2` m², `_mpa` MPa, `_kn` kN, `_v` V; other strings,
flags and fractions are dimensionless. Types are annotations, not enforced
runtime schemas. Output record fields have the same suffix convention.

| Public definition | Inputs, units, validation and defaults | Output contract | Evidence |
| --- | --- | --- | --- |
| `workflow.LugCheck` | name; demand/allowable MPa, utilization dimensionless, passes bool; no validation<br>Fields: name: str; demand_mpa: float; allowable_mpa: float; utilization: float; passes: bool | Mutable result record; all fields required | [src/digitalmodel/lifting_lug/workflow.py:L35-L40] |
| `workflow.design_load_kn` | static load kN, dimensionless dynamic and skew factors: each >0; no defaults or finite checks<br>Signature: (static_load_kn: float, dynamic_amplification: float, skew_factor: float) | float design load kN = static × dynamic × skew | [src/digitalmodel/lifting_lug/workflow.py:L43-L53] |
| `workflow.lug_checks` | Load kN, thickness/pin/hole/outer radius mm, yield MPa; thickness/yield >0; hole>=pin; R-hole/2>0; remaining ranges unchecked<br>Signature: (design_load_kn: float, total_thickness_mm: float, pin_diameter_mm: float, hole_diameter_mm: float, outer_radius_mm: float, yield_strength_mpa: float) | list of three LugCheck in bearing, net tension, shear order; each carries MPa, utilization and inclusive pass | [src/digitalmodel/lifting_lug/workflow.py:L56-L93] |
| `workflow.router` | cfg.lifting_lug.lug/load required; explicit finite skew>=1; optional num_slings integer 1–4; DAF default 1, cheek thickness 0; dimensions as lug_checks<br>Signature: (cfg: dict) | Mutates/returns cfg; design load, total thickness, checks, governing result, skew floor/input/source, screening_status, summary_json; writes JSON (default results/lifting_lug/lifting_lug_summary.json); below-floor entered skew forces fail | [src/digitalmodel/lifting_lug/workflow.py:L107-L164] |

## 3. Method and governing standards

| Method / constant | Basis and status | Evidence |
| --- | --- | --- |
| P=P_static × DAF × skew | Caller-defined dimensionless amplification; synthetic anchors verify multiplication only. | [src/digitalmodel/lifting_lug/workflow.py:L43-L53] |
| t=main+2×cheek; ligament=R−hole/2; bearing area=pin×t; net and shear area=2t×ligament; stress=P/area | Nominal geometry and force/area identity, with kN→N factor 1e3 and N/mm²=MPa. Two cheeks and two ligaments are explicit model assumptions, not universal padeye geometry. Empirical lug interaction NOT_EVALUATED. | [src/digitalmodel/lifting_lug/workflow.py:L56-L93] [src/digitalmodel/lifting_lug/workflow.py:L130-L140] |
| Allowable stress factors BEARING_FACTOR, TENSION_FACTOR, SHEAR_FACTOR on yield | UNVERIFIED: code says AISC ASD without any edition or clause. No AISC edition/allowable is asserted as sourced here; these symbols denote implementation behavior only. | [src/digitalmodel/lifting_lug/workflow.py:L14-L15] [src/digitalmodel/lifting_lug/workflow.py:L28-L31] |
| utilization=demand/allowable; pass iff utilization<=1 | Software acceptance relative to the supplied nominal allowable; not full code compliance. | [src/digitalmodel/lifting_lug/workflow.py:L96-L104] |
| Leg-count minimum SKL floor; required explicit factor; below-floor entered factor forces fail | Code cites DNVGL-ST-E273, April 2016 §3.5.5. Source not opened, numerical minima NOT_EVALUATED; defaults/lookup remain code behavior. | [src/digitalmodel/lifting_lug/workflow.py:L112-L129] [src/digitalmodel/lifting_lug/workflow.py:L142-L158] |
| DAF default unity, zero cheek thickness, default output directory and JSON filename | Software defaults; DAF is not an approved operational factor. | [src/digitalmodel/lifting_lug/workflow.py:L125-L133] [src/digitalmodel/lifting_lug/workflow.py:L167-L176] |

No edition or clause can be cited for AISC because none is named at the pin.
That is a defect finding, rather than an inferred reference. Separate report
work cites a net-ligament convention; it does not establish this workflow's
allowable stresses. No values are transcribed from an unread standard.

## 4. Validated worked examples and regression examples

All loads, dimensions, factors and yield stress are synthetic caller inputs.
The allowable numbers below are consequences of the code's UNVERIFIED factors,
not published acceptance limits. Geometry is stated to make arithmetic auditable.

| Example | Inputs, method and literal answer | Executable evidence |
| --- | --- | --- |
| Factored load | Static 100 kN, DAF 2, skew 1.5; P=100×2×1.5=300 kN | `tests/workflows/test_lifting_lug_agent_pack.py::test_design_load_literal` |
| Net section | t=10 mm, pin=hole=20 mm, R=30 mm, P=20 kN; ligament=30−20/2=20 mm; A_net=2×10×20=400 mm²; stress=20000/400=50 MPa. Synthetic Fy=250 MPa; implemented tension factor gives 150 MPa; utilization=50/150=1/3 (33.3%). | `tests/workflows/test_lifting_lug_agent_pack.py::test_net_section_literal` |
| Tear-out | Same dimensions and load; nominal two-plane A_shear=2×10×20=400 mm²; tau=50 MPa. Implemented shear factor with synthetic Fy gives 100 MPa; utilization=50/100=0.5 (50.0%). | `tests/workflows/test_lifting_lug_agent_pack.py::test_tearout_literal` |

The tests assert literal stresses and allowables; no expected result is obtained
by calling the calculation under test twice. The equality of net and shear areas
belongs to this nominal model and does not validate physical failure surfaces.

## 5. QA gates and executable checks

| Gate | Executable check | Meaning |
| --- | --- | --- |
| Explicit skew | `tests/workflows/test_lifting_lug_design.py::test_omitted_skew_factor_is_an_error` | Missing factor refused before summary output |
| Existing hand arithmetic | `tests/workflows/test_lifting_lug_design.py::test_lifting_lug_hand_calculation` | Existing nominal fixture regression |
| Public contract | `tests/docs/test_screening_agent_packs.py::test_public_contract_and_locators[lifting_lug]` | AST names and defining locators agree |
| Citation collection | `tests/docs/test_screening_agent_packs.py::test_all_cited_nodes_collect[lifting_lug]` | All example and gate nodes collect |

These gates establish software contracts only. Allowable-stress source
qualification and full padeye acceptance remain open.

## 6. Known failure modes and pitfalls

| File and lines | Defect / limitation | Follow-up |
| --- | --- | --- |
| [src/digitalmodel/lifting_lug/workflow.py:L14-L31] | AISC factors have no edition/clause; DNV title alone does not qualify allowable stresses. | Establish exact provisions and applicability before acceptance use. |
| [src/digitalmodel/lifting_lug/workflow.py:L43-L53] [src/digitalmodel/lifting_lug/workflow.py:L66-L76] | Direct functions do not reject nonfinite input; lug_checks does not require positive load or pin/hole diameters. Zero pin causes division by zero; negative demand can produce a passing check. | Add finite positive validation at calculation boundary. |
| [src/digitalmodel/lifting_lug/workflow.py:L80-L92] [src/digitalmodel/lifting_lug/workflow.py:L130-L133] | Cheeks are simply additive; net and shear share one nominal area, without load-transfer, out-of-plane, weld or interaction checks. | Define validated geometry and load path or restrict applicability. |
| [src/digitalmodel/lifting_lug/workflow.py:L125-L133] | Omitted DAF defaults to unity; cheek thicknesses can be individually invalid while total remains positive. | Require operating factor justification and individual geometry checks. |
| [src/digitalmodel/lifting_lug/workflow.py:L113-L124] | Without num_slings the leg-count lookup minimum is absent; explicit skew alone does not prove rigging adequacy. | Require leg-count evidence where minima apply. |
| [src/digitalmodel/lifting_lug/workflow.py:L161-L176] | Summary filename/directory are caller-selected and existing output can be overwritten. | Establish output ownership and collision policy. |

## 7. Lessons log

Nominal arithmetic and allowable provenance must be assessed separately. The
same area denominator does not establish equal physical tensile and shear
capacity. Explicit skew input does not remove the need to qualify DAF.

## 8. Ownership

Maintainers own API and source corrections. Engineering reviewers must qualify
the allowable stresses, connection geometry and rigging assumptions. Resolve the
missing AISC basis before describing any result as code-compliant.

## 9. Dependencies and other implementations

JSON, math, dataclasses and pathlib support the workflow. The skew lookup is
imported from an analytical installation module; that package import also loads
NumPy, pandas, SciPy and Pydantic. No licensed solver is required for these
calculations. [src/digitalmodel/lifting_lug/workflow.py:L18-L26]
[src/digitalmodel/orcaflex/installation_analysis.py:L16-L22]

Routing and implementation-map `agent_packs` metadata register EC; no accepted
implementation-family row for this package was found. The other padeye/report
work is reviewed separately and does not change this pinned API inventory.

## 10. Coverage and open items

Read both package Python files, the existing workflow tests, routing and
implementation map, and the two report/reference diffs. All package-owned public
definitions at the pin are listed. The local AST checker covers function-list
and locator drift until the shared checker arrives. No production code is
changed. AISC provenance, direct-input safety and model applicability remain
UNVERIFIED; section-6 items require independent engineering resolution.
