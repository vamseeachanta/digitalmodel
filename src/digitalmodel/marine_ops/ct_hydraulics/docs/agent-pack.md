# Coiled tubing hydraulics knowledge pack

| Header | Value |
|---|---|
| Module | `marine_ops/ct_hydraulics` |
| Repository | `digitalmodel` |
| Package | `src/digitalmodel/marine_ops/ct_hydraulics` |
| Pack revision | 0 |
| Code revision | `b47e42fbe7208c69b28dd436aa91fb129bc385f7` |
| Analysis family | Engineering calculations (EC); AQG r0 Python calculations |
| Quality role | Engineering calculations quality manager |
| Status | DRAFT |

## 1. Purpose and scope

`CTHydraulics.calculate` computes CT and annular losses, pump pressure, downhole pressure and equivalent circulating density; `router` adds these results to the configuration and requests a JSON summary. [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L15-L36] [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L59-L74]

The documented scope is steady, single-phase liquid circulation, with effective viscosity for non-Newtonian fluids; temperature effects, drag reduction and cuttings transport are excluded, and the annulus is concentric unless its hydraulic diameter is overridden. [docs/domains/coiled-tubing/CT_HYDRAULICS_THEORY.md:L8-L12] [docs/domains/coiled-tubing/CT_HYDRAULICS_THEORY.md:L66-L70]

The containing domain has maturity `beta`; this is registry metadata, rather than a measured accuracy or operating envelope. [docs/registry/module-routing.yaml:L228-L233]

UNVERIFIED: U1 — a qualified accuracy envelope and independent benchmark for the complete circulation calculation have not been established; issue pending (2). [docs/domains/coiled-tubing/CT_HYDRAULICS_THEORY.md:L8-L12] [tests/workflows/test_durable_workflows.py:L1474-L1484]

## 2. Inputs and outputs

The tables describe implemented defaults, rather than accepted engineering limits; numeric configuration values are converted using `float`, and geometry selection uses truthiness. [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L75-L112] [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L189-L203]

| Input under `ct_hydraulics` | Unit | Implemented range and default | Source |
|---|---|---|---|
| `flow.rate_gpm`, `fluid.density_ppg` | U.S. gal/min, lb/U.S. gal | No range guard; each defaults to 0 | [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L83-L84] [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L325-L330] |
| `fluid.ct_viscosity_cp`, `viscosity_cp`, `annulus_viscosity_cp` | cP | CT takes CT-specific, then common, then 0; false CT values become 0; absent/null annular value takes CT value | [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L85-L89] [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L220-L223] |
| `ct.od_in`, `length_m`, `roughness_in`, `reel_pressure_drop_psi` | inch, m, inch, psi | No range guard; each defaults to 0 | [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L91-L95] |
| `ct.id_in`, `wall_thickness_in`, `volume_gal` | inch, inch, U.S. gal | First truthy ID; otherwise OD minus twice wall; otherwise volume and length; otherwise `ValueError` | [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L189-L203] |
| `annulus.hole_id_in` | inch | First non-null annular value, then well value, then 0 | [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L100-L102] |
| `annulus.length_m` | m | First non-null annular value, measured depth, CT length | [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L103-L109] |
| `annulus.roughness_in`, `inner_od_in` | inch | First non-null annular value, then CT roughness/OD | [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L110-L111] |
| `annulus.hydraulic_diameter_override_in` | inch | Truthy override; otherwise hole ID minus inner OD; area is still geometric | [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L257-L263] |
| `well.tvd_m`, `measured_depth_m` | m | TVD defaults to measured depth, then annular length; explicit null is not coalesced | [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L137-L144] |
| `well.wellhead_pressure_psi`, `bha.pressure_drop_psi` | psi | Each defaults to 0 | [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L146-L147] |
| `options.flow_regime_transition_re` | dimensionless | Defaults to 2100, passed separately to both sections | [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L114-L135] |
| `options.include_annulus_friction_in_pump_pressure` | Boolean | Defaults true, converted with `bool` | [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L149-L159] |
| `output_dir`; top-level `_config_dir_path` | directory | Relative output defaults to `results`, below configuration directory; absent configuration directory suppresses file output | [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L38-L57] |

| Output | Unit and precision | Source |
|---|---|---|
| `summary`: pump, downhole, hydrostatic, wellhead, BHA, reel pressure | psi, 1 decimal place | [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L175-L182] |
| `summary.equivalent_circulating_density_ppg` | lb/U.S. gal, 3 decimal places | [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L183-L186] |
| `coiled_tubing`: ID, area, volume, velocity, Reynolds number, friction factor, pressure loss, travel time | inch (4), square inch (3), U.S. gal (1), m/s (3), dimensionless (1), dimensionless (5), psi (1), min (1) | [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L234-L243] |
| `annulus`: hydraulic diameter, area, volume, velocity, Reynolds number, friction factor, pressure loss, bottoms-up time | Same units/precision as CT; annular diameter and time keys differ | [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L285-L294] |
| `results/ct_hydraulics/ct_hydraulics_summary.json` | UTF-8 JSON; sorted keys, indentation 2, final newline | [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L49-L57] |

Depth is converted from metres to feet for hydrostatics; pressure terms are added as positive contributions, and annular friction can be excluded from pump pressure without changing ECD. No coordinate vector, origin or phase input is used by these scalar expressions. [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L137-L173]

## 3. Method and governing standards

The theory document supplies equations without a numbered governing standard or edition in its method and limitations sections; these are implementation descriptions, not a claim of compliance with a licensed standard. [docs/domains/coiled-tubing/CT_HYDRAULICS_THEORY.md:L14-L70]

| Method | Implementing definition and equation | Provenance |
|---|---|---|
| CT internal diameter | `_resolve_ct_id`: direct ID, OD minus twice wall, or square root of area from volume/length | [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L189-L203] |
| Circular and annular continuity | `_compute_pipe_section`, `_compute_annulus_section`: circular area or area difference; velocity equals volumetric rate divided by area; volume equals area times length | [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L216-L232] [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L257-L283] |
| Reynolds number | Density times velocity times hydraulic diameter divided by dynamic viscosity | [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L220-L224] [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L265-L271] |
| Darcy friction factor | `_friction_factor`: 0 for nonpositive Reynolds number; `64/Re` below configured transition; Haaland expression otherwise | [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L296-L301] [docs/domains/coiled-tubing/CT_HYDRAULICS_THEORY.md:L36-L42] |
| Pressure loss | `_darcy_weisbach`: friction factor times length/diameter times dynamic pressure, converted Pa to psi; nonpositive diameter returns 0 | [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L303-L314] |
| Hydrostatics and ECD | `calculate`: density/depth hydrostatic expression and annular pressure increment divided by depth/density conversion; zero depth returns 0 ECD | [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L137-L144] [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L168-L173] |
| Pump and downhole pressure | `calculate`: pump sums wellhead, CT loss, BHA loss, reel loss and optional annular loss; downhole sums wellhead, hydrostatic, CT and BHA losses | [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L146-L166] |
| Conversion and rounding helpers | `_ppg_to_kgm3`, `_gpm_to_m3s`, `_r`; Pint length, area and pressure conversion | [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L216-L232] [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L324-L334] |

UNVERIFIED: U2 — independent provenance and qualification of the fixed conversion coefficients and friction/annular assumptions remain open; the result contains no `Citation` sidecar, so section-3 expressions are not independently qualified references; issue pending (2). [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L175-L187] [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L296-L334]

## 4. Validated worked examples

Evidence record: the code pin in the header was tested before and after this documentation change, using `PYTHONPATH=src uv run --no-sources python -m pytest <nodes> -q -rA -p no:cacheprovider --basetemp=<scratch>`, without an `addopts` override; the workflow ran in a scratch copy because it writes outputs. These results record execution, rather than independent validation. [tests/workflows/test_durable_workflows.py:L58-L62] [tests/workflows/test_durable_workflows.py:L95-L100]

| Class and case | Source and input | Expected quantities and tolerance basis | Existing test node | Offline result |
|---|---|---|---|---|
| Regression D-CT-01; validated examples: 0 | Registry workflow and `examples/workflows/ct-hydraulics/input.yml` | Hydrostatic 4376.0 psi; pump 2258.6 psi; downhole 6476.1 psi; ECD 8.567 ppg; CT ID 1.482 inch; Reynolds number 131177.7 dimensionless; CT loss 1300.1 psi; each uses the existing `pytest.approx` default, with no independent tolerance study | `tests/workflows/test_durable_workflows.py::test_workflow_registry[ct-hydraulics]` | PASS before / PASS after; 1 run, 1 passed, 0 failed, 0 skipped, 0 NOT_RUN in each run; fixed expected values are regression evidence. [docs/registry/workflows.yaml:L1152-L1160] [examples/workflows/ct-hydraulics/input.yml:L9-L31] [tests/workflows/test_durable_workflows.py:L1474-L1484] |

The compatibility node `tests/compat/test_group_compat.py::TestFlatImports::test_flat_import[ct_hydraulics]` is an import check, not an additional worked example; its exception handler can tolerate an unrelated missing import. [tests/compat/test_group_compat.py:L102-L120]

Execution record: the contract/compatibility/routing command named that compatibility node, `tests/contracts` and `tests/docs/test_digitalmodel_routing_contract.py`; before and after each had 591 run, 591 passed, 0 failed, 0 skipped and 0 NOT_RUN. The workflow command named only the D-CT-01 node in its scratch copy; both commands used the header code pin with the documentation edits left uncommitted. [tests/compat/test_group_compat.py:L106-L120] [tests/docs/test_digitalmodel_routing_contract.py:L60-L67] [tests/workflows/test_durable_workflows.py:L1474-L1484]

## 5. QA gates

The gate mapping below uses AQG r0, section 10, family “Python calculations”; G5 uses AQG r0, section 9. The checks are pack acceptance procedures, not assertions that the module implements input validation. [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L75-L112]

| Gate | Applicability | Check IDs and recorded result |
|---|---|---|
| G0 | Applicable: inputs, units and method provenance | G0-01 PASS on the shipped input; G0-02 PASS on 10 malformed-input mutations; source qualification remains open in U1/U2. [examples/workflows/ct-hydraulics/input.yml:L9-L31] |
| G1 | Applicable: Python import and offline engine/test route | G1-01 PASS; G1-02 PASS before and after. The CT router calls the calculation and summary writer. [src/digitalmodel/engine.py:L432-L434] [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L29-L36] |
| G2 | Applicable: hand arithmetic for components and balances | G2-01 PASS; G2-02 PASS; these do not qualify the full physical model. [docs/domains/coiled-tubing/CT_HYDRAULICS_THEORY.md:L16-L64] |
| G3 | NOT_APPLICABLE: steady-state scalar calculation has no time-integration operation | [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L59-L74] |
| G4 | NOT_APPLICABLE: CT/annulus calculation has no diffraction mesh or body calculation | [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L114-L135] |
| G5 | Applicable: result evidence and reproducible summary | G5-01 PASS; a signed engineering appendix is not emitted by the summary writer. [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L38-L57] |

| Check | Executable criterion, numeric threshold and unit | Evidence |
|---|---|---|
| G0-01 | Run `preflight` below on D-CT-01: 0 failed assertions (count); finite positive rate in U.S. gal/min, density in ppg, viscosity in cP, lengths in m, and `0 < ID < OD < hole ID` in inch for this input; roughness/pressure increments at least 0 inch/psi | Input fingerprint and assertion completion; source selection/units follow the implemented inputs. [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L83-L147] |
| G0-02 | Run the 10 mutations below: reject 10 of 10 (count), including 0 rate, density, viscosity and TVD; invalid wall/annulus; grouped numeric text, numeric text, nonfinite value and string Boolean | Rejection count from the pack preflight, independently of whether `calculate` returns a value; does not validate a production schema. [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L83-L112] [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L137-L151] |
| G1-01 | Run the compatibility node in section 4: 1 passed, 0 failed (tests), plus direct canonical `CTHydraulics` import in the snippet with 0 exceptions (count) | Pytest result and successful canonical import; the direct check avoids the compatibility test's permissive exception branch. [src/digitalmodel/marine_ops/ct_hydraulics/__init__.py:L1-L5] [tests/compat/test_group_compat.py:L106-L120] |
| G1-02 | Run D-CT-01: 1 passed, 0 failed (tests), 0 missing declared output files (count) | Pytest assertions and scratch output existence. [tests/workflows/test_durable_workflows.py:L95-L100] [tests/workflows/test_durable_workflows.py:L1474-L1484] |
| G2-01 | At Reynolds number 1000 dimensionless, compare `_friction_factor` with `64/1000`: absolute error at most `1e-12` dimensionless; compare resolved CT ID with OD minus twice wall: absolute error at most `1e-12` inch | Hand expression checks below; this checks transcription of documented component equations. [docs/domains/coiled-tubing/CT_HYDRAULICS_THEORY.md:L38-L42] [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L189-L196] |
| G2-02 | Reconstruct pump and downhole sums from rounded components: each residual at most 0.1 psi; compare CT travel and annular bottoms-up time with volume/rate: each residual at most 0.1 min | Balance assertions below; tolerances reflect 1-decimal output rounding, not physical accuracy. [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L153-L183] [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L230-L242] [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L281-L293] |
| G5-01 | Write summary twice in scratch: 0 differing bytes (count), 0 JSON value differences (count), 1 final newline (count); missing configuration directory returns `None` on 1 of 1 checks (count); record input and summary SHA-256 | Sorted UTF-8 JSON and fingerprint record from the snippet; emitted path is absolute when the supplied directory is absolute. [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L46-L57] |

Reproduction: save the following block as a scratch Python script and run `PYTHONPATH=src uv run --no-sources python <script>` from the repository root with `TMPDIR` pointing to scratch; no solver is called by this direct module check. [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L1-L5]

```python
import copy, json, math, os, pathlib, hashlib
import yaml
from digitalmodel.marine_ops.ct_hydraulics import CTHydraulics

source = pathlib.Path('examples/workflows/ct-hydraulics/input.yml')
cfg = yaml.safe_load(source.read_text())['ct_hydraulics']
calc = CTHydraulics()

def preflight(c):
    def number(value, positive=False):
        assert type(value) in (int, float) and math.isfinite(value)
        assert value > 0 if positive else value >= 0
        return value
    w, ct, a, f = (c[k] for k in ('well', 'ct', 'annulus', 'fluid'))
    number(c['flow']['rate_gpm'], True)
    number(f['density_ppg'], True)
    number(f.get('ct_viscosity_cp', f.get('viscosity_cp')), True)
    if f.get('annulus_viscosity_cp') is not None:
        number(f['annulus_viscosity_cp'], True)
    od = number(ct['od_in'], True)
    number(ct['length_m'], True)
    for key in ('id_in', 'wall_thickness_in', 'volume_gal'):
        if key in ct: number(ct[key], True)
    inside = calc._resolve_ct_id(ct)
    assert 0 < inside < od
    hole = number(calc._coalesce(a.get('hole_id_in'), w.get('hole_id_in')), True)
    inner = number(calc._coalesce(a.get('inner_od_in'), od), True)
    assert hole > inner
    number(calc._coalesce(a.get('length_m'), w.get('measured_depth_m'), ct['length_m']), True)
    number(w['tvd_m'], True)
    if 'measured_depth_m' in w: assert w['tvd_m'] <= number(w['measured_depth_m'], True)
    for block, key in ((ct,'roughness_in'), (a,'roughness_in'), (ct,'reel_pressure_drop_psi'),
                       (w,'wellhead_pressure_psi'), (c.get('bha',{}),'pressure_drop_psi')):
        if key in block: number(block[key])
    if 'hydraulic_diameter_override_in' in a: number(a['hydraulic_diameter_override_in'], True)
    opts=c['options']
    number(opts['flow_regime_transition_re'], True)
    assert type(opts['include_annulus_friction_in_pump_pressure']) is bool
    assert isinstance(c.get('output_dir','results'),str) and c.get('output_dir','results')

preflight(cfg)
mutations = [('flow','rate_gpm',0), ('fluid','density_ppg',0), ('fluid','ct_viscosity_cp',0),
             ('ct','wall_thickness_in',2), ('annulus','hole_id_in',1), ('well','tvd_m',0),
             ('flow','rate_gpm','1,000'), ('flow','rate_gpm','60'),
             ('fluid','density_ppg',float('nan')), ('options','include_annulus_friction_in_pump_pressure','false')]
rejected=0
for block,key,value in mutations:
    bad=copy.deepcopy(cfg);bad[block][key]=value
    try: preflight(bad)
    except (AssertionError,ValueError,TypeError,KeyError): rejected+=1
assert rejected==len(mutations)
# Closed-form component checks use the repository theory, not a full-system benchmark.
assert abs(calc._friction_factor(1000,0,2100)-64/1000) <= 1e-12
assert abs(calc._resolve_ct_id(cfg['ct'])-(cfg['ct']['od_in']-2*cfg['ct']['wall_thickness_in'])) <= 1e-12
r=calc.calculate(cfg)
s=r['summary'];ct=r['coiled_tubing'];a=r['annulus']
assert abs(s['pump_pressure_psi']-(s['wellhead_pressure_psi']+ct['pressure_loss_psi']+s['bha_pressure_drop_psi']+s['reel_pressure_drop_psi']+a['pressure_loss_psi']))<=0.1
assert abs(s['downhole_pressure_psi']-(s['wellhead_pressure_psi']+s['hydrostatic_pressure_psi']+ct['pressure_loss_psi']+s['bha_pressure_drop_psi']))<=0.1
assert abs(ct['volume_gal']/cfg['flow']['rate_gpm']-ct['travel_time_min'])<=0.1
assert abs(a['volume_gal']/cfg['flow']['rate_gpm']-a['bottoms_up_time_min'])<=0.1
root=pathlib.Path(os.environ['TMPDIR'])/'qa-output'; root.mkdir(exist_ok=True)
assert calc._write_summary({},cfg,r) is None
path=pathlib.Path(calc._write_summary({'_config_dir_path':str(root)},cfg,r))
before=path.read_bytes()
calc._write_summary({'_config_dir_path':str(root)},cfg,r)
assert path.read_bytes()==before and before.endswith(b'\n')
assert json.loads(before)==r and path.is_absolute()
assert before.decode()==json.dumps(r,indent=2,sort_keys=True)+'\n'
observations={}
for block,key,value in mutations[:6]:
    bad=copy.deepcopy(cfg);bad[block][key]=value
    try:
        out=calc.calculate(bad)
        observations[f'{block}.{key}']=out['summary']
    except Exception as exc:
        observations[f'{block}.{key}']={'exception':type(exc).__name__}
record={'G0-01':'PASS','G0-02':{'status':'PASS','rejected':rejected,'total':len(mutations)},
        'G2-01':'PASS','G2-02':'PASS','G5-01':'PASS',
        'input_sha256':hashlib.sha256(source.read_bytes()).hexdigest(),
        'summary_sha256':hashlib.sha256(before).hexdigest(),'observations':observations}
(pathlib.Path(os.environ['TMPDIR'])/'qa-results.json').write_text(json.dumps(record,indent=2)+'\n')
print('G0-01 PASS; G0-02 PASS (10/10 rejected by preflight); G2-01 PASS; G2-02 PASS; G5-01 PASS')
print('Direct invalid-input observations:', ', '.join(f'{k}: '+v.get('exception','result returned') for k,v in observations.items()))
```

## 6. Known failure modes and pitfalls

sweep: 81 found

This count is matched source lines, including benign unit conversions and output formatting; each candidate has a disposition below. No `except`, `continue`, empty-result cache, epoch fallback or empty-list/dict return occurs in the package; empty dictionary defaults are included as configuration fallbacks. [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L1-L334]

Exact sweep command (the same expressions were also evaluated with Python `re`, case-sensitive):

```sh
rg -n 'except|continue|return None|else None|return \[\]|return \{\}|0\.0|1970|int\(|float\(|\.get\(|Q_\(|json.dump|0\.05|5 %|119\.826427|0\.052|231\.0' src/digitalmodel/marine_ops/ct_hydraulics --glob '*.py'
```

Each numeric conversion accepts Python numeric text without a locale parser; separators are not normalized. A zero fallback is an implemented behavior, not an approved engineering input. Draft 1 groups input/geometry/type validation, and draft 2 groups unit/reference qualification. [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L83-L112] [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L189-L203]

| Site | Symptom or disposition | Detection check | Tracking |
|---|---|---|---|
| [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L29-L29] | Missing module configuration becomes an empty dictionary before calculation. | G0-01 / G0-02 | issue pending (1) |
| [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L46-L46] | Missing configuration directory is not rejected. | G5-01 | issue pending (3) |
| [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L48-L48] | Summary output silently returns None without a configuration directory. | G5-01 | issue pending (3) |
| [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L49-L49] | Output directory defaults to results; arbitrary absolute paths are accepted. | G5-01 | issue pending (3) |
| [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L55-L55] | Normal serialization; no empty-result cache; nonfinite floats are not explicitly rejected. | G5-01 | issue pending (3) |
| [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L75-L75] | Missing well mapping becomes empty. | G0-01 / G0-02 | issue pending (1) |
| [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L76-L76] | Missing CT mapping becomes empty. | G0-01 / G0-02 | issue pending (1) |
| [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L77-L77] | Missing annulus mapping becomes empty. | G0-01 / G0-02 | issue pending (1) |
| [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L78-L78] | Missing fluid mapping becomes empty. | G0-01 / G0-02 | issue pending (1) |
| [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L79-L79] | Missing flow mapping becomes empty. | G0-01 / G0-02 | issue pending (1) |
| [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L80-L80] | Missing BHA mapping becomes empty. | G0-01 / G0-02 | issue pending (1) |
| [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L81-L81] | Missing options mapping becomes empty. | G0-01 / G0-02 | issue pending (1) |
| [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L83-L83] | Rate defaults to zero; external numeric text is coerced without unit validation. | G0-01 / G0-02 | issue pending (1) |
| [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L84-L84] | Density defaults to zero; numeric text is coerced without unit validation. | G0-01 / G0-02 | issue pending (1) |
| [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L85-L85] | CT viscosity coercion starts here; no positive-viscosity guard. | G0-01 / G0-02 | issue pending (1) |
| [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L86-L86] | CT/common viscosity missing, null or false becomes zero. | G0-01 / G0-02 | issue pending (1) |
| [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L88-L88] | Annular viscosity lookup accepts null for fallback. | G0-01 / G0-02 | issue pending (1) |
| [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L89-L89] | Missing/null annular viscosity inherits CT viscosity; supplied text is coerced. | G0-01 / G0-02 | issue pending (1) |
| [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L91-L91] | CT OD defaults to zero; unvalidated numeric coercion. | G0-01 / G0-02 | issue pending (1) |
| [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L93-L93] | CT length defaults to zero; unvalidated numeric coercion. | G0-01 / G0-02 | issue pending (1) |
| [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L94-L94] | CT roughness defaults to zero; unvalidated numeric coercion. | G0-01 / G0-02 | issue pending (1) |
| [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L95-L95] | Reel loss defaults to zero; unvalidated numeric coercion. | G0-01 / G0-02 | issue pending (1) |
| [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L100-L100] | Annular hole diameter numeric coercion starts here. | G0-01 / G0-02 | issue pending (1) |
| [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L101-L101] | Missing/null annular and well hole diameter falls back to zero. | G0-01 / G0-02 | issue pending (1) |
| [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L103-L103] | Annular length numeric coercion starts here. | G0-01 / G0-02 | issue pending (1) |
| [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L105-L105] | Annular length missing/null permits fallback. | G0-01 / G0-02 | issue pending (1) |
| [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L106-L106] | Measured depth missing/null permits fallback to CT length. | G0-01 / G0-02 | issue pending (1) |
| [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L110-L110] | Annular roughness missing/null inherits CT roughness, then float conversion. | G0-01 / G0-02 | issue pending (1) |
| [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L111-L111] | Annular inner OD missing/null inherits CT OD, then float conversion. | G0-01 / G0-02 | issue pending (1) |
| [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L112-L112] | Hydraulic diameter override is untyped until used. | G0-01 / G0-02 | issue pending (1) |
| [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L122-L122] | CT transition defaults to 2100 dimensionless with unchecked float conversion. | G0-01 / G0-02 | issue pending (1) |
| [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L134-L134] | Annular transition repeats the same unchecked default. | G0-01 / G0-02 | issue pending (1) |
| [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L137-L137] | TVD float conversion starts here; explicit null is not coalesced. | G0-01 / G0-02 | issue pending (1) |
| [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L138-L138] | TVD missing permits fallback; null does not. | G0-01 / G0-02 | issue pending (1) |
| [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L140-L140] | Missing measured depth falls back to annular length. | G0-01 / G0-02 | issue pending (1) |
| [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L143-L143] | Metres-to-feet unit assumption at hydrostatic input. | no detection check exists for independent unit provenance | issue pending (2) |
| [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L144-L144] | Zero depth silently gives zero hydrostatic pressure; fixed conversion coefficient is unqualified. | G0-01 / G0-02 | issue pending (1) |
| [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L146-L146] | Wellhead pressure defaults to zero; numeric text coerced. | G0-01 / G0-02 | issue pending (1) |
| [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L147-L147] | BHA drop defaults to zero; numeric text coerced. | G0-01 / G0-02 | issue pending (1) |
| [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L150-L150] | Boolean option defaults true; a nonempty string false evaluates true. | G0-01 / G0-02 | issue pending (1) |
| [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L158-L158] | Intentional zero annulus contribution when option is false; string Boolean can select wrong branch. | G0-01 / G0-02 | issue pending (1) |
| [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L170-L170] | ECD uses fixed hydrostatic conversion coefficient. | no detection check exists for independent unit provenance | issue pending (2) |
| [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L172-L172] | Zero TVD returns zero ECD even for nonzero fluid density. | G0-01 / G0-02 | issue pending (1) |
| [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L190-L190] | Direct ID lookup uses truthiness, so explicit zero is ignored. | G0-01 / G0-02 | issue pending (1) |
| [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L192-L192] | Truthy direct ID is converted without a positive-diameter guard. | G0-01 / G0-02 | issue pending (1) |
| [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L193-L193] | Missing OD defaults to zero in diameter resolution. | G0-01 / G0-02 | issue pending (1) |
| [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L194-L194] | Wall lookup uses truthiness; explicit zero is ignored. | G0-01 / G0-02 | issue pending (1) |
| [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L196-L196] | OD minus twice wall can be nonpositive, without geometry validation. | G0-01 / G0-02 | issue pending (1) |
| [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L197-L197] | Volume lookup uses truthiness; explicit zero is ignored. | G0-01 / G0-02 | issue pending (1) |
| [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L198-L198] | Length lookup uses truthiness in volume-based ID. | G0-01 / G0-02 | issue pending (1) |
| [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L200-L200] | Volume-based ID assumes metres and converts numeric text. | no detection check exists for independent unit provenance | issue pending (2) |
| [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L201-L201] | Volume-based ID assumes U.S. gallons and cubic inches; fixed factor and numeric coercion. | no detection check exists for independent unit provenance | issue pending (2) |
| [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L216-L216] | Pipe diameter assumed inch and converted to metres. | no detection check exists for independent unit provenance | issue pending (2) |
| [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L219-L219] | Zero pipe area silently gives zero velocity. | G0-01 / G0-02 | issue pending (1) |
| [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L221-L221] | Pipe viscosity assumes cP and converts to Pa s. | no detection check exists for independent unit provenance | issue pending (2) |
| [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L223-L223] | Zero pipe viscosity silently gives zero Reynolds number. | G0-01 / G0-02 | issue pending (1) |
| [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L225-L225] | Zero pipe diameter silently gives zero relative roughness. | G0-01 / G0-02 | issue pending (1) |
| [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L230-L230] | Pipe length assumed metres and converted to inches. | no detection check exists for independent unit provenance | issue pending (2) |
| [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L231-L231] | Zero pipe length silently gives zero volume; U.S.-gallon conversion. | no detection check exists for independent unit provenance | issue pending (2) |
| [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L232-L232] | Zero flow silently gives zero travel time. | G0-01 / G0-02 | issue pending (1) |
| [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L258-L258] | Truthy hydraulic-diameter override is converted without range validation. | G0-01 / G0-02 | issue pending (1) |
| [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L260-L260] | Default annular hydraulic diameter can be nonpositive. | G0-01 / G0-02 | issue pending (1) |
| [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L262-L262] | Annular area assumed square inches and converted to square metres. | no detection check exists for independent unit provenance | issue pending (2) |
| [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L263-L263] | Annular hydraulic diameter assumed inches and converted to metres. | no detection check exists for independent unit provenance | issue pending (2) |
| [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L264-L264] | Zero annular area silently gives zero velocity. | G0-01 / G0-02 | issue pending (1) |
| [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L266-L266] | Annular viscosity assumes cP and converts to Pa s. | no detection check exists for independent unit provenance | issue pending (2) |
| [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L270-L270] | Zero annular viscosity silently gives zero Reynolds number. | G0-01 / G0-02 | issue pending (1) |
| [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L273-L273] | Annular roughness assumed inch before relative-roughness conversion. | no detection check exists for independent unit provenance | issue pending (2) |
| [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L275-L275] | Zero annular diameter silently gives zero relative roughness. | G0-01 / G0-02 | issue pending (1) |
| [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L281-L281] | Annular length assumed metres and converted to inches. | no detection check exists for independent unit provenance | issue pending (2) |
| [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L282-L282] | Zero annular length gives zero volume; U.S.-gallon conversion. | no detection check exists for independent unit provenance | issue pending (2) |
| [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L283-L283] | Zero flow silently gives zero bottoms-up time. | G0-01 / G0-02 | issue pending (1) |
| [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L297-L297] | Nonpositive Reynolds number selects zero friction instead of rejection. | G0-01 / G0-02 | issue pending (1) |
| [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L298-L298] | Zero friction returned for nonpositive Reynolds number. | G0-01 / G0-02 | issue pending (1) |
| [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L311-L311] | Nonpositive diameter selects zero pressure loss instead of rejection. | G0-01 / G0-02 | issue pending (1) |
| [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L312-L312] | Zero pressure loss returned for nonpositive diameter. | G0-01 / G0-02 | issue pending (1) |
| [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L314-L314] | Normal Pa-to-psi conversion; no explicit nonfinite guard. | no detection check exists for independent unit provenance | issue pending (2) |
| [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L322-L322] | Empty `_coalesce` argument list returns None; current callers supply defaults, so this is a helper boundary, not a confirmed calculation defect. | no detection check exists for the empty-argument helper boundary | issue pending (1) |
| [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L326-L326] | Density conversion assumes lb/U.S. gal and uses fixed factor without emitted provenance. | no detection check exists for independent unit provenance | issue pending (2) |
| [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L330-L330] | Rate conversion assumes U.S. gal/min and uses fixed litre/minute factors. | no detection check exists for independent unit provenance | issue pending (2) |
| [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L334-L334] | Normal output float coercion and rounding; nonfinite values are not explicitly rejected. | G0-01 / G0-02 | issue pending (1) |

Additional confirmed defects: `_write_summary` documents a relative return path but returns the absolute path when the configuration directory is absolute; G5-01 detects this behavior, issue pending (3). The compatibility test permits some import failures; G1-01 therefore also performs a direct canonical import, issue pending (2). [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L41-L57] [tests/compat/test_group_compat.py:L106-L120]

Issue-search record: module-name and workflow-name queries and queries for coiled tubing, hydraulics validation, summary behavior and implementation mapping produced no confirmed module incident to cite; discovered input defects above are reproducible with G0-02 and direct `calculate` calls. No detection check exists for full-system physical accuracy; issue pending (2). [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L83-L112] [tests/workflows/test_durable_workflows.py:L1474-L1484]

## 7. Lessons log

| Date | Record | Lesson and pack change | Evidence | Shared-log entry |
|---|---|---|---|---|
| 2026-11-07 | Initial pack | Separate a fixed-output regression from independent validation; retain zero-fallback and conversion sites in the failure inventory | [tests/workflows/test_durable_workflows.py:L1474-L1484] [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L83-L112] | Not assigned; issue pending (2) |

## 8. Ownership

Review assignment requested: engineering calculations quality manager; proposed deputy: engineering calculations deputy quality manager; producing role: engineering calculations maintainer; unresolved method, tolerance and source questions go to the family quality manager, with cross-module scope questions to the maintainers. This is a review request, not a claim of clearance (AQG r0, section 11).

## 9. Dependencies and implementation inventory

The package imports Python JSON/math/path utilities and the shared Pint quantity constructor; its initializer exports `CTHydraulics`. [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L1-L5] [src/digitalmodel/marine_ops/ct_hydraulics/__init__.py:L1-L5] [src/digitalmodel/units.py:L7-L10]

The engine dispatches `basename: ct_hydraulics` to the router, and the durable workflow registry declares the example input, result YAML and summary JSON as offline outputs. [src/digitalmodel/engine.py:L432-L434] [docs/registry/workflows.yaml:L1152-L1160]

Implementation-map evidence uses a separate map pin: ref `origin/codex/implmap-work`, commit `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4`, data revision `270c8759c374b67a7f032953e7fcdb14e0224461`; citations to the map in this section use that map pin, rather than the header code pin. [docs/registry/implementation-map.yaml:L1-L3]

A8 inventory: 0 accepted implementation rows lie inside this package, so 0 module families and 0 counted families exist; inventory is empty and the counted-family grep is `NOT_APPLICABLE` for that reason. Rejected candidates are not implementation rows: `_resolve_ct_id` was rejected as an external-solver candidate and `_compute_annulus_section` as a stability candidate, both pending engineering classification. [docs/registry/implementation-map.yaml:L49301-L49308] [docs/registry/implementation-map.yaml:L51870-L51877]

Map follow-up: add families with in-package implementation rows for the section-3 methods: diameter/area/volume resolution, continuity and Reynolds number, laminar/Haaland friction, Darcy-Weisbach loss, hydrostatics/ECD, and pressure/circulation-time balances; issue pending (4). These methods are not counted until the map includes them with in-package rows. [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L189-L314] [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L137-L187]

Supplementary repository search (including `scripts/`, examples and tests) used `git grep -n -E 'Darcy|Haaland|darcy_weisbach|def .*friction_factor|def .*hydrostatic|def .*hydraulic_diameter' -- '*.py'`. The following inspected definitions illustrate outside-package overlap; these are map follow-up findings, not counted-family misses. [src/digitalmodel/common/hydraulics.py:L6-L24] [src/digitalmodel/field_development/onshore_layout.py:L309-L346]

| Other location | Disposition |
|---|---|
| `common/hydraulics.swamee_jain_friction_factor` | Independent laminar/Swamee-Jain formula with finite-value checks; different transition/correlation from CT. [src/digitalmodel/common/hydraulics.py:L6-L24] |
| `infrastructure/utils/pipe_friction.swamee_jain_friction_factor` | Independent laminar/Swamee-Jain formula; different turbulent formula from CT. [src/digitalmodel/infrastructure/utils/pipe_friction.py:L8-L24] |
| `field_development/onshore_layout.darcy_weisbach_pressure_drop` | Computes Darcy loss and hydrostatics, delegating friction factor. [src/digitalmodel/field_development/onshore_layout.py:L327-L346] |
| `production_engineering/vlp_correlations._friction_factor` | Independent laminar/Churchill formula. [src/digitalmodel/production_engineering/vlp_correlations.py:L139-L149] |
| `scripts/integrations/fluids_evaluation.demo_friction_factors` | Calls the external `fluids` package; helper/demo, rather than its own friction formula. [scripts/integrations/fluids_evaluation.py:L17-L45] |

## 10. Coverage and open items

Read coverage: all 2 package Python files, their full subfolder inventory, the module README, theory document, module survey mentions, parent pack references, workflow registry/input, engine dispatch, shared units, compatibility test and CT workflow assertion branch were inspected; no package-local test file was found. The example assertion branch and compatibility parameter are the two module-specific test nodes found by the module-name search. [src/digitalmodel/marine_ops/ct_hydraulics/__init__.py:L1-L5] [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L1-L334] [tests/compat/test_group_compat.py:L102-L120] [tests/workflows/test_durable_workflows.py:L1474-L1484]

| Documentation found under `docs/` | Read disposition |
|---|---|
| `docs/domains/marine_ops/ct_hydraulics/README.md` | API/description placeholders; issue pending (5). [docs/domains/marine_ops/ct_hydraulics/README.md:L1-L17] |
| `docs/domains/coiled-tubing/CT_HYDRAULICS_THEORY.md` | Equations/scope read; its legacy worked-example link has identifying text and is not reproduced here; issue pending (5). [docs/domains/coiled-tubing/CT_HYDRAULICS_THEORY.md:L3-L70] |
| `docs/domains/project-docs/MODULE_SURVEY.md` | Module status mentions are survey claims, not measured test/maturity evidence; issue pending (5). [docs/domains/project-docs/MODULE_SURVEY.md:L30-L30] [docs/domains/project-docs/MODULE_SURVEY.md:L129-L129] |
| `docs/audits/workflow-failure-evidence-coverage.html` | Generated coverage row read; not independent engineering evidence. [docs/audits/workflow-failure-evidence-coverage.html:L166-L166] |
| `docs/registry/agent-pack-audit/production-nde-functions-2.json` | Generated function inventory mentions read; not a physical validation record. [docs/registry/agent-pack-audit/production-nde-functions-2.json:L562-L565] |
| `docs/registry/workflows.yaml` | Durable workflow entry read and executed. [docs/registry/workflows.yaml:L1152-L1160] |
| `docs/registry/module-routing.yaml` | Existing containing-domain entry read; add module pack and EC mapping beneath its submodules. [docs/registry/module-routing.yaml:L228-L255] |

Additional discovery: 1 historical input file has an identifying filename and is intentionally unnamed; its values are not used as reference data and no client-derived values or private sidecar data are included. The public workflow is the sole worked-example input; codenaming and provenance review are issue pending (5). [docs/domains/coiled-tubing/CT_HYDRAULICS_THEORY.md:L72-L74] [docs/registry/workflows.yaml:L1152-L1160]

| Open item | Assigned role | Tracking |
|---|---|---|
| Input/schema/type validation, invalid geometry and zero fallbacks in section 6 | Engineering calculations maintainer | issue pending (1) |
| U1: independent full-system benchmark and accuracy envelope; U2: fixed conversion/method provenance and absent citation sidecar; permissive compatibility assertion and missing focused unit/boundary tests; shared-log registration | Engineering calculations quality manager | issue pending (2) |
| Summary return-path documentation mismatch, missing-directory evidence policy and unsigned summary limitations | Engineering calculations maintainer | issue pending (3) |
| Map families absent for section-3 calculations; outside-package overlap classification | Engineering calculations maintainer | issue pending (4) |
| Module README/survey completion, identifying discovery input/link codenaming and provenance | Documentation maintainer | issue pending (5) |

The calculation-output citation contract was readable; no licensed standard text or private repository was consulted, and no solver execution or solver-version qualification is claimed. Full-system physical qualification remains U1/U2, because fixed regression assertions do not supply an independent benchmark. [src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py:L175-L187] [tests/workflows/test_durable_workflows.py:L1474-L1484]
