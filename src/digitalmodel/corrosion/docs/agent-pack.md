# Galvanic screening knowledge pack

This pack documents the pinned code contract. Arithmetic evidence does not
qualify physical criteria, equipment selection or a final design. UNVERIFIED
means the numerical implementation is readable but its claimed governing basis
has not been established. NOT_EVALUATED means the required source was unreadable.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| 1 | 2026-10-22 | Initial pinned API inventory and hand anchors for repeatable review. |
| 2 | 2026-10-25 | Correct the qualitative area-ratio locator to cite the note definition. |

| Header | Value |
| --- | --- |
| Code pin (`origin/main`) | `3554b14fde67f44b1c52b81b6c72106282eaff29` |
| Package | `src/digitalmodel/corrosion/` |
| QA family | EC — engineering calculation screening |
| Evidence basis | Source lines at code pin; synthetic offline tests |

## 1. Purpose and scope

Anodic-index difference and categorical compatibility screening, not a corrosion-rate or life prediction. Area ratio appears only in notes. [src/digitalmodel/corrosion/galvanic_screening.py:L1-L45]

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
| `galvanic_screening.GalvanicScreenResult` | metal/environment strings, delta/allowable V, verdict string, optional anodic/cathodic strings; no own validation<br>Fields: metal_a: str; metal_b: str; environment: str; delta_v: float; allowable_delta_v: float; verdict: str; anodic: Optional[str]; cathodic: Optional[str]; notes: List[str] = field(default_factory=list) | Frozen fields with mutable notes list default_factory=list | [src/digitalmodel/corrosion/galvanic_screening.py:L102-L113] |
| `galvanic_screening.verdict_for_delta` | delta, allowable and marginal band V; no range/finiteness validation; band defaults MARGINAL_BAND_V<br>Signature: (delta_v: float, allowable_delta_v: float, marginal_band_v: float=MARGINAL_BAND_V) | str OK/MARGINAL/PROTECT; inclusive boundaries with numerical tolerance | [src/digitalmodel/corrosion/galvanic_screening.py:L134-L144] |
| `galvanic_screening.screen_couple` | metal keys required; environment default harsh_marine; mapping ENVIRONMENTS and band MARGINAL_BAND_V; unknown keys raise KeyError<br>Signature: (metal_a: str, metal_b: str, environment: str='harsh_marine', *, environments: Mapping[str, float]=ENVIRONMENTS, marginal_band_v: float=MARGINAL_BAND_V) | GalvanicScreenResult: rounded delta V, allowable, verdict, anodic/cathodic, recommendations; equal indices give None members | [src/digitalmodel/corrosion/galvanic_screening.py:L147-L215] |
| `galvanic_screening.compatibility_matrix` | Environment default harsh_marine; metals None means all table keys; custom environments/band accepted; no duplicate/empty validation<br>Signature: (environment: str='harsh_marine', *, metals: Optional[List[str]]=None, environments: Mapping[str, float]=ENVIRONMENTS, marginal_band_v: float=MARGINAL_BAND_V) | pandas DataFrame of verdict strings indexed by supplied names; calls screen_couple for each cell | [src/digitalmodel/corrosion/galvanic_screening.py:L218-L242] |
| `galvanic_screening.verdict_rank` | Verdict string; unknown key raises KeyError<br>Signature: (verdict: str) | int ordinal OK < MARGINAL < PROTECT; software ordering only | [src/digitalmodel/corrosion/galvanic_screening.py:L245-L247] |

## 3. Method and governing standards

| Method / constant | Basis and status | Evidence |
| --- | --- | --- |
| delta=abs(index_A−index_B); larger index labelled anodic; equal indices give no member | Difference arithmetic and implemented sign convention. Code attributes table to MIL-STD-889 without edition/table locator: NOT_EVALUATED externally. | [src/digitalmodel/corrosion/galvanic_screening.py:L147-L190] |
| ANODIC_INDEX_V metal values, ENVIRONMENTS limits, MARGINAL_BAND_V | NOT_EVALUATED: no edition/clause/table is supplied for either the metal indices, environmental thresholds or marginal band. No table rows or physical limits are reproduced here. | [src/digitalmodel/corrosion/galvanic_screening.py:L61-L86] [src/digitalmodel/corrosion/galvanic_screening.py:L88-L94] |
| OK if delta<=allowable+tolerance; MARGINAL if delta<=allowable+band+tolerance; otherwise PROTECT | Software classification. Inclusive tolerance 1e-12 V is a numerical convention with no physical source claim. | [src/digitalmodel/corrosion/galvanic_screening.py:L134-L144] |
| Round returned delta to six decimal places; matrix calls the same couple calculation; verdict_rank ordinal order | Software presentation, composition and ordering conventions, not independent evidence. | [src/digitalmodel/corrosion/galvanic_screening.py:L204-L247] |
| Small-anode/large-cathode notes and recommendations | Qualitative guidance only: no area, electrolyte or polarization model is calculated; physical adequacy NOT_EVALUATED. | [src/digitalmodel/corrosion/galvanic_screening.py:L93-L98] [src/digitalmodel/corrosion/galvanic_screening.py:L192-L202] |

Source retrieval: the [institutional MIL-STD-889C PDF address](https://www.dau.edu/sites/default/files/Migrated/CopDocuments/MIL-STD-889C.pdf)
returned HTTP 403 on 2026-10-22. The table could not be read. A public catalogue
probe returned an unrelated handbook and supplies no evidence for this method.
The requested two-metal standard-table difference is **NOT_EVALUATED**: no
numerical metal pair is asserted as a sourced hand anchor. It remains an open
item even though existing tests assert the implementation's table values.

## 4. Validated worked examples and regression examples

The following values are synthetic **inputs to verdict_for_delta**, not metal
properties or service limits. Each computes a difference independently and
uses caller-specified allowable 0.2 V and band 0.1 V. No standard acceptance
claim is made. The requested published-table anchor remains NOT_EVALUATED.

| Example | Inputs, hand method and literal answer | Executable evidence |
| --- | --- | --- |
| Inclusive OK | Synthetic indices 0.8 and 0.6 V: delta=abs(0.8−0.6)=0.2 V; delta<=0.2 V, answer OK | `tests/corrosion/test_corrosion_agent_pack.py::test_synthetic_ok_boundary` |
| Inclusive marginal | Synthetic indices 0.9 and 0.6 V: delta=0.3 V; 0.2<delta<=0.2+0.1, answer MARGINAL | `tests/corrosion/test_corrosion_agent_pack.py::test_synthetic_marginal_boundary` |
| Protection branch | Synthetic indices 1.0 and 0.6 V: delta=0.4 V; delta>0.3 V, answer PROTECT | `tests/corrosion/test_corrosion_agent_pack.py::test_synthetic_protect` |

Boundary assertions cover the implementation's numerical tolerance as well as
branch ordering. These are arithmetic regressions, not physical validation.

## 5. QA gates and executable checks

| Gate | Executable check | Meaning |
| --- | --- | --- |
| Existing pair behavior | `tests/corrosion/test_galvanic_screening.py::TestScreenCouple::test_symmetry` | Operand order does not change verdict |
| Public contract | `tests/docs/test_screening_agent_packs.py::test_public_contract_and_locators[corrosion]` | Public names and source ranges match |
| Citation collection | `tests/docs/test_screening_agent_packs.py::test_all_cited_nodes_collect[corrosion]` | Every section-4/5 node collects |

No listed gate verifies table provenance or suitability for a particular
service. Existing regression tests are evidence of behavior only.

## 6. Known failure modes and pitfalls

| File and lines | Defect / limitation | Follow-up |
| --- | --- | --- |
| [src/digitalmodel/corrosion/galvanic_screening.py:L43-L47] [src/digitalmodel/corrosion/galvanic_screening.py:L61-L94] | Claimed standard basis has no edition or primary table locator; environmental limits and marginal width lack a primary citation. | Read a public governing table and establish provenance before physical use. |
| [src/digitalmodel/corrosion/galvanic_screening.py:L134-L144] | Negative/nonfinite differences, thresholds and bands are accepted; NaN comparisons fall through to PROTECT, while infinite allowance can permit arbitrary differences. | Validate finite nonnegative inputs and a declared band policy. |
| [src/digitalmodel/corrosion/galvanic_screening.py:L147-L154] | Default environments argument references a mutable public dict; table mutation changes later results globally. | Use immutable tables or explicit validated snapshots. |
| [src/digitalmodel/corrosion/galvanic_screening.py:L197-L202] | Marginal recommendation text hardcodes the default band even when caller supplies another band. | Render the actual band or avoid a numeric claim. |
| [src/digitalmodel/corrosion/galvanic_screening.py:L102-L113] | Frozen result still holds a mutable notes list. | Document or enforce deep immutability if relied upon. |
| [src/digitalmodel/corrosion/galvanic_screening.py:L232-L242] | Empty metals bypass couple/environment lookup; duplicates yield ambiguous DataFrame labels. | Validate names and environment even for empty matrices. |
| [src/digitalmodel/corrosion/galvanic_screening.py:L175-L202] | Difference and generic notes omit area ratio magnitude, electrolyte, coating defects and polarization; no corrosion-rate calculation occurs. | Require a suitable physical assessment before acceptance. |

## 7. Lessons log

A regression anchor on a table is not proof of the table's provenance. Keep
synthetic classifier tests separate from published metal-property evidence.
Allowable differences are screening inputs, not predicted material loss.

## 8. Ownership

Maintainers own source traceability and table integrity. Engineering reviewers
must establish material condition, service environment and mitigation adequacy.
No lifetime or installation acceptance follows from the verdict alone.

## 9. Dependencies and other implementations

Dataclasses/typing support records; pandas supplies DataFrame output. Package
exports include two tables and selected public definitions; verdict_for_delta
and verdict_rank are public in their defining module and included here.
[src/digitalmodel/corrosion/__init__.py:L7-L21]
[src/digitalmodel/corrosion/galvanic_screening.py:L53-L57]

Routing and implementation-map `agent_packs` metadata register EC without
creating a new numerical implementation family. No accepted implementation-map
row for this package was found. The separate report does not establish numeric
standard-table provenance.

## 10. Coverage and open items

Read both package Python files, all existing galvanic-screening tests, routing,
implementation map and the report diff. All package-owned public definitions
are listed with locators and covered by the local AST inventory check.
The standard-table hand anchor and physical thresholds remain NOT_EVALUATED;
retrieve a readable public source and record edition/table before adding them.
Resolve section-6 defects separately; no production behavior changes here.
