# Production engineering literature and implementation basis

## Summary

Fourteen paper and book records are reviewed for inflow, vertical lift, nodal
analysis and artificial lift. The publisher-deposited DOI records and book
catalogue records were opened on 2026-10-14. Paper full texts and textbook
contents for those fourteen sources were not read. Additional equation passages
in an open thesis were read on 2026-10-18, as recorded below.
The records support conditional method screening;
they do not establish published-example validation or complete equation
lineage. The existing VLP implementation is a simplified traverse, not either
published correlation whose name survives as a deprecated alias.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 1 | 2026-10-14 | Record accessible literature, reconcile implementation descriptions and identify evidence required for numerical qualification. |
| 2 | 2026-10-18 | Add opened IPR equation passages, a composite join derivation and explicit selection qualification gaps. |

## Parameter and equation convention

The equations in this section describe the repository implementation in
`src/digitalmodel/production_engineering/ipr_models.py`. The new passage ledger
below verifies the IPR forms through an open thesis; original defining-paper
equations remain unread. Pressure `Pr` is reservoir pressure, `Pwf` is flowing
bottomhole pressure, and `Pb` is bubble-point pressure, all in psi. Rate `q`
and absolute-open-flow rate `qmax` are in bbl/d. `J` is productivity index in
bbl/d/psi. `C` is a fitted coefficient with units (bbl/d)/psi^(2n), and `n` is a
dimensionless fitted exponent.

- Linear PI: `q = J * (Pr - Pwf)` within the single-phase interval.
- Vogel: `q/qmax = 1 - 0.2*(Pwf/Pr) - 0.8*(Pwf/Pr)^2`.
- Fetkovich: `q = C * (Pr^2 - Pwf^2)^n`.
- Composite: linear above `Pb`; below `Pb`, a Vogel-shaped extension uses
  `Pwf/Pb` and joins the linear rate at `Pb`.

The VLP interface uses tubing depth in ft, bore and roughness in inches,
liquid rate in bbl/d, dimensionless water fraction, GOR in scf/bbl oil,
temperature in °F and pressure in psi. The published methods additionally
depend on holdup, friction and fluid-property definitions that cannot be
reconstructed from their bibliographic records alone.

## Open IPR equation passage ledger

Nass, M.A. (2010), *Inflow Performance Relationships (IPR) for Solution Gas
Drive Reservoirs — a Semi-Analytical Approach*, MS thesis, Texas A&M University.
[Public university PDF](https://blasingame.engr.tamu.edu/0_TAB_Grad/TAB_Grad_Thesis_Archive/MS_044_NASS%2CMaria_Thesis_TAMU_%28May_2010%29.pdf).
Read sections 1.2.1–1.2.2 on 2026-10-18. This is secondary equation evidence
for the earlier methods, not inspection of their original papers.

| Method | Opened passage, printed page | Equation and implementation |
| --- | --- | --- |
| Linear PI | Eq. (1.7), p. 3 | `q=J*(Pr-Pwf)`; `ipr_models.LinearIpr`. |
| Vogel | Eq. (1.12), p. 5, attributed to Vogel (1968) | `q/qmax=1-0.2*r-0.8*r²`, `r=Pwf/Pr`; `ipr_models.VogelIpr`. |
| Fetkovich | Eq. (1.16), p. 6, attributed to Fetkovich (1973) | `q/qmax=[1-(Pwf/Pr)²]^n`; algebraically the existing `C*(Pr²-Pwf²)^n` with `C=qmax/Pr^(2*n)`; `ipr_models.FetkovichIpr`. |

These passages establish forms and parameter meaning, not universal numerical
applicability envelopes. `C`, `n`, `J` and `qmax` remain supplied or fitted.

### Composite join derivation

This is an independent derivation of the existing composite, not a new claim
about its historical attribution. Write `f(r)=1-a*r-b*r²`, using `a=0.2`,
`b=0.8` from Eq. (1.12) above. Then `f(1)=0` and `f'(1)=-(a+2*b)=-1.8`.
Join the linear segment at `qb=J*(Pr-Pb)` to `q=qb+A*f(Pwf/Pb)`.
Matching slopes gives `-A*1.8/Pb=-J`, hence `A=J*Pb/1.8`.
Rate and first derivative are continuous at `Pb` for positive `J`, `Pb` and
`Pr>Pb`. This does not supply evidence about field accuracy.

`test_selection_assessment.py` writes out synthetic hand calculations for the
selected models: linear 1000, Vogel 700, Fetkovich 3000 and composite 1900
bbl/d, plus the composite boundary rate 1200 bbl/d. Exact comparisons use
these arithmetic results; they introduce no empirical acceptance tolerance.
They are not original-paper or measured-data validation fixtures.

## Defining papers and texts

### 1. Vogel — normalized solution-gas-drive inflow

Vogel, J.V. (1968). *Inflow Performance Relationships for Solution-Gas Drive
Wells*. Journal of Petroleum Technology 20(1), 83–92.
[doi:10.2118/1476-PA — deposited record](https://api.crossref.org/works/10.2118/1476-PA).

The deposited abstract and introduction describe simulated solution-gas-drive
reservoirs and the limitations of a constant PI for two-phase reservoir flow.
The model parameters are `Pr`, `Pwf` and `qmax`. The source calls for field
verification; a universal pressure or rate validity interval is not established.
`ipr_models.VogelIpr` implements the normalized polynomial above. Its detailed
original equation passage and original worked-example values remain unverified;
the open thesis passage above verifies the polynomial as a secondary source.

### 2. Fetkovich — measured multirate inflow

Fetkovich, M.J. (1973). *The Isochronal Testing of Oil Wells*. Fall Meeting of
the Society of Petroleum Engineers of AIME, SPE 4529.
[doi:10.2118/4529-MS — deposited record](https://api.crossref.org/works/10.2118/4529-MS).

The deposited record describes oil-well multipoint tests across undersaturated,
saturated and depleted states, with test interpretation depending on the
state. The observed exponent range is 0.568–1.000. Parameters are `Pr`, `Pwf`,
fitted `C` and fitted `n`. `ipr_models.FetkovichIpr` implements the back-pressure
form. The record does not expose its equation or a numeric worked example;
the thesis ledger above supplies secondary equation evidence. The
implementation's theoretical exponent discussion is not an observed-data
validity guarantee.

### 3. Hagedorn and Brown — vertical two-phase pressure gradient

Hagedorn, A.R., and Brown, K.E. (1965). *Experimental Study of Pressure
Gradients Occurring During Continuous Two-Phase Flow in Small-Diameter Vertical
Conduits*. Journal of Petroleum Technology 17(4), 475–484.
[doi:10.2118/940-PA — deposited record](https://api.crossref.org/works/10.2118/940-PA).

The record describes a vertical experimental well and variation of liquid rate,
gas-liquid ratio and viscosity. Geometry, phase rates and phase properties
govern the dimensional groups; full definitions, holdup equations and detailed
range checks are not established from the record. No implementation of the
published correlation exists here: `vlp_correlations.hagedorn_brown_pwf` is a
warning-emitting alias of the simplified traverse.

### 4. Beggs and Brill — inclined two-phase flow

Beggs, D.H., and Brill, J.P. (1973). *A Study of Two-Phase Flow in Inclined
Pipes*. Journal of Petroleum Technology 25(5), 607–617.
[doi:10.2118/4007-PA — deposited record](https://api.crossref.org/works/10.2118/4007-PA).

The record identifies inclination, liquid holdup and friction factor as the
method's basis and explicitly covers inclined pipes. Phase rates, pipe bore,
inclination and fluid properties are required; equation-specific parameter
definitions and numerical development ranges require the full text. No
published-method implementation exists here: `vlp_correlations.beggs_brill_pwf`
aliases the same simplified traverse as the Hagedorn-Brown alias.

### 5. Brown — artificial-lift text

Brown, K.E. (1977). *The Technology of Artificial Lift Methods*. PPC Books.
[Catalogue edition](https://openlibrary.org/books/OL4905846M).

Only the catalogue record was opened. Its title, author, date and publisher are
verified; equations, parameter definitions, design ranges and later-volume
attributions are not. No method is implemented on this record's authority.
The existing composite docstring's later Brown attribution remains a
secondary-source statement requiring the relevant volume and passage.

### 6. Brill and Mukherjee — multiphase-flow text

Brill, J.P., and Mukherjee, H. (1999). *Multiphase Flow in Wells*. Henry L.
Doherty Memorial Fund of AIME, Society of Petroleum Engineers.
[Catalogue edition](https://openlibrary.org/books/OL24385198M).

The edition record verifies both authors and the publication basis. Contents,
worked pressure traverses, property definitions, correlation ranges and
equation lineage were not accessible from that record. No numerical fixture
is attributed to the book, and no implemented published VLP method is claimed.

### 7. Economides, Hill and Ehlig-Economides — production systems

Economides, M.J., Hill, A.D., and Ehlig-Economides, C. (1994). *Petroleum
Production Systems*. PTR Prentice Hall.
[Catalogue edition](https://openlibrary.org/books/OL1425614M).

Only the edition record was read. Nodal-case inputs, output rates, equation
numbers and validity ranges are not established. `nodal_solver.NodalSolver`
numerically intersects supplied IPR and VLP curves; it is not validated against
a worked example from this book. No chapter/example locator is asserted here.

## Supporting and comparative literature

| Reference | Parameter basis and applicability established by the opened record | Implementation or gap |
| --- | --- | --- |
| Standing, M.B. (1970). *Inflow Performance Relationships for Damaged Wells Producing by Solution-Gas Drive*. JPT 22(11), 1399–1400. [doi:10.2118/3237-PA](https://api.crossref.org/works/10.2118/3237-PA). | Flow efficiency describes damaged versus undamaged inflow. The record distinguishes Vogel's early-life undamaged basis; detailed correction parameters and numeric bounds remain unverified. | No damaged-well model; selector rejects significant skin. |
| Klins, M.A., and Clark, J.W. (1993). *An Improved Method To Predict Future IPR Curves*. SPE Reservoir Engineering 8(4), 243–248. [doi:10.2118/20724-PA](https://api.crossref.org/works/10.2118/20724-PA). | Forecast IPR depends on depletion state; exact forecasting parameters and operating bounds require the full text. | No forecasting implementation; this is not the source of the repository's linear-plus-Vogel composite. |
| Brown, K.E., and Lea, J.F. (1985). *Nodal Systems Analysis of Oil and Gas Wells*. JPT 37(10), 1751–1763. [doi:10.2118/14714-PA](https://api.crossref.org/works/10.2118/14714-PA). | A systems approach joins reservoir-to-separator pressure and rate relationships. Detailed component equations and case bounds are not available from the record. | `nodal_solver.NodalSolver` implements an intersection algorithm only. |
| Vohra, I.R., Robinson, J.R., and Brill, J.P. (1974). *Evaluation of Three New Methods For Predicting Pressure Losses In Vertical Oilwell Tubing*. JPT 26(8), 829–832. [doi:10.2118/4689-PA](https://api.crossref.org/works/10.2118/4689-PA). | Comparison variables include rates, bore, API gravity and phase ratios. The record finds no universal winner; detailed range boundaries are not established here. | Selection rationale only; no new correlation. |
| Aggour, M.A., Al-Yousef, H.Y., and Al-Muraikhi, A.J. (1996). *Vertical Multiphase Flow Correlations for High Production Rates and Large Tubulars*. SPE Production & Facilities 11(1), 41–48. [doi:10.2118/28465-PA](https://api.crossref.org/works/10.2118/28465-PA). | The abstract compares tubing size, oil rate, water cut and GOR subsets. Beggs-Brill leads overall; Hagedorn-Brown leads above 80 % water cut. These are dataset findings, not general validity limits. | Conditional literature candidates only; published VLP implementations remain absent. |
| Ansari, A.M., Sylvester, N.D., Sarica, C., Shoham, O., and Brill, J.P. (1994). *A Comprehensive Mechanistic Model for Upward Two-Phase Flow in Wellbores*. SPE Production & Facilities 9(2), 143–151. [doi:10.2118/20630-PA](https://api.crossref.org/works/10.2118/20630-PA). | Upward bubble, slug and annular flow are modeled separately with holdup and pressure drop. Detailed parameters and numerical ranges require the full text. | No mechanistic implementation in this module. |
| Clegg, J.D., Bucaram, S.M., and Hein, N.W. (1993). *Recommendations and Comparisons for Selecting Artificial-Lift Methods*. JPT 45(12), 1128–1167. [doi:10.2118/24834-PA](https://api.crossref.org/works/10.2118/24834-PA). | Comparative lift-selection attributes are discussed. Numeric rate/depth envelopes and economic parameters were not obtained. | No artificial-lift sizing or selection is added by the IPR/VLP selector. |

Table 1. Supporting records and the limits of abstract-level evidence.

## Gray and open implementation evidence

The [Marlim3 correlation tutorial](https://marlim3.readthedocs.io/en/latest/tutorials/4_black_box_correlations/)
was opened, together with its public `src/core/GradientCorrelations.cpp` source.
The source's Gray documentation cites API 14B, *Subsurface Controlled Safety
Valve Sizing Computer Program*, June 1974, Appendix B, *Vertical Flow Correlation
in Gas Wells*. This is a secondary attribution, not inspection of the manual.
The open implementation identifies gas wells with liquid production and
requires geometry, phase velocities, densities, viscosities, surface tension
and absolute pressure. No coefficients or tabulated values from the manual
are reproduced here.

The source is useful implementation evidence, but is not a published worked
example from the required reference set. Equivalence to the GLR-corrected
revision is not established. No Gray routine or validation claim is added.

## Citation reconciliation and remaining qualification

The existing Vogel citation agrees with the opened record's 83–92 page range.
The existing composite description correctly separates its linear-plus-Vogel
construction from the Klins-Clark forecasting paper. Its Brown and Neely
secondary attributions remain unverified. The Fetkovich implementation's
theoretical exponent interval does not replace the observed interval stated
above. The VLP aliases are correctly described as aliases, with no claim to
implement the cited published methods.

The [selection policy](production-engineering-method-selection.md) follows
these distinctions. Completion of detailed equation lineage, parameter ranges
for every reference, and original numerical validation still requires source
passages not present in the opened records. The three catalogue records do not
contain worked examples. A proposed book DOI returned HTTP 404 and supplied no
evidence; the verified catalogue edition is used instead. No unavailable source
value is used as an engineering input.
