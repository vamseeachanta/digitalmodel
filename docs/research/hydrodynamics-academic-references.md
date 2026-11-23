# Hydrodynamics academic references

Academic bibliographic reference list for offshore hydrodynamics.

Source: research note on the issue, checked 2026-10-11.

Only bibliographic metadata is carried.

## How to read this list

- **40 entries**, grouped by sub-topic. Each has a BibTeX-style key, citation, identifier, access route and a one-line use note.
- **Identifier status** (checked 2026-10-11):
  - `[CR]` = DOI (and ISBN where shown) confirmed in the Crossref registration record for that DOI, i.e. the publisher-deposited metadata behind doi.org.
  - `[OL]` = ISBN confirmed in an Open Library catalogue record (Library of Congress control number noted where the record carries one).
  - `no identifier` = none exists or none was found; nothing was written from memory.
- **Access**: "free" links are publisher, author or institutional hosts only. "(200)" means the link resolved when checked; "(listed)" means it was returned by search but the host blocked an automated check. Everything else is "no free access" with the publisher/DOI landing page.
- Page ranges for book chapters are taken from the publisher's chapter-level DOI records, not from memory. Where no such record exists, no chapter pointer is given (see gaps at the end).
- Bibliographic metadata only; no copyrighted text is reproduced.


## A. Core textbooks and monographs

```json
{"id": "H1", "key": "journee2001",
 "citation": "Journée, J.M.J. & Massie, W.W. (2001). *Offshore Hydromechanics*, 1st ed. Delft University of Technology.",
 "identifier": "no identifier (course text, no ISBN)", "identifier_status": "no identifier",
 "access": "free — https://ocw.tudelft.nl/wp-content/uploads/OffshoreHydromechanics_Journee_Massie_01.pdf (200), via the TU Delft OpenCourseWare readings page https://ocw.tudelft.nl/course-readings/offshore-hydromechanics-part-1-readings/",
 "pointers": null, "use": "one-volume course text from hydrostatics and potential flow through wave loads, floating-body motions and mooring."}
```

```json
{"id": "H2", "key": "faltinsen1990",
 "citation": "Faltinsen, O.M. (1990). *Sea Loads on Ships and Offshore Structures*. Cambridge Ocean Technology Series, Cambridge University Press.",
 "identifier": "ISBN 978-0-521-37285-5 (hardback, 1990) / 978-0-521-45870-2 (paperback, 1993) [OL]", "identifier_status": "[OL]",
 "access": "no free access — https://www.cambridge.org/9780521458702",
 "pointers": null, "use": "linear wave-induced motions and loads, second-order drift and slow-drift, viscous loads, station-keeping, slamming."}
```

```json
{"id": "H3", "key": "newman1977",
 "citation": "Newman, J.N. (1977; 40th anniversary ed. 2018). *Marine Hydrodynamics*. MIT Press.",
 "identifier": "ISBN 978-0-262-53482-6 [OL]", "identifier_status": "[OL]",
 "access": "free (publisher open access) — https://library.oapen.org/handle/20.500.12657/26039 (listed); catalogue https://mitpress.mit.edu/9780262534826/marine-hydrodynamics/",
 "pointers": null, "use": "added mass, wave theory, radiation/diffraction of floating bodies, slender-body theory."}
```

```json
{"id": "H4", "key": "lewis1989pna3",
 "citation": "Lewis, E.V. (ed.) (1989). *Principles of Naval Architecture, Second Revision, Vol. III: Motions in Waves and Controllability*. SNAME.",
 "identifier": "ISBN 978-0-939773-02-2 [OL]", "identifier_status": "[OL]",
 "access": "no free access — https://www.sname.org",
 "pointers": null, "use": "ship motions in waves, seakeeping criteria, controllability."}
```

```json
{"id": "H5", "key": "sarpkaya1981",
 "citation": "Sarpkaya, T. & Isaacson, M. (1981). *Mechanics of Wave Forces on Offshore Structures*. Van Nostrand Reinhold.",
 "identifier": "ISBN 978-0-442-25402-5 [OL, LCCN 80020237]", "identifier_status": "[OL, LCCN 80020237]",
 "access": "no free access (out of print; library copies)",
 "pointers": null, "use": "Morison loading, drag/inertia coefficients, diffraction on large bodies."}
```

```json
{"id": "H6", "key": "sarpkaya2010",
 "citation": "Sarpkaya, T. (2010). *Wave Forces on Offshore Structures*. Cambridge University Press.",
 "identifier": "DOI 10.1017/CBO9781139195898; ISBN 978-0-521-89625-2 [CR]", "identifier_status": "[CR]",
 "access": "no free access — https://doi.org/10.1017/CBO9781139195898",
 "pointers": "separation and time-dependent flows pp. 39–108; waves and wave–structure interaction pp. 109–171; large bodies pp. 172–185; VIV pp. 186–264; hydrodynamic damping pp. 265–284", "use": "updated treatment of separated-flow wave loading and VIV."}
```

```json
{"id": "H7", "key": "chakrabarti1987",
 "citation": "Chakrabarti, S.K. (1987). *Hydrodynamics of Offshore Structures*. Computational Mechanics Publications / Springer-Verlag.",
 "identifier": "ISBN 978-0-905451-66-4 [OL, LCCN 87016845]", "identifier_status": "[OL, LCCN 87016845]",
 "access": "no free access (WIT Press catalogue https://www.witpress.com)",
 "pointers": null, "use": "design-oriented wave theories, wave force formulations, model-test correlation."}
```

```json
{"id": "H8", "key": "chakrabarti2005",
 "citation": "Chakrabarti, S.K. (ed.) (2005). *Handbook of Offshore Engineering* (2 vols). Elsevier.",
 "identifier": "ISBN 978-0-08-044381-2 [CR]", "identifier_status": "[CR]",
 "access": "no free access — https://doi.org/10.1016/B978-0-08-044381-2.50020-5 (chapter record of the same book)",
 "pointers": "Ocean Environment pp. 79–131; Loads and Responses pp. 133–196; Probabilistic Design pp. 197–277; Floating Offshore Platform Design pp. 419–661; Physical Modelling pp. 1001–1054", "use": "practitioner handbook tying environment, loads and response together."}
```

```json
{"id": "H9", "key": "molin2023",
 "citation": "Molin, B. (2023). *Offshore Structure Hydrodynamics*. Cambridge University Press.",
 "identifier": "DOI 10.1017/9781009198059; ISBN 978-1-009-19804-2 [CR]", "identifier_status": "[CR]",
 "access": "no free access — https://doi.org/10.1017/9781009198059",
 "pointers": "wave theories pp. 49–92; slender bodies pp. 93–131; large bodies, linear theory pp. 158–218; second-order effects pp. 219–286; other nonlinear effects pp. 287–331; model testing pp. 332–372", "use": "modern single-author text on first- and second-order wave loads."}
```

```json
{"id": "H10", "key": "mei2005",
 "citation": "Mei, C.C., Stiassnie, M. & Yue, D.K.-P. (2005). *Theory and Applications of Ocean Surface Waves* (2 parts). Advanced Series on Ocean Engineering 23, World Scientific.",
 "identifier": "DOI 10.1142/5566; ISBN 978-981-238-893-3 [CR] (later edition registered as DOI 10.1142/10212, ISBN 978-981-3147-17-1 [CR])", "identifier_status": "[CR]",
 "access": "no free access — https://doi.org/10.1142/5566",
 "pointers": null, "use": "rigorous linear and nonlinear wave theory, diffraction, floating-body dynamics."}
```

```json
{"id": "H11", "key": "dean1991",
 "citation": "Dean, R.G. & Dalrymple, R.A. (1991). *Water Wave Mechanics for Engineers and Scientists*. Advanced Series on Ocean Engineering 2, World Scientific.",
 "identifier": "DOI 10.1142/1232; ISBN 978-981-02-0421-1 [CR]", "identifier_status": "[CR]",
 "access": "no free access — https://doi.org/10.1142/1232",
 "pointers": "Wave Forces pp. 212–260", "use": "linear wave kinematics, dispersion, engineering wave properties."}
```

```json
{"id": "H12", "key": "lloyd1998",
 "citation": "Lloyd, A.R.J.M. (1998). *Seakeeping: Ship Behaviour in Rough Weather*, revised ed. A.R.J.M. Lloyd (first published Ellis Horwood, 1989).",
 "identifier": "ISBN 978-0-9532634-0-0 [OL]", "identifier_status": "[OL]",
 "access": "no free access (out of print)",
 "pointers": null, "use": "strip-theory seakeeping, motion statistics, operability criteria."}
```

```json
{"id": "H13", "key": "ochi1998",
 "citation": "Ochi, M.K. (1998). *Ocean Waves: The Stochastic Approach*. Cambridge Ocean Technology Series 6, Cambridge University Press.",
 "identifier": "DOI 10.1017/CBO9780511529559; ISBN 978-0-521-56378-9 [CR]", "identifier_status": "[CR]",
 "access": "no free access — https://doi.org/10.1017/CBO9780511529559",
 "pointers": null, "use": "spectral description of seas, short- and long-term statistics, extremes."}
```

```json
{"id": "H14", "key": "goda2010",
 "citation": "Goda, Y. (2010). *Random Seas and Design of Maritime Structures*, 3rd ed. Advanced Series on Ocean Engineering 33, World Scientific.",
 "identifier": "DOI 10.1142/7425; ISBN 978-981-4282-39-0 [CR]", "identifier_status": "[CR]",
 "access": "no free access — https://doi.org/10.1142/7425",
 "pointers": null, "use": "irregular-wave statistics, spectra, directional spreading for design."}
```

```json
{"id": "H15", "key": "sumer2006",
 "citation": "Sumer, B.M. & Fredsøe, J. (2006). *Hydrodynamics Around Cylindrical Structures*, revised ed. Advanced Series on Ocean Engineering 26, World Scientific.",
 "identifier": "DOI 10.1142/6248; ISBN 978-981-270-039-1 [CR]", "identifier_status": "[CR]",
 "access": "no free access — https://doi.org/10.1142/6248",
 "pointers": null, "use": "flow regimes, forces on cylinders in waves/current, wall proximity, VIV."}
```

```json
{"id": "H16", "key": "naess2012",
 "citation": "Naess, A. & Moan, T. (2012). *Stochastic Dynamics of Marine Structures*. Cambridge University Press.",
 "identifier": "DOI 10.1017/CBO9781139021364; ISBN 978-0-521-88155-5 [CR]", "identifier_status": "[CR]",
 "access": "no free access — https://doi.org/10.1017/CBO9781139021364",
 "pointers": null, "use": "random response, extreme-value prediction, fatigue under stochastic wave loading."}
```

## B. Open courseware and freely available reference texts

```json
{"id": "H17", "key": "mitocw224",
 "citation": "Sclavounos, P.D. (2002). *2.24 Ocean Wave Interaction with Ships and Offshore Energy Systems (13.022)*, lecture notes. MIT OpenCourseWare.",
 "identifier": "no identifier", "identifier_status": "no identifier",
 "access": "free — https://ocw.mit.edu/courses/2-24-ocean-wave-interaction-with-ships-and-offshore-energy-systems-13-022-spring-2002/ (200)",
 "pointers": null, "use": "linear radiation/diffraction, seakeeping, second-order drift, with problem sets."}
```

```json
{"id": "H18", "key": "mitocw220",
 "citation": "MIT Department of Mechanical Engineering (2005). *2.20 Marine Hydrodynamics (13.021)*, lecture notes. MIT OpenCourseWare.",
 "identifier": "no identifier", "identifier_status": "no identifier",
 "access": "free — https://ocw.mit.edu/courses/2-20-marine-hydrodynamics-13-021-spring-2005/ (200)",
 "pointers": null, "use": "graduate fundamentals — similitude, potential flow, added mass, surface waves; pairs with H3."}
```

```json
{"id": "H19", "key": "usnaen400",
 "citation": "United States Naval Academy, Naval Architecture & Ocean Engineering Dept. (2020). *EN400 Principles of Ship Performance — Course Notes*.",
 "identifier": "no identifier", "identifier_status": "no identifier",
 "access": "free — https://www.usna.edu/NAOE/_files/documents/Courses/EN400/EN400_Course_Notes_Summer_2020.pdf (200)",
 "pointers": null, "use": "introductory hydrostatics, stability, resistance and seakeeping (the issue lists this as \"Ship Hydrodynamics\"; the published course title is as given here)."}
```

```json
{"id": "H20", "key": "wehausen1960",
 "citation": "Wehausen, J.V. & Laitone, E.V. (1960). \"Surface Waves\", in *Encyclopedia of Physics / Handbuch der Physik*, Vol. IX, pp. 446–778. Springer.",
 "identifier": "DOI 10.1007/978-3-642-45944-3_6 [CR]", "identifier_status": "[CR]",
 "access": "free online edition (2002) hosted by UC Berkeley — https://surfacewaves.berkeley.edu/ (200)",
 "pointers": null, "use": "the classical reference for linear and nonlinear water-wave theory and Green functions."}
```

## C. Ship and floating-body motions (seminal papers)

```json
{"id": "H21", "key": "salvesen1970",
 "citation": "Salvesen, N., Tuck, E.O. & Faltinsen, O. (1970). \"Ship motions and sea loads\". *Transactions SNAME* 78, 250–287.",
 "identifier": "no identifier (no DOI found)", "identifier_status": "no identifier",
 "access": "no free access confirmed — https://www.sname.org",
 "pointers": null, "use": "the standard strip theory for five-degree-of-freedom motions and sectional loads."}
```

```json
{"id": "H22", "key": "cummins1962",
 "citation": "Cummins, W.E. (1962). \"The impulse response function and ship motions\". *Schiffstechnik* 9, 101–109; also David Taylor Model Basin Report 1661.",
 "identifier": "no identifier (repository handle 1721.3/49049)", "identifier_status": "no identifier",
 "access": "free — https://dome.mit.edu/handle/1721.3/49049 (200)",
 "pointers": null, "use": "time-domain equation of motion with retardation (convolution) kernels."}
```

```json
{"id": "H23", "key": "john1950",
 "citation": "John, F. (1950). \"On the motion of floating bodies II. Simple harmonic motions\". *Communications on Pure and Applied Mathematics* 3(1), 45–101.",
 "identifier": "DOI 10.1002/cpa.3160030106 [CR]", "identifier_status": "[CR]",
 "access": "no free access — https://doi.org/10.1002/cpa.3160030106",
 "pointers": null, "use": "boundary-value problem and uniqueness for linear floating-body motion; irregular frequencies."}
```

```json
{"id": "H24", "key": "wehausen1971",
 "citation": "Wehausen, J.V. (1971). \"The motion of floating bodies\". *Annual Review of Fluid Mechanics* 3, 237–268.",
 "identifier": "DOI 10.1146/annurev.fl.03.010171.001321 [CR]", "identifier_status": "[CR]",
 "access": "no free access — https://doi.org/10.1146/annurev.fl.03.010171.001321",
 "pointers": null, "use": "review of linear radiation/diffraction theory and reciprocity relations."}
```

```json
{"id": "H25", "key": "faltinsen1990arfm",
 "citation": "Faltinsen, O.M. (1990). \"Wave loads on offshore structures\". *Annual Review of Fluid Mechanics* 22, 35–56.",
 "identifier": "DOI 10.1146/annurev.fl.22.010190.000343 [CR]", "identifier_status": "[CR]",
 "access": "no free access — https://doi.org/10.1146/annurev.fl.22.010190.000343",
 "pointers": null, "use": "compact review of load regimes (inertia, drag, diffraction, nonlinear)."}
```

## D. Wave spectra, kinematics and statistics

```json
{"id": "H26", "key": "pierson1964",
 "citation": "Pierson, W.J. & Moskowitz, L. (1964). \"A proposed spectral form for fully developed wind seas based on the similarity theory of S. A. Kitaigorodskii\". *Journal of Geophysical Research* 69(24), 5181–5190.",
 "identifier": "DOI 10.1029/JZ069i024p05181 [CR]", "identifier_status": "[CR]",
 "access": "no free access — https://doi.org/10.1029/JZ069i024p05181",
 "pointers": null, "use": "Pierson–Moskowitz spectrum."}
```

```json
{"id": "H27", "key": "hasselmann1973",
 "citation": "Hasselmann, K., Barnett, T.P., Bouws, E., et al. (1973). \"Measurements of wind-wave growth and swell decay during the Joint North Sea Wave Project (JONSWAP)\". *Ergänzungsheft zur Deutschen Hydrographischen Zeitschrift*, Reihe A (8°), Nr. 12, 95 pp. Deutsches Hydrographisches Institut, Hamburg.",
 "identifier": "no identifier (no DOI found)", "identifier_status": "no identifier",
 "access": "repository record https://epic.awi.de/id/eprint/10163/ (listed; full-text availability not confirmed)",
 "pointers": null, "use": "JONSWAP spectrum and fetch-limited growth."}
```

```json
{"id": "H28", "key": "longuethiggins1952",
 "citation": "Longuet-Higgins, M.S. (1952). \"On the statistical distribution of the heights of sea waves\". *Journal of Marine Research* 11(3), 245–266.",
 "identifier": "no identifier (no DOI found; the repository record dates the issue 1953)", "identifier_status": "no identifier",
 "access": "free — https://elischolar.library.yale.edu/journal_of_marine_research/774 (200)",
 "pointers": null, "use": "Rayleigh distribution of wave heights, significant-height ratios."}
```

```json
{"id": "H29", "key": "fenton1985",
 "citation": "Fenton, J.D. (1985). \"A fifth-order Stokes theory for steady waves\". *Journal of Waterway, Port, Coastal, and Ocean Engineering* 111(2), 216–234.",
 "identifier": "DOI 10.1061/(ASCE)0733-950X(1985)111:2(216) [CR]", "identifier_status": "[CR]",
 "access": "no free access — https://doi.org/10.1061/(ASCE)0733-950X(1985)111:2(216)",
 "pointers": null, "use": "Stokes fifth-order regular-wave kinematics."}
```

```json
{"id": "H30", "key": "wheeler1970",
 "citation": "Wheeler, J.D. (1970). \"Method for calculating forces produced by irregular waves\". *Journal of Petroleum Technology* 22(3), 359–367.",
 "identifier": "DOI 10.2118/2712-PA [CR]", "identifier_status": "[CR]",
 "access": "no free access — https://doi.org/10.2118/2712-PA",
 "pointers": null, "use": "Wheeler stretching of kinematics above still water level."}
```

```json
{"id": "H31", "key": "forristall2000",
 "citation": "Forristall, G.Z. (2000). \"Wave crest distributions: observations and second-order theory\". *Journal of Physical Oceanography* 30(8), 1931–1943.",
 "identifier": "DOI 10.1175/1520-0485(2000)030<1931:WCDOAS>2.0.CO;2 [CR]", "identifier_status": "[CR]",
 "access": "no free access confirmed — https://doi.org/10.1175/1520-0485(2000)030%3C1931:WCDOAS%3E2.0.CO;2",
 "pointers": null, "use": "second-order crest-height distribution for air-gap and deck clearance."}
```

## E. Slender-member loading and vortex-induced vibration

```json
{"id": "H32", "key": "morison1950",
 "citation": "Morison, J.R., O'Brien, M.P., Johnson, J.W. & Schaaf, S.A. (1950). \"The force exerted by surface waves on piles\". *Journal of Petroleum Technology* 2(5), 149–154.",
 "identifier": "DOI 10.2118/950149-G [CR]", "identifier_status": "[CR]",
 "access": "no free access confirmed — https://doi.org/10.2118/950149-G",
 "pointers": null, "use": "the Morison equation."}
```

```json
{"id": "H33", "key": "keulegan1958",
 "citation": "Keulegan, G.H. & Carpenter, L.H. (1958). \"Forces on cylinders and plates in an oscillating fluid\". *Journal of Research of the National Bureau of Standards* 60(5), 423–440.",
 "identifier": "DOI 10.6028/jres.060.043 [CR]", "identifier_status": "[CR]",
 "access": "free (NIST) — https://nvlpubs.nist.gov/nistpubs/jres/60/jresv60n5p423_A1b.pdf (200)",
 "pointers": null, "use": "Keulegan–Carpenter number dependence of drag and inertia coefficients."}
```

```json
{"id": "H34", "key": "williamson2004",
 "citation": "Williamson, C.H.K. & Govardhan, R. (2004). \"Vortex-induced vibrations\". *Annual Review of Fluid Mechanics* 36, 413–455.",
 "identifier": "DOI 10.1146/annurev.fluid.36.050802.122128 [CR]", "identifier_status": "[CR]",
 "access": "no free access — https://doi.org/10.1146/annurev.fluid.36.050802.122128",
 "pointers": null, "use": "VIV response branches, mass-damping, wake modes."}
```

## F. Second-order and higher-order wave loads

```json
{"id": "H35", "key": "newman1967",
 "citation": "Newman, J.N. (1967). \"The drift force and moment on ships in waves\". *Journal of Ship Research* 11(1), 51–60.",
 "identifier": "DOI 10.5957/jsr.1967.11.1.51 [CR]", "identifier_status": "[CR]",
 "access": "no free access — https://doi.org/10.5957/jsr.1967.11.1.51",
 "pointers": null, "use": "far-field (momentum) mean drift force and yaw moment."}
```

```json
{"id": "H36", "key": "pinkster1979",
 "citation": "Pinkster, J.A. (1979). \"Mean and low frequency wave drifting forces on floating structures\". *Ocean Engineering* 6(6), 593–615.",
 "identifier": "DOI 10.1016/0029-8018(79)90010-6 [CR]", "identifier_status": "[CR]",
 "access": "no free access — https://doi.org/10.1016/0029-8018(79)90010-6",
 "pointers": null, "use": "near-field (pressure-integration) drift forces; basis of QTF computation. The companion 1980 Delft PhD thesis was not confirmed as openly downloadable."}
```

```json
{"id": "H37", "key": "molin1979",
 "citation": "Molin, B. (1979). \"Second-order diffraction loads upon three-dimensional bodies\". *Applied Ocean Research* 1(4), 197–202.",
 "identifier": "DOI 10.1016/0141-1187(79)90027-0 [CR]", "identifier_status": "[CR]",
 "access": "no free access — https://doi.org/10.1016/0141-1187(79)90027-0",
 "pointers": null, "use": "indirect method for second-order potential loads."}
```

```json
{"id": "H38", "key": "kim1989",
 "citation": "Kim, M.-H. & Yue, D.K.P. (1989). \"The complete second-order diffraction solution for an axisymmetric body. Part 1. Monochromatic incident waves\". *Journal of Fluid Mechanics* 200, 235–264.",
 "identifier": "DOI 10.1017/S0022112089000649 [CR] (Part 2, bichromatic waves and body motions: JFM 211, 557–593, DOI 10.1017/S0022112090001690 [CR])", "identifier_status": "[CR]",
 "access": "no free access — https://doi.org/10.1017/S0022112089000649",
 "pointers": null, "use": "sum- and difference-frequency loads (springing, slow drift)."}
```

```json
{"id": "H39", "key": "standing1987",
 "citation": "Standing, R.G., Brendling, W.J. & Wilson, D. (1987). \"Recent developments in the analysis of wave drift forces, low-frequency damping and response\". Offshore Technology Conference, paper OTC-5456-MS.",
 "identifier": "DOI 10.4043/5456-MS [CR]", "identifier_status": "[CR]",
 "access": "no free access — https://doi.org/10.4043/5456-MS",
 "pointers": null, "use": "wave-drift damping and slow-drift response in practice."}
```

```json
{"id": "H40", "key": "faltinsen1995",
 "citation": "Faltinsen, O.M., Newman, J.N. & Vinje, T. (1995). \"Nonlinear wave loads on a slender vertical cylinder\". *Journal of Fluid Mechanics* 289, 179–198.",
 "identifier": "DOI 10.1017/S0022112095001297 [CR]", "identifier_status": "[CR]",
 "access": "no free access — https://doi.org/10.1017/S0022112095001297",
 "pointers": null, "use": "third-order (FNV) loads behind ringing."}
```

## Topic-to-source cross-reference

| Topic | Primary | Supporting |
|---|---|---|
| Linear wave theory, dispersion, kinematics | H11, H10 | H1, H3, H20, H18 |
| Nonlinear regular waves, kinematic stretching | H29, H30 | H10, H9 |
| Wave spectra (PM, JONSWAP) | H26, H27 | H13, H14, H8 |
| Short-term wave and crest statistics, extremes | H28, H31 | H13, H16, H14 |
| Morison loading, drag/inertia coefficients | H32, H33 | H5, H6, H15, H7 |
| Diffraction/radiation, added mass and damping | H3, H23, H24 | H2, H9, H10, H17 |
| Strip theory and seakeeping | H21 | H4, H12, H2, H19 |
| Time-domain response (impulse response/retardation) | H22 | H1, H2 |
| Mean and slow-drift (difference-frequency) forces | H35, H36 | H37, H39, H2, H9 |
| Sum-frequency and higher-order loads (springing, ringing) | H38, H40 | H9, H2 |
| Vortex-induced vibration | H34 | H6, H15 |
| Stochastic response and extreme-value prediction | H16 | H13 |
| Model testing and physical modelling | H8, H9 | H7 |

See also (standards and recommended practices, editions being checked under a separate task): DNV-RP-C205, DNV-ST-N001, API RP 2FPS, ISO 19901-1, ISO 19901-7.

## Counts

- 40 entries: 32 with identifiers confirmed (`[CR]` 26, `[OL]` 6); 0 identifiers carried unverified; 8 with no identifier (H1, H17, H18, H19, H21, H22, H27, H28).
- Free-access items whose links resolved when checked: H1, H17, H18, H19, H20, H22, H28, H33 (8). Listed but not machine-checked: H3, H27.

## Gaps against the issue

- "DNV — Free Sea Loads training materials": no public, freely downloadable DNV training material on sea loads was located; it is left out rather than linked to an unofficial copy.
- "Marine Structures / Applied Ocean Research / Ocean Engineering (key reviews)": covered by individual papers in those and neighbouring journals (H36, H37) rather than a per-journal survey.
- Chapter pointers are given only where the publisher exposes chapter-level records (H6, H8, H9, H11). For H2, H3, H4, H5, H7 the chapter numbers still need to be read off the printed contents pages.
- Identifier checks used the Crossref registration record and Open Library catalogue records; publisher web pages for Cambridge and MIT Press blocked automated access.
- H21 (SNAME Transactions) and H27 (JONSWAP report) have no DOI that could be found; H27's repository copy could not be opened to confirm full text.
