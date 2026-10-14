# Mooring academic references

Academic and reference-text inventory for mooring design fundamentals (line mechanics, synthetic and wire rope, anchors, fatigue).

Source: research note on the issue, checked 2026-10-11.

Only bibliographic metadata is carried.

## How to read this list

- **40 entries**, grouped by sub-topic. Each has a BibTeX-style key, citation, identifier, access route and a one-line use note.
- **Identifier status** (checked 2026-10-11):
  - `[CR]` = DOI (and ISBN where shown) confirmed in the Crossref registration record for that DOI, i.e. the publisher-deposited metadata behind doi.org.
  - `[OL]` = ISBN confirmed in an Open Library catalogue record (Library of Congress control number noted where the record carries one).
  - `unverified` = an identifier was seen only on a bookseller, search-result or third-party page, not on a publisher or library-catalogue page that could be opened.
  - `no identifier` = none exists or none was found.
- **Access**: "free" links are publisher, author or institutional hosts only. "(200)" means the link resolved when checked; "(listed)" means it was returned by search but the host blocked an automated check. Everything else is "no free access" with the publisher/DOI landing page.
- Page ranges for book chapters come from the publisher's chapter-level DOI records.
- Bibliographic metadata only; no copyrighted text is reproduced.

## A. Textbooks, handbooks and guidance documents

```json
{"id": "M1", "key": "ma2019",
 "citation": "Ma, K.-T., Luo, Y., Kwan, T. & Wu, Y. (2019). *Mooring System Engineering for Offshore Structures*. Gulf Professional Publishing (Elsevier).",
 "identifier": "ISBN 978-0-12-818551-3 [CR]", "identifier_status": "[CR]",
 "access": "no free access — https://doi.org/10.1016/B978-0-12-818551-3.00004-1 (chapter record of the book)",
 "pointers": "mooring design pp. 63–83; mooring analysis pp. 85–114; fatigue analysis pp. 115–137; anchor selection pp. 155–174; hardware pp. 175–213; inspection and monitoring pp. 233–253; reliability pp. 255–279; integrity management pp. 281–297; floating wind pp. 299–315", "use": "current single-volume practitioner text for the whole mooring design cycle."}
```

```json
{"id": "M2", "key": "chakrabarti2005moor",
 "citation": "\"Mooring Systems\", in Chakrabarti, S.K. (ed.) (2005). *Handbook of Offshore Engineering*, pp. 663–708. Elsevier.",
 "identifier": "ISBN 978-0-08-044381-2 [CR]", "identifier_status": "[CR]",
 "access": "no free access — https://doi.org/10.1016/B978-0-08-044381-2.50020-5 (chapter record of the same book)",
 "pointers": null, "use": "catenary equations, line hardware, quasi-static and dynamic design procedure."}
```

```json
{"id": "M3", "key": "faltinsen1990",
 "citation": "Faltinsen, O.M. (1990). *Sea Loads on Ships and Offshore Structures*. Cambridge University Press.",
 "identifier": "ISBN 978-0-521-37285-5 (hardback) / 978-0-521-45870-2 (paperback, 1993) [OL]", "identifier_status": "[OL]",
 "access": "no free access — https://www.cambridge.org/9780521458702",
 "pointers": null, "use": "station-keeping chapter — catenary line statics, thruster and mooring forces, slow-drift excitation and damping."}
```

```json
{"id": "M4", "key": "barltrop1998",
 "citation": "Barltrop, N.D.P. (ed.) (1998). *Floating Structures: A Guide for Design and Analysis* (2 vols), CMPT publication 101/98. Oilfield Publications Ltd.",
 "identifier": "ISBN 978-1-870553-35-3 unverified (bookseller listing only)", "identifier_status": "unverified",
 "access": "no free access — Energy Institute catalogue https://www.energyinst.org/technical/publications/topics/offshore-safety/floating-structures-a-guide-for-design-and-analysis",
 "pointers": null, "use": "mooring analysis methods, line dynamics and design checks in a JIP-derived guide."}
```

```json
{"id": "M5", "key": "randolph2011",
 "citation": "Randolph, M. & Gourvenec, S. (2011). *Offshore Geotechnical Engineering*. Spon Press / CRC Press.",
 "identifier": "ISBN 978-0-415-47744-4 [OL, LCCN 2010016934]; e-book ISBN 978-1-315-27247-4, chapter DOI 10.1201/9781315272474-9 series [CR]", "identifier_status": "[OL, LCCN 2010016934]",
 "access": "no free access — https://www.routledge.com",
 "pointers": "offshore site investigation pp. 29–112; piled foundations pp. 145–235; anchoring systems pp. 308–360", "use": "anchor types, capacity methods and site-investigation context."}
```

```json
{"id": "M6", "key": "aubeny2017",
 "citation": "Aubeny, C. (2017). *Geomechanics of Marine Anchors*. CRC Press.",
 "identifier": "DOI 10.1201/9781351237376; ISBN 978-1-4987-2877-5 [CR]", "identifier_status": "[CR]",
 "access": "no free access — https://doi.org/10.1201/9781351237376",
 "pointers": "anchor line mechanics pp. 161–174; caisson and pile installation pp. 175–212; caisson and pile ultimate capacity pp. 213–242; drag embedded anchors pp. 281–332; direct embedment plate anchors pp. 333–366", "use": "plasticity-based capacity and trajectory models for every anchor family."}
```

```json
{"id": "M7", "key": "vryhof2015",
 "citation": "Vryhof Anchors B.V. (2015). *Vryhof Manual: The Guide to Anchoring*, 5th ed. (earlier editions 1984–2010 as *Anchor Manual*).",
 "identifier": "ISBN 978-90-9028801-7 unverified (seen only in a third-party copy)", "identifier_status": "unverified",
 "access": "no official free download was confirmed; copies found online are third-party uploads and are not linked. Request from the manufacturer",
 "pointers": null, "use": "drag-embedment anchor selection, holding-capacity charts, installation and proof-loading practice."}
```

```json
{"id": "M8", "key": "osig2022",
 "citation": "Society for Underwater Technology, OSIG Committee (2022). *Guidance Notes for the Planning and Execution of Geophysical and Geotechnical Ground Investigations for Offshore Renewable Energy Developments*, revised ed., 65 pp. SUT, London.",
 "identifier": "ISBN 978-0-906940-59-4 unverified (publisher page could not be opened by script)", "identifier_status": "unverified",
 "access": "free digital copy from the publisher — https://sut.org/wp-content/uploads/2022/10/OSIG-Guidance-Notes-September-2022.pdf (listed)",
 "pointers": null, "use": "scoping the site investigation that anchor design depends on."}
```

```json
{"id": "M9", "key": "thompson2012",
 "citation": "Thompson, D. & Beasley, D.J. (eds.) (2012). *Handbook for Marine Geotechnical Engineering*, SP-2209-OCN. Naval Facilities Engineering Command, Engineering Service Center, Port Hueneme.",
 "identifier": "no identifier (DTIC accession ADA556807)", "identifier_status": "no identifier",
 "access": "free, approved for public release — https://apps.dtic.mil/sti/pdfs/ADA556807.pdf (listed)",
 "pointers": null, "use": "US Navy (NCEL lineage) design methods for drag, pile, plate and deadweight anchors."}
```

```json
{"id": "M10", "key": "irvine1981",
 "citation": "Irvine, H.M. (1981). *Cable Structures*. MIT Press (Dover reprint 1992, ISBN 978-0-486-67127-7).",
 "identifier": "ISBN 978-0-262-09023-0 [OL]", "identifier_status": "[OL]",
 "access": "no free access — https://mitpress.mit.edu",
 "pointers": null, "use": "elastic catenary statics and linear cable dynamics."}
```

```json
{"id": "M11", "key": "feyrer2015",
 "citation": "Feyrer, K. (2015). *Wire Ropes: Tension, Endurance, Reliability*, 2nd ed. Springer.",
 "identifier": "DOI 10.1007/978-3-642-54996-0; ISBN 978-3-642-54995-3 [CR]", "identifier_status": "[CR]",
 "access": "no free access — https://doi.org/10.1007/978-3-642-54996-0",
 "pointers": null, "use": "wire rope stiffness, tension–tension and bending fatigue endurance formulae."}
```

```json
{"id": "M12", "key": "mckenna2004",
 "citation": "McKenna, H.A., Hearle, J.W.S. & O'Hear, N. (2004). *Handbook of Fibre Rope Technology*. Woodhead Publishing.",
 "identifier": "ISBN 978-1-85573-606-1 [CR]", "identifier_status": "[CR]",
 "access": "no free access — https://doi.org/10.1533/9781855739932.141 (chapter \"Rope mechanics\", pp. 141–184)",
 "pointers": null, "use": "fibre rope constructions, stiffness, creep and failure mechanisms."}
```

## B. Line statics and dynamics

```json
{"id": "M13", "key": "jonkman2007",
 "citation": "Jonkman, J.M. (2007). *Dynamics Modeling and Loads Analysis of an Offshore Floating Wind Turbine*, NREL/TP-500-41958. National Renewable Energy Laboratory.",
 "identifier": "DOI 10.2172/921803 [CR]", "identifier_status": "[CR]",
 "access": "free — https://www.osti.gov/biblio/921803 (200)",
 "pointers": null, "use": "closed-form quasi-static catenary equations with seabed contact, in implementable form."}
```

```json
{"id": "M14", "key": "mavrakos1996",
 "citation": "Mavrakos, S.A., Papazoglou, V.J., Triantafyllou, M.S. & Hatjigeorgiou, J. (1996). \"Deep water mooring dynamics\". *Marine Structures* 9(2), 181–209.",
 "identifier": "DOI 10.1016/0951-8339(94)00019-O [CR]", "identifier_status": "[CR]",
 "access": "no free access — https://doi.org/10.1016/0951-8339(94)00019-O",
 "pointers": null, "use": "dynamic tension amplification, buoys and clump weights."}
```

```json
{"id": "M15", "key": "webster1995",
 "citation": "Webster, W.C. (1995). \"Mooring-induced damping\". *Ocean Engineering* 22(6), 571–591.",
 "identifier": "DOI 10.1016/0029-8018(94)00027-5 [CR]", "identifier_status": "[CR]",
 "access": "no free access — https://doi.org/10.1016/0029-8018(94)00027-5",
 "pointers": null, "use": "line drag damping of slow-drift motion."}
```

```json
{"id": "M16", "key": "hall2015",
 "citation": "Hall, M. & Goupee, A. (2015). \"Validation of a lumped-mass mooring line model with DeepCwind semisubmersible model test data\". *Ocean Engineering* 104, 590–603.",
 "identifier": "DOI 10.1016/j.oceaneng.2015.05.035 [CR]", "identifier_status": "[CR]",
 "access": "no free access confirmed — https://doi.org/10.1016/j.oceaneng.2015.05.035",
 "pointers": null, "use": "lumped-mass dynamic line formulation and its validation."}
```

```json
{"id": "M17", "key": "davidson2017",
 "citation": "Davidson, J. & Ringwood, J.V. (2017). \"Mathematical modelling of mooring systems for wave energy converters — a review\". *Energies* 10(5), 666.",
 "identifier": "DOI 10.3390/en10050666 [CR]", "identifier_status": "[CR]",
 "access": "free (gold open access) — https://doi.org/10.3390/en10050666 (listed)",
 "pointers": null, "use": "survey of static, quasi-static and dynamic mooring models."}
```

## C. Synthetic (polyester) rope behaviour

```json
{"id": "M18", "key": "chaplin1992",
 "citation": "Chaplin, C.R. & Del Vecchio, C.J.M. (1992). \"Appraisal of lightweight moorings for deep water\". Offshore Technology Conference, OTC-6965-MS.",
 "identifier": "DOI 10.4043/6965-MS [CR]", "identifier_status": "[CR]",
 "access": "no free access — https://doi.org/10.4043/6965-MS",
 "pointers": null, "use": "early taut-leg polyester concept and stiffness characterisation."}
```

```json
{"id": "M19", "key": "davies2000",
 "citation": "Davies, P., Huard, G., Grosjean, F. & François, M. (2000). \"Creep and relaxation of polyester mooring lines\". Offshore Technology Conference, OTC-12176-MS.",
 "identifier": "DOI 10.4043/12176-MS [CR]", "identifier_status": "[CR]",
 "access": "no free access — https://doi.org/10.4043/12176-MS",
 "pointers": null, "use": "polyester creep and relaxation test data."}
```

```json
{"id": "M20", "key": "flory2004",
 "citation": "Flory, J.F., Banfield, S.J. & Petruska, D.J. (2004). \"Defining, measuring, and calculating the properties of fiber rope deepwater mooring lines\". Offshore Technology Conference, OTC-16151-MS.",
 "identifier": "DOI 10.4043/16151-MS [CR]", "identifier_status": "[CR]",
 "access": "no free access — https://doi.org/10.4043/16151-MS",
 "pointers": null, "use": "definitions of static, dynamic and quasi-static stiffness."}
```

```json
{"id": "M21", "key": "francois2008",
 "citation": "François, M. & Davies, P. (2008). \"Characterization of polyester mooring lines\". ASME OMAE2008, Vol. 1, pp. 169–177.",
 "identifier": "DOI 10.1115/OMAE2008-57136 [CR]", "identifier_status": "[CR]",
 "access": "no free access — https://doi.org/10.1115/OMAE2008-57136",
 "pointers": null, "use": "quasi-static/dynamic stiffness model used in class guidance."}
```

```json
{"id": "M22", "key": "huang2013",
 "citation": "Huang, W., Liu, H., Lian, Y. & Li, L. (2013). \"Modeling nonlinear creep and recovery behaviors of synthetic fiber ropes for deepwater moorings\". *Applied Ocean Research* 39, 113–120.",
 "identifier": "DOI 10.1016/j.apor.2012.10.004 [CR]", "identifier_status": "[CR]",
 "access": "no free access confirmed — https://doi.org/10.1016/j.apor.2012.10.004",
 "pointers": null, "use": "viscoelastic–viscoplastic creep model for polyester and HMPE."}
```

## D. Wire rope endurance

```json
{"id": "M23", "key": "chaplin1991",
 "citation": "Chaplin, C.R. & Potts, A.E. (1991). *Wire Rope Offshore — A Critical Review of Wire Rope Endurance Research Affecting Offshore Applications*, Offshore Technology Report OTH 91 341. HMSO for the Health and Safety Executive, 335 pp.",
 "identifier": "no identifier", "identifier_status": "no identifier",
 "access": "no free copy confirmed — catalogue record https://trid.trb.org/View/443616",
 "pointers": null, "use": "tension–tension and bending-over-sheave fatigue data behind wire rope design curves."}
```

## E. Anchors and embedded chain

```json
{"id": "M24", "key": "vivatrat1982",
 "citation": "Vivatrat, V., Valent, P.J. & Ponterio, A.A. (1982). \"The influence of chain friction on anchor pile design\". Offshore Technology Conference, OTC-4178-MS.",
 "identifier": "DOI 10.4043/4178-MS [CR]", "identifier_status": "[CR]",
 "access": "no free access — https://doi.org/10.4043/4178-MS",
 "pointers": null, "use": "inverse-catenary profile and load attenuation of embedded chain."}
```

```json
{"id": "M25", "key": "neubecker1995",
 "citation": "Neubecker, S.R. & Randolph, M.F. (1995). \"Profile and frictional capacity of embedded anchor chains\". *Journal of Geotechnical Engineering* 121(11), 797–803.",
 "identifier": "DOI 10.1061/(ASCE)0733-9410(1995)121:11(797) [CR]", "identifier_status": "[CR]",
 "access": "no free access — https://doi.org/10.1061/(ASCE)0733-9410(1995)121:11(797)",
 "pointers": null, "use": "closed-form embedded chain solution used in most anchor design tools."}
```

```json
{"id": "M26", "key": "andersen1999",
 "citation": "Andersen, K.H. & Jostad, H.P. (1999). \"Foundation design of skirted foundations and anchors in clay\". Offshore Technology Conference, OTC-10824-MS.",
 "identifier": "DOI 10.4043/10824-MS [CR]", "identifier_status": "[CR]",
 "access": "no free access — https://doi.org/10.4043/10824-MS",
 "pointers": null, "use": "suction anchor capacity, cyclic strength, set-up."}
```

```json
{"id": "M27", "key": "andersen2005",
 "citation": "Andersen, K.H., Murff, J.D., Randolph, M.F., et al. (2005). \"Suction anchors for deepwater applications\". In *Frontiers in Offshore Geotechnics* (ISFOG 2005), Taylor & Francis.",
 "identifier": "DOI 10.1201/NOE0415390637.ch1; ISBN 978-0-415-39063-7 [CR]", "identifier_status": "[CR]",
 "access": "no free access — https://doi.org/10.1201/NOE0415390637.ch1",
 "pointers": null, "use": "industry benchmark of installation and capacity prediction methods."}
```

```json
{"id": "M28", "key": "aubeny2003",
 "citation": "Aubeny, C.P., Han, S.W. & Murff, J.D. (2003). \"Inclined load capacity of suction caissons\". *International Journal for Numerical and Analytical Methods in Geomechanics* 27(14), 1235–1254.",
 "identifier": "DOI 10.1002/nag.319 [CR]", "identifier_status": "[CR]",
 "access": "no free access — https://doi.org/10.1002/nag.319",
 "pointers": null, "use": "upper-bound model for inclined loading and optimal padeye depth."}
```

```json
{"id": "M29", "key": "randolph2002",
 "citation": "Randolph, M.F. & House, A.R. (2002). \"Analysis of suction caisson capacity in clay\". Offshore Technology Conference, OTC-14236-MS.",
 "identifier": "DOI 10.4043/14236-MS [CR]", "identifier_status": "[CR]",
 "access": "no free access — https://doi.org/10.4043/14236-MS",
 "pointers": null, "use": "failure mechanisms and interaction of vertical and horizontal capacity."}
```

```json
{"id": "M30", "key": "murff2005",
 "citation": "Murff, J.D., Randolph, M.F., Elkhatib, S., et al. (2005). \"Vertically loaded plate anchors for deepwater applications\". In *Frontiers in Offshore Geotechnics* (ISFOG 2005), Taylor & Francis.",
 "identifier": "DOI 10.1201/NOE0415390637.ch2; ISBN 978-0-415-39063-7 [CR]", "identifier_status": "[CR]",
 "access": "no free access — https://doi.org/10.1201/NOE0415390637.ch2",
 "pointers": null, "use": "VLA/plate anchor capacity and installation prediction."}
```

```json
{"id": "M31", "key": "aubeny2010",
 "citation": "Aubeny, C.P. & Chi, C. (2010). \"Mechanics of drag embedment anchors in a soft seabed\". *Journal of Geotechnical and Geoenvironmental Engineering* 136(1), 57–68.",
 "identifier": "DOI 10.1061/(ASCE)GT.1943-5606.0000198 [CR]", "identifier_status": "[CR]",
 "access": "no free access — https://doi.org/10.1061/(ASCE)GT.1943-5606.0000198",
 "pointers": null, "use": "drag anchor trajectory and capacity prediction."}
```

```json
{"id": "M32", "key": "medeiros2002",
 "citation": "Medeiros, C.J. (2002). \"Low cost anchor system for flexible risers in deep waters\". Offshore Technology Conference, OTC-14151-MS.",
 "identifier": "DOI 10.4043/14151-MS [CR]", "identifier_status": "[CR]",
 "access": "no free access — https://doi.org/10.4043/14151-MS",
 "pointers": null, "use": "torpedo (dynamically installed) pile concept and field experience."}
```

```json
{"id": "M33", "key": "bea1992",
 "citation": "Bea, R.G. (1992). \"Pile capacity for axial cyclic loading\". *Journal of Geotechnical Engineering* 118(1), 34–50.",
 "identifier": "DOI 10.1061/(ASCE)0733-9410(1992)118:1(34) [CR]", "identifier_status": "[CR]",
 "access": "no free access — https://doi.org/10.1061/(ASCE)0733-9410(1992)118:1(34)",
 "pointers": null, "use": "cyclic degradation and rate effects for anchor piles."}
```

## F. Fatigue: cycle counting, chain T-N data, out-of-plane bending

```json
{"id": "M34", "key": "matsuishi1968",
 "citation": "Matsuishi, M. & Endo, T. (1968). \"Fatigue of metals subjected to varying stress\". Presented to the Japan Society of Mechanical Engineers, Fukuoka, March 1968.",
 "identifier": "no identifier (conference presentation)", "identifier_status": "no identifier",
 "access": "no free access",
 "pointers": null, "use": "origin of rainflow counting."}
```

```json
{"id": "M35", "key": "downing1982",
 "citation": "Downing, S.D. & Socie, D.F. (1982). \"Simple rainflow counting algorithms\". *International Journal of Fatigue* 4(1), 31–40.",
 "identifier": "DOI 10.1016/0142-1123(82)90018-4 [CR]", "identifier_status": "[CR]",
 "access": "no free access — https://doi.org/10.1016/0142-1123(82)90018-4",
 "pointers": null, "use": "the rainflow algorithm normally implemented in code."}
```

```json
{"id": "M36", "key": "dirlik1985",
 "citation": "Dirlik, T. (1985). *Application of Computers in Fatigue Analysis*. PhD thesis, University of Warwick.",
 "identifier": "no identifier (repository record 2949)", "identifier_status": "no identifier",
 "access": "free — https://wrap.warwick.ac.uk/id/eprint/2949/ (200)",
 "pointers": null, "use": "spectral (frequency-domain) rainflow range distribution."}
```

```json
{"id": "M37", "key": "stiff1996",
 "citation": "Stiff, J.J., Smith, D.W. & Casey, N.F. (1996). \"Fatigue of mooring chain in air and water — results and analysis\". Offshore Technology Conference, OTC-8147-MS.",
 "identifier": "DOI 10.4043/8147-MS [CR]", "identifier_status": "[CR]",
 "access": "no free access — https://doi.org/10.4043/8147-MS",
 "pointers": null, "use": "test basis of tension–tension (T-N) design curves for chain."}
```

```json
{"id": "M38", "key": "fernandez2014",
 "citation": "Fernández, J., Storesund, W. & Navas, J. (2014). \"Fatigue performance of grade R4 and R5 mooring chains in seawater\". ASME OMAE2014, Vol. 1A.",
 "identifier": "DOI 10.1115/OMAE2014-23491 [CR]", "identifier_status": "[CR]",
 "access": "no free access — https://doi.org/10.1115/OMAE2014-23491",
 "pointers": null, "use": "recent full-scale fatigue data for high-grade chain."}
```

```json
{"id": "M39", "key": "jean2005",
 "citation": "Jean, P., Goessens, K. & L'Hostis, D. (2005). \"Failure of chains by bending on deepwater mooring systems\". Offshore Technology Conference, OTC-17238-MS.",
 "identifier": "DOI 10.4043/17238-MS [CR]", "identifier_status": "[CR]",
 "access": "no free access — https://doi.org/10.4043/17238-MS",
 "pointers": null, "use": "first description of out-of-plane bending failure at fairleads/chain hawse."}
```

```json
{"id": "M40", "key": "rampi2015",
 "citation": "Rampi, L., Dewi, F. & Vargas, P. (2015). \"Chain out of plane bending (OPB) joint industry project (JIP) summary and main results\". Offshore Technology Conference, OTC-25779-MS.",
 "identifier": "DOI 10.4043/25779-MS [CR]", "identifier_status": "[CR]",
 "access": "no free access — https://doi.org/10.4043/25779-MS",
 "pointers": null, "use": "OPB interlink stiffness, stress model and fatigue design method."}
```

## Topic-to-source cross-reference

| Topic | Primary | Supporting |
|---|---|---|
| Catenary equations, quasi-static line analysis | M13, M2 | M3, M10, M1 |
| Line dynamics and mooring-induced damping | M14, M15, M16 | M17, M4, M1 |
| Polyester stiffness, creep and relaxation | M19, M20, M21, M22 | M18, M12 |
| Wire rope stiffness and fatigue | M23, M11 | M1 |
| Site investigation for anchors | M8, M5 | M9 |
| Embedded chain (inverse catenary) | M24, M25 | M6 |
| Drag-embedment anchors | M7, M31 | M9, M6, M5 |
| Suction caissons / anchor piles | M26, M27, M28, M29 | M6, M33 |
| Plate anchors (VLA and similar) | M30 | M6 |
| Torpedo / dynamically installed anchors | M32 | M6, M5 |
| Chain tension–tension fatigue (T-N) | M37, M38 | M1 |
| Out-of-plane bending fatigue | M39, M40 | M1 |
| Rainflow and spectral cycle counting | M34, M35, M36 | M1 |
| Reliability and integrity management | M1 | M4 |

See also (standards and recommended practices, editions being checked under a separate task): API RP 2SK, API RP 2SM, API RP 2I, DNV-OS-E301, DNV-OS-E302, DNV-OS-E303, DNV-RP-E301, DNV-RP-E302, DNV-RP-E303, DNV-RP-C203, ISO 19901-7, ABS guidance on fibre ropes for offshore mooring, BV NI 604 (chain OPB), ASTM E1049 (cycle counting).

## Counts

- 40 entries: 33 with identifiers confirmed (`[CR]` 30, `[OL]` 3); 3 carried as `unverified` (M4, M7, M8); 4 with no identifier (M9, M23, M34, M36).
- Free-access items whose links resolved when checked: M13, M36. Listed but blocked to automated checks: M8, M9, M17.

## Gaps against the issue

- "Vryhof Anchor Manual (free PDF)": no manufacturer-hosted free download was confirmed; only third-party copies were found, so no link is given and the ISBN stays unverified.
- "DNV — Mooring training materials": no public DNV training material was located.
- "API RP 2SK Annex C (technical basis) papers": no discrete public paper identified as that annex's technical basis was found; M37 (chain T-N tests), M20/M21 (rope stiffness) and M25 (embedded chain) are the closest underlying sources.
- "Bea — anchor pile mechanics papers": one paper confirmed (M33); a wider Bea bibliography was not compiled.
- "Drag anchor performance (Vivatrat, NCEL studies)": the NCEL lineage is covered through the public NAVFAC handbook (M9); the original NCEL drag-anchor design guide appears to be a limited-distribution report and is not listed.
- "Wire rope fatigue S-N curves": M23 has no identifier and no confirmed free copy; M11 is the verified alternative.
- A UK HSE research report on FPS mooring integrity (RR444) was considered, but its page returned "not found" on the day, so it is not listed.
- Author order for the two ISFOG 2005 keynote chapters (M27, M30) follows the usual citation; the Crossref record lists the same authors in a different order.
