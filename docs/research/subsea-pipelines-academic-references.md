# Subsea pipelines academic references

Academic reference list for subsea pipeline engineering (design, installation, on-bottom stability, free spans, buckling, pipe-soil interaction).

Source: research note on the issue, checked 2026-10-11.

- **40 entries**, grouped by sub-topic. Each has a BibTeX-style key, citation, identifier, access route and a one-line use note.
- **Identifier status** (checked 2026-10-11):
  - `[CR]` = DOI (and ISBN where shown) confirmed in the Crossref registration record for that DOI, i.e. the publisher-deposited metadata behind doi.org.
  - `[OL]` = ISBN confirmed in an Open Library catalogue record (Library of Congress control number noted where the record carries one).
  - `no identifier` = none exists or none was found; nothing was written from memory.
- **Access**: "free" links are publisher, author or institutional hosts only. "(200)" means the link resolved when checked. Everything else is "no free access" with the publisher/DOI landing page. LinkedIn and similar posts are not used as sources.
- Page ranges for book chapters come from the publisher's chapter-level DOI records and serve as the chapter pointers the issue asks for.
- Bibliographic metadata only; no copyrighted text is reproduced.


Each entry is a YAML record. `identifier` carries the literal DOI/ISBN values; `identifier_status` carries the note’s marks in occurrence order, including repeated marks and catalogue numbers. Counts classify each entry by its first mark. `citation` preserves the full bibliographic statement, including edition and identifier qualifications. `pointers` is null where no separate Pointers statement is supplied.

## A. Textbooks and monographs

### P1 `bai2005`

```yaml
id: P1
key: bai2005
citation: "Bai, Y. & Bai, Q. (2005). *Subsea Pipelines and Risers*. Elsevier. ISBN 978-0-08-044566-3 `[CR]`."
identifier: ["978-0-08-044566-3"]
identifier_status: ["[CR]"]
access: "no free access — https://doi.org/10.1016/B978-008044566-3.50032-4 (chapter record of the book)."
pointers: "wall thickness and material grade pp. 25–40; buckling/collapse pp. 41–66; limit-state strength design pp. 67–80; soil and pipe interaction pp. 83–88; hydrodynamics around pipes pp. 89–100; expansion, upheaval/lateral buckling pp. 115–128; on-bottom stability pp. 129–136; VIV and fatigue pp. 137–154; installation design pp. 597–636."
use: "broad design reference with limit-state emphasis."
```

### P2 `bai2014`

```yaml
id: P2
key: bai2014
citation: "Bai, Q. & Bai, Y. (2014). *Subsea Pipeline Design, Analysis, and Installation*. Gulf Professional Publishing (Elsevier). ISBN 978-0-12-386888-6 `[CR]`."
identifier: ["978-0-12-386888-6"]
identifier_status: ["[CR]"]
access: "no free access — https://doi.org/10.1016/B978-0-12-386888-6.00033-X (chapter record)."
pointers: "wall thickness pp. 23–39; buckling and collapse pp. 41–65; soil and pipe interaction pp. 121–151; thermal expansion design pp. 187–220; lateral buckling and pipeline walking pp. 221–253; upheaval buckling pp. 255–281; on-bottom stability pp. 319–335; spans and VIV fatigue pp. 337–363; installation design pp. 707–751; commissioning, operations and maintenance pp. 753–766."
use: "the updated and expanded successor to P1."
```

### P3 `palmer2008`

```yaml
id: P3
key: palmer2008
citation: "Palmer, A.C. & King, R.A. (2008). *Subsea Pipeline Engineering*, 2nd ed. PennWell. ISBN 978-1-59370-133-8 `[OL, LCCN 2007050317]` (1st ed. 2004, ISBN 978-1-59370-013-3 `[OL]`)."
identifier: ["978-1-59370-133-8", "978-1-59370-013-3"]
identifier_status: ["[OL, LCCN 2007050317]", "[OL]"]
access: "no free access — https://www.pennwellbooks.com."
pointers: null
use: "first-principles treatment of route selection, materials, hydraulics, strength, stability, installation, buckling and integrity."
```

### P4 `kyriakides2007`

```yaml
id: P4
key: kyriakides2007
citation: "Kyriakides, S. & Corona, E. (2007; 2nd ed. 2023). *Mechanics of Offshore Pipelines, Volume 1: Buckling and Collapse*. Elsevier. ISBN 978-0-08-046732-0 (1st ed.) / 978-0-12-816859-2 (2nd ed.) `[CR]`."
identifier: ["978-0-08-046732-0", "978-0-12-816859-2"]
identifier_status: ["[CR]"]
access: "no free access — https://doi.org/10.1016/B978-0-12-816859-2.00005-9 (chapter record)."
pointers: "(2nd ed.): installation methods pp. 21–74; collapse under external pressure pp. 115–164; UOE pipe collapse pp. 165–212; pure bending pp. 269–302; bending plus external pressure pp. 303–344; reeling pp. 369–411; axial compression pp. 511–557."
use: "definitive mechanics of collapse and local buckling."
```

### P5 `kyriakides2021`

```yaml
id: P5
key: kyriakides2021
citation: "Kyriakides, S. & Lee, L.-H. (2021). *Mechanics of Offshore Pipelines, Volume 2: Buckle Propagation and Arrest*. Gulf Professional Publishing (Elsevier). ISBN 978-0-12-817014-4 `[CR]`."
identifier: ["978-0-12-817014-4"]
identifier_status: ["[CR]"]
access: "no free access — https://doi.org/10.1016/B978-0-12-817014-4.00004-5 (chapter record)."
pointers: "propagation pressure pp. 37–77; pipe-in-pipe pp. 135–184; integral arrestors pp. 229–266; slip-on and clamped arrestors pp. 303–343."
use: "propagation pressure and buckle arrestor design."
```

### P6 `guo2014`

```yaml
id: P6
key: guo2014
citation: "Guo, B., Song, S., Ghalambor, A. & Lin, T.R. (2014). *Offshore Pipelines: Design, Installation, and Maintenance*, 2nd ed. Gulf Professional Publishing (Elsevier). ISBN 978-0-12-397949-0 `[CR]`."
identifier: ["978-0-12-397949-0"]
identifier_status: ["[CR]"]
access: "no free access — https://doi.org/10.1016/B978-0-12-397949-0.00011-X (chapter record)."
pointers: "diameter and wall thickness pp. 21–27; hydrodynamic stability pp. 29–51; pipeline span pp. 53–63; operating stresses pp. 65–78; installation methods pp. 135–146; installation bending stress control pp. 147–153; on-bottom stability control pp. 155–165; testing and precommissioning pp. 169–177; pigging pp. 233–253."
use: "compact worked-example text; the best book source here for pre-commissioning."
```

### P7 `braestrup2005`

```yaml
id: P7
key: braestrup2005
citation: "Braestrup, M.W. (ed.), Andersen, J.B., Andersen, L.W., Bryndum, M.B., Christensen, C.J. & Nielsen, N.-J.R. (2005). *Design and Installation of Marine Pipelines*. Blackwell Science. ISBN 978-0-632-05984-3 `[OL]`."
identifier: ["978-0-632-05984-3"]
identifier_status: ["[OL]"]
access: "no free access — https://www.wiley.com."
pointers: null
use: "project-sequence view from design basis through construction and commissioning."
```

### P8 `mousselli1981`

```yaml
id: P8
key: mousselli1981
citation: "Mousselli, A.H. (1981). *Offshore Pipeline Design, Analysis, and Methods*. PennWell Books. ISBN 978-0-87814-156-2 `[OL, LCCN 80029039]`."
identifier: ["978-0-87814-156-2"]
identifier_status: ["[OL, LCCN 80029039]"]
access: "no free access (out of print)."
pointers: null
use: "foundational text for stability, installation stress analysis and span assessment."
```

### P9 `bai2019`

```yaml
id: P9
key: bai2019
citation: "Bai, Y. & Bai, Q. (2019). *Subsea Engineering Handbook*, 2nd ed. Gulf Professional Publishing (Elsevier). ISBN 978-0-12-812622-6 `[CR]` (1st ed. 2010, ISBN 978-1-85617-689-7 `[CR]`)."
identifier: ["978-0-12-812622-6", "978-1-85617-689-7"]
identifier_status: ["[CR]", "[CR]"]
access: "no free access — https://doi.org/10.1016/B978-0-12-812622-6.00012-9 (chapter record)."
pointers: "hydraulics pp. 315–361; heat transfer and insulation pp. 363–408; pipeline ends and in-line structures pp. 613–638; subsea pipelines pp. 919–940."
use: "flow assurance and subsea-system context for pipeline design."
```

### P10 `bai2016`

```yaml
id: P10
key: bai2016
citation: "Bai, Y. & Jin, W.-L. (2016). *Marine Structural Design*, 2nd ed. Butterworth-Heinemann (Elsevier). ISBN 978-0-08-099997-5 `[CR]`."
identifier: ["978-0-08-099997-5"]
identifier_status: ["[CR]"]
access: "no free access — https://doi.org/10.1016/B978-0-08-099997-5.00011-3 (chapter record)."
pointers: null
use: "limit-state, fatigue and reliability methods shared with pipeline design."
```

### P11 `sumer2006`

```yaml
id: P11
key: sumer2006
citation: "Sumer, B.M. & Fredsøe, J. (2006). *Hydrodynamics Around Cylindrical Structures*, revised ed. World Scientific. DOI 10.1142/6248; ISBN 978-981-270-039-1 `[CR]`."
identifier: ["10.1142/6248", "978-981-270-039-1"]
identifier_status: ["[CR]"]
access: "no free access — https://doi.org/10.1142/6248."
pointers: null
use: "forces on near-bed cylinders and VIV behind stability and free-span checks."
```

### P12 `randolph2011pipe`

```yaml
id: P12
key: randolph2011pipe
citation: "\"Pipeline and riser geotechnics\", in Randolph, M. & Gourvenec, S. (2011). *Offshore Geotechnical Engineering*, pp. 404–443. Spon Press / CRC Press. ISBN 978-0-415-47744-4 `[OL, LCCN 2010016934]`; chapter DOI 10.1201/9781315272474-9 `[CR]`."
identifier: ["978-0-415-47744-4", "10.1201/9781315272474-9"]
identifier_status: ["[OL, LCCN 2010016934]", "[CR]"]
access: "no free access — https://doi.org/10.1201/9781315272474-9."
pointers: null
use: "embedment, axial and lateral resistance models."
```

### P13 `nogueira2005`

```yaml
id: P13
key: nogueira2005
citation: "Nogueira, A.C. & McKeehan, D.S. (2005). \"Design and construction of offshore pipelines\", in Chakrabarti, S.K. (ed.), *Handbook of Offshore Engineering*, pp. 891–937. Elsevier. DOI 10.1016/B978-0-08-044381-2.50018-7; ISBN 978-0-08-044381-2 `[CR]`."
identifier: ["10.1016/B978-0-08-044381-2.50018-7", "978-0-08-044381-2"]
identifier_status: ["[CR]"]
access: "no free access — https://doi.org/10.1016/B978-0-08-044381-2.50018-7."
pointers: null
use: "single-chapter overview of design and lay methods."
```

## B. Wall thickness, collapse, local buckling and limit states

### P14 `palmer1975`

```yaml
id: P14
key: palmer1975
citation: "Palmer, A.C. & Martin, J.H. (1975). \"Buckle propagation in submarine pipelines\". *Nature* 254, 46–48. DOI 10.1038/254046a0 `[CR]`."
identifier: ["10.1038/254046a0"]
identifier_status: ["[CR]"]
access: "no free access — https://doi.org/10.1038/254046a0."
pointers: null
use: "first propagation-pressure model."
```

### P15 `yeh1986`

```yaml
id: P15
key: yeh1986
citation: "Yeh, M.K. & Kyriakides, S. (1986). \"On the collapse of inelastic thick-walled tubes under external pressure\". *Journal of Energy Resources Technology* 108(1), 35–47. DOI 10.1115/1.3231239 `[CR]`."
identifier: ["10.1115/1.3231239"]
identifier_status: ["[CR]"]
access: "no free access — https://doi.org/10.1115/1.3231239."
pointers: null
use: "collapse pressure with ovality, residual stress and material nonlinearity."
```

### P16 `hauch2000`

```yaml
id: P16
key: hauch2000
citation: "Hauch, S.R. & Bai, Y. (2000). \"Bending moment capacity of pipes\". *Journal of Offshore Mechanics and Arctic Engineering* 122(4), 243–252. DOI 10.1115/1.1314866 `[CR]`."
identifier: ["10.1115/1.1314866"]
identifier_status: ["[CR]"]
access: "no free access — https://doi.org/10.1115/1.1314866."
pointers: null
use: "combined bending, pressure and axial force capacity."
```

### P17 `gresnigt1986`

```yaml
id: P17
key: gresnigt1986
citation: "Gresnigt, A.M. (1986). \"Plastic design of buried steel pipelines in settlement areas\". *HERON* 31(4). no identifier (no DOI)."
identifier: ["no identifier"]
identifier_status: ["no identifier"]
access: "free journal archive — https://heronjournal.nl/31-4/1.pdf (200; the issue index page could not be read by script, so confirm the title on opening)."
pointers: null
use: "strain-based bending and ovalisation limits adopted in later codes."
```

### P18 `sotberg1996`

```yaml
id: P18
key: sotberg1996
citation: "Sotberg, T., Bruschi, R. & Mørk, K. (1996). \"The SUPERB project: reliability-based design guideline for submarine pipelines\". Offshore Technology Conference, OTC-8220-MS. DOI 10.4043/8220-MS `[CR]`."
identifier: ["10.4043/8220-MS"]
identifier_status: ["[CR]"]
access: "no free access — https://doi.org/10.4043/8220-MS."
pointers: null
use: "calibration basis of limit-state (LRFD) pipeline design."
```

## C. Thermal expansion, lateral and upheaval buckling, walking

### P19 `hobbs1984`

```yaml
id: P19
key: hobbs1984
citation: "Hobbs, R.E. (1984). \"In-service buckling of heated pipelines\". *Journal of Transportation Engineering* 110(2), 175–189. DOI 10.1061/(ASCE)0733-947X(1984)110:2(175) `[CR]`."
identifier: ["10.1061/(ASCE)0733-947X(1984)110:2(175)"]
identifier_status: ["[CR]"]
access: "no free access — https://doi.org/10.1061/(ASCE)0733-947X(1984)110:2(175)."
pointers: null
use: "analytical lateral and vertical buckling modes of a perfect pipe."
```

### P20 `taylor1986`

```yaml
id: P20
key: taylor1986
citation: "Taylor, N. & Gan, A.B. (1986). \"Submarine pipeline buckling — imperfection studies\". *Thin-Walled Structures* 4(4), 295–323. DOI 10.1016/0263-8231(86)90035-2 `[CR]`."
identifier: ["10.1016/0263-8231(86)90035-2"]
identifier_status: ["[CR]"]
access: "no free access — https://doi.org/10.1016/0263-8231(86)90035-2."
pointers: null
use: "effect of initial imperfections on buckling temperature."
```

### P21 `palmer1990`

```yaml
id: P21
key: palmer1990
citation: "Palmer, A.C., Ellinas, C.P., Richards, D.M. & Guijt, J. (1990). \"Design of submarine pipelines against upheaval buckling\". Offshore Technology Conference, OTC-6335-MS. DOI 10.4043/6335-MS `[CR]`."
identifier: ["10.4043/6335-MS"]
identifier_status: ["[CR]"]
access: "no free access — https://doi.org/10.4043/6335-MS."
pointers: null
use: "semi-empirical download requirement for buried lines."
```

### P22 `pedersen1988`

```yaml
id: P22
key: pedersen1988
citation: "Pedersen, P.T. & Jensen, J.J. (1988). \"Upheaval creep of buried heated pipelines with initial imperfections\". *Marine Structures* 1(1), 11–22. DOI 10.1016/0951-8339(88)90008-1 `[CR]`."
identifier: ["10.1016/0951-8339(88)90008-1"]
identifier_status: ["[CR]"]
access: "no free access — https://doi.org/10.1016/0951-8339(88)90008-1."
pointers: null
use: "progressive upheaval under thermal cycling."
```

### P23 `bruton2006`

```yaml
id: P23
key: bruton2006
citation: "Bruton, D., White, D., Cheuk, C., Bolton, M. & Carr, M. (2006). \"Pipe/soil interaction behavior during lateral buckling, including large-amplitude cyclic displacement tests by the Safebuck JIP\". Offshore Technology Conference, OTC-17944-MS. DOI 10.4043/17944-MS `[CR]`."
identifier: ["10.4043/17944-MS"]
identifier_status: ["[CR]"]
access: "no free access — https://doi.org/10.4043/17944-MS."
pointers: null
use: "breakout, residual and berm resistance for lateral buckling design."
```

### P24 `bruton2008`

```yaml
id: P24
key: bruton2008
citation: "Bruton, D., Bolton, M., Carr, M. & White, D. (2008). \"Pipe-soil interaction with flowlines during lateral buckling and pipeline walking — the SAFEBUCK JIP\". Offshore Technology Conference, OTC-19589-MS. DOI 10.4043/19589-MS `[CR]`."
identifier: ["10.4043/19589-MS"]
identifier_status: ["[CR]"]
access: "no free access — https://doi.org/10.4043/19589-MS."
pointers: null
use: "axial and lateral friction ranges for buckling and walking assessments."
```

### P25 `carr2008`

```yaml
id: P25
key: carr2008
citation: "Carr, M., Sinclair, F. & Bruton, D. (2008). \"Pipeline walking — understanding the field layout challenges and analytical solutions developed for the Safebuck JIP\". *SPE Projects, Facilities & Construction* 3(3), 1–9. DOI 10.2118/120022-PA `[CR]` (conference version OTC-17945-MS, DOI 10.4043/17945-MS `[CR]`)."
identifier: ["10.2118/120022-PA", "10.4043/17945-MS"]
identifier_status: ["[CR]", "[CR]"]
access: "no free access — https://doi.org/10.2118/120022-PA."
pointers: null
use: "closed-form walking rates for thermal transients, seabed slope and riser tension."
```

### P26 `fyrileiv2005`

```yaml
id: P26
key: fyrileiv2005
citation: "Fyrileiv, O. & Collberg, L. (2005). \"Influence of pressure in pipeline design: effective axial force\". ASME OMAE2005, Vol. 3, pp. 629–636. DOI 10.1115/OMAE2005-67502 `[CR]`."
identifier: ["10.1115/OMAE2005-67502"]
identifier_status: ["[CR]"]
access: "no free access — https://doi.org/10.1115/OMAE2005-67502."
pointers: null
use: "the effective axial force concept that drives global buckling and span checks."
```

### P27 `wang2018`

```yaml
id: P27
key: wang2018
citation: "Wang, Z., van der Heijden, G.H.M. & Tang, Y. (2018). \"Localised upheaval buckling of buried subsea pipelines\". *Marine Structures* 60, 165–185. DOI 10.1016/j.marstruc.2018.03.010 `[CR]`."
identifier: ["10.1016/j.marstruc.2018.03.010"]
identifier_status: ["[CR]"]
access: "free (institutional repository copy) — https://discovery.ucl.ac.uk/10046235/1/paper.pdf (200)."
pointers: null
use: "recent analytical treatment with a usable literature review."
```

## D. Pipe-soil interaction

### P28 `wagner1987`

```yaml
id: P28
key: wagner1987
citation: "Wagner, D.A., Murff, J.D., Brennodden, H. & Sveggen, O. (1987). \"Pipe-soil interaction model\". Offshore Technology Conference, OTC-5504-MS. DOI 10.4043/5504-MS `[CR]` (journal version in *J. Waterway, Port, Coastal, and Ocean Engineering*, 1989; identifier not checked)."
identifier: ["10.4043/5504-MS"]
identifier_status: ["[CR]"]
access: "no free access — https://doi.org/10.4043/5504-MS."
pointers: null
use: "lateral resistance as friction plus penetration-dependent passive term."
```

### P29 `verley1994`

```yaml
id: P29
key: verley1994
citation: "Verley, R. & Sotberg, T. (1994). \"A soil resistance model for pipelines placed on sandy soils\". *Journal of Offshore Mechanics and Arctic Engineering* 116(3), 145–153. DOI 10.1115/1.2920143 `[CR]`."
identifier: ["10.1115/1.2920143"]
identifier_status: ["[CR]"]
access: "no free access — https://doi.org/10.1115/1.2920143."
pointers: null
use: "sand resistance model used in dynamic stability analysis."
```

### P30 `white2008`

```yaml
id: P30
key: white2008
citation: "White, D.J. & Cheuk, C.Y. (2008). \"Modelling the soil resistance on seabed pipelines during large cycles of lateral movement\". *Marine Structures* 21(1), 59–79. DOI 10.1016/j.marstruc.2007.05.001 `[CR]`."
identifier: ["10.1016/j.marstruc.2007.05.001"]
identifier_status: ["[CR]"]
access: "no free access — https://doi.org/10.1016/j.marstruc.2007.05.001."
pointers: null
use: "berm growth and cyclic lateral resistance."
```

### P31 `white2010`

```yaml
id: P31
key: white2010
citation: "White, D.J. & Cathie, D.N. (2010). \"Geotechnics for subsea pipelines\". In *Frontiers in Offshore Geotechnics II*, pp. 105–142. CRC Press. DOI 10.1201/b10132-9 `[CR]`."
identifier: ["10.1201/b10132-9"]
identifier_status: ["[CR]"]
access: "no free access — https://doi.org/10.1201/b10132-9."
pointers: null
use: "state-of-practice review of embedment, axial and lateral response."
```

## E. On-bottom stability

### P32 `wolfram1987`

```yaml
id: P32
key: wolfram1987
citation: "Wolfram, W.R., Getz, J.R. & Verley, R.L.P. (1987). \"PIPESTAB project: improved design basis for submarine pipeline stability\". Offshore Technology Conference, OTC-5501-MS. DOI 10.4043/5501-MS `[CR]`."
identifier: ["10.4043/5501-MS"]
identifier_status: ["[CR]"]
access: "no free access — https://doi.org/10.4043/5501-MS."
pointers: null
use: "research basis of the generalized and dynamic stability methods."
```

### P33 `allen1989`

```yaml
id: P33
key: allen1989
citation: "Allen, D.W., Lammert, W.F., Hale, J.R. & Jacobsen, V. (1989). \"Submarine pipeline on-bottom stability: recent AGA research\". Offshore Technology Conference, OTC-6055-MS. DOI 10.4043/6055-MS `[CR]`."
identifier: ["10.4043/6055-MS"]
identifier_status: ["[CR]"]
access: "no free access — https://doi.org/10.4043/6055-MS."
pointers: null
use: "AGA/PRCI stability research programme and software levels."
```

### P34 `zeitoun2008`

```yaml
id: P34
key: zeitoun2008
citation: "Zeitoun, H.O., Tørnes, K., Cumming, G. & Branković, M. (2008). \"Pipeline stability: state of the art\". ASME OMAE2008, Vol. 3, pp. 213–228. DOI 10.1115/OMAE2008-57284 `[CR]`."
identifier: ["10.1115/OMAE2008-57284"]
identifier_status: ["[CR]"]
access: "no free access — https://doi.org/10.1115/OMAE2008-57284."
pointers: null
use: "comparison of absolute, generalized and dynamic stability approaches."
```

### P35 `prci2002`

```yaml
id: P35
key: prci2002
citation: "Pipeline Research Council International (2002). *Submarine Pipeline On-Bottom Stability, Volume 1: Analysis and Design Guidelines*, catalogue no. L51989. DOI 10.55274/R0011168 `[CR]`."
identifier: ["10.55274/R0011168"]
identifier_status: ["[CR]"]
access: "no free access confirmed (PRCI catalogue) — https://doi.org/10.55274/R0011168."
pointers: null
use: "the PRCI three-level stability design guideline; example of a PRCI public-catalogue report."
```

## F. Free spans, VIV and seabed interaction

### P36 `fyrileiv2002`

```yaml
id: P36
key: fyrileiv2002
citation: "Fyrileiv, O. & Mørk, K. (2002). \"Structural response of pipeline free spans based on beam theory\". ASME OMAE2002, Vol. 4, pp. 175–183. DOI 10.1115/OMAE2002-28458 `[CR]`."
identifier: ["10.1115/OMAE2002-28458"]
identifier_status: ["[CR]"]
access: "no free access — https://doi.org/10.1115/OMAE2002-28458."
pointers: null
use: "span natural frequency and stress formulae with effective axial force and soil stiffness."
```

### P37 `drago2015`

```yaml
id: P37
key: drago2015
citation: "Drago, M., Mattioli, M., Bruschi, R. & Vitali, L. (2015). \"Insights on the design of free-spanning pipelines\". *Philosophical Transactions of the Royal Society A* 373(2033), 20140111. DOI 10.1098/rsta.2014.0111 `[CR]`."
identifier: ["10.1098/rsta.2014.0111"]
identifier_status: ["[CR]"]
access: "no free access confirmed — https://doi.org/10.1098/rsta.2014.0111."
pointers: null
use: "review of span design practice and VIV fatigue screening."
```

### P38 `fredsoe2016`

```yaml
id: P38
key: fredsoe2016
citation: "Fredsøe, J. (2016). \"Pipeline–seabed interaction\". *Journal of Waterway, Port, Coastal, and Ocean Engineering* 142(6), 03116002. DOI 10.1061/(ASCE)WW.1943-5460.0000352 `[CR]`."
identifier: ["10.1061/(ASCE)WW.1943-5460.0000352"]
identifier_status: ["[CR]"]
access: "free accepted manuscript in the author's institutional repository — https://orbit.dtu.dk/en/publications/47f6d1ca-2a63-426f-9bbe-931a9fe2b71a (200)."
pointers: null
use: "review of scour, self-burial, span development and liquefaction."
```

## G. Installation

### P39 `palmer1974`

```yaml
id: P39
key: palmer1974
citation: "Palmer, A.C., Hutchinson, G. & Ells, J.W. (1974). \"Configuration of submarine pipelines during laying operations\". *Journal of Engineering for Industry* 96(4), 1112–1118. DOI 10.1115/1.3438484 `[CR]`."
identifier: ["10.1115/1.3438484"]
identifier_status: ["[CR]"]
access: "no free access — https://doi.org/10.1115/1.3438484."
pointers: null
use: "stiffened-catenary solution for lay shape and bending."
```

### P40 `bruschi2015`

```yaml
id: P40
key: bruschi2015
citation: "Bruschi, R., Vitali, L., Marchionni, L., Parrella, A. & Mancini, A. (2015). \"Pipe technology and installation equipment for frontier deep water projects\". *Ocean Engineering* 108, 369–392. DOI 10.1016/j.oceaneng.2015.08.008 `[CR]`."
identifier: ["10.1016/j.oceaneng.2015.08.008"]
identifier_status: ["[CR]"]
access: "no free access — https://doi.org/10.1016/j.oceaneng.2015.08.008."
pointers: null
use: "S-lay and J-lay limits, collapse and strain criteria in deep water."
```

## Topic-to-source cross-reference

| Topic | Primary | Supporting |
|---|---|---|
| Wall thickness, pressure containment, limit states | P18, P1, P2 | P6, P3, P10 |
| Collapse, local buckling, propagation and arrest | P4, P5, P15, P16 | P14, P17 |
| Pipe-soil interaction | P28, P29, P30, P31 | P12, P23, P24, P2 |
| Lateral buckling | P19, P23, P24 | P20, P26, P2 |
| Upheaval buckling | P21, P22, P27 | P20, P2, P3 |
| Pipeline walking | P25 | P24, P2 |
| On-bottom stability | P32, P33, P34, P35 | P29, P11, P6, P8 |
| Free span and VIV | P36, P37 | P38, P11, P2, P6 |
| Scour and self-burial | P38 | P11 |
| Installation | P39, P40 | P4, P6, P8, P7, P13 |
| Pre-commissioning and commissioning | P6 (pp. 169–177), P2 (pp. 753–766) | P7, P3 |
| Flow assurance and thermal design context | P9 | P1, P2 |

See also: DNV-ST-F101, DNV-RP-F105, DNV-RP-F109, DNV-RP-F110, DNV-RP-F114, DNV-RP-F101, API RP 1111, API 5L, ISO 13623, ASME B31.4, ASME B31.8, BS PD 8010-2.

## Counts

- 40 entries: 39 with identifiers confirmed (`[CR]` 35, `[OL]` 4); 0 identifiers carried unverified; 1 with no identifier (P17).
- Free-access PDFs whose links resolved when checked: P17, P27, P38 (3).

## Corrections to the issue's target list

- Hobbs (1984) was published in the ASCE *Journal of Transportation Engineering* (P19), not "J. Pipelines".
- *Mechanics of Offshore Pipelines* Vol. 2 is by Kyriakides & Lee (P5), not Kyriakides & Corona.
- Palmer & King: only the 1st (2004) and 2nd (2008) editions appear in the catalogue checked; a "3rd ed. 2017" could not be confirmed.
- Guo et al. 2nd ed. is by Guo, Song, Ghalambor & Lin with a 2014 imprint; Bai & Jin 2nd ed. is registered with a 2016 date.
- *Subsea Engineering Handbook* is by Y. Bai & Q. Bai (P9). Gerwick's separate book is *Construction of Marine and Offshore Structures*, 3rd ed., CRC Press, 2007, ISBN 978-0-8493-3052-0 `[CR]`, not listed as an entry here.
- The Wagner et al. pipe-soil paper is confirmed as OTC-5504 (1987).

## Gaps against the issue

- Not located as citable public sources: DNV pipeline course notes or monographs by Sotberg & Bruschi (the SUPERB paper P18 is the confirmed substitute); the Mørk/Bjørnsen/Verley derivation of the generalized stability method (P32 and P34 cover the method's basis); Sotberg & Bruschi (1992); Verley & Lund (1995) on clay soils (no DOI record found); public SAFEBUCK deliverable summaries (the JIP papers P23–P25 are used instead).
- "DNV — pipeline design course materials (free PDFs)" and "OnePetro freely-listed papers": none confirmed as free; OnePetro items are listed as "no free access".
- "PRCI public reports": one catalogue report with a DOI is listed (P35); it is not confirmed as free to download.
- Journal sweeps (JOMAE, Marine Structures, Ocean Engineering, IJPVP, Engineering Structures) are represented by individual papers rather than a per-journal survey; no IJPVP or Engineering Structures paper made the final 40.
- Pre-commissioning has thin academic coverage; only book chapters are offered.
- Chapter pointers are page ranges from chapter-level DOI records; P3, P7 and P8 have no such records, so their chapter numbers still need to be read off the printed contents pages.
- P17's PDF resolves at the journal's archive path, but its title page was not read.
- "Citation contract for downstream wiki use": the contract itself was not available to check against; entries carry stable keys, full citations and identifiers so they can be mapped to it.
- Identifier checks used the Crossref registration record and Open Library catalogue records rather than rendered publisher pages, several of which block automated access.
