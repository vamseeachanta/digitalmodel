# Parachute Frame Structural Analysis — Domain Reference

> Canonical location: `digitalmodel/docs/domains/structural/parachute/`
> WRK-5082: GT1R R35 Drag Car Parachute Frame Force Calculation

## Reference Documents

| File | Content | For |
|---|---|---|
| `parachute-aerodynamics.md` | Drag equation, Cd values, Cx factors, NHRA rules, Stroud data | Study + methodology |
| `material-properties-4130.md` | 4130 chromoly properties, allowable stresses, connections | Study + design checks |
| `structural-analysis-method.md` | Stiffness method, von Mises, ASME, tube formulas | Study + methodology |
| `gt1r-product-data.md` | T1 Race Development product specs, vehicle data, client actions | Project reference |

## Key Online Sources

### Parachute Engineering
- [Knacke — Parachute Recovery Systems Design Manual **FULL PDF** (DTIC ADA247666)](https://apps.dtic.mil/sti/tr/pdf/ADA247666.pdf)
- [Knacke on Internet Archive](https://archive.org/details/parachuterecover0000knac)
- [AFFDL-TR-78-151 Recovery Systems Design Guide (DTIC)](https://apps.dtic.mil/sti/tr/pdf/ADA070251.pdf)
- [NASA TN D-6458 — Flat Circular Parachute Cd](https://ntrs.nasa.gov/api/citations/19710023919/downloads/19710023919.pdf)
- [NASA TN D-5619 — Loading in Unfurling Parachutes](https://ntrs.nasa.gov/api/citations/19700005898/downloads/19700005898.pdf)
- [NSWC TR 86-142 — Opening Force Analysis](https://apps.dtic.mil/sti/tr/pdf/ADA170962.pdf)
- [Utah State MAE 6530 — Parachutes 101](http://mae-nas.eng.usu.edu/MAE_6530_Web/New_Course/launch_design/Section3.5.pdf)
- [DSPORT — Drag Racing Parachutes 101](https://dsportmag.com/the-tech/education/quick-tech-drag-racing-parachutes-101/)
- [Dragzine — Five Things About Parachutes with Stroud Safety](https://www.dragzine.com/tech-stories/chassis-safety/five-things-you-need-to-know-about-parachutes-with-stroud-safety/)
- [Bob Stroud Q&A (Dragstuff)](https://www.dragstuff.com/techarticles/stroud-parachute-qa.html)
- [LandRacing — Parachute Yank Loads](https://www.landracing.com/index.php/8-dr-mayfs-analysis/dr-mayfs-analyses/19-parachute-yank-loads)
- [SFI Foundation — Specification List (SFI 9.1 = parachutes)](https://www.sfifoundation.com/wp-content/pdfs/SpecList3.pdf)

### Material Properties
- [MatWeb — AISI 4130 Normalized](https://www.matweb.com/search/DataSheet.aspx?MatGUID=e1ccebe90cf94502b35c2a4745f63593)
- [AZoM — AISI 4130 Alloy Steel](https://www.azom.com/article.aspx?ArticleID=6742)
- [Modulus Metal — AISI 4130 Normalized Properties](https://www.modulusmetal.com/aisi-4130-low-alloy-steel-normalized-mechanical-properties/)

### Structural Analysis
- [McGuire/Gallagher/Ziemian — Matrix Structural Analysis **FREE PDF**](https://digitalcommons.bucknell.edu/books/7/)
- [Duke CEE 421 — Frame Element Stiffness](https://people.duke.edu/~hpgavin/cee421/frame-element.pdf)
- [UF MAE — Finite Element Analysis of Beams and Frames](https://web.mae.ufl.edu/nkim/IntroFEA/Chapter3.pdf)
- [AISC ASD Specification](https://www.aisc.org/globalassets/aisc/manual/15th-ed-ref-list/specification-for-structural-steel-buildings-allowable-stress-design-and-plastic-design.pdf)
- [Von Mises ASD Discussion (Eng-Tips)](https://www.eng-tips.com/threads/aisc-asd-allowable-stress-checks-for-von-mises.453786/)
- [SFI 25.1 Tube Chassis Notice](https://www.sfifoundation.com/wp-content/pdfs/SFI25-XNotice05-16-18.pdf)

### Product Data
- [T1 Race Development — GT1R Parachute Kit](https://www.t1racedevelopment.com/product/gt1r-r35-bolt-on-parachute-kit/)
- [Stroud Safety — Parachutes](https://stroudsafety.com/parachutes/)
- [NHRA Parachute Tether Spec V2](https://www.nhraracer.com/Files/Tech/NHRA_Parachute_Tether_Specification_V2.pdf)

### Cross-Validation
- [ChatGPT Analysis](https://chatgpt.com/s/t_69b37c9a90888191be1caf035639aa89) — independent 27,000 lbs at 250 MPH (Cd=1.5, no Cx)
