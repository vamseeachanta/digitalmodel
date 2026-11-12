---
title: Clamp repair of cracked offshore tubular joints (external reference)
type: external-reference
source: LinkedIn post, https://lnkd.in/p/gi_TWhkt (seen 2026-10-20)
paper: Fazel Mojtahedi, F., Aslani, F. and Butt, P. (2026), Marine Structures 110, 104124, doi:10.1016/j.marstruc.2026.104124
recorded: 2026-10-20
recorded_by: maintainers
---

# Clamp repair of cracked offshore tubular joints (external reference)

## Source

- **Post:** a LinkedIn post, short link https://lnkd.in/p/gi_TWhkt, seen 2026-10-20. It shows an AI-generated infographic of a mechanical clamp repair workflow for a cracked tubular joint on a fixed jacket: inspection, clamp design, installation, monitoring.
  - The post states that the graphic's proportions, bolt layout and clamp geometry may not match a real engineered design.
  - The post serves as a pointer only, not as a design source.
- **Paper cited by the post (the substantive reference):** Fazel Mojtahedi, F., Aslani, F. and Butt, P. (2026). *Integrity of offshore tubular joints: Mechanics, repair strategies, and structural health monitoring towards an integrated lifecycle framework.* Marine Structures, 110, 104124. https://doi.org/10.1016/j.marstruc.2026.104124
  - Title, authors, volume, article number and licence entry confirmed against the registered Crossref metadata on 2026-10-20.

## Content

- **Mechanical clamps on cracked or fatigue-damaged tubular joints.** The post states that the clamp carries load around the damaged joint and helps arrest further crack growth.
- **Clamps compared with welded reinforcement and grouting.** The post lists these advantages:
  - no hot work underwater;
  - often no major production shutdown;
  - suited to fatigue-cracked joints;
  - restores the load path;
  - a track record on ageing jackets.
- The comparison above is the post's and is not used in reports until confirmed against the paper.
- **Paper scope, from its title:** joint mechanics, repair strategies, and structural health monitoring within a lifecycle integrity framework.

## Related digitalmodel areas

| Topic | Area |
|---|---|
| Joint fatigue and crack growth before and after repair | `src/digitalmodel/fatigue/scf_library.py` (tubular-joint SCFs), `src/digitalmodel/fatigue/crack_growth.py`, `docs/domains/fatigue` |
| Jacket member and joint checks | `src/digitalmodel/structural/jacket_topside/joint_checks.py`, `docs/domains/structural` |
| Fitness for service and repair selection for ageing structures | `src/digitalmodel/asset_integrity`, `docs/domains/asset-integrity` |

## Source rights

- The registered version-of-record metadata for the paper lists a CC BY 4.0 licence (Crossref, checked 2026-10-20). Any quotation or figure taken from it carries the attribution that licence requires.
- The reuse rights of the post and its infographic are unverified. Neither the infographic nor the post's text is reproduced in this repository; this page only summarises and links to it.

## Conditions of use

- Methods, values and comparisons are taken from the paper, not the infographic. Reports cite the paper and the governing code clauses.
- Clamp design itself is outside the post's scope. Clamp load transfer, bolt pretension, the friction or grout interface, and fatigue of the clamped joint each require their own engineering basis.
- **Review status:** the full paper has not yet been reviewed. Its key findings are to be recorded on this page after review.
