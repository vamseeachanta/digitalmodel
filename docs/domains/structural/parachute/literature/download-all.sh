#!/usr/bin/env bash
# Download all parachute frame structural analysis literature
# Run from terminal: bash download-all.sh
# For protected URLs, use Chrome browser manually.
set -euo pipefail
DEST="$(cd "$(dirname "$0")" && pwd)"

download() {
  local name="$1" url="$2" file="$3" timeout="${4:-120}"
  echo "  [$name]..."
  if curl -L --max-time "$timeout" -o "${DEST}/${file}" "$url" 2>/dev/null; then
    size=$(du -h "${DEST}/${file}" | cut -f1)
    if head -c 5 "${DEST}/${file}" | grep -q '%PDF'; then
      echo "    OK: ${size} — ${file}"
    else
      echo "    WARN: Downloaded ${size} but not a PDF — may be HTML redirect"
      echo "    Manual: ${url}"
    fi
  else
    echo "    FAILED — try browser: ${url}"
  fi
}

echo "=========================================="
echo "WRK-5082 Parachute Frame Literature"
echo "=========================================="

echo ""
echo "=== Parachute Engineering (Primary) ==="

download "Knacke (1992) — Parachute Recovery Systems Design Manual (DTIC ADA247666)" \
  "https://apps.dtic.mil/sti/tr/pdf/ADA247666.pdf" \
  "knacke-1992-parachute-recovery-systems-design-manual.pdf" 300

download "Ewing/Bixby/Knacke (1978) — Recovery Systems Design Guide (AFFDL-TR-78-151)" \
  "https://apps.dtic.mil/sti/tr/pdf/ADA070251.pdf" \
  "ewing-1978-recovery-systems-design-guide-AFFDL-TR-78-151.pdf" 300

echo ""
echo "=== Parachute Aerodynamics (NASA) ==="

download "NASA TN D-6458 — Drag coefficients for flat circular parachutes (1971)" \
  "https://ntrs.nasa.gov/api/citations/19710023919/downloads/19710023919.pdf" \
  "nasa-1971-drag-coefficients-flat-circular-parachutes-TN-D-6458.pdf"

download "NASA TN D-5619 — Loading in unfurling parachutes (1970)" \
  "https://ntrs.nasa.gov/api/citations/19700005898/downloads/19700005898.pdf" \
  "nasa-1970-loading-unfurling-parachutes-TN-D-5619.pdf"

download "NASA TM-X-1484 — Scale factors for parachute opening (1967)" \
  "https://ntrs.nasa.gov/api/citations/19670026449/downloads/19670026449.pdf" \
  "nasa-1967-scale-factors-parachute-opening-TM-X-1484.pdf"

echo ""
echo "=== Opening Shock / Deployment Forces ==="

download "Waye (1986) — Generic parachute opening force analysis (NSWC TR 86-142)" \
  "https://apps.dtic.mil/sti/tr/pdf/ADA170962.pdf" \
  "waye-1986-parachute-opening-force-analysis-NSWC-TR-86-142.pdf"

download "DTIC ADA201050 — Theoretical parachute opening force analysis" \
  "https://apps.dtic.mil/sti/tr/pdf/ADA201050.pdf" \
  "dtic-theoretical-parachute-opening-force.pdf"

download "NASA — Extrapolating test drop data with opening shock (2017)" \
  "https://ntrs.nasa.gov/api/citations/20170003949/downloads/20170003949.pdf" \
  "nasa-2017-extrapolating-drop-data-opening-shock.pdf"

echo ""
echo "=== Structural Analysis Methodology ==="

download "Duke CEE 421 — Frame element stiffness matrices (Gavin)" \
  "https://people.duke.edu/~hpgavin/cee421/frame-element.pdf" \
  "gavin-duke-cee421-frame-element-stiffness.pdf"

download "Duke CEE 421 — Geometric stiffness in 2D/3D frames (Gavin)" \
  "https://people.duke.edu/~hpgavin/cee421/frame-finite-def.pdf" \
  "gavin-duke-cee421-geometric-stiffness-frames.pdf"

download "UF MAE — Finite element analysis of beams and frames Ch3 (Kim)" \
  "https://web.mae.ufl.edu/nkim/IntroFEA/Chapter3.pdf" \
  "kim-uf-intro-fea-chapter3-beams-frames.pdf"

download "Colin Caprani — Matrix stiffness method lecture notes" \
  "http://www.colincaprani.com/files/notes/SAIV/Matrix%20Stiffness%20Method%200910.pdf" \
  "caprani-matrix-stiffness-method.pdf"

download "USU MAE 6530 — Parachutes 101 (recovery systems lecture)" \
  "http://mae-nas.eng.usu.edu/MAE_6530_Web/New_Course/launch_design/Section3.5.pdf" \
  "usu-mae6530-parachutes-101.pdf"

echo ""
echo "=== Material Properties & Allowable Stress ==="

download "AISC ASD Specification (allowable stress design)" \
  "https://www.aisc.org/globalassets/aisc/manual/15th-ed-ref-list/specification-for-structural-steel-buildings-allowable-stress-design-and-plastic-design.pdf" \
  "aisc-asd-specification-allowable-stress-design.pdf"

download "SFI 25.X Tube Chassis Notice" \
  "https://www.sfifoundation.com/wp-content/pdfs/SFI25-XNotice05-16-18.pdf" \
  "sfi-25x-tube-chassis-notice.pdf"

download "SFI Foundation — Specification list" \
  "https://www.sfifoundation.com/wp-content/pdfs/SpecList3.pdf" \
  "sfi-specification-list.pdf"

echo ""
echo "=== NHRA Regulations ==="

download "NHRA Parachute Tether Specification V2" \
  "https://www.nhraracer.com/Files/Tech/NHRA_Parachute_Tether_Specification_V2.pdf" \
  "nhra-parachute-tether-specification-v2.pdf"

download "NHRA Exhibition Vehicle Rulebook Supplement (2025)" \
  "https://www.nhraracer.com/Files/Tech/Exhibition%20Vehicle%20Rulebook%20Supplement%2006-04-2025.pdf" \
  "nhra-exhibition-vehicle-rulebook-supplement-2025.pdf"

download "NHRA 2023-2024 Rule Amendments" \
  "https://radraceway.com/wp-content/uploads/2024/01/2023-to-2024-Rulebook-Amendments.pdf" \
  "nhra-2023-2024-rule-amendments.pdf"

download "FIA Drag Racing SFI Specifications (2024)" \
  "https://www.fia.com/sites/default/files/2024_fia_drag_racing_-_sfi_specification_v221123_clean.pdf" \
  "fia-2024-drag-racing-sfi-specifications.pdf"

echo ""
echo "=== Connection Design ==="

download "AISC Bolting and Welding Primer" \
  "https://www.aisc.org/globalassets/continuing-education/quiz-handouts/bolting-and-welding-primer-handout.pdf" \
  "aisc-bolting-welding-primer.pdf"

download "Purdue CE 470 — Weld Design" \
  "https://engineering.purdue.edu/~ahvarma/CE%20470/Final_Fall_2013/F13_CE470Ch3Welds.pdf" \
  "purdue-ce470-weld-design.pdf"

download "Midalloy ER70S-2 Filler Wire Datasheet" \
  "https://www.midalloy.com/wp-content/uploads/2016/08/Midalloy-ER70S-2.pdf" \
  "midalloy-er70s2-filler-wire-datasheet.pdf"

echo ""
echo "=== Racing Chassis Analysis Papers ==="

download "ASEE — Two approaches to optimize Formula SAE chassis FEA" \
  "https://peer.asee.org/two-approaches-to-optimize-formula-sae-chassis-design-using-finite-element-analysis.pdf" \
  "asee-formula-sae-chassis-fea-optimization.pdf"

download "IOP — Structural analysis of chassis using AISI 4130 and AA 7068" \
  "https://iopscience.iop.org/article/10.1088/1757-899X/1059/1/012034/pdf" \
  "iop-chassis-analysis-4130-vs-7068.pdf"

echo ""
echo "=== Manual Downloads Required (browser) ==="
echo "  McGuire/Gallagher/Ziemian — Matrix Structural Analysis (free):"
echo "    https://digitalcommons.bucknell.edu/books/7/"
echo "  Knacke on Internet Archive:"
echo "    https://archive.org/details/parachuterecover0000knac"
echo "  MatWeb — AISI 4130 Normalized datasheet:"
echo "    https://www.matweb.com/search/DataSheet.aspx?MatGUID=e1ccebe90cf94502b35c2a4745f63593"
echo "  Stroud Safety parachute sizing chart (image on site):"
echo "    https://stroudsafety.com/parachutes/"

echo ""
echo "=== Current Library ==="
ls -lhS "${DEST}"/*.pdf 2>/dev/null | head -30 || echo "No PDFs found."
echo ""
echo "Total files: $(ls "${DEST}"/*.pdf 2>/dev/null | wc -l) PDFs"
