---
code_id: OCIMF-STATIC-TOWING-2020
publisher: OCIMF
revision: "2020"
dataset_id: towing-487-ocimf-vlcc-wind-v1
owner_repo: digitalmodel
readiness: published-table-regression-only
verified_on: "2026-11-13"
source_sha256: c4172f98b9b2522aa25a6ee3ce1393b15b073f7f0f4165a05f9522a2c70f759e
coefficients_sha256: a1ce5b03915fef963e21c688bea79d1741902a950fb1bd2e48717736cbc1be29
raw_copy_allowed: false
sources:
  - https://www.ocimf.org/document-libary/58-static-towing-assembly-guidelines-2020/file
  - https://safety4sea.com/wp-content/uploads/2020/04/OCIMF-Static-Towing-Assembly-Guidelines-2020_04.pdf
---

# OCIMF Static Towing Assembly Guidelines (2020), numerical regression basis

The retrieved publication copy is verified by the source digest above. The
publisher endpoint returned HTTP 403 at verification; the second source URL
returned the PDF. No source PDF or licensed standard text is distributed here.
The coefficient digest covers the 37 triples as JSON arrays of floats, using
compact separators. All 37 rows were compared with the extracted source table.

MARIN report 31746-1-PO, report p29 (PDF p66), Table 5, provides the dimensionless
VLCC wind coefficients used by `ocimf_vlcc.py`. The 40 and 50 degree rows are
used for the 45 degree interpolation regression. Report p25, Table 1, provides
VLCC Af=2300 m2, Al=6500 m2 and Lpp=354.4 m. Report p10, section 4.4, specifies
the frontal/lateral wind-area convention and the Lpp moment arm. Wind speed
20 m/s is a declared test input; air density is 1.25 kg/m3.

This is a numerical validation of the published mean ballast/loaded VLCC table.
Applicability to a barge, current coefficients and operational towing acceptance
are not established. The barge example retains the existing approximation.
Interpolation is linear between source headings and periodic at 360 degrees.
