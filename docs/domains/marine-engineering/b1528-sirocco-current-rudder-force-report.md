# B1528 SIROCCO Current/Rudder Force Review — Issue #2760

Prepared for engineer review on 2026-05-09.

## 1. Introduction

The B1528 SIROCCO is a moored vessel. When current flows past it, two separate loads act on the ship:

1. A **hull current load** — the current pushes on the hull, producing a longitudinal force (X), a transverse force (Y), and a yaw moment (N) about the centre of gravity.
2. A **rudder-induced load** — current also flows past the rudder. When the rudder is held at an angle, it generates an additional force at the stern that contributes to X, Y, and N.

This report estimates both load components for the approved review case:

- Current speed: **3.08 knots**
- Current heading ψ: **+5° off the bow** (port positive)
- Rudder angle δ: **+28°** (port)
- Propeller: **stopped** (rpm = 0; rotation factor Cr = 1.0)

All forces are reported in ship-fixed axes about the centre of gravity. This is a **screening calculation** intended to support mooring reaction-load reviews — it is not a certified hydrodynamic model. Limitations are in §6.

## 2. Design Data & Assumptions

Values used by the calculation are sourced from the packaged B1528 SIROCCO YAML input pack.

| Parameter | Value |
|---|---:|
| LBP (length between perpendiculars) | 225.5 m |
| Beam | 32.26 m |
| Draft | 12.20 m |
| Water depth | 100.0 m |
| Water depth / Draft (WD/T) | 8.20 |
| Centre of gravity (longitudinal datum) | midship |
| Centre of gravity (vertical, above keel) | 6.1 m |
| Yaw lever arm (rudder force to CoG) | 135.30 m |
| Rudder area | 44.940 m² |
| Rudder span | 9.00 m |
| Water density ρ | 1025.0 kg/m³ |
| Whicker-Fehlner constant β | 600.0 |
| Propeller rotation factor Cr | 1.0 (rpm = 0) |
| Current speed (default) | 3.08 kn |
| Current heading ψ (default) | +5° (port positive) |
| Rudder angle δ (default) | +28° (port positive) |

## 3. Axes & Sign Conventions

All forces and moments are reported in **ship-fixed axes at the centre of gravity** using the right-hand rule.

| Symbol | Meaning | Sign convention |
|---|---|---|
| **+X** | longitudinal force (along ship centreline) | forward |
| **+Y** | transverse force (lateral, across ship) | port |
| **+N** | yaw moment about CoG | bow-to-port |
| **ψ** | current heading offset from bow | port positive |
| **δ** | rudder angle relative to ship centreline | port positive |
| **α** | effective rudder inflow angle = δ - ψ | (derived) |

**See the schematic in the HTML report (§3 schematic-axes-conventions)** for the ship outline with arrows showing each axis and angle.

## 4. Load Due to Current

The current load on the hull is estimated using **generic-reference OCIMF MEG3/MEG4 loaded-tanker current coefficients** interpolated from the licensed off-repo workbook. These curves are a tanker-class basis — they are not specific to SIROCCO and are used here as a screening estimate only.

Three coefficients drive the load:

- **Cxc** (longitudinal current coefficient) from Annex A figure A9
- **Cyc** (transverse current coefficient) from Annex A figure A10
- **Cxyc** (yaw moment coefficient) from Annex A figure A11

Workbook bucket selection: loaded-tanker family, water-depth-to-draft ratio WD/T > 6 (vessel WD/T = 8.20). Coefficients are interpolated at the table angle `180° − |ψ|`, which equals **175°** for the default heading ψ = +5°.

### 4.1. Force calculation

Dynamic pressure: `q = 0.5 × ρ × V²`  
Frontal projected area: `A_f = beam × draft`  
Lateral projected area: `A_l = LBP × draft`

Force components:

- **Longitudinal (X):** `Xc = q × A_f × Cxc`
- **Transverse (Y):** `Yc = q × A_l × Cyc`

**Sample calculation at default values:**

- V = 3.08 kn × 0.51444 = **1.5845 m/s**
- q = 0.5 × 1025 kg/m³ × (1.5845 m/s)² = **1286.7 Pa**
- A_f = 32.26 × 12.20 = **393.57 m²**
- A_l = 225.5 × 12.20 = **2751.10 m²**
- Cxc(175°) ≈ **−0.0324**, Cyc(175°) ≈ **+0.0341**
- **Xc** ≈ -16.4 kN (longitudinal current force on ship)
- **Yc** ≈ 120.7 kN (transverse current force on ship)

### 4.2. Yaw moment about CoG

Two methods estimate the current yaw moment, shown side by side as a sanity check:

- **Method A — OCIMF direct (default):** `Nc_A = q × A_l × LBP × Cxyc(175°)`
- **Method B — force × lever arm:** `Nc_B = Yc × CoP_lever_arm`

These two methods rest on different assumptions and **may differ**. The HTML report's Chart 4 shows both with a caption that explicitly says this comparison is **not equality-based** — it is a sanity-check overlay.

**See the per-section schematic in the HTML report (§4 schematic-current-loading and schematic-current-moment).**

## 5. Load Due to Rudder

The rudder-induced force comes from current flowing past the rudder blade at the effective inflow angle:

`α = δ - ψ`

At the default case (δ = +28° port, ψ = +5° port): **α = +23°**.

Two screening rudder models are presented side by side (Pass C wires Model B numerically; this Pass A lays out the structure):

- **Model A — Whicker-Fehlner normal-force basis** (current default): `F = β × A_R × V² × Cr × sin(α)`
  - Source: B1528 SIROCCO source pack (`β = 600`, `Cr = 1.0`)
  - Captures the bulk of normal force at small-to-moderate α; simple sin(α) projection
- **Model B — thin-plate drag/lift** (Pass C): `Cn(α) ≈ 2π·sin(α)` for small angles, with stall handling at ~25–30°
  - Reference: Faltinsen, *Sea Loads on Ships and Offshore Structures*, 1990, §6.5
  - Independent reference model for sanity-checking Model A

**Both models are screening-level**; neither is a validated rudder hydrodynamic model. Differences at large angles (>20°) reflect stall-region behaviour and the simplifying assumptions of each.

**Sample calculation at default values (Model A):**

- α = δ - ψ = 28° − 5° = **23°**
- F = β × A_R × V² × Cr = 600 × 44.940 × (1.5845)² × 1.0 = **67694.1 N**
- Fn (normal) = F × sin(α) = **26450.2 N**
- Longitudinal X_rudder ≈ **8.17 kN**
- Transverse Y_rudder ≈ **25.16 kN**
- Yaw moment N_rudder ≈ **3403.56 kN·m** (+bow-to-port)

**See the per-section schematic in the HTML report (§5 schematic-rudder-loading).**

## 6. Limitations

- Generic-reference OCIMF tanker-current coefficients are **not vessel-specific to SIROCCO**. The report basis is an off-class screening tier.
- Both rudder models are **screening-level**; neither is a validated rudder hydrodynamic model.
- Component sums (X_total = Xc + Xr, etc.) are reported for engineering review; they are **not a validated whole-vessel force balance**.
- 3.08 kn is the issue #2760 default current speed; practical plots/tables are bounded to 0..4 kn.
- When rudder_angle_deg equals heading_offset_deg, alpha is zero and this rudder-induced component is zero; that is not total hull current load.
- This report excludes: hull current force at oblique headings beyond the generic basis range, mooring-line stiffness, tug loads, bank effects, current-profile variation, propeller race, IMO/class compliance conclusions.

Original limitations from the YAML input pack:

- rudder-induced moored-current loads plus generic/reference OCIMF tanker-current review loads are reported for comparison
- hull-current terms use report-specific coefficients resolved through the approved off-repo workbook route, not ship-specific SIROCCO current-coefficient curves or certified coefficients
- bank effect, tug loads, mooring-line stiffness, current-profile variation, and propeller race are excluded
- oblique-current extension is a first-cut visualization using alpha = rudder angle - heading offset
- not a validated oblique-current hull/rudder interaction model
- Cr=1.0 is a neutral no-propeller-rotation correction value
- no class compliance conclusion

---

## Traceability

- GitHub issue: [workspace-hub #2760](https://github.com/vamseeachanta/workspace-hub/issues/2760)
- Approved plan: [issue #2760 plan](https://github.com/vamseeachanta/digitalmodel/blob/main/docs/plans/2026-05-20-issue-2760-b1528-sirocco-force-review-revision.md)
- Source pack: [workspace-hub #2569](https://github.com/vamseeachanta/workspace-hub/issues/2569)
- Packaged input YAML: [b1528_sirocco_current_heading_rudder.yml](https://github.com/vamseeachanta/digitalmodel/blob/main/src/digitalmodel/naval_architecture/data/b1528_sirocco_current_heading_rudder.yml)
- Report generator: [b1528_sirocco_current_heading_rudder_report.py](https://github.com/vamseeachanta/digitalmodel/blob/main/src/digitalmodel/naval_architecture/b1528_sirocco_current_heading_rudder_report.py)
- Sweep coverage: 990 rows (990 engineering + 0 chart-default).
- Source pack notes the report basis is **not ship-specific SIROCCO current-coefficient curves**.
