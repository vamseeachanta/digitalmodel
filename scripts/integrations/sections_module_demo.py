"""Demo: digitalmodel.sections module — production cross-section property API.

Computes and prints properties for an AISC W14x90 I-section and a CHS 914x25
offshore tubular, using the standardized digitalmodel.sections interface.

Reference: vamseeachanta/workspace-hub#1498
"""

from digitalmodel.sections import compute_chs, compute_i_section

# ---------------------------------------------------------------------------
# 1. AISC W14x90 (W360x134 metric)
# ---------------------------------------------------------------------------
w14x90 = compute_i_section(
    d=356.0,    # depth, mm
    b=368.0,    # flange width, mm
    t_f=18.0,   # flange thickness, mm
    t_w=11.2,   # web thickness, mm
    r=25.4,     # fillet radius, mm
    n_r=16,
    mesh_size=10,
    label="AISC W14x90",
)

print(f"{'=' * 60}")
print(f"  {w14x90.label}")
print(f"{'=' * 60}")
print(f"  A   = {w14x90.area:>14.1f} mm^2")
print(f"  Ix  = {w14x90.ixx:>14.1f} mm^4")
print(f"  Iy  = {w14x90.iyy:>14.1f} mm^4")
print(f"  Zx  = {w14x90.zx:>14.1f} mm^3  (plastic)")
print(f"  Sx  = {w14x90.sx:>14.1f} mm^3  (elastic)")
print(f"  J   = {w14x90.j:>14.1f} mm^4")
print(f"  Cw  = {w14x90.cw:>14.1f} mm^6")
print(f"  rx  = {w14x90.rx:>14.2f} mm")
print(f"  ry  = {w14x90.ry:>14.2f} mm")

# ---------------------------------------------------------------------------
# 2. Offshore CHS 914x25 (36" OD x 1" WT)
# ---------------------------------------------------------------------------
chs = compute_chs(
    d=914.4,    # outside diameter, mm
    t=25.4,     # wall thickness, mm
    n=64,
    mesh_size=15,
    label="CHS 914x25",
)

print(f"\n{'=' * 60}")
print(f"  {chs.label}")
print(f"{'=' * 60}")
print(f"  A   = {chs.area:>14.1f} mm^2")
print(f"  Ix  = {chs.ixx:>14.1f} mm^4")
print(f"  Iy  = {chs.iyy:>14.1f} mm^4")
print(f"  Zx  = {chs.zx:>14.1f} mm^3  (plastic)")
print(f"  Sx  = {chs.sx:>14.1f} mm^3  (elastic)")
print(f"  J   = {chs.j:>14.1f} mm^4")
print(f"  rx  = {chs.rx:>14.2f} mm")
print(f"  ry  = {chs.ry:>14.2f} mm")

# ---------------------------------------------------------------------------
# 3. Pipeline-ready dict output
# ---------------------------------------------------------------------------
print(f"\n{'=' * 60}")
print("  JSON-serializable dict output")
print(f"{'=' * 60}")
import json

print(json.dumps(w14x90.to_dict(), indent=2))
