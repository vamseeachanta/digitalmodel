#!/usr/bin/env python3
"""
Generate WAMIT GDF mesh for rectangular barge.

Barge geometry:
- 100m long (X: -50 to +50)
- 20m beam (Y: -10 to +10)
- 8m draft (Z: -8 to 0)

Mesh density (refined for AQWA element quality checks):
- Bottom: 40x8 panels
- Long sides (Y=+/-10): 40x4 panels each
- Short sides (X=+/-50): 8x4 panels each
- Total: 704 quads

Normal convention: Normals point INTO THE FLUID (outward from body).
"""

import numpy as np
from pathlib import Path


def cross_product(v1, v2):
    """Compute cross product of two 3D vectors."""
    return np.array([
        v1[1] * v2[2] - v1[2] * v2[1],
        v1[2] * v2[0] - v1[0] * v2[2],
        v1[0] * v2[1] - v1[1] * v2[0]
    ])


def compute_panel_normal(v1, v2, v3, v4):
    """
    Compute normal vector for a quadrilateral panel.
    Uses right-hand rule: normal = (V2-V1) × (V3-V1)
    """
    edge1 = v2 - v1
    edge2 = v3 - v1
    normal = cross_product(edge1, edge2)
    # Normalize
    norm = np.linalg.norm(normal)
    if norm > 0:
        normal = normal / norm
    return normal


def generate_bottom_panels(length, beam, draft, nx, ny):
    """
    Generate bottom panels.
    Normal must point -Z (downward, into fluid).
    """
    panels = []
    dx = length / nx
    dy = beam / ny
    x0 = -length / 2
    y0 = -beam / 2
    z = -draft

    for i in range(nx):
        for j in range(ny):
            x1 = x0 + i * dx
            x2 = x0 + (i + 1) * dx
            y1 = y0 + j * dy
            y2 = y0 + (j + 1) * dy

            # Order vertices to get normal pointing -Z (into fluid)
            # Clockwise when viewed from +Z
            v1 = np.array([x1, y1, z])
            v2 = np.array([x1, y2, z])
            v3 = np.array([x2, y2, z])
            v4 = np.array([x2, y1, z])

            panels.append((v1, v2, v3, v4))

    return panels


def generate_side_panels_y_neg(length, draft, nx, nz):
    """
    Generate side panels at Y = -beam/2.
    Normal must point -Y (outward from body, into fluid).
    """
    panels = []
    dx = length / nx
    dz = draft / nz
    x0 = -length / 2
    y = -10.0  # -beam/2
    z0 = -draft

    for i in range(nx):
        for k in range(nz):
            x1 = x0 + i * dx
            x2 = x0 + (i + 1) * dx
            z1 = z0 + k * dz
            z2 = z0 + (k + 1) * dz

            # Order vertices to get normal pointing -Y (into fluid)
            v1 = np.array([x1, y, z1])
            v2 = np.array([x2, y, z1])
            v3 = np.array([x2, y, z2])
            v4 = np.array([x1, y, z2])

            panels.append((v1, v2, v3, v4))

    return panels


def generate_side_panels_y_pos(length, draft, nx, nz):
    """
    Generate side panels at Y = +beam/2.
    Normal must point +Y (outward from body, into fluid).
    """
    panels = []
    dx = length / nx
    dz = draft / nz
    x0 = -length / 2
    y = 10.0  # +beam/2
    z0 = -draft

    for i in range(nx):
        for k in range(nz):
            x1 = x0 + i * dx
            x2 = x0 + (i + 1) * dx
            z1 = z0 + k * dz
            z2 = z0 + (k + 1) * dz

            # Order vertices to get normal pointing +Y (into fluid)
            v1 = np.array([x2, y, z1])
            v2 = np.array([x1, y, z1])
            v3 = np.array([x1, y, z2])
            v4 = np.array([x2, y, z2])

            panels.append((v1, v2, v3, v4))

    return panels


def generate_side_panels_x_neg(beam, draft, ny, nz):
    """
    Generate side panels at X = -length/2.
    Normal must point -X (outward from body, into fluid).
    """
    panels = []
    dy = beam / ny
    dz = draft / nz
    x = -50.0  # -length/2
    y0 = -beam / 2
    z0 = -draft

    for j in range(ny):
        for k in range(nz):
            y1 = y0 + j * dy
            y2 = y0 + (j + 1) * dy
            z1 = z0 + k * dz
            z2 = z0 + (k + 1) * dz

            # Order vertices to get normal pointing -X (into fluid)
            v1 = np.array([x, y2, z1])
            v2 = np.array([x, y1, z1])
            v3 = np.array([x, y1, z2])
            v4 = np.array([x, y2, z2])

            panels.append((v1, v2, v3, v4))

    return panels


def generate_side_panels_x_pos(beam, draft, ny, nz):
    """
    Generate side panels at X = +length/2.
    Normal must point +X (outward from body, into fluid).
    """
    panels = []
    dy = beam / ny
    dz = draft / nz
    x = 50.0  # +length/2
    y0 = -beam / 2
    z0 = -draft

    for j in range(ny):
        for k in range(nz):
            y1 = y0 + j * dy
            y2 = y0 + (j + 1) * dy
            z1 = z0 + k * dz
            z2 = z0 + (k + 1) * dz

            # Order vertices to get normal pointing +X (into fluid)
            v1 = np.array([x, y1, z1])
            v2 = np.array([x, y2, z1])
            v3 = np.array([x, y2, z2])
            v4 = np.array([x, y1, z2])

            panels.append((v1, v2, v3, v4))

    return panels


def write_gdf(filename, panels, title="Rectangular Barge - 100m x 20m x 8m draft"):
    """Write panels to WAMIT GDF file."""
    with open(filename, 'w') as f:
        # Header
        f.write(f"{title}\n")
        f.write("  1.0000  9.80665\n")  # ULEN GRAV
        f.write("  0  0\n")  # ISX ISY (no symmetry)
        f.write(f"  {len(panels)}\n")  # NPAN

        # Write panels
        for v1, v2, v3, v4 in panels:
            f.write(f"  {v1[0]:12.6f}  {v1[1]:12.6f}  {v1[2]:12.6f}\n")
            f.write(f"  {v2[0]:12.6f}  {v2[1]:12.6f}  {v2[2]:12.6f}\n")
            f.write(f"  {v3[0]:12.6f}  {v3[1]:12.6f}  {v3[2]:12.6f}\n")
            f.write(f"  {v4[0]:12.6f}  {v4[1]:12.6f}  {v4[2]:12.6f}\n")


def verify_normals(panels, face_name):
    """Verify that all normals for a face point in the expected direction."""
    print(f"\n{face_name}:")
    print(f"  Panels: {len(panels)}")

    # Check first panel
    v1, v2, v3, v4 = panels[0]
    normal = compute_panel_normal(v1, v2, v3, v4)
    print(f"  First panel vertices:")
    print(f"    V1: {v1}")
    print(f"    V2: {v2}")
    print(f"    V3: {v3}")
    print(f"    V4: {v4}")
    print(f"  Normal (first panel): [{normal[0]:7.4f}, {normal[1]:7.4f}, {normal[2]:7.4f}]")

    # Check all normals
    normals = [compute_panel_normal(v1, v2, v3, v4) for v1, v2, v3, v4 in panels]
    avg_normal = np.mean(normals, axis=0)
    print(f"  Average normal: [{avg_normal[0]:7.4f}, {avg_normal[1]:7.4f}, {avg_normal[2]:7.4f}]")


def main():
    # Barge dimensions
    length = 100.0  # m
    beam = 20.0     # m
    draft = 8.0     # m

    # Mesh density — panels must be roughly square to pass AQWA quality checks
    # (facet radius and area ratio constraints at 90-degree corners)
    nx_bottom = 40  # panels in X direction for bottom
    ny_bottom = 8   # panels in Y direction for bottom
    nx_long_side = 40  # panels in X direction for long sides
    ny_short_side = 8  # panels in Y direction for short sides
    nz = 4  # panels in Z direction for all sides

    print("Generating barge mesh...")
    print(f"Dimensions: {length}m (L) × {beam}m (B) × {draft}m (T)")
    print(f"Mesh density: Bottom {nx_bottom}×{ny_bottom}, Long sides {nx_long_side}×{nz}, Short sides {ny_short_side}×{nz}")

    # Generate panels
    bottom_panels = generate_bottom_panels(length, beam, draft, nx_bottom, ny_bottom)
    side_y_neg = generate_side_panels_y_neg(length, draft, nx_long_side, nz)
    side_y_pos = generate_side_panels_y_pos(length, draft, nx_long_side, nz)
    side_x_neg = generate_side_panels_x_neg(beam, draft, ny_short_side, nz)
    side_x_pos = generate_side_panels_x_pos(beam, draft, ny_short_side, nz)

    # Combine all panels
    all_panels = bottom_panels + side_y_neg + side_y_pos + side_x_neg + side_x_pos

    # Verify normals
    print("\n" + "="*60)
    print("NORMAL VERIFICATION")
    print("="*60)
    verify_normals(bottom_panels, "Bottom (Z=-8, normal should be -Z)")
    verify_normals(side_y_neg, "Side Y=-10 (normal should be -Y)")
    verify_normals(side_y_pos, "Side Y=+10 (normal should be +Y)")
    verify_normals(side_x_neg, "Side X=-50 (normal should be -X)")
    verify_normals(side_x_pos, "Side X=+50 (normal should be +X)")

    # Write GDF file
    output_path = Path(__file__).parents[2] / "specs" / "modules" / "benchmark" / "barge_generated.gdf"
    output_path.parent.mkdir(parents=True, exist_ok=True)

    write_gdf(output_path, all_panels)

    print("\n" + "="*60)
    print("MESH SUMMARY")
    print("="*60)
    print(f"Total panels: {len(all_panels)}")
    print(f"  Bottom:      {len(bottom_panels)} panels")
    print(f"  Side Y=-10:  {len(side_y_neg)} panels")
    print(f"  Side Y=+10:  {len(side_y_pos)} panels")
    print(f"  Side X=-50:  {len(side_x_neg)} panels")
    print(f"  Side X=+50:  {len(side_x_pos)} panels")
    print(f"\nOutput written to: {output_path}")
    print(f"File size: {output_path.stat().st_size} bytes")

    # Verify line count
    with open(output_path, 'r') as f:
        lines = f.readlines()
    expected_lines = 4 + len(all_panels) * 4
    print(f"Line count: {len(lines)} (expected: {expected_lines})")

    if len(lines) == expected_lines:
        print("\nMesh generation successful!")
    else:
        print(f"\nWarning: Line count mismatch!")


if __name__ == "__main__":
    main()
