# ABOUTME: Bolted connection FEA setup — pretension, bolt circles, flanges, gaskets
# ABOUTME: Generates APDL commands for bolt modeling; no ANSYS installation needed

"""
Bolt Analysis
=============

Generates ANSYS APDL commands for bolted connection finite element analysis:

    - Bolt pretension elements (PRETS179)
    - Bolt circle pattern generation (keypoint/node placement)
    - Flange analysis setup (raised face, ring type, full face)
    - Gasket element generation (INTER195)
    - Multi-step load sequences (pretension → operating → hydrotest)

Typical applications: pressure vessel flange-bolt-gasket assemblies,
structural bolted connections, equipment hold-down bolts.

Default values are for ASME B16.5 Class 300 flanges with SA-193 B7 bolts.
"""

from __future__ import annotations

import math
from dataclasses import dataclass, field
from typing import Optional


# ---------------------------------------------------------------------------
# Configuration dataclasses
# ---------------------------------------------------------------------------

@dataclass
class BoltConfig:
    """Single bolt definition."""
    bolt_id: int = 1
    diameter_mm: float = 25.4  # 1" bolt
    pitch_mm: float = 3.175  # UN thread pitch
    material_id: int = 2
    pretension_force_n: float = 120000.0  # typical for 1" SA-193 B7
    length_mm: float = 150.0
    thread_length_mm: float = 50.0
    element_type: str = "BEAM188"  # BEAM188 or SOLID186


@dataclass
class BoltCircleConfig:
    """Bolt circle pattern definition."""
    num_bolts: int = 16
    bolt_circle_diameter_mm: float = 550.0  # PCD
    bolt: BoltConfig = field(default_factory=BoltConfig)
    start_angle_deg: float = 0.0  # angle of first bolt from X-axis
    stud_type: str = "through"  # through, stud, tap


@dataclass
class FlangeConfig:
    """Flange geometry for bolted joint analysis."""
    flange_type: str = "weld_neck"  # weld_neck, slip_on, blind, lap_joint
    flange_od_mm: float = 635.0
    flange_id_mm: float = 304.8
    flange_thickness_mm: float = 55.6
    hub_length_mm: float = 100.0
    hub_thickness_mm: float = 38.0
    raised_face_height_mm: float = 1.6
    raised_face_od_mm: float = 395.0
    bolt_circle: BoltCircleConfig = field(default_factory=BoltCircleConfig)
    material_id: int = 1
    rating_class: str = "300"


@dataclass
class GasketConfig:
    """Gasket definition for flange joint."""
    gasket_type: str = "spiral_wound"  # spiral_wound, sheet, ring_joint, ptfe
    gasket_od_mm: float = 393.7
    gasket_id_mm: float = 323.9
    initial_thickness_mm: float = 4.5
    seating_stress_mpa: float = 68.9  # y factor (ASME Table 2-5.1)
    gasket_factor_m: float = 3.0  # m factor (ASME Table 2-5.1)
    compression_modulus_mpa: float = 5000.0
    unloading_modulus_mpa: float = 10000.0
    max_compression_ratio: float = 0.3  # max 30% compression


# ---------------------------------------------------------------------------
# Generator
# ---------------------------------------------------------------------------

class BoltAnalysisGenerator:
    """Generate APDL commands for bolted connection FEA."""

    def generate_bolt_circle_geometry(
        self, circle: BoltCircleConfig
    ) -> str:
        """Generate keypoints and lines for bolt circle pattern.

        Creates bolt positions evenly spaced around the bolt circle.

        Returns
        -------
        str
            APDL commands for bolt circle keypoints.
        """
        pcd_radius = circle.bolt_circle_diameter_mm / 2.0
        angle_step = 360.0 / circle.num_bolts
        start = circle.start_angle_deg

        lines = [
            "/PREP7",
            "! --- Bolt Circle Geometry ---",
            f"! PCD = {circle.bolt_circle_diameter_mm} mm, "
            f"{circle.num_bolts} bolts",
            f"! Bolt diameter = {circle.bolt.diameter_mm} mm",
            "",
        ]

        for i in range(circle.num_bolts):
            angle_deg = start + i * angle_step
            angle_rad = math.radians(angle_deg)
            x = pcd_radius * math.cos(angle_rad)
            y = pcd_radius * math.sin(angle_rad)
            kp_base = 1000 + i * 2
            kp_top = kp_base + 1

            lines.append(f"! Bolt {i + 1} at {angle_deg:.1f} degrees")
            lines.append(f"K,{kp_base},{x:.4f},{y:.4f},0")
            lines.append(
                f"K,{kp_top},{x:.4f},{y:.4f},{circle.bolt.length_mm}"
            )
            lines.append(f"L,{kp_base},{kp_top}")
            lines.append("")

        return "\n".join(lines)

    def generate_bolt_pretension(
        self, circle: BoltCircleConfig
    ) -> str:
        """Generate APDL pretension element commands (PRETS179).

        Each bolt gets a pretension section at its mid-length.

        Returns
        -------
        str
            APDL commands for bolt pretension setup.
        """
        lines = [
            "! --- Bolt Pretension Elements ---",
            f"! Pretension force per bolt: {circle.bolt.pretension_force_n} N",
            f"! Total bolt load: "
            f"{circle.bolt.pretension_force_n * circle.num_bolts:.0f} N",
            "",
            f"ET,10,PRETS179  ! Pretension element",
            "",
            "! Define pretension sections at bolt mid-planes",
        ]

        for i in range(circle.num_bolts):
            section_id = 100 + i
            lines.extend([
                f"! Bolt {i + 1}",
                f"SECTYPE,{section_id},PRETENSION",
                f"PSMESH,{section_id},BOLT_{i + 1},,,",
            ])

        lines.extend([
            "",
            "! Apply pretension force in load step 1",
            "/SOLU",
        ])

        for i in range(circle.num_bolts):
            section_id = 100 + i
            lines.append(
                f"SLOAD,{section_id},,FORC,{circle.bolt.pretension_force_n}"
            )

        lines.append("")
        return "\n".join(lines)

    def generate_bolt_material(self, bolt: BoltConfig) -> str:
        """Generate material properties for SA-193 B7 bolt steel.

        Returns
        -------
        str
            APDL MP commands for bolt material.
        """
        lines = [
            f"! --- Bolt Material (SA-193 B7) ID={bolt.material_id} ---",
            f"MP,EX,{bolt.material_id},205000",
            f"MP,NUXY,{bolt.material_id},0.3",
            f"MP,DENS,{bolt.material_id},7.85E-09",
            f"MP,ALPX,{bolt.material_id},1.15E-05",
            f"! Yield strength: 724 MPa (bolt diameter <= 2.5\")",
            f"! Allowable stress at ambient: 172 MPa (ASME Table 3)",
            "",
        ]
        return "\n".join(lines)

    def generate_gasket_elements(self, gasket: GasketConfig) -> str:
        """Generate APDL commands for gasket interface elements (INTER195).

        Defines gasket material behavior with nonlinear compression
        curve and separate unloading stiffness.

        Returns
        -------
        str
            APDL commands for gasket element setup.
        """
        lines = [
            "! --- Gasket Elements (INTER195) ---",
            f"! Type: {gasket.gasket_type}",
            f"! OD={gasket.gasket_od_mm} mm, ID={gasket.gasket_id_mm} mm",
            f"! Thickness: {gasket.initial_thickness_mm} mm",
            "",
            "ET,20,INTER195  ! Gasket element",
            "KEYOPT,20,1,0   ! No tensile capacity",
            "KEYOPT,20,2,0   ! Pressure-closure behavior",
            "",
            "! Gasket material: compression curve",
            "TB,GASKET,20,,,PARA",
            f"TBDATA,1,{gasket.initial_thickness_mm}",
            f"TBDATA,2,{gasket.compression_modulus_mpa}",
            f"TBDATA,3,{gasket.unloading_modulus_mpa}",
            "",
            "! Gasket compression data points",
            "TB,GASKET,20,1,,COMP",
        ]

        # Generate a simplified compression curve
        max_closure = gasket.initial_thickness_mm * gasket.max_compression_ratio
        num_points = 5
        for i in range(num_points + 1):
            closure = max_closure * i / num_points
            # Simple bilinear response
            stress = gasket.compression_modulus_mpa * closure / gasket.initial_thickness_mm
            lines.append(f"TBPT,,{closure:.4f},{stress:.2f}")

        lines.extend([
            "",
            f"! Seating stress (y): {gasket.seating_stress_mpa} MPa",
            f"! Gasket factor (m): {gasket.gasket_factor_m}",
            f"! Operating gasket stress = m * P",
            "",
        ])

        return "\n".join(lines)

    def generate_flange_model(self, flange: FlangeConfig) -> str:
        """Generate APDL geometry commands for flange model.

        Returns
        -------
        str
            APDL commands for flange geometry.
        """
        lines = [
            "/PREP7",
            f"! --- Flange Model ({flange.flange_type}) ---",
            f"! Class {flange.rating_class}",
            f"! OD={flange.flange_od_mm} mm, ID={flange.flange_id_mm} mm",
            f"! Thickness={flange.flange_thickness_mm} mm",
            "",
            "! Flange body",
            f"CYL4,0,0,{flange.flange_id_mm / 2},{flange.flange_od_mm / 2},"
            f",,{flange.flange_thickness_mm}",
            "",
        ]

        if flange.raised_face_height_mm > 0:
            lines.extend([
                "! Raised face",
                f"CYL4,0,0,{flange.flange_id_mm / 2},"
                f"{flange.raised_face_od_mm / 2},,,"
                f"{flange.raised_face_height_mm}",
                "",
            ])

        if flange.flange_type == "weld_neck" and flange.hub_length_mm > 0:
            lines.extend([
                "! Hub (weld neck)",
                f"CYL4,0,0,{flange.flange_id_mm / 2},"
                f"{flange.flange_id_mm / 2 + flange.hub_thickness_mm},,,"
                f"{flange.hub_length_mm}",
                "",
            ])

        lines.extend([
            "! Material assignment",
            f"MAT,{flange.material_id}",
            "",
        ])

        return "\n".join(lines)

    def generate_multistep_solve(
        self,
        circle: BoltCircleConfig,
        design_pressure_mpa: float = 10.0,
    ) -> str:
        """Generate multi-load-step solution for flange analysis.

        Load steps:
            1. Bolt pretension only
            2. Pretension + design pressure
            3. Pretension + hydrotest (1.3 × design)

        Returns
        -------
        str
            APDL commands for multi-step solution.
        """
        hydrotest = 1.3 * design_pressure_mpa

        lines = [
            "/SOLU",
            "ANTYPE,0  ! Static",
            "NLGEOM,ON",
            "",
            "! ===== LOAD STEP 1: Bolt Pretension Only =====",
            f"NSUBST,20,100,5",
            "AUTOTS,ON",
            "TIME,1.0",
        ]

        for i in range(circle.num_bolts):
            section_id = 100 + i
            lines.append(
                f"SLOAD,{section_id},,FORC,{circle.bolt.pretension_force_n}"
            )

        lines.extend([
            "LSWRITE,1",
            "",
            "! ===== LOAD STEP 2: Pretension + Design Pressure =====",
            "TIME,2.0",
        ])

        # Lock bolt lengths (convert to displacement control)
        for i in range(circle.num_bolts):
            section_id = 100 + i
            lines.append(f"SLOAD,{section_id},,LOCK")

        lines.extend([
            f"SFA,ALL,1,PRES,{design_pressure_mpa}",
            "LSWRITE,2",
            "",
            "! ===== LOAD STEP 3: Pretension + Hydrotest =====",
            "TIME,3.0",
            f"SFA,ALL,1,PRES,{hydrotest:.2f}",
            "LSWRITE,3",
            "",
            "LSSOLVE,1,3",
            "FINISH",
            "",
        ])

        return "\n".join(lines)
