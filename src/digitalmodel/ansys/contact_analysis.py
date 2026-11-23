# ABOUTME: Contact analysis APDL command generation — CONTA174/TARGE170, friction, gaskets
# ABOUTME: Surface-to-surface contact setup for FEA; no ANSYS installation needed

"""
Contact Analysis
================

Generates ANSYS APDL commands for contact analysis setup:

    - Surface-to-surface contact pairs (CONTA174/TARGE170)
    - Node-to-surface contact (CONTA175/TARGE170)
    - Friction modeling (Coulomb, penalty, augmented Lagrangian)
    - Interference fit / shrink fit modeling
    - Seal and gasket compression
    - Bolt bearing contact

Typical applications: flanged joints, shrink-fitted hubs, seal grooves,
equipment supports, pipe-clamp interfaces.

All functions return APDL command text.  No ANSYS installation required.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Optional


# ---------------------------------------------------------------------------
# Configuration dataclasses
# ---------------------------------------------------------------------------

@dataclass
class ContactPairConfig:
    """Contact pair definition for APDL contact elements."""
    pair_id: int = 1
    contact_type: str = "surface_to_surface"  # surface_to_surface, node_to_surface
    algorithm: str = "augmented_lagrangian"  # penalty, augmented_lagrangian, mpc, lagrangian
    friction_coefficient: float = 0.15
    contact_stiffness_factor: float = 1.0
    penetration_tolerance: float = 0.1  # fraction of element size
    pinball_radius: float = 0.0  # 0 = auto
    initial_contact: str = "auto"  # auto, include_initial, exclude_initial
    contact_surface_id: int = 1  # area/component ID for contact side
    target_surface_id: int = 2  # area/component ID for target side
    description: str = ""


@dataclass
class InterferenceFitConfig:
    """Interference fit modeling parameters."""
    pair_id: int = 1
    radial_interference_mm: float = 0.05
    inner_radius_mm: float = 100.0
    outer_radius_mm: float = 200.0
    ramp_substeps: int = 10
    contact_surface_id: int = 1
    target_surface_id: int = 2


@dataclass
class SealConfig:
    """Seal/O-ring contact configuration."""
    seal_type: str = "o_ring"  # o_ring, flat_gasket, lip_seal
    material_id: int = 5
    initial_compression_mm: float = 0.5
    seal_od_mm: float = 120.0
    seal_id_mm: float = 100.0
    shore_hardness: float = 70.0  # Shore A
    friction_coefficient: float = 0.3
    contact_surface_id: int = 1
    target_surface_id: int = 2


@dataclass
class ContactResults:
    """Expected contact result quantities."""
    max_contact_pressure_mpa: float = 0.0
    max_penetration_mm: float = 0.0
    contact_area_mm2: float = 0.0
    total_normal_force_n: float = 0.0
    total_friction_force_n: float = 0.0
    gap_status: str = ""  # open, closed, sliding, sticking


# ---------------------------------------------------------------------------
# Contact analysis generator
# ---------------------------------------------------------------------------

class ContactAnalysisGenerator:
    """Generate APDL commands for contact analysis setup."""

    def generate_contact_pair(self, config: ContactPairConfig) -> str:
        """Generate APDL commands for a contact pair.

        Creates contact and target element types, real constants,
        and keyoptions for the specified contact algorithm.

        Returns
        -------
        str
            APDL commands for contact pair definition.
        """
        # Element type IDs: contact=pair_id*10, target=pair_id*10+1
        et_contact = config.pair_id * 10
        et_target = config.pair_id * 10 + 1

        if config.contact_type == "node_to_surface":
            contact_elem = "CONTA175"
        else:
            contact_elem = "CONTA174"

        target_elem = "TARGE170"

        # Algorithm keyopt mapping
        algo_map = {
            "penalty": 0,
            "augmented_lagrangian": 1,
            "mpc": 2,
            "lagrangian": 4,
        }
        algo_val = algo_map.get(config.algorithm, 1)

        # Initial contact keyopt(9)
        init_map = {
            "auto": 0,
            "include_initial": 1,
            "exclude_initial": 4,
        }
        init_val = init_map.get(config.initial_contact, 0)

        lines = [
            "/PREP7",
            f"! --- Contact Pair {config.pair_id}: {config.description} ---",
            f"! Algorithm: {config.algorithm}",
            f"! Friction: {config.friction_coefficient}",
            "",
            f"! Contact element",
            f"ET,{et_contact},{contact_elem}",
            f"KEYOPT,{et_contact},2,{algo_val}  ! Contact algorithm",
            f"KEYOPT,{et_contact},5,0  ! Close gap/reduce penetration",
            f"KEYOPT,{et_contact},9,{init_val}  ! Initial contact",
            f"KEYOPT,{et_contact},10,2  ! Update contact stiffness",
            f"KEYOPT,{et_contact},12,0  ! Standard contact",
            "",
            f"! Target element",
            f"ET,{et_target},{target_elem}",
            "",
            f"! Real constants for contact pair",
            f"R,{config.pair_id}",
            f"RMORE,,,{config.contact_stiffness_factor},"
            f"{config.penetration_tolerance},,",
            "",
            f"! Friction coefficient",
            f"MP,MU,{config.pair_id},{config.friction_coefficient}",
            "",
        ]

        if config.pinball_radius > 0:
            lines.append(f"RMORE,{config.pinball_radius}  ! Pinball radius")
            lines.append("")

        # Generate surface assignments
        lines.extend([
            f"! Assign contact surface (area {config.contact_surface_id})",
            f"ASEL,S,AREA,,{config.contact_surface_id}",
            f"NSLA,S,1",
            f"TYPE,{et_contact}",
            f"REAL,{config.pair_id}",
            f"ESURF",
            "",
            f"! Assign target surface (area {config.target_surface_id})",
            f"ASEL,S,AREA,,{config.target_surface_id}",
            f"NSLA,S,1",
            f"TYPE,{et_target}",
            f"REAL,{config.pair_id}",
            f"ESURF",
            "",
            "ALLSEL",
            "",
        ])

        return "\n".join(lines)

    def generate_interference_fit(
        self, config: InterferenceFitConfig
    ) -> str:
        """Generate APDL commands for interference/shrink fit contact.

        Uses ramped initial penetration approach to model
        the press-fit assembly process.

        Returns
        -------
        str
            APDL commands for interference fit setup.
        """
        lines = [
            f"! --- Interference Fit (Pair {config.pair_id}) ---",
            f"! Radial interference: {config.radial_interference_mm} mm",
            f"! R_inner={config.inner_radius_mm} mm, "
            f"R_outer={config.outer_radius_mm} mm",
            "",
            "! Contact pair with interference",
        ]

        # Generate a standard contact pair first
        cp = ContactPairConfig(
            pair_id=config.pair_id,
            algorithm="augmented_lagrangian",
            friction_coefficient=0.15,
            initial_contact="include_initial",
            contact_surface_id=config.contact_surface_id,
            target_surface_id=config.target_surface_id,
            description="Interference fit",
        )
        lines.append(self.generate_contact_pair(cp))

        # Add interference-specific settings
        et_contact = config.pair_id * 10
        lines.extend([
            "! Interference fit settings",
            f"KEYOPT,{et_contact},9,1  ! Include initial interference",
            "",
            "! Solution: ramp interference over substeps",
            "/SOLU",
            "ANTYPE,0",
            "NLGEOM,ON",
            f"NSUBST,{config.ramp_substeps},{config.ramp_substeps * 5},"
            f"{config.ramp_substeps // 2}",
            "AUTOTS,ON",
            "CNVTOL,F,,0.001",
            "",
            f"! Initial penetration = {config.radial_interference_mm} mm",
            f"! (Geometry overlap models the interference)",
            "",
            "SOLVE",
            "FINISH",
            "",
        ])

        return "\n".join(lines)

    def generate_seal_contact(self, seal: SealConfig) -> str:
        """Generate APDL commands for seal/O-ring contact modeling.

        Includes hyperelastic material definition for elastomeric seals.

        Returns
        -------
        str
            APDL commands for seal contact setup.
        """
        # Approximate Mooney-Rivlin constants from Shore A hardness
        # C10 ≈ 0.5 * (hardness/100)^2 MPa (simplified approximation)
        c10 = 0.5 * (seal.shore_hardness / 100.0) ** 2
        c01 = c10 * 0.25  # Typical ratio

        lines = [
            f"! --- Seal Contact ({seal.seal_type}) ---",
            f"! OD={seal.seal_od_mm} mm, ID={seal.seal_id_mm} mm",
            f"! Shore A hardness: {seal.shore_hardness}",
            f"! Initial compression: {seal.initial_compression_mm} mm",
            "",
            f"! Seal material (Mooney-Rivlin hyperelastic)",
            f"! Material ID: {seal.material_id}",
            f"TB,HYPER,{seal.material_id},1,2,MOON",
            f"TBDATA,1,{c10:.4f},{c01:.4f}",
            "",
            f"MP,DENS,{seal.material_id},1.2E-09  ! Rubber density",
            "",
            "! Contact pair for seal surfaces",
        ]

        cp = ContactPairConfig(
            pair_id=seal.material_id,
            algorithm="augmented_lagrangian",
            friction_coefficient=seal.friction_coefficient,
            initial_contact="include_initial",
            contact_surface_id=seal.contact_surface_id,
            target_surface_id=seal.target_surface_id,
            description=f"Seal ({seal.seal_type})",
        )
        lines.append(self.generate_contact_pair(cp))

        return "\n".join(lines)

    def generate_contact_results_extraction(
        self, pair_ids: list[int]
    ) -> str:
        """Generate APDL commands to extract contact result quantities.

        Returns
        -------
        str
            APDL commands for contact status and pressure extraction.
        """
        lines = [
            "/POST1",
            "SET,LAST",
            "! --- Contact Results Extraction ---",
            "",
        ]

        for pid in pair_ids:
            et_contact = pid * 10
            lines.extend([
                f"! Contact Pair {pid}",
                f"ESEL,S,TYPE,,{et_contact}",
                "",
                "! Contact pressure",
                f"ETABLE,CPRES_{pid},CONT,PRES",
                f"PRETAB,CPRES_{pid}",
                "",
                "! Contact penetration",
                f"ETABLE,CPEN_{pid},CONT,PENE",
                f"PRETAB,CPEN_{pid}",
                "",
                "! Contact status (0=open, 1=closed, 2=sliding, 3=sticking)",
                f"ETABLE,CSTAT_{pid},CONT,STAT",
                f"PRETAB,CSTAT_{pid}",
                "",
                "! Friction stress",
                f"ETABLE,CFRIC_{pid},CONT,SFRIC",
                f"PRETAB,CFRIC_{pid}",
                "",
                f"! Summary for pair {pid}",
                f"*GET,CPMAX_{pid},ELEM,0,ETAB,CPRES_{pid},MAX",
                f"*GET,PENMAX_{pid},ELEM,0,ETAB,CPEN_{pid},MAX",
                f"*MSG,INFO,CPMAX_{pid},PENMAX_{pid}",
                f"Contact pair {pid}: Max pressure=%G MPa, Max penetration=%G mm",
                "",
            ])

        lines.append("ALLSEL")
        lines.append("")
        return "\n".join(lines)

    def generate_frictional_contact_solution(
        self, config: ContactPairConfig
    ) -> str:
        """Generate APDL solution settings optimized for frictional contact.

        Returns
        -------
        str
            APDL solution commands for contact convergence.
        """
        lines = [
            "/SOLU",
            "! --- Contact Solution Settings ---",
            "ANTYPE,0  ! Static",
            "NLGEOM,ON",
            "",
            "! Contact-specific convergence settings",
            "NSUBST,20,100,5",
            "AUTOTS,ON",
            "CNVTOL,F,,0.005,2",
            "CNVTOL,M,,0.005,2",
            "",
            "! Line search for contact stability",
            "LNSRCH,ON",
            "",
            "! Contact diagnostics",
            "CNCHECK,AUTO",
            "",
            f"! Friction: mu={config.friction_coefficient}",
            "! Use unsymmetric solver for friction > 0",
        ]

        if config.friction_coefficient > 0:
            lines.append("NROPT,UNSYM  ! Unsymmetric Newton-Raphson")

        lines.extend([
            "",
            "SOLVE",
            "FINISH",
            "",
        ])

        return "\n".join(lines)
