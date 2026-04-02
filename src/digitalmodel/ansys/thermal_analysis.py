# ABOUTME: Thermal analysis APDL command generation — steady-state and transient
# ABOUTME: Convection, radiation, thermal coupling for structural analysis

"""
Thermal Analysis
================

Generates ANSYS APDL commands for thermal analysis:

    - Steady-state thermal analysis (heat conduction, convection, radiation)
    - Transient thermal analysis (temperature ramp-up/cool-down)
    - Convection boundary conditions (film coefficients)
    - Radiation boundary conditions (surface-to-surface, to-ambient)
    - Thermal gradient extraction for structural coupling
    - Thermal-structural sequential coupling setup

Typical applications: pressure vessel startup/shutdown cycles,
heat exchanger thermal analysis, insulated equipment, fire scenarios.

Default values are for typical offshore process equipment:
    - Operating temperature: 200°C
    - Ambient temperature: 25°C (air) / 5°C (seawater)
    - Convection film coefficients per standard correlations
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Optional


# ---------------------------------------------------------------------------
# Configuration dataclasses
# ---------------------------------------------------------------------------

@dataclass
class ThermalMaterialConfig:
    """Thermal material properties."""
    mat_id: int = 1
    name: str = "Carbon_Steel_SA516"
    thermal_conductivity_w_mk: float = 50.0  # W/(m·K) at 200°C
    specific_heat_j_kgk: float = 480.0  # J/(kg·K)
    density_kg_m3: float = 7850.0
    emissivity: float = 0.8  # oxidized steel surface


@dataclass
class ConvectionBC:
    """Convection boundary condition."""
    surface_id: int = 1
    surface_type: str = "AREA"  # AREA, LINE, NODE
    film_coefficient_w_m2k: float = 10.0  # W/(m²·K)
    bulk_temperature_c: float = 25.0
    description: str = "Natural convection in air"


@dataclass
class RadiationBC:
    """Radiation boundary condition."""
    surface_id: int = 1
    surface_type: str = "AREA"
    emissivity: float = 0.8
    ambient_temperature_c: float = 25.0
    stefan_boltzmann: float = 5.67e-8  # W/(m²·K⁴)
    form_factor: float = 1.0
    enclosure_id: int = 0  # 0 = to ambient space


@dataclass
class HeatFluxBC:
    """Heat flux or heat generation boundary condition."""
    surface_id: int = 1
    surface_type: str = "AREA"
    heat_flux_w_m2: float = 0.0  # surface heat flux
    heat_generation_w_m3: float = 0.0  # volumetric heat gen
    bc_type: str = "flux"  # flux, generation, temperature


@dataclass
class TransientConfig:
    """Transient thermal analysis timing parameters."""
    total_time_s: float = 3600.0  # 1 hour
    time_step_s: float = 60.0  # 1 minute
    min_time_step_s: float = 1.0
    max_time_step_s: float = 300.0
    initial_temperature_c: float = 25.0
    auto_time_stepping: bool = True


@dataclass
class ThermalAnalysisConfig:
    """Complete thermal analysis configuration."""
    analysis_type: str = "steady_state"  # steady_state, transient
    materials: list[ThermalMaterialConfig] = field(default_factory=list)
    convection_bcs: list[ConvectionBC] = field(default_factory=list)
    radiation_bcs: list[RadiationBC] = field(default_factory=list)
    heat_flux_bcs: list[HeatFluxBC] = field(default_factory=list)
    transient: TransientConfig = field(default_factory=TransientConfig)
    element_type: str = "SOLID70"  # SOLID70 (3D), PLANE55 (2D), SHELL131
    coupled_structural: bool = False


# ---------------------------------------------------------------------------
# Thermal analysis generator
# ---------------------------------------------------------------------------

class ThermalAnalysisGenerator:
    """Generate APDL commands for thermal analysis."""

    def generate_thermal_element_type(
        self, config: ThermalAnalysisConfig
    ) -> str:
        """Generate thermal element type commands.

        Returns
        -------
        str
            APDL commands for thermal element definition.
        """
        lines = [
            "/PREP7",
            f"! --- Thermal Element Type ---",
            f"ET,1,{config.element_type}",
        ]

        if config.element_type == "SOLID70":
            lines.append("! 3D 8-node thermal solid")
        elif config.element_type == "PLANE55":
            lines.append("! 2D 4-node thermal solid")
            lines.append("KEYOPT,1,3,1  ! Axisymmetric")
        elif config.element_type == "SHELL131":
            lines.append("! 4-node thermal shell")
            lines.append("KEYOPT,1,3,0  ! Through-thickness DOFs")

        lines.append("")
        return "\n".join(lines)

    def generate_thermal_materials(
        self, materials: list[ThermalMaterialConfig]
    ) -> str:
        """Generate APDL material property commands for thermal analysis.

        Returns
        -------
        str
            APDL MP commands for thermal properties.
        """
        lines = ["! --- Thermal Material Properties ---"]

        for mat in materials:
            # Convert density: kg/m3 to tonne/mm3 for consistency
            dens = mat.density_kg_m3 * 1e-12

            lines.extend([
                f"! Material {mat.mat_id}: {mat.name}",
                f"MP,KXX,{mat.mat_id},{mat.thermal_conductivity_w_mk}",
                f"MP,C,{mat.mat_id},{mat.specific_heat_j_kgk}",
                f"MP,DENS,{mat.mat_id},{dens:.6E}",
                f"MP,EMIS,{mat.mat_id},{mat.emissivity}",
                "",
            ])

        return "\n".join(lines)

    def generate_convection_bcs(
        self, convection_bcs: list[ConvectionBC]
    ) -> str:
        """Generate APDL convection boundary condition commands.

        Returns
        -------
        str
            APDL SF/SFA commands for convection.
        """
        lines = ["! --- Convection Boundary Conditions ---"]

        for bc in convection_bcs:
            lines.append(f"! {bc.description}")
            lines.append(
                f"! h={bc.film_coefficient_w_m2k} W/(m²·K), "
                f"T_bulk={bc.bulk_temperature_c}°C"
            )
            if bc.surface_type == "AREA":
                lines.append(
                    f"SFA,{bc.surface_id},1,CONV,"
                    f"{bc.film_coefficient_w_m2k},{bc.bulk_temperature_c}"
                )
            elif bc.surface_type == "LINE":
                lines.append(
                    f"SFL,{bc.surface_id},CONV,"
                    f"{bc.film_coefficient_w_m2k},{bc.bulk_temperature_c}"
                )
            else:
                lines.append(
                    f"SF,{bc.surface_id},CONV,"
                    f"{bc.film_coefficient_w_m2k},{bc.bulk_temperature_c}"
                )
            lines.append("")

        return "\n".join(lines)

    def generate_radiation_bcs(
        self, radiation_bcs: list[RadiationBC]
    ) -> str:
        """Generate APDL radiation boundary condition commands.

        Supports radiation to ambient space and surface-to-surface
        radiation with form factors.

        Returns
        -------
        str
            APDL commands for radiation BCs.
        """
        lines = ["! --- Radiation Boundary Conditions ---"]

        for bc in radiation_bcs:
            T_amb_k = bc.ambient_temperature_c + 273.15
            lines.extend([
                f"! Radiation: emissivity={bc.emissivity}, "
                f"T_amb={bc.ambient_temperature_c}°C ({T_amb_k:.1f} K)",
                f"STEF,{bc.stefan_boltzmann:.4E}  ! Stefan-Boltzmann constant",
                f"TOFFST,273.15  ! Offset to convert °C to K",
                "",
            ])

            if bc.enclosure_id == 0:
                # Radiation to space (ambient)
                if bc.surface_type == "AREA":
                    lines.append(
                        f"SFA,{bc.surface_id},1,RDSF,"
                        f"{bc.emissivity},{bc.form_factor}"
                    )
                else:
                    lines.append(
                        f"SF,{bc.surface_id},RDSF,"
                        f"{bc.emissivity},{bc.form_factor}"
                    )
            else:
                # Surface-to-surface radiation (enclosure)
                lines.extend([
                    f"RDEC,{bc.enclosure_id},{bc.surface_id},"
                    f"{bc.emissivity},{bc.form_factor}",
                ])
            lines.append("")

        return "\n".join(lines)

    def generate_heat_flux_bcs(
        self, heat_flux_bcs: list[HeatFluxBC]
    ) -> str:
        """Generate APDL heat flux/generation commands.

        Returns
        -------
        str
            APDL commands for heat flux BCs.
        """
        lines = ["! --- Heat Flux / Generation ---"]

        for bc in heat_flux_bcs:
            if bc.bc_type == "flux":
                lines.append(f"! Surface heat flux: {bc.heat_flux_w_m2} W/m²")
                if bc.surface_type == "AREA":
                    lines.append(f"SFA,{bc.surface_id},1,HFLUX,{bc.heat_flux_w_m2}")
                else:
                    lines.append(f"SF,{bc.surface_id},HFLUX,{bc.heat_flux_w_m2}")
            elif bc.bc_type == "generation":
                lines.append(
                    f"! Volumetric heat gen: {bc.heat_generation_w_m3} W/m³"
                )
                lines.append(f"BFE,ALL,HGEN,,{bc.heat_generation_w_m3}")
            elif bc.bc_type == "temperature":
                lines.append(f"! Fixed temperature: {bc.heat_flux_w_m2}°C")
                if bc.surface_type == "AREA":
                    lines.append(f"DA,{bc.surface_id},TEMP,{bc.heat_flux_w_m2}")
                else:
                    lines.append(f"D,{bc.surface_id},TEMP,{bc.heat_flux_w_m2}")
            lines.append("")

        return "\n".join(lines)

    def generate_steady_state_solution(self) -> str:
        """Generate APDL commands for steady-state thermal solution.

        Returns
        -------
        str
            APDL solution commands.
        """
        lines = [
            "/SOLU",
            "! --- Steady-State Thermal Solution ---",
            "ANTYPE,0  ! Static (steady-state thermal)",
            "NSUBST,10,50,5",
            "AUTOTS,ON",
            "CNVTOL,HEAT,,0.001",
            "",
            "SOLVE",
            "FINISH",
            "",
        ]
        return "\n".join(lines)

    def generate_transient_solution(
        self, config: TransientConfig
    ) -> str:
        """Generate APDL commands for transient thermal solution.

        Returns
        -------
        str
            APDL solution commands for transient analysis.
        """
        lines = [
            "/SOLU",
            "! --- Transient Thermal Solution ---",
            "ANTYPE,4  ! Transient analysis",
            f"TRNOPT,FULL  ! Full transient",
            "",
            f"! Initial temperature: {config.initial_temperature_c}°C",
            f"TUNIF,{config.initial_temperature_c}",
            "",
            f"! Time stepping",
            f"TIME,{config.total_time_s}",
            f"DELTIM,{config.time_step_s},{config.min_time_step_s},"
            f"{config.max_time_step_s}",
        ]

        if config.auto_time_stepping:
            lines.append("AUTOTS,ON")
        else:
            lines.append("AUTOTS,OFF")

        lines.extend([
            "",
            "CNVTOL,HEAT,,0.001",
            "OUTRES,ALL,ALL",
            "",
            "SOLVE",
            "FINISH",
            "",
        ])

        return "\n".join(lines)

    def generate_thermal_results_extraction(self) -> str:
        """Generate APDL commands for thermal results extraction.

        Extracts temperature field, heat flux, and thermal gradients.

        Returns
        -------
        str
            APDL post-processing commands.
        """
        lines = [
            "/POST1",
            "SET,LAST",
            "! --- Thermal Results ---",
            "",
            "! Temperature distribution",
            "PRNSOL,TEMP",
            "PLNSOL,TEMP",
            "",
            "! Thermal gradient",
            "PRNSOL,TG,SUM",
            "",
            "! Heat flux",
            "PRNSOL,TF,SUM",
            "",
            "! Temperature extremes",
            "*GET,T_MAX,NODE,0,TEMP,MAX",
            "*GET,T_MIN,NODE,0,TEMP,MIN",
            "*MSG,INFO,T_MAX,T_MIN",
            "Temperature range: %G to %G °C",
            "",
            "FINISH",
            "",
        ]
        return "\n".join(lines)

    def generate_thermal_structural_coupling(
        self, thermal_results_file: str = "thermal.rth"
    ) -> str:
        """Generate APDL commands for sequential thermal-structural coupling.

        Reads thermal results and applies as body loads for structural analysis.

        Parameters
        ----------
        thermal_results_file : str
            Filename of thermal results database.

        Returns
        -------
        str
            APDL commands for coupled analysis.
        """
        lines = [
            "! ===================================================",
            "! Sequential Thermal-Structural Coupling",
            "! ===================================================",
            "",
            "! --- Step 1: Switch to structural element types ---",
            "/PREP7",
            "ETCHG,TTS  ! Convert thermal elements to structural",
            "",
            "! --- Step 2: Define structural material properties ---",
            "! (Ensure EX, NUXY, ALPX are defined for all materials)",
            "",
            "! --- Step 3: Read thermal results as body loads ---",
            f"LDREAD,TEMP,,,,,,{thermal_results_file}",
            "",
            "! --- Step 4: Structural boundary conditions ---",
            "! (Apply supports and mechanical loads)",
            "",
            "! --- Step 5: Solve structural ---",
            "/SOLU",
            "ANTYPE,0  ! Static structural",
            "NLGEOM,ON",
            f"TREF,25  ! Reference temperature",
            "",
            "! Thermal strains from temperature field",
            "BFUNIF,TEMP,0  ! Clear uniform temp (use nodal temps)",
            "",
            "SOLVE",
            "FINISH",
            "",
        ]
        return "\n".join(lines)

    def generate_full_thermal_script(
        self, config: ThermalAnalysisConfig
    ) -> str:
        """Generate a complete thermal analysis APDL script.

        Parameters
        ----------
        config : ThermalAnalysisConfig
            Complete thermal analysis configuration.

        Returns
        -------
        str
            Full APDL script text.
        """
        sections = [
            "/BATCH",
            "/TITLE,Thermal Analysis",
            "",
            self.generate_thermal_element_type(config),
            self.generate_thermal_materials(config.materials),
        ]

        if config.convection_bcs:
            sections.append(self.generate_convection_bcs(config.convection_bcs))
        if config.radiation_bcs:
            sections.append(self.generate_radiation_bcs(config.radiation_bcs))
        if config.heat_flux_bcs:
            sections.append(self.generate_heat_flux_bcs(config.heat_flux_bcs))

        if config.analysis_type == "transient":
            sections.append(self.generate_transient_solution(config.transient))
        else:
            sections.append(self.generate_steady_state_solution())

        sections.append(self.generate_thermal_results_extraction())

        if config.coupled_structural:
            sections.append(self.generate_thermal_structural_coupling())

        return "\n".join(sections)
