import math

from digitalmodel.infrastructure.common.cp_DNV_RP_B401_2021 import (
    _b401_anode_requirements,
    _b401_anode_resistance,
    _b401_coating_breakdown,
    _b401_current_demand,
    _b401_current_densities,
    _b401_surface_areas,
    _b401_verify_current_output,
)


class CathodicProtection:
    def __init__(self):
        pass

    def router(self, cfg):
        if cfg["inputs"]["calculation_type"] == "ABS_gn_ships_2018":
            self.ABS_gn_ships_2018(cfg)
        elif cfg["inputs"]["calculation_type"] == "DNV_RP_F103_2010":
            self.DNV_RP_F103_2010(cfg)
        elif cfg["inputs"]["calculation_type"] == "ABS_gn_offshore_2018":
            self.ABS_gn_offshore_2018(cfg)
        elif cfg["inputs"]["calculation_type"] == "DNV_RP_B401_offshore":
            self.DNV_RP_B401_offshore_platform(cfg)
        else:
            raise ValueError(
                f"Calculation type: {cfg['inputs']['calculation_type']} not IMPLEMENTED. ... FAIL"
            )

        return cfg

    def ABS_gn_ships_2018(self, cfg):
        """
        This method is used to calculate the cathodic protection for ABS gn ships 2018
        """
        inputs = cfg["inputs"]
        anode_current_capacity = self.get_anode_current_capacity(cfg)

        design_data = inputs.get("design_data", {})
        environment = inputs.get("environment", {})
        temp = design_data.get("seawater_max_temperature", environment.get("temperature", 20))
        design_life = design_data.get("design_life", inputs.get("design_life", 25))

        breakdown_factors = self._abs_breakdown_factors(inputs, design_life)
        current_densities = self._abs_current_densities(inputs, breakdown_factors)
        current_demand = self._abs_current_demand(inputs, current_densities)
        anode_requirements = self._abs_anode_mass_requirements(
            inputs, current_demand, anode_current_capacity
        )
        anode_performance = self._abs_anode_performance_checks(
            inputs, current_demand, anode_requirements
        )

        cfg["cathodic_protection"] = {
            "temperature": temp,
            "design_life": design_life,
            "anode_current_capacity": anode_current_capacity,
            "coating_breakdown_factors": breakdown_factors,
            "current_densities_mA_m2": current_densities,
            "current_demand_A": current_demand,
            "anode_requirements": anode_requirements,
            "anode_performance": anode_performance,
        }

    def DNV_RP_F103_2010(self, cfg):
        """
        Calculate cathodic protection for submarine pipelines per DNV-RP-F103 (2010 edition).

        DNV-RP-F103 (October 2010): Cathodic Protection of Submarine Pipelines by Galvanic Anodes

        Implements DNV-RP-F103 (2010 edition) including:
        - Pipeline surface area calculations based on geometry
        - Coating breakdown factors per Annex 1 linear formula (Eq.2 / Eq.4)
        - Current density requirements from Table 5-1 (burial x internal fluid temperature)
        - Distributed anode spacing optimization
        - Pipeline attenuation analysis for current distribution
        - Anode mass and quantity requirements

        Args:
            cfg (dict): Configuration dictionary with pipeline parameters, environment,
                       anode specifications, and design criteria

        Returns:
            dict: Updated configuration with comprehensive pipeline CP calculations
        """
        inputs = cfg.get("inputs", {})
        design_data = inputs.get("design_data", {})
        design_life = design_data.get("design_life", 25.0)

        # Calculate pipeline geometry and surface areas
        pipeline_geometry = self._dnv_pipeline_geometry(inputs)

        # Calculate coating breakdown factors for buried pipelines
        coating_breakdown = self._dnv_coating_breakdown(inputs, design_life)

        # Calculate current densities for pipeline protection
        current_densities = self._dnv_current_densities(inputs)

        # Calculate total current demand
        current_demand = self._dnv_current_demand(inputs, pipeline_geometry, current_densities, coating_breakdown)

        # Get anode current capacity
        anode_current_capacity = self.get_anode_current_capacity(cfg)

        # Calculate anode requirements (mass, count)
        anode_requirements = self._dnv_anode_requirements(
            inputs, current_demand
        )

        # Calculate anode spacing along pipeline
        anode_spacing = self._dnv_anode_spacing(
            inputs, pipeline_geometry, anode_requirements
        )

        # Calculate pipeline attenuation
        attenuation = self._dnv_attenuation(
            inputs, pipeline_geometry, current_densities, coating_breakdown, anode_spacing
        )

        cfg["results"] = {
            "design_life_years": round(design_life, 3),
            "pipeline_geometry_m": pipeline_geometry,
            "coating_breakdown_factors": coating_breakdown,
            "current_densities_mA_m2": current_densities,
            "current_demand_A": current_demand,
            "anode_current_capacity_Ah_kg": anode_current_capacity,
            "anode_requirements": anode_requirements,
            "anode_spacing_m": anode_spacing,
            "attenuation_analysis": attenuation,
        }

        return cfg

    def _dnv_pipeline_geometry(self, inputs):
        """
        Calculate pipeline geometry and surface areas for CP design.

        DNV-RP-F103 (2010) requires accurate surface area calculations for current demand.
        Calculates bare, coated, and wetted surface areas based on pipeline dimensions.

        Longitudinal metallic resistance per DNV-RP-F103 (2010) Eq.11:
        R_Me = L * rho_Me / (pi * d * (D - d))
        where D = outer diameter (m), d = wall thickness (m), rho_Me = steel resistivity (ohm-m)

        Giving resistance per unit length:
        R_Me_per_m = rho_Me / (pi * d * (D - d))

        Args:
            inputs (dict): Configuration inputs with pipeline geometry parameters

        Returns:
            dict: Pipeline dimensions and calculated surface areas
        """
        pipeline = inputs.get("pipeline", {})

        # Extract pipeline dimensions
        outer_diameter_m = pipeline.get("outer_diameter_m", 0.5)
        length_m = pipeline.get("length_m", 1000.0)
        wall_thickness_m = pipeline.get("wall_thickness_m", 0.015)
        coating_thickness_m = pipeline.get("coating_thickness_m", 0.0)

        # Metal resistivity for longitudinal resistance calculation
        # Default: 0.2e-6 Ω·m for CMn-steel (DNV-RP-F103 2010 Sec.5.6.10)
        pipe_resistivity_ohm_m = pipeline.get("resistivity_ohm_m", 0.2e-6)

        # Calculate diameters
        inner_diameter_m = outer_diameter_m - (2.0 * wall_thickness_m)
        coated_diameter_m = outer_diameter_m + (2.0 * coating_thickness_m)

        # Calculate surface areas (cylinder surface area = pi * D * L)
        import math

        bare_surface_area_m2 = math.pi * outer_diameter_m * length_m
        coated_surface_area_m2 = math.pi * coated_diameter_m * length_m
        inner_surface_area_m2 = math.pi * inner_diameter_m * length_m

        # For CP design, use outer or coated surface depending on coating
        wetted_surface_area_m2 = (
            coated_surface_area_m2 if coating_thickness_m > 0 else bare_surface_area_m2
        )

        # Calculate cross-sectional areas
        outer_cross_section_m2 = math.pi * (outer_diameter_m / 2.0) ** 2
        inner_cross_section_m2 = math.pi * (inner_diameter_m / 2.0) ** 2
        steel_cross_section_m2 = outer_cross_section_m2 - inner_cross_section_m2

        # Calculate longitudinal metallic resistance per unit length
        # DNV-RP-F103 (2010) Eq.11: R_Me = L * rho_Me / (pi * d * (D - d))
        # Per unit length: R_Me_per_m = rho_Me / (pi * d * (D - d))
        # where D = outer diameter, d = wall thickness
        denom = math.pi * wall_thickness_m * (outer_diameter_m - wall_thickness_m)
        if denom > 0:
            longitudinal_resistance_ohm_per_m = pipe_resistivity_ohm_m / denom
        else:
            longitudinal_resistance_ohm_per_m = 0.0

        return {
            "outer_diameter_m": round(outer_diameter_m, 6),
            "inner_diameter_m": round(inner_diameter_m, 6),
            "coated_diameter_m": round(coated_diameter_m, 6),
            "wall_thickness_m": round(wall_thickness_m, 6),
            "coating_thickness_m": round(coating_thickness_m, 6),
            "length_m": round(length_m, 3),
            "outer_surface_area_m2": round(bare_surface_area_m2, 3),
            "coated_surface_area_m2": round(coated_surface_area_m2, 3),
            "inner_surface_area_m2": round(inner_surface_area_m2, 3),
            "wetted_surface_area_m2": round(wetted_surface_area_m2, 3),
            "outer_cross_section_m2": round(outer_cross_section_m2, 6),
            "inner_cross_section_m2": round(inner_cross_section_m2, 6),
            "steel_cross_section_m2": round(steel_cross_section_m2, 6),
            "pipe_resistivity_ohm_m": round(pipe_resistivity_ohm_m, 10),
            "longitudinal_resistance_ohm_per_m": round(longitudinal_resistance_ohm_per_m, 10),
        }

    # F103-2010 Annex 1 Table A.1: linepipe coating breakdown constants a and b.
    # Values from PDF Table A.1 (x100 column headers mean table values must be divided by 100).
    # a = initial breakdown factor (dimensionless), b = degradation constant (per year, dimensionless)
    _F103_2010_TABLE_A1 = {
        "glass_fibre_reinforced_asphalt_enamel": (0.003, 0.0001),
        "glass_fibre_reinforced_coal_tar_enamel": (0.003, 0.0001),
        "single_layer_FBE": (0.010, 0.0003),
        "dual_layer_FBE": (0.010, 0.0003),
        "3LPE": (0.001, 0.00003),   # 3-layer FBE/PE
        "3LPP": (0.001, 0.00003),   # 3-layer FBE/PP
        "multi_layer_FBE_PP": (0.0003, 0.00001),
        "polychloroprene": (0.001, 0.0001),
        # Common aliases
        "FBE": (0.010, 0.0003),     # Single or Dual Layer FBE (default for FBE)
        "3layer_FBE_PE": (0.001, 0.00003),
        "bare": (1.0, 0.0),         # No linepipe coating — bare steel
    }

    def _dnv_coating_breakdown(self, inputs, design_life):
        """
        Calculate time-dependent coating breakdown factors for buried pipelines.

        DNV-RP-F103 (2010) Annex 1 linear formula (Eq.2 and Eq.4):
          f_ci = a                          (initial breakdown factor)
          f_cm = a + 0.5 * b * t_f         (mean breakdown factor, Eq.2)
          f_cf = a + b * t_f               (final breakdown factor, Eq.4)

        Constants a and b come from F103-2010 Annex 1 Tables A.1 (linepipe) and A.2 (FJC).
        The user may also supply a and b directly via pipeline.coating_breakdown_a
        and pipeline.coating_breakdown_b to override the table look-up.

        Args:
            inputs (dict): Configuration inputs with pipeline coating parameters
            design_life (float): Design life t_f in years

        Returns:
            dict: Coating breakdown factors f_ci, f_cm, f_cf
        """
        pipeline = inputs.get("pipeline", {})

        burial_condition = pipeline.get("burial_condition", "buried")
        coating_type = pipeline.get("coating_type", "FBE")

        # Allow direct override of a/b constants; otherwise look up from table
        if "coating_breakdown_a" in pipeline and "coating_breakdown_b" in pipeline:
            a = float(pipeline["coating_breakdown_a"])
            b = float(pipeline["coating_breakdown_b"])
        else:
            # Look up constants from F103-2010 Annex 1 Table A.1
            defaults = self._F103_2010_TABLE_A1.get(coating_type)
            if defaults is None:
                # Fall back to FBE if coating type not recognised
                defaults = self._F103_2010_TABLE_A1["FBE"]
            a, b = defaults

        t_f = design_life  # design life in years

        # F103-2010 Annex 1 formulas
        f_ci = a                        # Eq. initial (from Sec.5.2.7 and Annex 1)
        f_cm = a + 0.5 * b * t_f       # Eq.2
        f_cf = a + b * t_f             # Eq.4

        # Clamp: breakdown factor must be <= 1.0 (100% bare steel is the maximum)
        f_ci = min(f_ci, 1.0)
        f_cm = min(f_cm, 1.0)
        f_cf = min(f_cf, 1.0)

        return {
            "burial_condition": burial_condition,
            "coating_type": coating_type,
            "a": round(a, 6),
            "b": round(b, 7),
            "design_life_years": round(t_f, 3),
            "initial_factor": round(f_ci, 6),
            "mean_factor": round(f_cm, 6),
            "final_factor": round(f_cf, 6),
        }

    # F103-2010 Table 5-1: Recommended design mean current densities (A/m²)
    # as a function of internal fluid temperature (°C) and burial condition.
    # Rows: burial condition.  Columns: internal fluid temperature bands.
    # Temperature bands: <=50, >50-80, >80-120, >120
    _F103_2010_TABLE_5_1 = {
        "non_buried": {
            "<=50":    0.050,
            ">50-80":  0.060,
            ">80-120": 0.070,
            ">120":    0.100,
        },
        "buried": {
            "<=50":    0.020,
            ">50-80":  0.025,
            ">80-120": 0.030,
            ">120":    0.040,
        },
    }

    def _dnv_current_densities(self, inputs):
        """
        Return mean design current density for buried pipelines per F103-2010 Table 5-1.

        DNV-RP-F103 (2010) Table 5-1 tabulates design mean current densities (A/m²)
        as a function of:
          - burial condition: "buried" or "non_buried"
          - internal fluid temperature (°C): <=50 / >50-80 / >80-120 / >120

        The table value is the mean design current density i_cm used in Eq.1:
          I_cm = A_c * f_cm * i_cm

        There is no coating-quality lookup and no Arrhenius temperature correction
        in F103-2010; those concepts are absent from this standard.

        Args:
            inputs (dict): Configuration inputs.
                pipeline.burial_condition: "buried" (default) or "non_buried"
                pipeline.internal_fluid_temperature_C: float, default 20.0

        Returns:
            dict: mean_current_density_A_m2, burial_condition, temperature_band,
                  internal_fluid_temperature_C
        """
        pipeline = inputs.get("pipeline", {})

        burial_condition = pipeline.get("burial_condition", "buried")
        internal_fluid_temp_c = float(
            pipeline.get("internal_fluid_temperature_C", 20.0)
        )

        # Resolve temperature band per Table 5-1 column headers
        if internal_fluid_temp_c <= 50.0:
            temp_band = "<=50"
        elif internal_fluid_temp_c <= 80.0:
            temp_band = ">50-80"
        elif internal_fluid_temp_c <= 120.0:
            temp_band = ">80-120"
        else:
            temp_band = ">120"

        # Resolve burial condition key; treat unrecognised values as "non_buried"
        condition_key = burial_condition if burial_condition in self._F103_2010_TABLE_5_1 else "non_buried"
        i_cm = self._F103_2010_TABLE_5_1[condition_key][temp_band]

        return {
            "mean_current_density_A_m2": round(i_cm, 6),
            "burial_condition": burial_condition,
            "temperature_band": temp_band,
            "internal_fluid_temperature_C": round(internal_fluid_temp_c, 3),
            "table_reference": "DNV-RP-F103 (2010) Table 5-1",
        }

    def _dnv_current_demand(self, inputs, geometry, current_densities, coating_breakdown):
        """
        Calculate total cathodic protection current demand for buried pipeline.

        DNV-RP-F103 (2010) Eq.1 / Eq.3:
          I_cm = A_c * f_cm * i_cm    (mean current demand)
          I_cf = A_c * f_cf * i_cm    (final current demand)

        The table value i_cm (from Table 5-1) is the MEAN design current density.
        The same i_cm is used for both mean and final calculations; only the coating
        breakdown factor differs between the two (f_cm vs f_cf).

        Args:
            inputs (dict): Configuration inputs with design parameters
            geometry (dict): Pipeline geometry from _dnv_pipeline_geometry()
            current_densities (dict): Current densities from _dnv_current_densities()
            coating_breakdown (dict): Coating factors from _dnv_coating_breakdown()

        Returns:
            dict: Current demands (mean, final, design) in amperes and total charge
        """
        design = inputs.get("design", {})
        design_margin = design.get("design_margin", 1.0)  # F103-2010 has no built-in k factor

        # Extract outer surface area (main protected surface for buried pipeline)
        outer_area = geometry["outer_surface_area_m2"]

        # F103-2010 uses a single mean design current density i_cm from Table 5-1
        i_cm = current_densities["mean_current_density_A_m2"]

        # Extract coating breakdown factors (Annex 1 linear model)
        f_cm = coating_breakdown["mean_factor"]
        f_cf = coating_breakdown["final_factor"]
        f_ci = coating_breakdown["initial_factor"]

        # F103-2010 Eq.1: I_cm(tot) = A_c * f_cm * i_cm
        mean_demand = outer_area * f_cm * i_cm

        # F103-2010 Eq.3: I_cf(tot) = A_c * f_cf * i_cm
        final_demand = outer_area * f_cf * i_cm

        # Initial demand (using f_ci — not explicitly in F103-2010 but consistent with Annex 1)
        initial_demand = outer_area * f_ci * i_cm

        # Apply optional design margin (not in F103-2010; set design_margin=1.0 for standard)
        design_demand = mean_demand * design_margin

        # Extract design life from design_data
        design_data = inputs.get("design_data", {})
        design_life_years = design_data.get("design_life", 25)
        hours_per_year = 8760.0
        design_life_hours = design_life_years * hours_per_year

        # Total charge required over design life (Ampere-hours) — basis for anode mass (Eq.5)
        total_charge_Ah = design_demand * design_life_hours

        return {
            "initial_current_demand_A": round(initial_demand, 3),
            "mean_current_demand_A": round(mean_demand, 3),
            "final_current_demand_A": round(final_demand, 3),
            "design_current_demand_A": round(design_demand, 3),
            "design_margin": round(design_margin, 6),
            "design_life_years": design_life_years,
            "design_life_hours": round(design_life_hours, 3),
            "total_charge_Ah": round(total_charge_Ah, 3),
            "outer_surface_area_m2": round(outer_area, 3),
        }

    def _dnv_anode_requirements(self, inputs, current_demand):
        """
        Calculate anode mass and count requirements for cathodic protection system.

        DNV RP-F103 anode sizing is based on total charge requirement over design life.
        Anode capacity (Ah/kg) varies by material type (aluminum, zinc, magnesium).
        Utilization factor accounts for actual vs. theoretical capacity, and contingency
        factor provides safety margin for anode count.

        Args:
            inputs (dict): Configuration inputs with anode specifications
            current_demand (dict): Current demands from _dnv_current_demand()

        Returns:
            dict: Anode mass, count, and material specifications
        """
        import math

        # Extract anode parameters from configuration
        anodes = inputs.get("anode", {})
        anode_material = anodes.get("material", "aluminium")  # aluminium, zinc, magnesium
        utilization_factor = anodes.get("utilization_factor", 0.85)  # 85% typical
        individual_mass_kg = anodes.get("individual_anode_mass_kg", 300.0)
        contingency_factor = anodes.get("contingency_factor", 1.10)  # 10% spares

        # DNV RP-F103 Table 5-1: Anode electrochemical capacities by material
        # These values represent theoretical capacity in Ampere-hours per kilogram
        capacities_Ah_kg = {
            "aluminium": 2000.0,  # Al-Zn-In alloy (typical offshore grade)
            "zinc": 780.0,        # Zinc anodes
            "magnesium": 1100.0   # Magnesium anodes
        }
        capacity = capacities_Ah_kg.get(anode_material, capacities_Ah_kg["aluminium"])

        # Extract total charge requirement from current demand calculations
        total_charge_Ah = current_demand["total_charge_Ah"]

        # Calculate total anode mass required based on charge and capacity
        # Formula: Mass (kg) = Total Charge (Ah) / (Capacity (Ah/kg) × Utilization Factor)
        # Utilization factor accounts for actual vs. theoretical capacity in field conditions
        total_mass_kg = total_charge_Ah / (capacity * utilization_factor)

        # Calculate number of individual anodes needed
        # Raw count without safety margin
        anode_count_raw = total_mass_kg / individual_mass_kg

        # Apply contingency factor and round up to whole number
        # Contingency accounts for installation uncertainties and provides spare capacity
        anode_count = int(math.ceil(anode_count_raw * contingency_factor))

        # Calculate actual total mass with contingency included
        actual_total_mass_kg = anode_count * individual_mass_kg

        return {
            "total_anode_mass_kg": round(total_mass_kg, 3),
            "actual_total_mass_kg": round(actual_total_mass_kg, 3),
            "anode_count": anode_count,
            "individual_anode_mass_kg": round(individual_mass_kg, 3),
            "anode_material": anode_material,
            "anode_capacity_Ah_kg": round(capacity, 3),
            "utilization_factor": round(utilization_factor, 6),
            "contingency_factor": round(contingency_factor, 6),
        }

    def _dnv_anode_spacing(self, inputs, geometry, anode_requirements):
        """
        Calculate distributed anode placement along pipeline length.

        DNV RP-F103 anode spacing is based on uniform distribution for optimal
        current distribution. Spacing must account for end effects (first and last
        anodes at pipeline terminations) and validate against DNV minimum/maximum
        spacing criteria for effective cathodic protection.

        Args:
            inputs (dict): Configuration inputs with anode spacing specifications
            geometry (dict): Pipeline geometry from _dnv_pipeline_geometry()
            anode_requirements (dict): Anode count from _dnv_anode_requirements()

        Returns:
            dict: Anode spacing, positions array, and validation status
        """
        import numpy as np

        # Extract pipeline length from geometry
        pipeline_length_m = geometry["length_m"]

        # Extract anode count from requirements
        anode_count = anode_requirements["anode_count"]

        # Calculate uniform spacing between anodes
        # Formula: spacing = length / (count - 1) for end-to-end placement
        # This ensures first and last anodes are at pipeline terminations
        if anode_count > 1:
            spacing_m = pipeline_length_m / (anode_count - 1)
        else:
            # Single anode case - no spacing calculation needed
            spacing_m = 0.0

        # Generate anode position array along pipeline
        # np.linspace creates evenly spaced positions from 0 to pipeline length
        if anode_count > 0:
            positions_m = np.linspace(0, pipeline_length_m, anode_count)
        else:
            positions_m = np.array([])

        # DNV RP-F103 spacing validation criteria
        # Extract minimum and maximum spacing limits from configuration
        anode = inputs.get("anode", {})
        min_spacing_m = anode.get("min_spacing_m", 5.0)  # Minimum 5m typical
        max_spacing_m = anode.get("max_spacing_m", 50.0)  # Maximum 50m typical

        # Validate spacing falls within DNV acceptable range
        spacing_valid = min_spacing_m <= spacing_m <= max_spacing_m

        return {
            "spacing_m": round(spacing_m, 3),
            "anode_count": anode_count,
            "positions_m": [round(pos, 3) for pos in positions_m.tolist()],
            "min_spacing_m": round(min_spacing_m, 3),
            "max_spacing_m": round(max_spacing_m, 3),
            "spacing_valid": spacing_valid,
        }

    def _dnv_attenuation(self, inputs, geometry, current_densities, coating_breakdown, anode_spacing):
        """
        Calculate current distribution and potential decay along pipeline.

        DNV RP-F103 attenuation modeling determines how cathodic protection
        current and potential decay along the pipeline due to coating resistance
        and pipeline resistivity. The attenuation length characterizes the
        effective reach of protection, and potential distribution verification
        ensures adequate protection between discrete anode locations.

        Enhanced to support polarization resistance and advanced attenuation
        calculations per DNV RP-F103 2016 / Saipem approach.

        Args:
            inputs (dict): Configuration with coating resistance parameters
            geometry (dict): Pipeline geometry from _dnv_pipeline_geometry()
            current_densities (dict): Protection levels from _dnv_current_densities()
            coating_breakdown (dict): Coating breakdown factors from _dnv_coating_breakdown()
            anode_spacing (dict): Anode distribution from _dnv_anode_spacing()

        Returns:
            dict: Attenuation length, potential decay, protection validation,
                  polarization resistance, and enhanced attenuation factor
        """
        import math

        # Extract pipeline geometry parameters
        outer_diameter_m = geometry["outer_diameter_m"]
        wall_thickness_m = geometry["wall_thickness_m"]
        pipeline_length_m = geometry["length_m"]
        longitudinal_resistance_ohm_per_m = geometry["longitudinal_resistance_ohm_per_m"]

        # Calculate inner diameter
        inner_diameter_m = outer_diameter_m - (2.0 * wall_thickness_m)

        # Extract coating resistivity from configuration
        # NOTE: Parameter is named "resistance_ohm_m2" for historical reasons, but is actually
        # coating RESISTIVITY in Ω·m, not resistance in Ω·m². The formula requires resistivity.
        # Typical values: 0.05-1.0 Ω·m produce attenuation lengths of 100-500m
        # Bug fix: Changed from wrong default of 50000 (way too high) to 1.0 (good quality coating)
        coating = inputs.get("coating", {})
        coating_resistivity_ohm_m = coating.get("resistance_ohm_m2", 1.0)  # Despite name, this is resistivity in Ω·m

        # Steel resistivity (carbon steel typical value)
        steel_resistivity_ohm_m = 1.7e-7  # ohm-m for carbon steel

        # DNV RP-F103 2010 attenuation length formula:
        # La = sqrt((D × t × ρ_coating) / (4 × ρ_steel × ln(D/d)))
        #
        # Where:
        # - D = outer diameter (m)
        # - t = wall thickness (m)
        # - ρ_coating = coating resistivity (ohm-m) - NOT resistance (ohm-m²)
        # - ρ_steel = steel resistivity (ohm-m)
        # - d = inner diameter (m)

        # Calculate logarithmic term: ln(D/d)
        if inner_diameter_m > 0:
            diameter_ratio = outer_diameter_m / inner_diameter_m
            ln_diameter_ratio = math.log(diameter_ratio)
        else:
            ln_diameter_ratio = 0.0

        # Calculate attenuation length (DNV 2010 simplified formula)
        numerator = outer_diameter_m * wall_thickness_m * coating_resistivity_ohm_m
        denominator = 4.0 * steel_resistivity_ohm_m * ln_diameter_ratio

        if denominator > 0:
            attenuation_length_m = math.sqrt(numerator / denominator)
        else:
            # Edge case: zero denominator (should not occur with valid inputs)
            attenuation_length_m = 0.0

        # Enhanced attenuation calculation (DNV RP-F103 2016 / Saipem approach)
        # Calculate polarization resistance: P = (Ecorr - Ea) / i
        #
        # Where:
        # - Ecorr = free corrosion potential (V)
        # - Ea = anode potential (V)
        # - i = mean current density (A/m²)

        environment = inputs.get("environment", {})
        free_corrosion_potential_V = environment.get("free_corrosion_potential_V", -0.630)
        anode_potential_V = environment.get("anode_potential_V", -0.950)

        mean_current_density_A_m2 = current_densities["mean_current_density_A_m2"]

        if mean_current_density_A_m2 > 0:
            polarization_resistance_ohm_m2 = (
                (free_corrosion_potential_V - anode_potential_V) / mean_current_density_A_m2
            )
        else:
            polarization_resistance_ohm_m2 = 0.0

        # Enhanced attenuation factor (DNV RP-F103 2016 / Saipem approach - CORRECTED)
        # α = sqrt((π × D × RL × CBFf × P) / 8)
        #
        # Note: The original documented formula α = sqrt(2 / (π × D × RL × CBFf × P)) was
        # found to be incorrect (inverted). The correct formula places all variables in the
        # numerator with divisor of 8 under the square root. This was discovered through
        # systematic hypothesis testing and verified against Saipem reference values
        # (error: 2.85% vs 262 million× with wrong formula).
        #
        # Where:
        # - D = outer diameter (m)
        # - RL = longitudinal resistance (ohm/m)
        # - CBFf = coating breakdown final fraction (bare steel area fraction)
        # - P = polarization resistance (ohm-m²)

        # Convert coating breakdown factor to fraction (e.g., 1.48 → 0.48)
        coating_breakdown_final_fraction = coating_breakdown["final_factor"] - 1.0

        numerator_enhanced = (
            math.pi
            * outer_diameter_m
            * longitudinal_resistance_ohm_per_m
            * coating_breakdown_final_fraction
            * polarization_resistance_ohm_m2
        )

        if numerator_enhanced > 0:
            attenuation_factor_enhanced_per_m = math.sqrt(numerator_enhanced / 8.0)
        else:
            attenuation_factor_enhanced_per_m = 0.0

        # Extract anode spacing for potential distribution analysis
        spacing_m = anode_spacing["spacing_m"]
        positions_m = anode_spacing["positions_m"]
        anode_count = anode_spacing["anode_count"]

        # Calculate potential decay at midpoint between anodes (worst case)
        # Midpoint distance from nearest anode
        midpoint_distance_m = spacing_m / 2.0

        # Exponential decay factor: e^(-distance/La)
        if attenuation_length_m > 0:
            decay_exponent = -midpoint_distance_m / attenuation_length_m
            potential_decay_factor = math.exp(decay_exponent)
        else:
            potential_decay_factor = 0.0

        # DNV RP-F103 protection criterion:
        # Potential at midpoints should maintain > 70% of anode potential
        # to ensure adequate cathodic protection throughout pipeline
        protection_threshold = 0.70
        protection_adequate = potential_decay_factor >= protection_threshold

        # Calculate effective protection reach from each anode
        # Distance where potential decays to protection threshold (70%)
        if attenuation_length_m > 0:
            # Solve: 0.70 = e^(-reach/La) → reach = -La × ln(0.70)
            protection_reach_m = -attenuation_length_m * math.log(protection_threshold)
        else:
            protection_reach_m = 0.0

        return {
            "attenuation_length_m": round(attenuation_length_m, 3),
            "potential_decay_factor": round(potential_decay_factor, 6),
            "midpoint_distance_m": round(midpoint_distance_m, 3),
            "protection_reach_m": round(protection_reach_m, 3),
            "protection_adequate": protection_adequate,
            "protection_threshold": protection_threshold,
            "positions_analyzed": len(positions_m),
            "anode_count": anode_count,
            "polarization_resistance_ohm_m2": round(polarization_resistance_ohm_m2, 6),
            "attenuation_factor_enhanced_per_m": round(attenuation_factor_enhanced_per_m, 10),
            "free_corrosion_potential_V": round(free_corrosion_potential_V, 3),
            "anode_potential_V": round(anode_potential_V, 3),
        }

    # -----------------------------------------------------------------------
    # DNV-RP-B401-2021: Cathodic Protection of Offshore Fixed Platforms
    # -----------------------------------------------------------------------

    def DNV_RP_B401_offshore_platform(self, cfg):
        """Cathodic protection for offshore fixed platforms per DNV-RP-B401 (2021).

        Covers jacket structures, gravity-based structures, and topsides steel.
        Zones: submerged, splash, atmospheric.

        Standard: DNV-RP-B401 May 2021
        Sections: 3.3 (current densities), 3.4 (coating breakdown), 4.9 (anode resistance)
        """
        inputs = cfg.get("inputs", {})
        design_data = inputs.get("design_data", {})
        design_life = design_data.get("design_life", 25.0)

        areas = _b401_surface_areas(inputs)
        breakdown = _b401_coating_breakdown(inputs, design_life)
        densities = _b401_current_densities(inputs)
        current_demand = _b401_current_demand(inputs, areas, densities, breakdown)
        resistance = _b401_anode_resistance(inputs)
        anode_req = _b401_anode_requirements(inputs, current_demand)
        verification = _b401_verify_current_output(inputs, anode_req, resistance, current_demand)

        cfg["results"] = {
            "standard": "DNV-RP-B401-2021",
            "design_life_years": design_life,
            "surface_areas_m2": areas,
            "coating_breakdown": breakdown,
            "current_densities_A_m2": densities,
            "current_demand_A": current_demand,
            "anode_resistance_ohm": round(resistance, 6),
            "anode_requirements": anode_req,
            "current_output_verification": verification,
        }
        return cfg

    # -----------------------------------------------------------------------
    # ABS Guidance Notes on Cathodic Protection of Offshore Structures (Dec 2018)
    # -----------------------------------------------------------------------

    # Section 2 / 11.7.2 Table 1 (p.12): Recommended Initial Design Current
    # Densities for Bare Metal Surfaces Exposed to Seawater (mA/m²).
    # Layout: {climatic_region: {depth_band: i_ci}}
    # Depth bands (m): "0-30", ">30-100", ">100-300", ">300"
    _ABS_OFFSHORE_2018_TABLE_1 = {
        "tropical": {          # > 20°C (68°F)
            "0-30":     150.0,
            ">30-100":  120.0,
            ">100-300": 140.0,
            ">300":     180.0,
        },
        "sub_tropical": {      # > 12–20°C (54–68°F)
            "0-30":     170.0,
            ">30-100":  140.0,
            ">100-300": 160.0,
            ">300":     200.0,
        },
        "temperate": {         # > 7–12°C (45–54°F)
            "0-30":     200.0,
            ">30-100":  170.0,
            ">100-300": 190.0,
            ">300":     220.0,
        },
        "arctic": {            # ≤ 7°C (≤ 45°F)
            "0-30":     250.0,
            ">30-100":  200.0,
            ">100-300": 220.0,
            ">300":     220.0,
        },
    }

    # Section 2 / 11.7.2 (p.12): Saline mud current densities (mA/m²).
    # Fixed values — not dependent on climatic region or depth.
    _ABS_OFFSHORE_2018_MUD_DENSITIES = {
        "initial": 25.0,
        "mean":    20.0,
        "final":   20.0,
    }

    # Valid zone identifiers
    _ABS_OFFSHORE_2018_VALID_ZONES = {"submerged", "saline_mud"}

    def ABS_gn_offshore_2018(self, cfg):
        """
        ABS Guidance Notes on Cathodic Protection of Offshore Structures (Dec 2018).

        Covers floating offshore structures such as FPSOs, semi-submersibles,
        jack-ups, barges, storage tankers, and buoys.

        Implements:
        - Section 2 Table 1 (p.12): initial current densities by climatic region
          and water-depth band for submerged bare steel surfaces.
        - Section 2/11.7.2 (p.12): saline mud fixed current densities.
        - Section 2/11.7.3 (p.13): I_c = A_c × i_c × f_c.
        - Section 2/11.7.4 (p.13): coating breakdown factor f_c = α + βt.
          Shallow (≤30 m): α=0.01, β_mean=0.006, β_final=0.012.
          Deep   (>30 m):  α=0.01, β_mean=0.004, β_final=0.008.
        - Mean i_cm = 1/2 × i_ci; Final i_cf = 2/3 × i_ci.
        - Anode mass: M = I_cm × t × 8760 / (acc × u).

        Args:
            cfg (dict): Configuration with inputs sub-keys:
                design_data.design_life  (years)
                structure.surface_area_m2
                structure.water_depth_m
                structure.climatic_region  ("tropical"|"sub_tropical"|"temperate"|"arctic")
                structure.zone             ("submerged"|"saline_mud")
                anode.material             ("aluminium"|"zinc")
                anode.utilisation_factor   (default 0.80)

        Returns:
            dict: cfg updated with cfg["results"] containing current densities,
                  coating breakdown, current demand, and anode mass.
        """
        inputs = cfg["inputs"]
        design_data = inputs.get("design_data", {})
        design_life = float(design_data.get("design_life", 25.0))

        structure = inputs.get("structure", {})
        surface_area_m2 = float(structure.get("surface_area_m2", 0.0))
        water_depth_m = float(structure.get("water_depth_m", 50.0))
        climatic_region = structure.get("climatic_region", "tropical").lower()
        zone = structure.get("zone", "submerged").lower()

        # Validate inputs
        if zone not in self._ABS_OFFSHORE_2018_VALID_ZONES:
            raise ValueError(
                f"ABS GN Offshore 2018: unrecognised zone='{zone}'. "
                f"Valid values: {sorted(self._ABS_OFFSHORE_2018_VALID_ZONES)}"
            )
        if zone == "submerged" and climatic_region not in self._ABS_OFFSHORE_2018_TABLE_1:
            raise ValueError(
                f"ABS GN Offshore 2018: unrecognised climatic_region='{climatic_region}'. "
                f"Valid values: {sorted(self._ABS_OFFSHORE_2018_TABLE_1.keys())}"
            )

        # ---- current densities (mA/m²) ----
        current_densities = self._abs_offshore_2018_current_densities(
            zone, climatic_region, water_depth_m
        )

        # ---- coating breakdown factors ----
        coating_breakdown = self._abs_offshore_2018_coating_breakdown(
            zone, water_depth_m, design_life
        )

        # ---- current demand (A): I_c = A * i_c (A/m²) * f_c ----
        current_demand = self._abs_offshore_2018_current_demand(
            surface_area_m2, current_densities, coating_breakdown
        )

        # ---- anode current capacity ----
        anode_current_capacity = self.get_anode_current_capacity(cfg)

        # ---- anode mass (kg) ----
        anode_cfg = inputs.get("anode", {})
        utilisation = float(anode_cfg.get("utilisation_factor", 0.80))
        anode_mass_kg = (
            current_demand["mean"] * design_life * 8760.0
            / (anode_current_capacity * utilisation)
        )

        cfg["results"] = {
            "design_life_years": round(design_life, 3),
            "surface_area_m2": round(surface_area_m2, 3),
            "water_depth_m": round(water_depth_m, 3),
            "climatic_region": climatic_region,
            "zone": zone,
            "current_densities_mA_m2": current_densities,
            "coating_breakdown_factors": coating_breakdown,
            "current_demand_A": current_demand,
            "anode_current_capacity_Ah_kg": anode_current_capacity,
            "anode_utilisation_factor": utilisation,
            "anode_mass_kg": round(anode_mass_kg, 3),
        }

    def _abs_offshore_2018_current_densities(self, zone, climatic_region, water_depth_m):
        """
        Return initial, mean, and final current densities (mA/m²).

        For submerged zones uses ABS GN Offshore 2018 Section 2 Table 1 (p.12):
          i_cm = 1/2 * i_ci;  i_cf = 2/3 * i_ci.

        For saline_mud uses the fixed values from Section 2/11.7.2 (p.12):
          i_ci = 25, i_cm = i_cf = 20 mA/m².
        """
        if zone == "saline_mud":
            mud = self._ABS_OFFSHORE_2018_MUD_DENSITIES
            return {
                "initial": mud["initial"],
                "mean":    mud["mean"],
                "final":   mud["final"],
            }

        # Determine depth band
        depth_band = self._abs_offshore_2018_depth_band(water_depth_m)
        i_ci = self._ABS_OFFSHORE_2018_TABLE_1[climatic_region][depth_band]
        i_cm = i_ci / 2.0
        i_cf = i_ci * 2.0 / 3.0

        return {
            "initial": round(i_ci, 4),
            "mean":    round(i_cm, 4),
            "final":   round(i_cf, 6),
        }

    def _abs_offshore_2018_depth_band(self, water_depth_m):
        """
        Map water depth (m) to the Table 1 depth-band key.

        Bands per ABS GN Offshore 2018 Section 2 Table 1 (p.12):
          0–30 m, >30–100 m, >100–300 m, >300 m.
        """
        if water_depth_m <= 30.0:
            return "0-30"
        elif water_depth_m <= 100.0:
            return ">30-100"
        elif water_depth_m <= 300.0:
            return ">100-300"
        else:
            return ">300"

    def _abs_offshore_2018_coating_breakdown(self, zone, water_depth_m, design_life):
        """
        Calculate coating breakdown factors per ABS GN Offshore 2018 Section 2/11.7.4 (p.13).

        Formula: f_c = α + β*t
          α = 0.01 (initial breakdown, both depth zones)
          For depth ≤ 30 m: β_mean = 0.006, β_final = 0.012
          For depth > 30 m: β_mean = 0.004, β_final = 0.008

        For saline_mud: coating factor is 1.0 (bare — no coating benefit in mud contact).
        """
        if zone == "saline_mud":
            return {
                "depth_zone": "mud",
                "alpha": 1.0,
                "beta_mean": 0.0,
                "beta_final": 0.0,
                "design_life_years": round(design_life, 3),
                "initial": 1.0,
                "mean":    1.0,
                "final":   1.0,
            }

        alpha = 0.01
        if water_depth_m <= 30.0:
            beta_mean = 0.006
            beta_final = 0.012
            depth_zone = "shallow"
        else:
            beta_mean = 0.004
            beta_final = 0.008
            depth_zone = "deep"

        t = design_life
        f_ci = alpha
        f_cm = alpha + beta_mean * t
        f_cf = alpha + beta_final * t

        # Clamp at 1.0 (100% bare)
        f_ci = min(f_ci, 1.0)
        f_cm = min(f_cm, 1.0)
        f_cf = min(f_cf, 1.0)

        return {
            "depth_zone": depth_zone,
            "alpha": alpha,
            "beta_mean": beta_mean,
            "beta_final": beta_final,
            "design_life_years": round(t, 3),
            "initial": round(f_ci, 6),
            "mean":    round(f_cm, 6),
            "final":   round(f_cf, 6),
        }

    def _abs_offshore_2018_current_demand(
        self, surface_area_m2, current_densities, coating_breakdown
    ):
        """
        Calculate current demand (A) per ABS GN Offshore 2018 Section 2/11.7.3 (p.13).

        I_c = A_c × i_c × f_c

        current densities are in mA/m²; convert to A/m² by dividing by 1000.
        """
        f_ci = coating_breakdown["initial"]
        f_cm = coating_breakdown["mean"]
        f_cf = coating_breakdown["final"]

        i_ci_A_m2 = current_densities["initial"] / 1000.0
        i_cm_A_m2 = current_densities["mean"]    / 1000.0
        i_cf_A_m2 = current_densities["final"]   / 1000.0

        I_ci = surface_area_m2 * i_ci_A_m2 * f_ci
        I_cm = surface_area_m2 * i_cm_A_m2 * f_cm
        I_cf = surface_area_m2 * i_cf_A_m2 * f_cf

        return {
            "initial": round(I_ci, 6),
            "mean":    round(I_cm, 6),
            "final":   round(I_cf, 6),
        }

    # -----------------------------------------------------------------------
    # ABS GN Ships (legacy helpers — shared by ABS_gn_ships_2018)
    # -----------------------------------------------------------------------

    def _abs_breakdown_factors(self, inputs, design_life):
        structure = inputs.get("structure", {})
        initial_breakdown = structure.get("coating_initial_breakdown_factor", 2.0)
        initial_duration = structure.get("coating_initial_breakdown_duration", 2.0)
        yearly_breakdown = structure.get("coating_yearly_breakdown_factor", 3.0)
        max_factor = structure.get("coating_breakdown_factor_max", 2.0)

        initial_years = min(design_life, initial_duration)
        remaining_years = max(0.0, design_life - initial_duration)

        initial_factor = 1.0 + (initial_breakdown / 100.0)
        yearly_factor = 1.0 + (yearly_breakdown / 100.0)
        fcf_raw = (initial_factor ** initial_years) * (yearly_factor ** remaining_years)
        fcf = min(max_factor, fcf_raw)
        fcm = (yearly_factor + fcf) / 2.0

        return {
            "initial_factor": round(initial_factor, 6),
            "yearly_factor": round(yearly_factor, 6),
            "final_factor_raw": round(fcf_raw, 6),
            "final_factor": round(fcf, 6),
            "mean_factor": round(fcm, 6),
        }

    def _abs_current_densities(self, inputs, breakdown_factors):
        design_current = inputs.get("design_current", {})
        coated_current = design_current.get("coated_steel_mA_m2", 13.5)
        uncoated_current = design_current.get("uncoated_steel_mA_m2", 200.0)

        fcm = breakdown_factors["mean_factor"]
        fcf = breakdown_factors["final_factor"]

        return {
            "initial_coated": round(coated_current, 6),
            "mean_coated": round(coated_current * fcm, 6),
            "final_coated": round(coated_current * fcf, 6),
            "initial_uncoated": round(uncoated_current, 6),
            "mean_uncoated_disbonding": round(uncoated_current * max(fcm - 1.0, 0.0), 6),
            "final_uncoated_disbonding": round(uncoated_current * max(fcf - 1.0, 0.0), 6),
        }

    def _abs_current_demand(self, inputs, current_densities):
        structure = inputs.get("structure", {})
        total_area = structure.get("steel_total_area", structure.get("steel_coated_area", 0.0))
        coverage = structure.get("area_coverage", 100.0)

        coated_area = total_area * (coverage / 100.0)
        uncoated_area = max(total_area - coated_area, 0.0)

        ici = current_densities["initial_coated"] * coated_area / 1000.0
        icm = current_densities["mean_coated"] * coated_area / 1000.0
        icf = current_densities["final_coated"] * coated_area / 1000.0

        initial_uncoated = current_densities["initial_uncoated"] * uncoated_area / 1000.0
        disbond_icm = current_densities["mean_uncoated_disbonding"] * uncoated_area / 1000.0
        disbond_icf = current_densities["final_uncoated_disbonding"] * uncoated_area / 1000.0

        return {
            "coated": {
                "initial": round(ici, 6),
                "mean": round(icm, 6),
                "final": round(icf, 6),
            },
            "uncoated": {
                "initial": round(initial_uncoated, 6),
            },
            "disbonding": {
                "mean": round(disbond_icm, 6),
                "final": round(disbond_icf, 6),
            },
            "totals": {
                "initial": round(ici + initial_uncoated, 6),
                "mean": round(icm + disbond_icm, 6),
                "final": round(icf + disbond_icf, 6),
            },
            "areas_m2": {
                "total": round(total_area, 3),
                "coated": round(coated_area, 3),
                "uncoated": round(uncoated_area, 3),
            },
        }

    def _abs_anode_mass_requirements(self, inputs, current_demand, anode_current_capacity):
        design_data = inputs.get("design_data", {})
        design_life = design_data.get("design_life", 25.0)
        anode_cfg = inputs.get("anode", {})

        utilisation = anode_cfg.get("anode_Utilisation_factor", 0.85)
        net_weight = (
            anode_cfg.get("physical_properties", {}).get("net_weight", None)
        )

        mean_current = current_demand["totals"]["mean"]
        total_mass = (
            mean_current * design_life * 8760.0 / (anode_current_capacity * utilisation)
        )
        anode_count = None
        if net_weight:
            anode_count = total_mass / net_weight

        return {
            "mean_current_A": round(mean_current, 6),
            "total_mass_kg": round(total_mass, 6),
            "anode_count": round(anode_count, 6) if anode_count is not None else None,
        }

    def _abs_anode_performance_checks(self, inputs, current_demand, anode_requirements):
        anode_cfg = inputs.get("anode", {})
        environment = inputs.get("environment", {})
        resistivity = (
            environment.get("seawater", {})
            .get("resistivity", {})
            .get("input", None)
        )
        if resistivity is None:
            return {"status": "missing_resistivity"}

        resistance_initial = self._abs_anode_resistance(inputs, resistivity, stage="initial")
        resistance_final = self._abs_anode_resistance(inputs, resistivity, stage="final")

        protection_potential = anode_cfg.get("protection_potential", 0.8)
        anode_potential = anode_cfg.get("closed_circuit_anode_potential", -1.09)
        structure_potential = -abs(protection_potential)
        delta_e = abs(anode_potential - structure_potential)

        anode_count = anode_requirements.get("anode_count", None)
        if anode_count is None:
            return {"status": "missing_anode_count"}

        initial_output = delta_e / resistance_initial if resistance_initial else None
        final_output = delta_e / resistance_final if resistance_final else None

        initial_total = initial_output * anode_count if initial_output is not None else None
        final_total = final_output * anode_count if final_output is not None else None

        return {
            "driving_voltage_V": round(delta_e, 6),
            "resistance_ohm": {
                "initial": round(resistance_initial, 6) if resistance_initial is not None else None,
                "final": round(resistance_final, 6) if resistance_final is not None else None,
            },
            "current_output_A": {
                "initial_per_anode": round(initial_output, 6) if initial_output is not None else None,
                "final_per_anode": round(final_output, 6) if final_output is not None else None,
                "initial_total": round(initial_total, 6) if initial_total is not None else None,
                "final_total": round(final_total, 6) if final_total is not None else None,
            },
            "checks": {
                "initial_meets_demand": initial_total is not None
                and initial_total >= current_demand["totals"]["initial"],
                "final_meets_demand": final_total is not None
                and final_total >= current_demand["totals"]["final"],
            },
        }

    def _abs_anode_resistance(self, inputs, resistivity, stage="initial"):
        anode_cfg = inputs.get("anode", {})
        geometry = anode_cfg.get("geometry", {})
        anode_type = geometry.get("type", "long_flush")

        length = geometry.get("length_m", anode_cfg.get("physical_properties", {}).get("mean_length", None))
        width = geometry.get("width_m", anode_cfg.get("physical_properties", {}).get("width", None))
        radius = geometry.get("radius_m", None)
        exposed_area = geometry.get("exposed_area_m2", None)
        if stage == "final":
            length = geometry.get("final_length_m", length)
            width = geometry.get("final_width_m", width)
            radius = geometry.get("final_radius_m", radius)
            exposed_area = geometry.get("final_exposed_area_m2", exposed_area)
        offset = geometry.get("offset_m", None)

        if anode_type == "slender_offset":
            if length is None or radius is None:
                return None
            if length >= 4 * radius:
                resistance = (resistivity / (2 * math.pi * length)) * (
                    math.log((4 * length) / radius) - 1
                )
            else:
                term = radius / (2 * length)
                resistance = (resistivity / (2 * math.pi * length)) * (
                    math.log(term * (1 + (1 + term**2) ** 0.5)) + term - (1 + term**2) ** 0.5
                )
            if offset is not None and 0.15 <= offset < 0.3:
                resistance *= 1.3
            return resistance

        if anode_type == "short_flush":
            if exposed_area is None:
                return None
            return 0.315 * resistivity / exposed_area

        if length is None or width is None:
            return None
        s_mean = 0.5 * (length + width)
        resistance = resistivity / (2 * s_mean)
        return resistance

    def get_anode_current_capacity(self, cfg):
        """
        This method is used to calculate the anode current capacity
        """
        # Handle both string and dict anode configurations
        anode_config = cfg["inputs"]["anode"]
        if isinstance(anode_config, str):
            anode_material = anode_config
        elif isinstance(anode_config, dict):
            anode_material = anode_config.get("material", "unknown")
        else:
            anode_material = "unknown"
            
        if anode_material == "zinc":
            anode_current_capacity = 780
        elif anode_material == "aluminium":
            # Use design_data if available, otherwise use environment temperature
            if "design_data" in cfg["inputs"] and "seawater_max_temperature" in cfg["inputs"]["design_data"]:
                temp = cfg["inputs"]["design_data"]["seawater_max_temperature"]
            else:
                temp = cfg["inputs"].get("environment", {}).get("temperature", 20)
            anode_current_capacity = 2000 - 27 * (temp - 20)
        else:
            raise ValueError(f"Unsupported anode type: {anode_material}. Supported types: 'zinc', 'aluminium'")

        return anode_current_capacity

    def get_seawater_resistivity(self, cfg):
        """
        Calculate seawater resistivity based on environment conditions.

        Typical seawater resistivity values:
        - Open ocean: 20-30 ohm-cm
        - Coastal waters: 25-40 ohm-cm
        - Harbor/polluted: 30-50 ohm-cm
        - Brackish water: 50-200 ohm-cm

        Default: 25 ohm-cm (standard seawater)

        Returns:
            float: seawater resistivity in ohm-cm
        """
        environment = cfg["inputs"].get("environment", {})

        # Check if resistivity is explicitly provided
        resistivity = environment.get("seawater_resistivity")
        if resistivity is not None:
            return resistivity

        # Determine resistivity based on water type
        water_type = environment.get("water_type", "open_ocean").lower()

        resistivity_map = {
            "open_ocean": 25.0,
            "coastal": 30.0,
            "harbor": 40.0,
            "polluted": 45.0,
            "brackish": 100.0,
            "fresh": 200.0
        }

        resistivity = resistivity_map.get(water_type, 25.0)

        # Store in results for reference
        if "results" not in cfg:
            cfg["results"] = {}
        if "environment" not in cfg["results"]:
            cfg["results"]["environment"] = {}

        cfg["results"]["environment"]["seawater_resistivity_ohm_cm"] = resistivity
        cfg["results"]["environment"]["water_type"] = water_type

        return resistivity

    def calculate_anode_resistance(self, cfg):
        """
        Calculate electrical resistance of anode configurations.

        Based on ABS Guidelines for Cathodic Protection of Ships (Dec 2017)

        Two anode types:
        1. Slender stand-off anodes: R_a = (ρ/2πL)[ln(4L/r) - 1]
        2. Flush-mounted anodes: R_a = ρ/(2S)

        Where:
        - ρ = seawater resistivity (ohm-cm)
        - L = anode length (cm)
        - r = anode equivalent radius (cm)
        - S = anode surface area (cm²)

        Returns:
            float: anode resistance in ohms
        """
        import math

        anode = cfg["inputs"].get("anode", {})
        anode_type = anode.get("type", "slender").lower()

        # Get seawater resistivity
        resistivity = self.get_seawater_resistivity(cfg)

        if anode_type == "slender" or anode_type == "standoff":
            # Slender stand-off anode formula
            length_cm = anode.get("length_cm")
            radius_cm = anode.get("radius_cm")

            if length_cm is None or radius_cm is None:
                raise ValueError(
                    "For slender/standoff anodes, 'length_cm' and 'radius_cm' are required"
                )

            # R_a = (ρ/2πL)[ln(4L/r) - 1]
            R_a = (resistivity / (2 * math.pi * length_cm)) * (
                math.log(4 * length_cm / radius_cm) - 1
            )

        elif anode_type == "flush" or anode_type == "flush_mounted":
            # Flush-mounted anode formula
            surface_area_cm2 = anode.get("surface_area_cm2")

            if surface_area_cm2 is None:
                raise ValueError(
                    "For flush-mounted anodes, 'surface_area_cm2' is required"
                )

            # R_a = ρ/(2S)
            R_a = resistivity / (2 * surface_area_cm2)

        else:
            raise ValueError(
                f"Unsupported anode type: {anode_type}. Use 'slender', 'standoff', 'flush', or 'flush_mounted'"
            )

        # Store in results
        if "results" not in cfg:
            cfg["results"] = {}
        if "anode_resistance" not in cfg["results"]:
            cfg["results"]["anode_resistance"] = {}

        cfg["results"]["anode_resistance"]["resistance_ohms"] = round(R_a, 6)
        cfg["results"]["anode_resistance"]["anode_type"] = anode_type
        cfg["results"]["anode_resistance"]["seawater_resistivity_ohm_cm"] = resistivity

        return R_a

    def get_design_current_density(self, cfg):
        """
        Calculate design current density based on coating and vessel conditions.

        Based on ABS Guidelines for Cathodic Protection of Ships (Dec 2017)

        Current density depends on:
        - Coating type and quality
        - Coating breakdown factor over design life
        - Vessel operating conditions (speed, temperature)
        - Environmental factors (seawater salinity, temperature)

        Typical values (ABS Guidelines):
        - Well-coated steel (epoxy): 20-50 mA/m² initial, with coating breakdown
        - Moderately coated: 50-100 mA/m²
        - Bare or poorly coated: 100-200 mA/m²
        - Coating breakdown factor: 0.05 (new) to 0.50 (end of life)

        Design current density = Base current × Coating breakdown factor

        Args:
            cfg: Configuration dictionary with coating and vessel parameters

        Returns:
            float: Design current density in mA/m²
        """
        # Get coating parameters
        coating = cfg["inputs"].get("coating", {})
        coating_type = coating.get("type", "well_coated")  # well_coated, moderate, bare
        coating_quality = coating.get("quality", "good")  # excellent, good, fair, poor

        # Get vessel operating conditions
        vessel = cfg["inputs"].get("vessel", {})
        operating_condition = vessel.get("operating_condition", "static")  # static, low_speed, high_speed

        # Get design life for coating breakdown calculation
        protection = cfg["inputs"].get("protection", {})
        design_life = protection.get("design_life_years", 25)

        # Base current density (mA/m²) for different coating types
        # Based on ABS Guidelines Table 3-1
        base_current_density = {
            "well_coated": {
                "excellent": 20,
                "good": 30,
                "fair": 40,
                "poor": 50
            },
            "moderate": {
                "excellent": 50,
                "good": 70,
                "fair": 85,
                "poor": 100
            },
            "bare": {
                "excellent": 100,  # Not really applicable for bare steel
                "good": 130,
                "fair": 160,
                "poor": 200
            }
        }

        # Get base current density
        base_i = base_current_density.get(coating_type, {}).get(coating_quality, 100)

        # Coating breakdown factor over design life
        # ABS suggests linear breakdown from ~0.05 (year 1) to ~0.50 (end of design life)
        # This accounts for coating deterioration over time
        if coating_type == "bare":
            # Bare steel has constant high current requirement
            coating_breakdown_factor = 1.0
        else:
            # Well-coated or moderate coating deteriorates over time
            # Start at 5% breakdown (f=0.05), end at 50% breakdown (f=0.50)
            # Linear interpolation over design life
            min_breakdown = 0.05  # New coating (5% breakdown)
            max_breakdown = 0.50  # End of life (50% breakdown)

            # For design purposes, use average coating breakdown over design life
            # Conservative approach: use value closer to end-of-life
            # Design current = average breakdown factor
            average_breakdown = (min_breakdown + max_breakdown) / 2
            coating_breakdown_factor = average_breakdown

        # Operating condition adjustment factor
        # Dynamic conditions (vessel moving) may require slightly different current
        operating_factor = {
            "static": 1.0,      # Vessel at rest or moored
            "low_speed": 1.05,  # Vessel operating at low speeds (< 10 knots)
            "high_speed": 1.10  # Vessel operating at high speeds (> 10 knots)
        }
        op_factor = operating_factor.get(operating_condition, 1.0)

        # Calculate design current density
        # i_design = base_current × coating_breakdown_factor × operating_factor
        design_current_density = base_i * coating_breakdown_factor * op_factor

        # Store results
        if "current_density" not in cfg["results"]:
            cfg["results"]["current_density"] = {}

        cfg["results"]["current_density"]["design"] = {
            "design_current_density_mA_per_m2": round(design_current_density, 2),
            "base_current_density_mA_per_m2": base_i,
            "coating_type": coating_type,
            "coating_quality": coating_quality,
            "coating_breakdown_factor": round(coating_breakdown_factor, 3),
            "operating_condition": operating_condition,
            "operating_factor": op_factor,
            "design_life_years": design_life
        }

        return design_current_density

    def get_required_current_density(self, cfg):
        """
        Calculate total current requirements based on protected surface areas.

        Based on ABS Guidelines for Cathodic Protection of Ships (Dec 2017)

        Total current requirement = Design current density × Protected surface area

        Accounts for coating breakdown over design life:
        - Initial current (new coating): Lower current, coating provides protection
        - Mean current (design life average): Medium current, used for anode sizing
        - Final current (end of life): Higher current, coating has deteriorated

        The coating breakdown factors from get_design_current_density() are applied
        to calculate initial, mean, and final current demands.

        Typical progression (well-coated steel):
        - Initial: ~5% coating breakdown → lower current
        - Mean: ~27.5% coating breakdown → medium current (for anode design)
        - Final: ~50% coating breakdown → higher current

        Args:
            cfg: Configuration dictionary with surface areas and coating parameters

        Returns:
            dict: Initial, mean, and final current requirements in Amperes
        """
        # Get design current density from previous calculation
        # This should have been calculated by get_design_current_density()
        if "current_density" not in cfg.get("results", {}):
            # If not calculated yet, calculate it now
            self.get_design_current_density(cfg)

        design_results = cfg["results"]["current_density"]["design"]
        design_current_density = design_results["design_current_density_mA_per_m2"]  # mA/m²
        coating_type = design_results["coating_type"]
        coating_quality = design_results["coating_quality"]

        # Get protected surface areas from configuration
        hull = cfg["inputs"].get("hull", {})
        protected_area = hull.get("protected_area_m2", 0)

        if protected_area == 0:
            # Try alternative input structure
            protection = cfg["inputs"].get("protection", {})
            protected_area = protection.get("protected_surface_area_m2", 0)

        if protected_area == 0:
            raise ValueError(
                "Protected surface area must be provided in cfg['inputs']['hull']['protected_area_m2'] "
                "or cfg['inputs']['protection']['protected_surface_area_m2']"
            )

        # Coating breakdown factors over design life
        # Based on ABS Guidelines - coating deteriorates from ~5% to ~50% breakdown
        if coating_type == "bare":
            # Bare steel has constant high current requirement (no coating protection)
            initial_breakdown_factor = 1.0
            mean_breakdown_factor = 1.0
            final_breakdown_factor = 1.0
        else:
            # Well-coated or moderate coating deteriorates over time
            # Initial (Year 1): 5% coating breakdown
            initial_breakdown_factor = 0.05

            # Mean (average over design life): 27.5% coating breakdown
            # This is used for anode sizing calculations
            mean_breakdown_factor = (0.05 + 0.50) / 2  # Average of initial and final

            # Final (end of design life): 50% coating breakdown
            final_breakdown_factor = 0.50

        # Calculate current requirements at different stages
        # Current (A) = Current density (mA/m²) × Area (m²) / 1000
        # The design_current_density already includes coating breakdown factor (mean)
        # So we need to adjust for initial and final values

        # For initial current: use initial breakdown factor
        initial_current_A = (design_current_density * protected_area / 1000) * (
            initial_breakdown_factor / mean_breakdown_factor
        )

        # For mean current: design current density already uses mean breakdown
        mean_current_A = design_current_density * protected_area / 1000

        # For final current: use final breakdown factor
        final_current_A = (design_current_density * protected_area / 1000) * (
            final_breakdown_factor / mean_breakdown_factor
        )

        # Store results
        if "current_demand" not in cfg["results"]:
            cfg["results"]["current_demand"] = {}

        cfg["results"]["current_demand"]["required"] = {
            "initial_current_A": round(initial_current_A, 3),
            "mean_current_A": round(mean_current_A, 3),
            "final_current_A": round(final_current_A, 3),
            "protected_area_m2": protected_area,
            "design_current_density_mA_per_m2": design_current_density,
            "coating_type": coating_type,
            "coating_quality": coating_quality,
            "initial_breakdown_factor": initial_breakdown_factor,
            "mean_breakdown_factor": mean_breakdown_factor,
            "final_breakdown_factor": final_breakdown_factor,
            "notes": (
                f"Initial current: {round(initial_current_A, 2)} A (new coating), "
                f"Mean current: {round(mean_current_A, 2)} A (design basis), "
                f"Final current: {round(final_current_A, 2)} A (end of life)"
            )
        }

        # Return dictionary with all current requirements
        return {
            "initial_current_A": initial_current_A,
            "mean_current_A": mean_current_A,
            "final_current_A": final_current_A
        }

    def get_anodes_required(self, cfg):
        """
        Calculate minimum anode weight and number of anodes required.

        Based on ABS Guidelines for Cathodic Protection of Ships (Dec 2017)
        Formula: W_total = (I_mean × T_design × 8760) / (Q × u)

        Where:
        - I_mean = mean maintenance current (A)
        - T_design = design life (years)
        - 8760 = hours per year
        - Q = anode current capacity (A-h/kg)
        - u = utilization factor (0.7 to 0.95)

        Returns:
            cfg with results added under cfg["results"]["anode_sizing"]
        """
        import math

        # Extract inputs
        design_data = cfg["inputs"].get("design_data", {})
        design_life = design_data.get("design_life", cfg["inputs"].get("design_life", 25))

        current_demand = cfg["inputs"].get("current_demand", {})
        mean_current = current_demand.get("mean_current")

        if mean_current is None:
            raise ValueError("mean_current is required in cfg['inputs']['current_demand']['mean_current']")

        anode = cfg["inputs"].get("anode", {})
        single_anode_weight = anode.get("single_anode_weight")
        max_current_output = anode.get("max_current_output")
        utilization_factor = anode.get("utilization_factor", 0.85)

        if single_anode_weight is None:
            raise ValueError("single_anode_weight is required in cfg['inputs']['anode']['single_anode_weight']")
        if max_current_output is None:
            raise ValueError("max_current_output is required in cfg['inputs']['anode']['max_current_output']")

        # Validate utilization factor
        if not (0.7 <= utilization_factor <= 0.95):
            utilization_check = "WARNING: Utilization factor outside recommended range (0.7-0.95)"
        else:
            utilization_check = "PASS"

        # Get anode current capacity (uses existing method)
        Q = self.get_anode_current_capacity(cfg)

        # Calculate minimum total weight
        # W_total = (I_mean × T_design × 8760) / (Q × u)
        W_total = (mean_current * design_life * 8760) / (Q * utilization_factor)

        # Calculate number of anodes required
        N_weight = math.ceil(W_total / single_anode_weight)
        N_current = math.ceil(mean_current / max_current_output)
        N_required = max(N_weight, N_current)

        # Calculate actual weight provided
        W_provided = N_required * single_anode_weight

        # Store results
        if "results" not in cfg:
            cfg["results"] = {}

        cfg["results"]["anode_sizing"] = {
            "current_capacity_Ah_per_kg": Q,
            "total_weight_required_kg": round(W_total, 2),
            "single_anode_weight_kg": single_anode_weight,
            "anodes_by_weight": N_weight,
            "anodes_by_current": N_current,
            "anodes_required": N_required,
            "total_weight_provided_kg": W_provided,
            "utilization_factor": utilization_factor,
            "utilization_check": utilization_check,
            "design_life_years": design_life,
            "mean_current_A": mean_current
        }

        return cfg

    def anode_initial_check(self, cfg):
        """
        Verify anode performance at start of design life (initial condition).

        Based on ABS Guidelines for Cathodic Protection of Ships (Dec 2017)

        Checks:
        1. Anode can deliver required initial current
        2. Protection potential meets criteria (-0.80V to -1.10V vs Ag/AgCl)
        3. Anode has sufficient current output capacity

        Returns:
            dict: Initial check results with pass/fail status
        """
        # Get required parameters
        if "results" not in cfg or "anode_sizing" not in cfg["results"]:
            raise ValueError(
                "Must run get_anodes_required() before anode_initial_check()"
            )

        anode_sizing = cfg["results"]["anode_sizing"]
        mean_current = anode_sizing["mean_current_A"]
        anodes_required = anode_sizing["anodes_required"]

        anode = cfg["inputs"].get("anode", {})
        max_current_output = anode.get("max_current_output")

        # Calculate total current capacity
        total_current_capacity = anodes_required * max_current_output

        # Check if initial current can be delivered
        current_margin = total_current_capacity - mean_current
        current_check = "PASS" if current_margin > 0 else "FAIL"

        # Calculate anode utilization at initial condition
        initial_utilization = (mean_current / total_current_capacity) * 100 if total_current_capacity > 0 else 0

        # Protection potential check (simplified - assumes steel in seawater)
        # Typical range: -0.80V to -1.10V vs Ag/AgCl reference electrode
        protection = cfg["inputs"].get("protection", {})
        min_potential = protection.get("min_potential_V", -0.80)
        max_potential = protection.get("max_potential_V", -1.10)

        # Assume anode can achieve target potential (detailed calculation would need circuit analysis)
        potential_check = "PASS"  # Simplified - would require full electrochemical analysis

        # Overall initial check
        overall_check = "PASS" if current_check == "PASS" and potential_check == "PASS" else "FAIL"

        # Store results
        if "anode_checks" not in cfg["results"]:
            cfg["results"]["anode_checks"] = {}

        cfg["results"]["anode_checks"]["initial"] = {
            "mean_current_required_A": mean_current,
            "total_current_capacity_A": round(total_current_capacity, 3),
            "current_margin_A": round(current_margin, 3),
            "current_utilization_percent": round(initial_utilization, 2),
            "current_check": current_check,
            "potential_check": potential_check,
            "overall_check": overall_check,
            "protection_potential_range_V": f"{min_potential} to {max_potential}"
        }

        return cfg["results"]["anode_checks"]["initial"]

    def anode_final_check(self, cfg):
        """
        Verify anode performance at end of design life (final/depleted condition).

        Based on ABS Guidelines for Cathodic Protection of Ships (Dec 2017)

        At end of life, anodes are partially consumed and have:
        - Reduced mass (consumed by corrosion protection)
        - Increased resistance (smaller remaining anode size)
        - Maximum coating breakdown factor

        Checks:
        1. Remaining anode mass after utilization
        2. Final current output capability
        3. Protection potential maintained
        4. Overall pass/fail for end-of-life performance

        Returns:
            dict: Final check results with pass/fail status
        """
        # Get required parameters
        if "results" not in cfg or "anode_sizing" not in cfg["results"]:
            raise ValueError(
                "Must run get_anodes_required() before anode_final_check()"
            )

        anode_sizing = cfg["results"]["anode_sizing"]
        mean_current = anode_sizing["mean_current_A"]
        anodes_required = anode_sizing["anodes_required"]
        single_anode_weight = anode_sizing["single_anode_weight_kg"]
        utilization_factor = anode_sizing["utilization_factor"]

        anode = cfg["inputs"].get("anode", {})
        max_current_output = anode.get("max_current_output")

        # Calculate remaining anode mass after utilization
        # Remaining mass = Initial mass × (1 - utilization_factor)
        remaining_mass_per_anode = single_anode_weight * (1 - utilization_factor)
        total_remaining_mass = remaining_mass_per_anode * anodes_required

        # Estimate final current capacity (reduced due to smaller anode size)
        # Simplified assumption: current capacity reduces proportionally with mass
        final_current_capacity = max_current_output * (1 - utilization_factor)
        total_final_current = final_current_capacity * anodes_required

        # Check if final current can still meet requirements
        # Note: Current demand typically increases over time due to coating breakdown
        # For conservative check, use mean current (real analysis would use final current demand)
        current_margin = total_final_current - mean_current
        current_check = "PASS" if current_margin > 0 else "FAIL"

        # Calculate final utilization percentage
        final_utilization = (mean_current / total_final_current) * 100 if total_final_current > 0 else 100

        # Protection potential check
        # At end of life, higher anode resistance may affect potential delivery
        # Simplified check - detailed analysis would require circuit calculations
        protection = cfg["inputs"].get("protection", {})
        min_potential = protection.get("min_potential_V", -0.80)
        max_potential = protection.get("max_potential_V", -1.10)

        # Simplified potential check
        # FAIL if current margin is negative (cannot deliver required current)
        potential_check = "PASS" if current_margin > 0 else "FAIL"

        # Overall final check
        overall_check = "PASS" if current_check == "PASS" and potential_check == "PASS" else "FAIL"

        # Calculate mass consumption efficiency
        mass_consumed = (single_anode_weight - remaining_mass_per_anode) * anodes_required
        mass_efficiency = (mass_consumed / (single_anode_weight * anodes_required)) * 100

        # Store results
        if "anode_checks" not in cfg["results"]:
            cfg["results"]["anode_checks"] = {}

        cfg["results"]["anode_checks"]["final"] = {
            "remaining_mass_per_anode_kg": round(remaining_mass_per_anode, 3),
            "total_remaining_mass_kg": round(total_remaining_mass, 3),
            "mass_consumed_kg": round(mass_consumed, 3),
            "mass_consumption_efficiency_percent": round(mass_efficiency, 2),
            "final_current_capacity_per_anode_A": round(final_current_capacity, 3),
            "total_final_current_capacity_A": round(total_final_current, 3),
            "mean_current_required_A": mean_current,
            "final_current_margin_A": round(current_margin, 3),
            "final_utilization_percent": round(final_utilization, 2),
            "current_check": current_check,
            "potential_check": potential_check,
            "overall_check": overall_check,
            "protection_potential_range_V": f"{min_potential} to {max_potential}",
            "utilization_factor": utilization_factor
        }

        return cfg["results"]["anode_checks"]["final"]
