import math


class CTHydraulics:
    """Steady-state coiled tubing hydraulics calculations."""

    def __init__(self) -> None:
        pass

    def router(self, cfg: dict) -> dict:
        ct_cfg = cfg.get("ct_hydraulics", {})
        ct_cfg["results"] = self.calculate(ct_cfg)
        cfg["ct_hydraulics"] = ct_cfg
        return cfg

    def calculate(self, ct_cfg: dict) -> dict:
        well = ct_cfg.get("well", {})
        ct = ct_cfg.get("ct", {})
        annulus = ct_cfg.get("annulus", {})
        fluid = ct_cfg.get("fluid", {})
        flow = ct_cfg.get("flow", {})
        bha = ct_cfg.get("bha", {})
        options = ct_cfg.get("options", {})

        rate_gpm = float(flow.get("rate_gpm", 0.0))
        density_ppg = float(fluid.get("density_ppg", 0.0))
        ct_visc_cp = float(
            fluid.get("ct_viscosity_cp", fluid.get("viscosity_cp", 0.0)) or 0.0
        )
        ann_visc_value = fluid.get("annulus_viscosity_cp")
        ann_visc_cp = float(ann_visc_value) if ann_visc_value is not None else ct_visc_cp

        ct_od_in = float(ct.get("od_in", 0.0))
        ct_id_in = self._resolve_ct_id(ct)
        ct_length_m = float(ct.get("length_m", 0.0))
        ct_rough_in = float(ct.get("roughness_in", 0.0))
        reel_pressure_drop_psi = float(ct.get("reel_pressure_drop_psi", 0.0))

        hole_id_in = float(
            annulus.get("hole_id_in", well.get("hole_id_in", 0.0))
        )
        ann_length_m = float(
            annulus.get("length_m", well.get("measured_depth_m", ct_length_m))
        )
        ann_rough_in = float(annulus.get("roughness_in", ct_rough_in))
        ann_inner_od_in = float(annulus.get("inner_od_in", ct_od_in))
        ann_dh_override_in = annulus.get("hydraulic_diameter_override_in")

        ct_section = self._compute_pipe_section(
            name="ct",
            length_m=ct_length_m,
            diameter_in=ct_id_in,
            roughness_in=ct_rough_in,
            rate_gpm=rate_gpm,
            density_ppg=density_ppg,
            viscosity_cp=ct_visc_cp,
            re_transition=float(options.get("flow_regime_transition_re", 2100.0)),
        )

        annulus_section = self._compute_annulus_section(
            length_m=ann_length_m,
            hole_id_in=hole_id_in,
            inner_od_in=ann_inner_od_in,
            roughness_in=ann_rough_in,
            rate_gpm=rate_gpm,
            density_ppg=density_ppg,
            viscosity_cp=ann_visc_cp,
            hydraulic_diameter_override_in=ann_dh_override_in,
            re_transition=float(options.get("flow_regime_transition_re", 2100.0)),
        )

        tvd_m = float(
            well.get(
                "tvd_m",
                well.get("measured_depth_m", ann_length_m),
            )
        )
        tvd_ft = tvd_m * 3.28084
        hydrostatic_psi = 0.052 * density_ppg * tvd_ft if tvd_ft else 0.0

        wellhead_pressure_psi = float(well.get("wellhead_pressure_psi", 0.0))
        bha_pressure_drop_psi = float(bha.get("pressure_drop_psi", 0.0))

        include_annulus = bool(
            options.get("include_annulus_friction_in_pump_pressure", True)
        )

        pump_pressure_psi = (
            wellhead_pressure_psi
            + ct_section["pressure_loss_psi"]
            + bha_pressure_drop_psi
            + reel_pressure_drop_psi
            + (annulus_section["pressure_loss_psi"] if include_annulus else 0.0)
        )

        downhole_pressure_psi = (
            wellhead_pressure_psi
            + hydrostatic_psi
            + ct_section["pressure_loss_psi"]
            + bha_pressure_drop_psi
        )

        ecd_ppg = (
            density_ppg
            + annulus_section["pressure_loss_psi"] / (0.052 * tvd_ft)
            if tvd_ft
            else 0.0
        )

        return {
            "summary": {
                "pump_pressure_psi": self._r(pump_pressure_psi, 1),
                "downhole_pressure_psi": self._r(downhole_pressure_psi, 1),
                "hydrostatic_pressure_psi": self._r(hydrostatic_psi, 1),
                "wellhead_pressure_psi": self._r(wellhead_pressure_psi, 1),
                "bha_pressure_drop_psi": self._r(bha_pressure_drop_psi, 1),
                "reel_pressure_drop_psi": self._r(reel_pressure_drop_psi, 1),
                "equivalent_circulating_density_ppg": self._r(ecd_ppg, 3),
            },
            "coiled_tubing": ct_section,
            "annulus": annulus_section,
        }

    def _resolve_ct_id(self, ct_cfg: dict) -> float:
        id_in = ct_cfg.get("id_in")
        if id_in:
            return float(id_in)
        od_in = float(ct_cfg.get("od_in", 0.0))
        wall_thickness_in = ct_cfg.get("wall_thickness_in")
        if wall_thickness_in:
            return float(od_in - 2.0 * float(wall_thickness_in))
        volume_gal = ct_cfg.get("volume_gal")
        length_m = ct_cfg.get("length_m")
        if volume_gal and length_m:
            length_in = float(length_m) / 0.0254
            area_in2 = float(volume_gal) * 231.0 / length_in
            return math.sqrt(4.0 * area_in2 / math.pi)
        raise ValueError("ct.id_in, ct.wall_thickness_in, or ct.volume_gal required")

    def _compute_pipe_section(
        self,
        name: str,
        length_m: float,
        diameter_in: float,
        roughness_in: float,
        rate_gpm: float,
        density_ppg: float,
        viscosity_cp: float,
        re_transition: float,
    ) -> dict:
        diameter_m = diameter_in * 0.0254
        area_m2 = math.pi / 4.0 * diameter_m**2
        area_in2 = math.pi / 4.0 * diameter_in**2
        velocity_m_s = self._gpm_to_m3s(rate_gpm) / area_m2 if area_m2 else 0.0
        density = self._ppg_to_kgm3(density_ppg)
        viscosity = viscosity_cp / 1000.0
        reynolds = (
            density * velocity_m_s * diameter_m / viscosity if viscosity else 0.0
        )
        rel_rough = (roughness_in * 0.0254) / diameter_m if diameter_m else 0.0
        friction_factor = self._friction_factor(reynolds, rel_rough, re_transition)
        pressure_loss_psi = self._darcy_weisbach(
            friction_factor, length_m, diameter_m, density, velocity_m_s
        )
        length_in = length_m / 0.0254
        volume_gal = area_in2 * length_in / 231.0 if length_in else 0.0
        travel_time_min = volume_gal / rate_gpm if rate_gpm else 0.0

        return {
            "id_in": self._r(diameter_in, 4),
            "area_in2": self._r(area_in2, 3),
            "volume_gal": self._r(volume_gal, 1),
            "velocity_m_s": self._r(velocity_m_s, 3),
            "reynolds_number": self._r(reynolds, 1),
            "friction_factor": self._r(friction_factor, 5),
            "pressure_loss_psi": self._r(pressure_loss_psi, 1),
            "travel_time_min": self._r(travel_time_min, 1),
        }

    def _compute_annulus_section(
        self,
        length_m: float,
        hole_id_in: float,
        inner_od_in: float,
        roughness_in: float,
        rate_gpm: float,
        density_ppg: float,
        viscosity_cp: float,
        hydraulic_diameter_override_in,
        re_transition: float,
    ) -> dict:
        if hydraulic_diameter_override_in:
            hydraulic_diameter_in = float(hydraulic_diameter_override_in)
        else:
            hydraulic_diameter_in = float(hole_id_in - inner_od_in)
        area_in2 = math.pi / 4.0 * (hole_id_in**2 - inner_od_in**2)
        area_m2 = area_in2 * (0.0254**2)
        hydraulic_diameter_m = hydraulic_diameter_in * 0.0254
        velocity_m_s = self._gpm_to_m3s(rate_gpm) / area_m2 if area_m2 else 0.0
        density = self._ppg_to_kgm3(density_ppg)
        viscosity = viscosity_cp / 1000.0
        reynolds = (
            density * velocity_m_s * hydraulic_diameter_m / viscosity
            if viscosity
            else 0.0
        )
        rel_rough = (
            (roughness_in * 0.0254) / hydraulic_diameter_m
            if hydraulic_diameter_m
            else 0.0
        )
        friction_factor = self._friction_factor(reynolds, rel_rough, re_transition)
        pressure_loss_psi = self._darcy_weisbach(
            friction_factor, length_m, hydraulic_diameter_m, density, velocity_m_s
        )
        length_in = length_m / 0.0254
        volume_gal = area_in2 * length_in / 231.0 if length_in else 0.0
        bottoms_up_time_min = volume_gal / rate_gpm if rate_gpm else 0.0

        return {
            "hydraulic_diameter_in": self._r(hydraulic_diameter_in, 4),
            "area_in2": self._r(area_in2, 3),
            "volume_gal": self._r(volume_gal, 1),
            "velocity_m_s": self._r(velocity_m_s, 3),
            "reynolds_number": self._r(reynolds, 1),
            "friction_factor": self._r(friction_factor, 5),
            "pressure_loss_psi": self._r(pressure_loss_psi, 1),
            "bottoms_up_time_min": self._r(bottoms_up_time_min, 1),
        }

    def _friction_factor(self, reynolds: float, rel_rough: float, re_transition: float) -> float:
        if reynolds <= 0.0:
            return 0.0
        if reynolds < re_transition:
            return 64.0 / reynolds
        return (-1.8 * math.log10((rel_rough / 3.7) ** 1.11 + 6.9 / reynolds)) ** -2

    def _darcy_weisbach(
        self,
        friction_factor: float,
        length_m: float,
        diameter_m: float,
        density: float,
        velocity_m_s: float,
    ) -> float:
        if diameter_m <= 0.0:
            return 0.0
        delta_p_pa = friction_factor * (length_m / diameter_m) * 0.5 * density * velocity_m_s**2
        return delta_p_pa / 6894.757

    @staticmethod
    def _ppg_to_kgm3(ppg: float) -> float:
        return ppg * 119.826427

    @staticmethod
    def _gpm_to_m3s(gpm: float) -> float:
        return gpm * 0.003785411784 / 60.0

    @staticmethod
    def _r(value: float, digits: int = 3) -> float:
        return round(float(value), digits)
