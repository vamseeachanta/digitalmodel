# ABOUTME: WellboreHydraulics — generalised annular hydraulics for arbitrary pipe/hole geometry
# ABOUTME: Reference: API RP 13D (Bingham Plastic), SPE Drilling Engineering, Chapter 4

"""
WellboreHydraulics
==================

Generalised steady-state wellbore hydraulics for arbitrary pipe-in-hole geometry.
Uses the Bingham Plastic rheological model (plastic viscosity + yield point).

Key outputs:
    annular_velocity()         ft/min — average upward fluid velocity in annulus
    pressure_drop_annulus()    psi    — frictional pressure loss over the TVD interval
    ecd(annular_pressure_loss) ppg    — equivalent circulating density
    cuttings_transport_ratio() 0–1   — ratio of AV to minimum cleaning velocity

Units throughout:
    lengths / diameters : inches (geometry), feet (depth)
    flow rate           : gal/min
    mud weight          : ppg (lb/gal)
    viscosity           : cp
    yield point         : lbf/100 ft^2
    pressure            : psi
"""


class WellboreHydraulics:
    """
    Wellbore hydraulics calculator for a single pipe-in-hole section.

    Parameters
    ----------
    pipe_od : float
        Drill pipe outer diameter, inches.
    pipe_id : float
        Drill pipe inner diameter, inches.
    hole_diameter : float
        Borehole / casing inner diameter, inches.  Must be > pipe_od.
    mud_weight : float
        Static mud weight, ppg.  Must be > 0.
    flow_rate : float
        Pump flow rate, gal/min.  Must be >= 0.
    tvd : float
        True vertical depth of the interval, ft.  Must be > 0.
    plastic_viscosity : float
        Bingham Plastic plastic viscosity (PV), cp.  Must be >= 0.
    yield_point : float
        Bingham Plastic yield point (YP), lbf/100 ft^2.  Must be >= 0.
    """

    # Minimum annular velocity for adequate cuttings transport (water-based mud)
    _MIN_CLEANING_AV_FT_MIN: float = 120.0

    def __init__(
        self,
        pipe_od: float,
        pipe_id: float,
        hole_diameter: float,
        mud_weight: float,
        flow_rate: float,
        tvd: float,
        plastic_viscosity: float,
        yield_point: float,
    ) -> None:
        self._validate(
            pipe_od, pipe_id, hole_diameter, mud_weight, flow_rate, tvd,
            plastic_viscosity, yield_point,
        )
        self.pipe_od = float(pipe_od)
        self.pipe_id = float(pipe_id)
        self.hole_diameter = float(hole_diameter)
        self.mud_weight = float(mud_weight)
        self.flow_rate = float(flow_rate)
        self.tvd = float(tvd)
        self.plastic_viscosity = float(plastic_viscosity)
        self.yield_point = float(yield_point)

    # ------------------------------------------------------------------
    # Public API
    # ------------------------------------------------------------------

    def annular_velocity(self) -> float:
        """
        Average annular velocity (upward), ft/min.

        Formula (field units, Bourgoyne et al. SPE Textbook Vol 2):
            AV = 24.51 * q / (D_hole^2 - D_pipe_od^2)

        where q is in gal/min and D in inches.

        Derivation of constant:
            AV [ft/min] = q [gal/min] * 231 [in^3/gal]
                          / (pi/4 * (D_h^2 - D_od^2) [in^2]) / 12  [in/ft]
                        = q * 231 * 144 / (pi * (D_h^2 - D_od^2) * 1728)
                        = q * 24.51 / (D_h^2 - D_od^2)
        """
        if self.flow_rate == 0.0:
            return 0.0
        return 24.51 * self.flow_rate / (
            self.hole_diameter ** 2 - self.pipe_od ** 2
        )

    def pressure_drop_annulus(self) -> float:
        """
        Annular frictional pressure drop over tvd, psi.

        Uses the Bingham Plastic laminar slot-flow approximation derived from
        SI first principles and converted to field units:

            dp/dL [psi/ft] = PV * AV / (239400 * D_eq^2)
                           + YP / (200 * D_eq)

        where:
            AV    = annular velocity, ft/min
            D_eq  = hole_diameter - pipe_od, inches
            PV    = plastic viscosity, cp
            YP    = yield point, lbf/100 ft^2

        Constant derivation:
            Slot-flow Bingham: dp/dz [Pa/m] = 12*mu*v/w^2 + 6*tau_y/w
            Converting v[ft/min -> m/s], w[in -> m], mu[cp -> Pa.s], tau_y[lbf/100ft^2 -> Pa]:
            K1 = 12 * 0.3048 / (1000 * 196.85 * 0.0254^2 * 6894.757) ≈ 1/239400
            K2 = 6 * 0.4788 * 0.3048 / (0.0254 * 6894.757)           ≈ 1/200

        Returns frictional pressure loss = (dp/dL) * tvd.
        """
        av = self.annular_velocity()
        if av == 0.0:
            return 0.0
        d_eq = self.hole_diameter - self.pipe_od  # inches
        dp_per_ft = (
            (self.plastic_viscosity * av) / (239400.0 * d_eq ** 2)
            + self.yield_point / (200.0 * d_eq)
        )
        return dp_per_ft * self.tvd

    def ecd(self, annular_pressure_loss: float) -> float:
        """
        Equivalent circulating density, ppg.

        Formula:
            ECD = mud_weight + annular_pressure_loss / (0.052 * tvd)

        Parameters
        ----------
        annular_pressure_loss : float
            Annular frictional pressure loss, psi.  Typically the value
            returned by pressure_drop_annulus(), but callers may supply
            additional contributions (e.g. cuttings load, BHA tool drop).

        Returns
        -------
        float
            ECD in ppg.
        """
        return self.mud_weight + annular_pressure_loss / (0.052 * self.tvd)

    def cuttings_transport_ratio(self) -> float:
        """
        Cuttings transport ratio (CTR), dimensionless, capped in [0, 1].

        CTR = AV / MIN_AV   (capped at 1.0)

        Threshold: 120 ft/min for water-based mud (industry consensus).
        CTR >= 1.0 indicates adequate hole cleaning; < 0.5 is poor.
        """
        av = self.annular_velocity()
        if av <= 0.0:
            return 0.0
        return min(av / self._MIN_CLEANING_AV_FT_MIN, 1.0)

    # ------------------------------------------------------------------
    # Internal validation
    # ------------------------------------------------------------------

    @staticmethod
    def _validate(
        pipe_od: float,
        pipe_id: float,
        hole_diameter: float,
        mud_weight: float,
        flow_rate: float,
        tvd: float,
        plastic_viscosity: float,
        yield_point: float,
    ) -> None:
        if pipe_id >= pipe_od:
            raise ValueError(
                f"pipe_id ({pipe_id}) must be less than pipe_od ({pipe_od})"
            )
        if hole_diameter <= pipe_od:
            raise ValueError(
                f"hole_diameter ({hole_diameter}) must be greater than pipe_od ({pipe_od})"
            )
        if mud_weight <= 0.0:
            raise ValueError(f"mud_weight must be > 0, got {mud_weight}")
        if flow_rate < 0.0:
            raise ValueError(f"flow_rate must be >= 0, got {flow_rate}")
        if tvd <= 0.0:
            raise ValueError(f"tvd must be > 0, got {tvd}")
        if plastic_viscosity < 0.0:
            raise ValueError(f"plastic_viscosity must be >= 0, got {plastic_viscosity}")
        if yield_point < 0.0:
            raise ValueError(f"yield_point must be >= 0, got {yield_point}")
