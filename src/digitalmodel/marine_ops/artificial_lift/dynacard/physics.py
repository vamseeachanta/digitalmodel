# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

import numpy as np
from .calculations import calculate_pump_fillage
from .exceptions import PhysicsError
from .solver_guards import annotate_results, validate_stroke
from .models import DynacardAnalysisContext, CardData, AnalysisResults, DownholeLoadBasis
from .constants import (
    FEET_TO_INCHES,
    STEEL_YOUNGS_MODULUS_PSI,
    STEEL_DENSITY_LB_PER_FT3,
    SQUARE_FEET_TO_SQUARE_INCHES,
    SECONDS_PER_MINUTE,
    WAVE_SPEED_IN_PER_S,
    GIBBS_DAMPING_COEFFICIENT,
    BUOYANCY_FACTOR,
    LOAD_ATTENUATION_FACTOR,
    STROKE_SCALING_FACTOR,
    BUCKLING_DETECTION_LOAD_THRESHOLD_LBS,
)

class DynacardPhysicsSolver:
    """
    Legacy Gibbs comparison solver with propagated position and scaled load.

    Load is an affine surface-load approximation, not a wave-equation solution.
    Physical qualification of this hybrid card is not established.
    """

    def __init__(self, context: DynacardAnalysisContext):
        """Initialize the Gibbs analytical solver.

        Args:
            context: Complete well analysis context including surface card,
                rod string, pump, and operating parameters.
        """
        self.ctx = context
        self.results = AnalysisResults()

    def solve_wave_equation(self):
        """
        Implements the Gibbs analytical diagnostic method.
        Propagates position in the frequency domain and approximates load.
        The returned load basis records the executed legacy transformation
        and its unqualified status; it does not certify derived diagnostics.
        """
        # 1. Inputs and Constants from Legacy "Gold Standard"
        u_surf = np.array(self.ctx.surface_card.position)
        f_surf = np.array(self.ctx.surface_card.load)
        Nt = len(u_surf)

        L = sum(section.length for section in self.ctx.rod_string) * FEET_TO_INCHES
        E = STEEL_YOUNGS_MODULUS_PSI

        # Wave speed from Legacy reference
        a = WAVE_SPEED_IN_PER_S

        avg_d = np.mean([s.diameter for s in self.ctx.rod_string])
        A = np.pi * (avg_d**2) / 4.0

        # 2. Fourier Decomposition
        U_s = np.fft.fft(u_surf)
        F_s = np.fft.fft(f_surf)
        # Time step based on SPM
        dt_total = (SECONDS_PER_MINUTE / self.ctx.spm)
        freqs = np.fft.fftfreq(Nt, d=(dt_total / Nt))

        # 3. Frequency Domain Transformation
        U_d = np.zeros_like(U_s, dtype=complex)

        # Damping coefficient (c) - Fixed well-specific value for parity
        c = GIBBS_DAMPING_COEFFICIENT

        for k in range(len(freqs)):
            w = 2 * np.pi * freqs[k]
            if w == 0:
                U_d[k] = U_s[k]
                continue

            # Damped propagation constant
            gamma = np.sqrt((-w**2 + 1j * w * c) + 0j) / a

            cosh_gl = np.cosh(gamma * L)
            sinh_gl = np.sinh(gamma * L)

            # Gibbs analytical solution for position at x=L
            U_d[k] = U_s[k] * cosh_gl - (F_s[k] / (E * A * gamma)) * sinh_gl

        # 4. Inverse Transform to Time Domain
        dh_pos = np.real(np.fft.ifft(U_d))

        # Final Buoyant Adjustment (Exact matching legacy subtraction)
        # Buoyant Weight = Rod Weight * (1 - rho_fluid/rho_steel)
        total_rod_weight = sum(
            s.length * (s.diameter**2 * np.pi / 4) * (STEEL_DENSITY_LB_PER_FT3 / SQUARE_FEET_TO_SQUARE_INCHES)
            for s in self.ctx.rod_string
        )
        buoyant_weight = total_rod_weight * BUOYANCY_FACTOR

        # Attenuation correction for load - final alignment
        dh_load = (f_surf - buoyant_weight) * LOAD_ATTENUATION_FACTOR

        # Stroke Synchronization
        dh_pos = dh_pos - np.min(dh_pos)
        # This legacy empirical factor remains unqualified (#1857). The Gibbs
        # comparison path is restricted to measured surface travel by
        # GIBBS_STROKE_RATIO_LIMIT; that is an implementation envelope, not a
        # physical prohibition on plunger overtravel.
        dh_pos = dh_pos * STROKE_SCALING_FACTOR

        validate_stroke(u_surf, dh_pos, dh_load, "gibbs")

        self.results.downhole_card = CardData(
            position=dh_pos.tolist(),
            load=dh_load.tolist()
        )
        self.results.downhole_load_basis = DownholeLoadBasis(
            buoyant_rod_weight_lbf=buoyant_weight,
            attenuation_factor=LOAD_ATTENUATION_FACTOR,
        )
        self.results.peak_polished_rod_load = np.max(f_surf)
        self.results.minimum_polished_rod_load = np.min(f_surf)
        self.results.ctx = self.ctx

        return annotate_results(self.results, self.ctx, "gibbs")

    def detect_buckling(self):
        """Detect rod buckling from the computed downhole card.

        Checks whether the minimum downhole load falls below the
        buckling detection threshold.

        Returns:
            True if buckling is detected, False otherwise.
        """
        if not self.results.downhole_card:
            return False
        loads = np.array(self.results.downhole_card.load)
        if np.min(loads) < BUCKLING_DETECTION_LOAD_THRESHOLD_LBS:
            self.results.buckling_detected = True
        return self.results.buckling_detected

    def calculate_fillage(self):
        """Calculate pump fillage from the solved downhole card.

        Delegates to :func:`calculations.calculate_pump_fillage`, which
        derives fillage from the net and gross plunger travel of the downhole
        card. This is the same computation ``DynacardAnalyzer.run_full_analysis``
        runs, so ``results.pump_fillage`` carries one meaning and one scale
        regardless of which entry point produced it.

        Returns:
            Pump fillage as a percentage of gross stroke (0-100).

        Raises:
            PhysicsError: If :meth:`solve_wave_equation` has not been run, so
                there is no downhole card to measure. This method previously
                assigned ``DEFAULT_PUMP_FILLAGE`` and returned it without
                examining the card, reporting the same fillage for every well
                -- and on a 0-1 scale, against the 0-100 percentage the
                analyzer pipeline writes into the same field (issue #1952 D1).
        """
        if not self.results.downhole_card:
            raise PhysicsError(
                "Pump fillage requires a downhole card; "
                "call solve_wave_equation() first.",
                solver="gibbs",
            )

        self.results.pump_fillage = calculate_pump_fillage(
            self.results.downhole_card
        ).fillage
        return self.results.pump_fillage
