# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

import numpy as np
from .calculations import calculate_pump_fillage
from .exceptions import PhysicsError
from .solver_guards import annotate_results, validate_stroke
from .models import DynacardAnalysisContext, CardData, AnalysisResults
from .constants import (
    FEET_TO_INCHES,
    SQUARE_FEET_TO_SQUARE_INCHES,
    SECONDS_PER_MINUTE,
    BUCKLING_DETECTION_LOAD_THRESHOLD_LBS,
)


class DynacardPhysicsSolver:
    """Coupled frequency-domain solution for a vertical, tapered elastic rod.

    Rod sections are ordered from surface to pump. ``damping_factor`` is the
    nonnegative viscous decay rate c in inverse seconds for
    u_tt + c*u_t = a²*u_xx. Zero explicitly selects the lossless model.
    This path does not estimate damping or establish field diagnostic accuracy.
    Inputs must be uniformly timed over one cycle, excluding a repeated endpoint.
    """

    def __init__(self, context: DynacardAnalysisContext):
        self.ctx = context
        self.results = AnalysisResults()

    def _validate_inputs(self):
        """Reject invalid boundary and material inputs before Fourier arithmetic."""
        u = np.asarray(self.ctx.surface_card.position, dtype=float)
        f = np.asarray(self.ctx.surface_card.load, dtype=float)
        validate_stroke(u, u, f, "gibbs")
        if (
            not np.isfinite(self.ctx.spm)
            or self.ctx.spm <= 0
            or not np.isfinite(self.ctx.fluid_density)
            or self.ctx.fluid_density < 0
        ):
            raise PhysicsError(
                "positive SPM and nonnegative fluid density required", solver="gibbs"
            )
        if not self.ctx.rod_string:
            raise PhysicsError("rod string must contain a section", solver="gibbs")
        for section in self.ctx.rod_string:
            positive = [
                section.length,
                section.diameter,
                section.modulus_of_elasticity,
                section.speed_of_sound,
                section.density,
            ]
            if (
                not np.all(np.isfinite(positive))
                or min(positive) <= 0
                or not np.isfinite(section.damping_factor)
                or section.damping_factor < 0
            ):
                raise PhysicsError(
                    "invalid rod section properties or damping rate", solver="gibbs"
                )
        return u, f

    def solve_wave_equation(self):
        """Propagate displacement AND tensile force through every rod section.

        Basis: Applied Mechanics of Solids (online edition read 2026-10-19), section 4.3.3:
        https://solidmechanics.org/Text/Chapter4_3/Chapter4_3.php
        With x downward, u upward, F=-EA*u_x. Fourier substitution in
        u_tt+c*u_t=a²*u_xx gives gamma²=(-omega²+i*omega*c)/a².
        Differentiating the displacement solution gives the companion force
        equation (also specified in #1955). See gibbs-transfer.md for derivation.
        """
        u_surf, f_surf = self._validate_inputs()
        count = len(u_surf)
        period = SECONDS_PER_MINUTE / self.ctx.spm
        omega = 2 * np.pi * np.fft.rfftfreq(count, d=period / count)
        displacement = np.fft.rfft(u_surf)
        force = np.fft.rfft(f_surf)

        # Section matrices enforce continuity of displacement and total force
        # at each taper. Geometric A=pi*d²/4 and unit conversions are exact.
        try:
            with np.errstate(over="raise", invalid="raise", divide="raise"):
                for section in self.ctx.rod_string:
                    length = section.length * FEET_TO_INCHES
                    area = np.pi * section.diameter**2 / 4
                    ea = section.modulus_of_elasticity * area
                    speed = section.speed_of_sound * FEET_TO_INCHES
                    gamma = (
                        np.sqrt(
                            -(omega[1:] ** 2) + 1j * omega[1:] * section.damping_factor
                        )
                        / speed
                    )
                    z = gamma * length
                    cosine, sine = np.cosh(z), np.sinh(z)
                    # sinh(gamma*L)/gamma = L*sinh(z)/z. np.sinc uses
                    # sin(pi*x)/(pi*x); the imaginary argument supplies sinh.
                    compliance = length / ea * np.sinc(z / (1j * np.pi))
                    old_u, old_f = displacement[1:].copy(), force[1:].copy()
                    displacement[1:] = cosine * old_u - compliance * old_f
                    force[1:] = cosine * old_f - ea * gamma * sine * old_u

                    # Static equilibrium: F_bottom=F_top-W_b. Integrating
                    # u_x=-F/EA gives -(F_top-W_b/2)*L/EA. Archimedes plus
                    # weight/volume input densities gives W_b=(rho_s-rho_f)*A*L.
                    weight = (
                        (section.density - self.ctx.fluid_density)
                        * area
                        * section.length
                        / SQUARE_FEET_TO_SQUARE_INCHES
                    )
                    displacement[0] -= (force[0] - count * weight / 2) * length / ea
                    force[0] -= count * weight
                dh_pos = np.fft.irfft(displacement, n=count)
                dh_load = np.fft.irfft(force, n=count)
                dh_pos -= np.min(dh_pos)
        except (FloatingPointError, OverflowError) as exc:
            raise PhysicsError(
                "non-finite frequency-domain propagation", solver="gibbs"
            ) from exc

        # irfft uses the real projection of the unpaired Nyquist component.
        # No empirical stroke or load multiplier is applied.
        validate_stroke(u_surf, dh_pos, dh_load, "gibbs")
        self.results.downhole_card = CardData(
            position=dh_pos.tolist(), load=dh_load.tolist()
        )
        self.results.peak_polished_rod_load = float(np.max(f_surf))
        self.results.minimum_polished_rod_load = float(np.min(f_surf))
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
