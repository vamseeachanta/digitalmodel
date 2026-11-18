# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Synthetic cylinder shedding arithmetic using the package conversions.

Run from the repository root with the package dependencies installed::

    PYTHONPATH=src python examples/openfoam/cylinder_dimensionless.py

Basis: the Strouhal definition documented by ``strouhal_number`` in
``src/digitalmodel/solvers/openfoam/validation/cylinder.py``.
Independently, the advection time is D/U, so the number of cycles in that
time is St = f*(D/U). Units cancel as (cycles/s)*m/(m/s); cycles are a count.
Rearranging gives f = St*U/D in Hz.

All dimensional inputs below are synthetic choices, with these hand balances:

* f = 3.0 Hz, D = 0.4 m, U = 2.0 m/s: St = 3.0*0.4/2.0 = 0.6,
  and inverse f = 0.6*2.0/0.4 = 3.0 Hz.
* f = 1.5 Hz, D = 1.2 m, U = 3.0 m/s: St = 1.5*1.2/3.0 = 0.6,
  and inverse f = 0.6*3.0/1.2 = 1.5 Hz.

Sampling units: any sampled time coordinate would be in seconds and f in
cycles per second (Hz), rather than radians per second or cycles per sample.
Here f is prescribed: no sampled signal, sampling interval, FFT or frequency
estimation is used. Diameters are in metres and speeds in metres per second.

This arithmetic example introduces no empirical benchmark value and performs
no flow solve or wake validation. Only St is preserved by the dimensional
changes; no Reynolds number is prescribed. The native laminar cylinder
benchmark remains pending, including cylinder geometry, force histories,
sampling convergence and comparison of drag and shedding with references.
"""

from digitalmodel.solvers.openfoam.validation import (
    shedding_frequency,
    strouhal_number,
)


def example_results() -> list[dict[str, float]]:
    """Return the forward and inverse conversions for the hand balances above."""
    rows = []
    # Source of every input: the synthetic hand balances in the module docstring.
    for frequency_hz, diameter_m, velocity_m_s in (
        (3.0, 0.4, 2.0),
        (1.5, 1.2, 3.0),
    ):
        st = strouhal_number(frequency_hz, diameter_m, velocity_m_s)
        recovered_hz = shedding_frequency(st, velocity_m_s, diameter_m)
        rows.append(
            {
                "frequency_hz": frequency_hz,
                "diameter_m": diameter_m,
                "velocity_m_s": velocity_m_s,
                "strouhal": st,
                "recovered_frequency_hz": recovered_hz,
            }
        )
    return rows


if __name__ == "__main__":
    print("Synthetic arithmetic only; native cylinder benchmark pending.")
    for row in example_results():
        print(" ".join(f"{name}={value:.1f}" for name, value in row.items()))
