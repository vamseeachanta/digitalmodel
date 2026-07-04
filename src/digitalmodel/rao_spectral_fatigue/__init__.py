"""Frequency-domain fatigue from a stress RAO and a wave spectrum.

Generalises the ``spectral_fatigue`` workflow's PSD input: instead of supplying
a precomputed stress PSD, supply a stress transfer function (RAO) ``H(f)`` and a
parametric wave spectrum (Hs, Tp). The stress response PSD is formed by the
standard linear transfer relation ``S_stress(f) = |H(f)|**2 * S_wave(f)`` and
handed to the tested ``digitalmodel.fatigue.spectral_fatigue`` damage chain.
"""
