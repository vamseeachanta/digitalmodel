"""Finite physical input guards shared by the screening entry points."""
import math


def require_positive(name, value):
    if not math.isfinite(value) or value <= 0:
        raise ValueError(f'{name} must be finite and positive')


def require_nonnegative(name, value):
    if not math.isfinite(value) or value < 0:
        raise ValueError(f'{name} must be finite and nonnegative')


def validate_lock_in_inputs(reduced_velocity, vr_min, vr_max):
    require_nonnegative('Reduced velocity', reduced_velocity)
    require_positive('Minimum reduced velocity', vr_min)
    require_positive('Maximum reduced velocity', vr_max)
    if vr_min >= vr_max:
        raise ValueError('Lock-in minimum must be below maximum')
