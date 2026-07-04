"""Vessel seakeeping operability screening from motion RAOs and a sea state.

Wraps the tested ``digitalmodel.hydrodynamics.seakeeping`` frequency-domain
engine (RAO x wave spectrum -> response spectrum -> significant motion
amplitude) into a registry workflow: per degree of freedom, the significant
motion amplitude is compared against an operability criterion across a sea-state
scatter, yielding a per-DOF operability percentage and a top-level pass/fail.
"""
