"""Optional native dynamics progress logging without modifying solver inputs."""
from contextlib import contextmanager
import math
import time


@contextmanager
def dynamics_progress(model, interval_seconds, logger, label, *, clock=time.monotonic):
    """Log first, wall-time-spaced, and final callbacks; preserve cancellation.

    OrcFxAPI supplies simulation time/start/stop to its native callback. No
    polling, model property changes, time stepping, or cancellation is added.
    A pre-existing callback is called first and its return value is unchanged.
    """
    if interval_seconds is None:
        yield
        return
    interval = float(interval_seconds)
    if not math.isfinite(interval) or interval <= 0:
        raise ValueError('Progress interval must be finite and positive')
    previous = model.dynamicsProgressHandler
    started, last_logged = clock(), None

    def progress(current_model, simulation_time, start, stop):
        nonlocal last_logged
        cancel = previous(current_model, simulation_time, start, stop) if previous else False
        now = clock()
        if last_logged is None or now - last_logged >= interval or simulation_time >= stop:
            logger.info('Dynamics progress: %s wall_s=%.3f sim_time_s=%.6f '
                        'sim_elapsed_s=%.6f start_s=%.6f stop_s=%.6f',
                        label, now - started, float(simulation_time),
                        float(simulation_time - start), float(start), float(stop))
            last_logged = now
        return cancel

    model.dynamicsProgressHandler = progress
    try:
        yield
    finally:
        model.dynamicsProgressHandler = previous
