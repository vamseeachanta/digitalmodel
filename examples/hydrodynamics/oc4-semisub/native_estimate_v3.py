# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Conditional dense-solver estimates from the supplied no-lid benchmark."""

import math


def estimate(hull_panels, frequencies, *, lid_panels=0):
    """Use N² memory and N³ time; these assumptions require a native pilot.

    Supplied observations: 14,064 panels, 3043.96 s over 32 frequencies,
    35.608 GB peak; chunk averages 82.7 to 112.3 s per frequency. Hardware,
    heading comparability and actual native automatic-lid count are unverified.
    """
    for number, positive in ((hull_panels, True), (frequencies, True), (lid_panels, False)):
        if (not isinstance(number, (int, float)) or not math.isfinite(number)
                or number != int(number) or number < (1 if positive else 0)):
            raise ValueError("Counts must be finite integers with nonempty hull and frequency axes")
    ratio = (hull_panels + lid_panels) / 14064
    mean = 3043.96 / 32
    return {"status": "ESTIMATED_NOT_MEASURED", "hull_panels": hull_panels,
            "reference_lid_panels": lid_panels, "effective_panels": hull_panels + lid_panels,
            "frequencies": frequencies, "panel_ratio": ratio,
            "memory_gb": 35.608 * ratio**2, "time_h": mean * frequencies * ratio**3 / 3600,
            "chunk_time_range_h": [v * frequencies * ratio**3 / 3600 for v in (82.7, 112.3)],
            "scaling_status": "ASSUMED, based on previous experience",
            "capacity_established": False, "native_lid_count_verified": False}
