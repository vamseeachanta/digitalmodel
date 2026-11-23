# El Centro 1940 NS Accelerogram

Fixture: `elcentro_ns_1940.txt`

Source used for this committed digitization:
`https://www.vibrationdata.com/elcentro.dat`, linked from Vibrationdata's
public data page as "El Centro Earthquake 1940. Time History Data File for
North-South Component, time(sec) & Acceleration (G)".

Event/station provenance cross-check:
COSMOS Virtual Data Center event 88, El Centro, CA - Array Sta 9; Imperial
Valley Irrigation District, USGS station 0117. COSMOS lists corrected
acceleration, velocity, displacement, and spectra for this event/station.

Redistribution basis: this is the public Vibrationdata digitization of the
USGS/COSMOS strong-motion record, not a PEER NGA restricted download. The
underlying station/event record is US-government-origin strong-motion data.

Pinned fixture stats from the committed file:

- Units: g
- Samples: 1560
- Time step: 0.02 s
- Time range: 0.0 to 31.18 s
- PGA: 0.31882 g
- sha256: `9836d860a6943ae728bed3b52e6dbbf0d4f54fd123f52b54e5980fd6e6c9d0aa`

This PR1 fixture is used only for the registered offline preprocessing
workflow. Published response-spectrum and intensity-measure validation remains
deferred to later issue slices.
