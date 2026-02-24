**RE: Setting Line Orientations**

====================

Hi,

If you already know the azimuth and declination of the line at the two ends relative to the global reference frame, your goal is to report those angles about a new axis system, that of the two vessel objects to which the line will be connected.

The easiest way to accomplish this is to fix the lines to the global frame, input the azimuth and declination angles accordingly, then update the line connection status so that endA is connected to the host and endB to the installation vessel. You can do this via the API and the line end azimuth and declination angles will be updated automatically so that they are quoted in the vessel local axis system.

If you want to do this yourself, I recommend you look up Euler rotation matrices.

Kind regards,
Max Nicholson
====================

**2024-03-20**

I have a Host vessel and an Installation vessel and an angle between them w.r.t global directions.

I know the azimuth and declination in global coordinates. How can I define end orientations of the lines with a formula (not going into OrcaFlex GUI at all). Do you have an knowledge article  or something I can use?

Example data:
Host: 0  0             -9.46      0             -0.21      0
Installation vessel: 100   150        0             0             0             26

Line orientations. I know the target declination in the lines I need at the Host (5 deg with vertical) and Installation vessel (0 deg with vertical)

Thank you,
Vamsee
