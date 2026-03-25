**RE: OrcaFlex | Goal Seek Analysis**

Dear Vamsee

Yes we have some examples that use the fsolve module from the scipy optimize toolbox to iterate through some design variables until a certain target has been reached.
<https://docs.scipy.org/doc/scipy/reference/generated/scipy.optimize.fsolve.html>

The most recent of these was from our 2018 UGM where this tool was used to change the pipe length in order to meet a certain clearance from a roller support on the vessel. The models and pdf which discuss the python script can be downloaded from here.

There is also an older example from our 2013 UGM that uses the same fsolve module to adjust a line length in order to get a towed fish buoy to sit at a certain depth for statics. The files and documents from this example can be downloaded here. Please note this script was written in Python 2, so would need some minor adjustment to run in Python 3.

Both of these examples work by running through iterative static solves. For your first case that you mention below you would need to run dynamics and check for a threshold etc being exceeded before resetting the wave height and increasing it. This is more of an iterative approach (so simply changing wave height and rerunning) so could be done without fsolve say.

There are many Python optimisation tools available, fsolve was one we happened to use for our examples, but you may want to consider others.

Best Regards

Ian Dooley

====================

From: Achanta Vamsee <Vamsee.Achanta.guest@saipem.com>
Sent: Saturday, March 23, 2024 6:56 AM
To: Orcina <orcina@orcina.com>
Subject: OrcaFlex | Goal Seek Analysis

Dear Support,

Do you have a good example of goal seek analysis using python. Some examples, I want to achieve are below

a/ Get a parameter value (MBR) by varying wave height (WaveHeight)

b/ achieve top angle (declination) by varying catenary length (Line, Length[3])

Thank you,
Vamsee
