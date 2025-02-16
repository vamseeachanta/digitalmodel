Dear Vamsee,

There are no scripts for these plots, they were created in Excel, after pasting into a spreadsheet the output from an OrcaFlex vessel response report:
https://www.orcina.com/webhelp/OrcaFlex/Redirector.htm?Vesselresponsereports.htm

If you still wish to proceed using Python, I can provide pointers into the documentation to assist you in starting your scripts, but we cannot write them for you.

You can access results from OrcaWave directly using the API by following the guidance here:
https://www.orcina.com/webhelp/OrcFxAPI/Redirector.htm?Pythonreference,Diffraction.htm
Via OrcaFlex, you can access the vessel response report through Python, see here:
https://www.orcina.com/webhelp/OrcFxAPI/Redirector.htm?Pythonreference,OrcaFlexVesselObject.htm
or you could access the OrcaFlex vessel RAO data directly, and for that it would be essential to read the object data page:
https://www.orcina.com/webhelp/OrcFxAPI/Redirector.htm?Pythoninterface,Objectdata.htm

Note that the preferred way to import OrcaWave output into OrcaFlex is to use OrcaFlex model LoadData, per:

import OrcFxAPI
model = OrcFxAPI.Model()
model.LoadData("diffraction.owr")

The call to LoadData will result in an OrcaFlex vessel type and vessel object being created in the OrcaFlex model for each included body in the OrcaWave analysis.


There are also no separate input files for the water depth and damping cases. Each change is the alteration of only a single data item in the OrcaWave data file. The water depth from 100 to 400, and the critical damping value in roll-roll element of the external matrix, between 0 and 36.01e3.

For L01, the OrcaWave data for percentage critical damping was not used, that 36.01e3 value was entered directly instead. In the very first diffraction analysis of the OrcaFlex default vessel, which was done years before OrcaWave existed, that damping value is noted to be approximately 8% of critical, but that percentage might have changed as a result of the improvements to the diffraction mesh that OrcaWave makes possible.


I hope that something here is useful.

Regards,

Colin Lewis.
==============================

Dear Vamsee,

Automation scripts are usually produced by our clients for their internal use. We have written automation scripts in-house to suit our needs in the same way, but there is not yet any repository where clients can browse through these.

If you have a specific activity that you are automating, please give us as much detail as you can, and we can see if there is a script here that matches. If you have started a script and it is giving you error messages, or the behaviour is not as expected, we can support with that too, so please do share it.

In case you have not seen it, this page is a good start into the documentation of the Python interface to OrcaWave:
https://www.orcina.com/webhelp/OrcFxAPI/Redirector.htm?Pythonreference,Diffraction.htm

I hope that something here is useful.

Regards,

Colin Lewis.
 
T :  +44(0)1229 584742
E :  orcina@orcina.com
W:  www.orcina.com
     


==============================

From: Vamsee Achanta <vamseea@acma-inc.com> 
Sent: 13 February 2025 05:02
To: Orcina <orcina@orcina.com>
Subject: OrcaWave | Python Scripts

Dear Support Representative,

Similar to 58 or so OrcaFlex python scripts, do we have access to any OrcaWave scripts that we can leverage to run, postprocess etc. and save results, plots etc.?

Thank you very much,
Vamsee
