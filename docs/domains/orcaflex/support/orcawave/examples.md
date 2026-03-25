> **Note**: OrcaWave is a **frequency-domain** diffraction/radiation solver (`OrcFxAPI.Diffraction`).
> It is NOT the same as OrcaFlex (`OrcFxAPI.Model`), which is a time-domain dynamics solver.
> Both use `import OrcFxAPI` but serve different purposes.
> OrcaWave produces `.owr` results (RAOs, added mass, QTF); OrcaFlex produces `.sim` time histories.

=========================================

From: Orcina <orcina@orcina.com>
Sent: Thursday, October 24, 2024 9:39 AM
To: Vamsee Achanta <vamseea@none.com>
Cc: Scott McClure <scottm@none.com>
Subject: RE: OrcaFlex Vessel Data | AQWA vs. OrcaWave Interface

Vamsee

OrcaFlex can import AQWA output directly, and has been able to do so for many years. This is documented here:

<https://www.orcina.com/webhelp/OrcaFlex/Redirector.htm?Importinghydrodynamicdata.htm>
<https://www.orcina.com/webhelp/OrcaFlex/Redirector.htm?Importinghydrodynamicdata,AQWA.htm>

For OrcaWave, again OrcaFlex can import the data directly. Because we develop both OrcaWave and OrcaFlex, the import process is extremely smooth. You are less likely to encounter wrinkles when importing OrcaWave data, although the AQWA import process is still pretty efficient.

Similar to OrcaFlex examples (link below), do you have good starting examples for OrcaWave to perform hydrodynamic analysis? I am specifically looking for both UI examples and also any python interfacing code.
<https://www.orcina.com/resources/examples/>

If you follow the link that you provided you will find a number of examples dedicated to diffraction.

For the Python interface to OrcFxAPI, again there is documentation. There is the main reference documentation here: <https://www.orcina.com/webhelp/OrcFxAPI/>

But there is also a more discursive document here: <https://www.orcina.com/resources/documentation/>  (An introduction to the Python interface to OrcaFlex). Although it says OrcaFlex, this covers OrcaWave too. We often treat OrcaWave as being part of the broader OrcaFlex family, hence the naming doesn't always call it out explicitly.

Best regards,

David Heffernan

=========================================

Dear Support Representative,

Sub: OrcaFlex Vessel Data | AQWA vs. OrcaWave Interface

Does OrcaFlex have the ability to read hydrodynamic output seamlessly for the following programs (or) do we need to provide the displacement raos, load raos & QTFs in a grid format and feed to OrcaFlex?
a/ Aqwa output (Project already has this data)
b/ OrcaWave output (Project did not run OrcaWave)

Similar to OrcaFlex examples (link below), do you have good starting examples for OrcaWave to perform hydrodynamic analysis? I am specifically looking for both UI examples and also any python interfacing code.
<https://www.orcina.com/resources/examples/>

Thank you,
Vamsee
