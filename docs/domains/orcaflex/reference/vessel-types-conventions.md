# Vessel types: Conventions

> Source: Orcina WebHelp (https://www.orcina.com/webhelp/)

The conventions page (on the [vessel types data form](Vesseltypes.htm)) contains settings that define the meaning of the [RAO](Vesseltypes,RAOs.htm), [QTF](Vesseltypes,WavedriftandsumfrequencyQTFs.htm#QTFs), and (in the case of the direction conventions)  [stiffness, added mass and damping](Vesseltypes,Stiffness,addedmassanddamping.htm) data. This enables you to enter these data directly from many other programs without having to convert the values between different sets of conventions. Instead you can tell OrcaFlex the conventions that apply to those data and OrcaFlex will then automatically allow for those conventions when it uses the data.

|  |  |
| --- | --- |
| Warning: | In general, the conventions apply to **all** displacement RAO, load RAO and QTF data. You cannot mix data with differing conventions in the same model (but see [rotational RAOs](#VesselTypeRotationalConventions) below). |
|  | The QTF data source conventions are only used to describe the diffraction analysis QTF calculations to OrcaFlex – they are **not** used in application of any other vessel data. |

Although RAOs are simple enough in principle, a number of complications make them notoriously error-prone and difficult to check in practice. The main issues are:

* Different coordinate systems.
* Different definitions of phase angle and rotational RAOs.
* Use of vessel symmetry, e.g. to obtain motions in seas from the port side given data for seas from the starboard side.

OrcaFlex provides easy ways of handling these problem areas.

The use of differing coordinate systems and conventions by different suppliers of data is the main source of confusion. It is vital that you know the conventions that apply to the RAO tables that you are using. Unfortunately, not all RAO tables fully document the conventions used: see [RAO data checklist](Vesseltypes,RAOdatachecklist.htm) for help finding out what conventions apply to your data and see [checking RAOs](Vesseltypes,CheckingRAOs.htm) to check that the conventions are set correctly.

### Translational RAO conventions

Translational [displacement RAOs](Vesseltypes,RAOs.htm) are always non-dimensional (e.g. metres/metre or feet/foot).

Translational [load RAOs](Vesseltypes,RAOs.htm) are always given as force per unit wave amplitude (e.g. kN/m).

### Rotational RAO conventions

Roll, pitch and yaw [displacement RAOs](Vesseltypes,RAOs.htm) may use one of the following three conventions:

* As rotation angles per unit **wave amplitude**. The values are dimensional, e.g. in degrees/metre, radians/foot etc.
* As rotation angles per unit **maximum wave slope**. Maximum wave slope is the true maximum slope of the wave surface, which is $\pi h/l$ radians ($=180\,h/l$ degrees) for wave height $h$ and wave length $l$. The values are non-dimensional, which can be thought of as either degrees/degree or radians/radian which, of course, are equivalent.
* As rotation angles per unit **wave steepness**. Wave steepness is a commonly used angular measure of a wave, defined by steepness $=h/l$ radians ($= \frac{180}{\pi}\,h/l$ degrees). The values are again non-dimensional, i.e. either degrees/degree or (equivalently) radians/radian, but of course the RAOs are numerically larger (by a factor of $\pi$) than those using the maximum wave slope convention.

In each case, the angles of rotation may be given in either **degrees** or **radians**; the convention on the data form should be specified accordingly. For the two non-dimensional cases, the units (degrees or radians) of the max wave slope or wave steepness must be the same as those for the angles of rotation. Note that, so long as the units are consistent in this way, the actual numerical values for these non-dimensional RAOs are the same regardless of whether the unit selected is degrees or radians.

Rotational [load RAOs](Vesseltypes,RAOs.htm) must be given as moment per unit length (e.g. kN.m/m), and [rotational QTFs](Vesseltypes,WavedriftandsumfrequencyQTFs.htm) as moment per unit length squared (e.g. kN.m/m2). Therefore, the degrees/radians switch and the unit amplitude / steepness / maximum slope switch do **not** apply to load RAOs. This is an exception to the above rule that the conventions apply to *all* RAOs and QTFs.

|  |  |
| --- | --- |
| Warning: | If rotational displacement RAOs are given relative to wave slope or steepness, then OrcaFlex (internally) converts them to be relative to wave amplitude using the **deep water** wavelength, **not** the wavelength for the water depth specified in the model. |

### Waves are referred to by

The RAO and QTF data can be specified by period in seconds, by angular frequency in radians/second or by frequency in hertz.

### RAO & QTF phases

The RAO & QTF phase convention is specified by three data items:

1. Phases are either **leads** or **lags**.
2. Phases are in either **degrees** or **radians**.
3. The phase defines the time at which the maximum positive value of the motion occurs. This is relative to the time at which the wave **crest**, **trough**, **zero up-crossing** or **zero down-crossing** passes the [phase origin](Vesseltypes,RAOs.htm#RAOPhaseOrigin).

|  |  |
| --- | --- |
| Note: | These phase conventions do not apply to [Newman approximation wave drift QTFs](Vesseltypes,WavedriftandsumfrequencyQTFs.htm#QTFNewmansApproximation), since they are entered without any phase information. But the phase conventions do apply to [full difference frequency and sum frequency QTFs](Vesseltypes,WavedriftandsumfrequencyQTFs.htm#FullQTFs). |

### Directions

You must specify the directions that correspond to positive motion or load in the RAO and QTF data, and in the [stiffness](Vesseltypes,Stiffness,addedmassanddamping.htm#VesselTypeHydrostaticStiffness), [damping](Vesseltypes,Stiffness,addedmassanddamping.htm#VesselTypeDamping) and [added mass](Vesseltypes,Stiffness,addedmassanddamping.htm#VesselTypeAddedMass) matrices. The most common convention is as given by the default OrcaFlex vessel type: a right-handed system with Z upwards and clockwise rotations being positive.

### Symmetry

You can specify symmetry of the vessel type. OrcaFlex will then use the [RAO](Vesseltypes,RAOs.htm) and [QTF](Vesseltypes,WavedriftandsumfrequencyQTFs.htm#QTFs) tables for wave directions on one side of the symmetry plane to derive tables for the reflected directions on the other side of the plane, or in the case of circular symmetry to derive the RAOs and QTFs for any arbitrary direction from the single direction given. [Other damping](Vesseltypes,Otherdamping.htm) is also treated in two different ways, depending on whether circular or non-circular symmetry is selected.

The symmetry can be set to:

* **None:** The vessel type has no symmetry. The directions specified must cover all the wave directions used in the simulation.
* **xz plane** (or **yz plane**)**:** this specifies that the xz (or yz) plane through the [RAO origin](Vesseltypes,RAOs.htm#VesselTypeRAOOrigin) is a plane of symmetry. For each direction given, OrcaFlex uses symmetry to derive tables for the reflected direction on the other side of the plane.
* **xz and yz planes:** This specifies that **both** the xz and yz planes through the RAO origin are planes of symmetry. For each direction given, OrcaFlex uses symmetry to derive tables for the reflected directions in the other 3 quadrants.
* **Circular:** This specifies that the vessel has circular symmetry about the RAO origin. RAO/QTF tables can only be given for one wave direction, and OrcaFlex uses symmetry to derive tables for all other directions.

|  |  |
| --- | --- |
| Warning: | If you specify some planes of symmetry then all the RAO origins, QTF origins and phase origins **must** be on all the planes of symmetry. Or if you specify circular symmetry then all the RAO origins, QTF origins and phase origins **must** be on the vertical axis of symmetry. |

|  |  |
| --- | --- |
| Note: | These symmetry conventions do not apply to [sea state RAOs](Vesseltypes,SeastatedisturbanceRAOs.htm). Sea state RAOs are defined at points located outside the vessel hull, in open water. The vessel hull symmetry properties are therefore not relevant to these data. |

### QTF data source conventions

The precise calculation of a [common second order load](Vesseltheory,Wavedriftandsumfrequencyloads.htm#QTFCommonSecondOrderLoads) may be affected by the QTF data source conventions, so these conventions must be set to match those of the diffraction program that was used to calculate the QTFs.

Firstly, given that displacement RAO data are available, correctly applying rotational motion from these data requires OrcaFlex to know the [order](Vesseltheory,Vesselrotations.htm#OrderOfApplicationOfRotations) of successive rotations and the [axes](Vesseltheory,Vesselrotations.htm#SuccessiveRotationsAxes) about which they should be applied. See [vessel rotations](Vesseltheory,Vesselrotations.htm) for the meaning of the different options available.

OrcaFlex will set the rotation conventions automatically upon [import](Importinghydrodynamicdata.htm) of [displacement RAO](Vesseltypes,RAOs.htm) data from a WAMIT input file; see the [WAMIT import](Importinghydrodynamicdata,WAMIT.htm#WAMITRAORotationConvention) page for details.

Secondly, the QTF data might refer to any of the frames of reference available to the diffraction analysis. Typically, these are

* A fixed global reference frame
* An instantaneous frame that follows the assumed translation of the vessel, but does *not* rotate.
* An instantaneous frame that follows both the assumed translation *and* rotation of the vessel

OrcaFlex caters only for the first two cases (the usual ones), in which the QTF data are output by the diffraction package using a non-rotating frame. You must also therefore specify which of these is the case – either the **earth** (fixed global) or the **body** (instantaneous translation) frame of reference.

The reference frame convention is only set automatically upon import of OrcaWave results. For other data sources, we must refer you to the suppliers of those programs to enquire about these calculation details.
