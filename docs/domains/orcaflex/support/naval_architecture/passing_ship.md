Hi Vamsee,

Your understanding of OrcaWave is correct. It is a diffraction program used to calculate loading and response for wet bodies due to surface water waves.

OrcaWave can be used to provide OrcaFlex with vessel type data such load RAOs, displacement RAOs, added mass and damping, mean drift loads and full QTFs. If you want to model other effects such as wind loading, current loading or additional damping, the user must provide the coefficients.

Kind regards,
Max Nicholson

T :  +44(0)1229 584742
E :  <orcina@orcina.com>
W:  <www.orcina.com>

From: Vamsee Achanta <vamseea@none.com>
Sent: 04 November 2024 21:33
To: Orcina <orcina@orcina.com>
Cc: Scott McClure <scottm@none.com>
Subject: RE: OrcaWave | Multibody Analysis

Max,

A friendly follow-up to close out this support question.

OrcaWave is only for diffraction analysis and can only handle variations in wave fields around bodies. Not current flow fields.
OrcaFlex purely takes the output from OrcaWave + drag coefficients we provide. Drag coefficients responsibility is on the user.

Please confirm our understanding.

Thank you,
Vamsee

From: Scott McClure <scottm@none.com>
Sent: Wednesday, October 30, 2024 11:26 AM
To: Orcina <orcina@orcina.com>; Vamsee Achanta <vamseea@none.com>
Subject: RE: OrcaWave | Multibody Analysis

Max,

I am not sure I fully understand your last sentence.
I assume OrcaFlex can handle current flow using predefined current coefficients for an array of relative headings and is then capable of analyzing the system response to that current?
Do you mean that OrcaFlex cannot determine the coefficients themselves? I think that is what you are really saying here. Please confirm.

Thanks,
Scott

From: Orcina <orcina@orcina.com>
Sent: Wednesday, October 30, 2024 10:59 AM
To: Vamsee Achanta <vamseea@none.com>
Cc: Scott McClure <scottm@none.com>
Subject: RE: OrcaWave | Multibody Analysis

Dear Vamsee,

OrcaWave can undertake multibody diffraction analysis. In doing so, each body is influenced by the presence of the others through wave diffraction and wave radiation. For more information, I suggest you look at our multibody example available on our website (see L03). The example shows how multibody diffraction can be used to analyse a single semi sub platform divided into several bodies. However the same ideas can also be applied to groups of independent floating objects.

When you run a multibody diffraction analysis, you should expect to report extended added mass and damping matrices which capture the impact that oscillation of one body has on another member of the group. Furthermore there is potential for significant change to the load RAOs and displacement RAOs as well as the QTFs if you choose to calculate them.

OrcaWave/OrcaFlex are not capable of calculating fluid loading coefficients for bodies in a flow field. Instead, OrcaFlex is typically used take that loading coefficients and analyse the system response when subjected to various environmental load cases.

Kind regards,
Max Nicholson

T :  +44(0)1229 584742
E :  <orcina@orcina.com>
W:  <www.orcina.com>

From: Vamsee Achanta <vamseea@none.com>
Sent: 30 October 2024 03:27
To: Orcina <orcina@orcina.com>
Cc: Scott McClure <scottm@none.com>
Subject: OrcaWave | Multibody Analysis

Dear Support Representative,

Can we run a multibody diffraction analysis where each body is sufficiently close to influence the flow-field around the other bodies. We would like to do the following:
a/ Hydrodynamic data (i.e. RAOs, QTFs etc.) for each body taking the flow-field effect (as compared to free floating body) into account?
b/ This is a far shot which is the scope of CFD software but worth asking anyway. Is there a provision to get current coefficients/forces on each body taking the flow-field effect into account?

Is there an example that covers any of the above example(s) or scenarios?

Thank you very much,
Vamsee

Orcina Ltd., Daltongate, Ulverston, Cumbria LA12 7AJ, UK. Company no. 1996191, registered in England & Wales.
