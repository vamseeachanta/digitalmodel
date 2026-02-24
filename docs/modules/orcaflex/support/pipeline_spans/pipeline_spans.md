Hello Vamsee,

We have a number of pipelay examples available on our website. The models explore different methods for representing a stinger. I have also attached a model and a series of links to this email to demonstrate other relevant features:
<https://www.orcina.com/resources/examples/?key=e>

? Pipeline spanning (lay to rough seabed).dat
o Simple pipelay operation onto a 2D profiled seabed. This could be developed further to use a 3D seabed described by a list of seabed coordinates. You could use this kind of model as the basis for in place pipeline analysis, carrying the seabed friction properties through with a restart analysis.
? For modelling interaction between the pipeline and the seabed in the tangential plane, one option is to apply the regular OrcaFlex friction model. Alternatively, if you want to model something more complex, such as modelling breakout from a berm, you can assign lateral and axial resistance profiles. This feature was introduced in v11.2 and you can find more information on this subject by watching the following user group meeting video (<https://www.youtube.com/watch?v=-p4RO4hMt7I&t=265s>).
? In v11.3 we enhanced line modelling further by allowing users to define line contents in a tabular form. This feature can be used in combination with expansion tables to model pipeline buckling due to changes in the temperature of the pipeline contents. For more information, see the following video (<https://www.youtube.com/watch?v=HlCJHK1Flx8&t=240s>).
? When modelling the seabed reaction load in the direction normal to the seabed, typically users treat seabed stiffness as a global property. However for pipeline analysis, there may be reason to vary the seabed stiffness along the length of the pipeline. We made changes in v11.4 to allow users to accomplish this. Details of the changes are highlighted in this video (<https://www.youtube.com/watch?v=eNEez6rL-yA&t=264s>).
? Finally, we intend to release a new version of OrcaFlex towards the end of this year (v11.5). The release will include enhancements to allow users to nominate cover models which can be used to model the load acting on a buried line. This enhancement is intended to allow users to model upheaval buckling.

I hope you find this information useful.

Kind regards,
Max Nicholson
=======================

From: Vamsee Achanta <vamseea@acma-inc.com>
Sent: 29 October 2024 11:22
To: Orcina <orcina@orcina.com>
Subject: OrcaFlex | Pipeline Analysis Capabilities

Dear Support Representative,

Do you have an example of pipeline installation analysis i.e. laydown pipe and assess the free-spans similar what happens in SAGE or similar software?

Mainly interested in knowing the extend or capabilities of the OrcaFlex for pipeline basic analysis that is usually dealt in ABAQUS or MATHCAD i.e. free-span, lateral buckling, global buckling etc.

Thank you,
Vamsee
