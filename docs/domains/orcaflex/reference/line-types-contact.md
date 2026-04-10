# Line types: Contact data

> Source: Orcina WebHelp (https://www.orcina.com/webhelp/)

## Outer contact diameter

Contact between lines and the [seabed](Environment,Seabeddata.htm), [elastic solids](Shapes.htm#ShapesElasticSolids) or [clashing with other lines](Linetheory,Clashing.htm) takes full account of the diameter of the line: this value gives the diameter to be used in these contact calculations. If the value is '~', then the value given for the line [outer diameter](Linetypes,Geometry,massexpansiondata.htm#LineTypeDiameters) is used.

Contact diameter is also used in the calculation of [line contact clearance](Lineresults,Contact.htm#LineResultsClearance) results and as the drawing diameter for [shaded graphics](3Dviews.htm#GraphicsMode) views.

|  |  |
| --- | --- |
| Note: | For contact between lines and the seabed or elastic solids, contact is modelled between a node and the contact surface. For a node at a line section boundary, we choose the contact diameter to be the *larger* of the contact diameters of the line types either side of the node. For line-to-line contact, the contact is between pairs of segments: since each segment has a single well-defined line type, there is no ambiguity over contact diameter. |

## Inner contact diameter

The [inside](Linecontact,Modelling.htm#LineContactInteractionModel) or [around](Linecontact,Modelling.htm#LineContactInteractionModel) types of [line contact](Linecontact,Modelling.htm) both represent the placement of one line *within* another. For this contact model, an inner contact diameter is therefore required to describe the bore of the outermost line in the contact relationship. If the value is '~', then the value given for the line [inner diameter](Linetypes,Geometry,massexpansiondata.htm#LineTypeDiameters) is used.

At a line section boundary, for a node of the [penetrating](Linecontact,Data.htm#PenetratingAndSplinedLines) line in a line contact relationship, we choose the inner contact diameter to be the smaller of the contact diameters of the line types either side of the node.

|  |  |
| --- | --- |
| Note: | Although the contact diameters cannot be directly specified for [homogeneous pipe](Linetypes,Data.htm#LineTypeCategory) category line types, both the inner and outer contact diameter presented for these line types will account for any [coatings or linings](Linetypes,Coatingsliningsdata.htm) applied to the homogeneous pipe line type. |

## Clash stiffness and clash damping

The stiffness and damping values used by the [clashing algorithm](Linetheory,Clashing.htm). Damping is only included when using the [explicit integration scheme](Generaldata,Dynamics.htm#ExplicitIntegration).
