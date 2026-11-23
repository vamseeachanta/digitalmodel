# Shapes: Data

> Source: Orcina WebHelp (https://www.orcina.com/webhelp/)

## Data common to all shapes

### Name

Used to refer to the [shape](Shapes.htm).

### Type

One of [elastic solid](Shapes.htm#ShapesElasticSolids), [trapped water](Shapes.htm#ShapesTrappedWater), [drawing](Shapes.htm#ShapesDrawing), [wire frame](Shapes.htm#ShapesWireFrame) or [label](Shapes.htm#ShapesLabels).

### Shape

One of [block](Shapes,Blocks.htm), [cylinder](Shapes,Cylinders.htm), [curved plate](Shapes,Curvedplates.htm) or [plane](Shapes,Planes.htm).

### Connection

A shape can be **fixed**, **anchored** or [connected to another object](Objectconnections.htm).

### Position

Each shape has position and orientation data. The position is always that of the origin of the shape's local axes. For [blocks](Shapes,Blocks.htm), [wire frame shapes](Shapes,Wireframe.htm) and [label shapes](Shapes,Labels.htm) it is named **origin**; for [cylinders](Shapes,Cylinders.htm) and [curved plates](Shapes,Curvedplates.htm) it is named **end position**; and for [planes](Shapes,Planes.htm) it is named **point on plane**. In all cases, the position is specified by $x$, $y$ and $z$ [connection coordinates](Objectconnections.htm#ConnectionCoordinates). The nature of the orientation data differs between different shapes; these data are described separately for each shape.

## Further data

Each **shape** has further data specific to that shape, described under the corresponding topic: see [blocks](Shapes,Blocks.htm), [cylinders](Shapes,Cylinders.htm), [curved plates](Shapes,Curvedplates.htm), [planes](Shapes,Planes.htm), [wire frame shapes](Shapes,Wireframe.htm) and [label shapes](Shapes,Labels.htm).

## Data for elastic solids

The [elastic solid](Shapes.htm#ShapesElasticSolids)  **type** (independent of **shape**) has additional data for its stiffness and damping properties which do not apply to any other type. They are not intended for detailed modelling of inter-object contact, so the precise values given are not usually important: it is more important that the overall behaviour of the interaction is reasonable. Their stiffness should, then, be sufficiently large to keep penetration small, but not so large as to introduce very short natural periods which in turn require very short simulation time steps.

### Normal stiffness

Can be either linear or nonlinear. To specify a linear stiffness, enter a single stiffness value that is the reaction force that the solid applies per unit depth of penetration per unit area of contact. For nonlinear stiffness, use [variable data](Variabledata.htm) to specify a table of reaction force per unit area of contact against depth of penetration.

### Shear stiffness

The **shear stiffness** is used by the [friction calculation](Frictiontheory.htm). A value of 0 disables friction. A value of ~ tells OrcaFlex to use the normal stiffness value: if the normal stiffness is nonlinear, then the normal stiffness corresponding to zero penetration is used.

### Damping

The [percentage of critical damping](Shapetheory.htm) for the elastic solid. Damping is only included when using the [explicit integration scheme](Generaldata,Dynamics.htm#ExplicitIntegration).
