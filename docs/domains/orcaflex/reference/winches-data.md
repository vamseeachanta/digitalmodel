# Winches: Data

> Source: Orcina WebHelp (https://www.orcina.com/webhelp/)

### Name

Used to refer to the winch.

### Type

May be either **simple** or **detailed**. See [winches](Winches.htm#Types) for more details.

### Connect to object and position

The (massless) winch wire connects at least two objects, one at each end of the wire.

If more than two are specified, then the winch wire passes from the first connection point to the last via the given intermediate points. At intermediate connections, the winch wire slides freely through these points as if passing via small frictionless pulleys mounted there. The winch wire tension on either side then pulls on the intermediate points, so applying forces and (if the points are offset) moments to the objects concerned.

Each connection can be **fixed**, **anchored** or [connected to another object](Objectconnections.htm). Each connection point has $x$, $y$ and $z$ [connection coordinates](Objectconnections.htm#ConnectionCoordinates) that determines its initial positions.

### Release at start of stage

The winch wire can be [released](Generaldata,Analysis.htm#AnalysisStageRelease) at the start of a given stage of the simulation, by setting this number to the stage number required. Once released the winch no longer applies any forces to the objects it connects. If no release is required, then set this item to '~'.
