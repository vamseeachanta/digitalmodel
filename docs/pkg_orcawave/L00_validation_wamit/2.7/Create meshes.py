from __future__ import print_function
import numpy
from numpy import pi, sin, cos, linspace, array, ceil


def MakePyramidPanels(zCount, sCountAtFreeSurface, UseCosineSpacing, Nadir, SurfaceVertices):
    Draught = -Nadir[2]
    if UseCosineSpacing:
        z1D = -Draught * (cos(linspace(0, pi, zCount + 1)) + 1) / 2  # double cosine spacing
    else:
        z1D = linspace(-Draught, 0, zCount + 1)
    Panels = []
    for iFace in range(len(SurfaceVertices)):
        V1 = SurfaceVertices[(iFace + 1) % len(SurfaceVertices)]
        V2 = SurfaceVertices[iFace]

        def Node(s, z):
            S1 = Nadir + (V1 - Nadir) * (z + Draught) / Draught
            S2 = Nadir + (V2 - Nadir) * (z + Draught) / Draught
            return S1 + s * (S2 - S1)
        # Add a triangular panel at the nadir
        Panel = [Node(0, z1D[0]), Node(0, z1D[0]), Node(1, z1D[1]), Node(0, z1D[1]), ]
        Panels.append(Panel)
        # Add quad panels through the rest of the face
        for zIndex in range(1, zCount):
            sCount = int(sCountAtFreeSurface * (z1D[zIndex + 1] + Draught) / Draught)
            sCount = max(sCount, 1)
            s1D = linspace(0, 1, sCount + 1)
            for sIndex in range(sCount):
                Panel = [
                    Node(s1D[sIndex], z1D[zIndex]),
                    Node(s1D[sIndex + 1], z1D[zIndex]),
                    Node(s1D[sIndex + 1], z1D[zIndex + 1]),
                    Node(s1D[sIndex], z1D[zIndex + 1]),
                ]
                Panels.append(Panel)
    return Panels


def MakeCSPanels(zCount, sCountAtFreeSurface, Nadir, SurfaceVertices, ScaleFactor):
    # First get panels for the submerged portion of the control surface
    Panels = MakePyramidPanels(zCount, sCountAtFreeSurface, False, Nadir * ScaleFactor, SurfaceVertices * ScaleFactor)
    # Second add panels on the free surface portion of the control surface
    Draught = -Nadir[2] * ScaleFactor
    for iFace in range(len(SurfaceVertices)):
        Inner1 = SurfaceVertices[(iFace + 1) % len(SurfaceVertices)]
        Inner2 = SurfaceVertices[iFace]
        Outer1, Outer2 = Inner1 * ScaleFactor, Inner2 * ScaleFactor

        def Node(s, r):
            S1 = Outer1 + (Inner1 - Outer1) * r
            S2 = Outer2 + (Inner2 - Outer2) * r
            return S1 + s * (S2 - S1)
        rCount = zCount // 2
        sCount = sCountAtFreeSurface // 2
        r1D = linspace(0, 1, rCount + 1)
        s1D = linspace(0, 1, sCount + 1)
        for rIndex in range(rCount):
            for sIndex in range(sCount):
                Panel = [
                    Node(s1D[sIndex], r1D[rIndex]),
                    Node(s1D[sIndex + 1], r1D[rIndex]),
                    Node(s1D[sIndex + 1], r1D[rIndex + 1]),
                    Node(s1D[sIndex], r1D[rIndex + 1]),
                ]
                Panels.append(Panel)
    return Panels


def SaveToGDF(Panels, ISX, ISY, gdffilename):
    gdfString = 'gdf created by Python script\n'
    gdfString += '1.0 9.806650    ULEN, GRAV\n'
    gdfString += '{:d} {:d}             ISX, ISY\n'.format(ISX, ISY)
    gdfString += '{:<6d}          NPAN\n'.format(len(Panels))
    for iPanel, Panel in enumerate(Panels):
        for i in [0, 1, 2, 3]:
            gdfString += '  ' * i + '{:<20.14f}{:<20.14f}{:<20.14f}'.format(Panel[i][0], Panel[i][1], Panel[i][2])
            if i == 3:
                gdfString += 'end of panel {:d}\n'.format(iPanel + 1)
            else:
                gdfString += '\n'

    with open(gdffilename, 'w') as outputfile:
        outputfile.write(gdfString)
    outputfile.closed


def SaveToCSF(Panels, ISX, ISY, csffilename):
    csfString = 'csf created by Python script\n'
    csfString += '0               ILOWHICSF\n'
    csfString += '{:d} {:d}             ISX, ISY\n'.format(ISX, ISY)
    csfString += '{:<6d}          NPAN\n'.format(len(Panels))
    for iPanel, Panel in enumerate(Panels):
        for i in [0, 1, 2, 3]:
            csfString += '  ' * i + '{:<20.14f}{:<20.14f}{:<20.14f}'.format(Panel[i][0], Panel[i][1], Panel[i][2])
            if i == 3:
                csfString += 'end of panel {:d}\n'.format(iPanel + 1)
            else:
                csfString += '\n'

    with open(csffilename, 'w') as outputfile:
        outputfile.write(csfString)
    outputfile.closed


################################################################################
Nadir = array([0., 0., -3.])
SurfaceVertices = array([[-2., 5., 0.], [8., -5., 0.], [-3., -5., 0.]])  # clockwise viewed from above
SideLengths = numpy.linalg.norm(SurfaceVertices - SurfaceVertices[[1, 2, 0]], axis=1)
MeanSideLength = numpy.mean(SideLengths)


for zCount in range(6, 30, 2):
    print('zCount = {:d}'.format(zCount))
    sCountAtFreeSurface = int(zCount * MeanSideLength / -Nadir[2])

    SBPanels = MakePyramidPanels(zCount, sCountAtFreeSurface, True, Nadir, SurfaceVertices)
    print('Pyramid gdf has {:d} panels'.format(len(SBPanels)))
    SaveToGDF(SBPanels, 0, 0, r'OrcaWave v11.0 files\PyramidZC{:02d}.gdf'.format(zCount))
    SaveToGDF(SBPanels, 0, 0, r'Wamit v7.3 files\Convergence Study\{:02d}\PyramidZC{:02d}.gdf'.format(zCount, zCount))

    CSPanels = MakeCSPanels(zCount, sCountAtFreeSurface, Nadir, SurfaceVertices, 2)
    print('Pyramid csf has {:d} panels'.format(len(CSPanels)))
    SaveToCSF(CSPanels, 0, 0, r'OrcaWave v11.0 files\PyramidZC{:02d}.csf'.format(zCount))
    SaveToCSF(CSPanels, 0, 0, r'Wamit v7.3 files\Convergence Study\{:02d}\PyramidZC{:02d}.csf'.format(zCount, zCount))
    print('Ratio of csf to gdf panels = {:.2f}'.format(float(len(CSPanels)) / len(SBPanels)))
