from math import pi, sin, cos
import numpy


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


def MakePanels(phiCount):
    thetaCount = phiCount
    theta1D = numpy.linspace(0.5 * pi, pi, thetaCount + 1)
    phi1D = numpy.linspace(0, 0.5 * pi, phiCount + 1)

    def Node(theta, phi):
        return [sin(theta) * cos(phi), sin(theta) * sin(phi), cos(theta)]

    Panels = []
    for tIndex in range(thetaCount):
        for pIndex in range(phiCount):
            Panel = [
                Node(theta1D[tIndex], phi1D[pIndex]),
                Node(theta1D[tIndex + 1], phi1D[pIndex]),
                Node(theta1D[tIndex + 1], phi1D[pIndex + 1]),
                Node(theta1D[tIndex], phi1D[pIndex + 1])
            ]
            Panels.append(Panel)

    def Node(r, phi):
        return [r * cos(phi), r * sin(phi), 0]

    rCount = int(numpy.ceil(phiCount / (0.5 * pi)))
    r1D = numpy.linspace(0, 1, rCount + 1)
    for rIndex in range(rCount):
        for pIndex in range(phiCount):
            Panel = [
                Node(r1D[rIndex], phi1D[pIndex]),
                Node(r1D[rIndex + 1], phi1D[pIndex]),
                Node(r1D[rIndex + 1], phi1D[pIndex + 1]),
                Node(r1D[rIndex], phi1D[pIndex + 1])
            ]
            Panels.append(Panel)

    return Panels


for phiCount in [6, 8, 11, 16, 22, 32, 44]:
    Panels = MakePanels(phiCount)
    print('{:d}\t{:d}'.format(phiCount, len(Panels)))
    SaveToGDF(Panels, 1, 1, r'OrcaWave v11.0 files\HemisphereAndLid{:04d}.gdf'.format(len(Panels)))
