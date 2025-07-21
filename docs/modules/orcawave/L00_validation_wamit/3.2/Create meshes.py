from math import pi, sin, cos
import numpy


def SaveToGDF(Panels, ISX, ISY, gdffilename):
    gdfString = 'gdf created by Python script\n'
    gdfString += '1.0 9.806650    ULEN, GRAV\n'
    gdfString += '1 1             ISX, ISY\n'
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
    csfString += '1 1             ISX, ISY\n'
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


def SaveToFDF(Panels, fdffilename, RINNER, NTCL, NAL, DELR, NCIRE, NGSP):

    def Pad(s):
        while len(s) < 20:
            s += ' '
        return s

    NPF = len(Panels)
    fdfString = 'fdf created by Python script\n'
    fdfString += Pad('{:.1f}'.format(RINNER)) + 'RINNER\n'
    fdfString += Pad('{:d} {:d}'.format(NPF, NTCL)) + 'NPF, NTCL\n'
    fdfString += Pad('{:d} {:.1f} {:d} {:d}'.format(NAL, DELR, NCIRE, NGSP)) + 'NAL, DELR, NCIRE, NGSP\n'
    for iPanel, Panel in enumerate(Panels):
        for i in [0, 1]:
            fdfString += '  ' * i + \
                '{:<20.14f}{:<20.14f}{:<20.14f}{:<20.14f}'.format(Panel[0][i], Panel[1][i], Panel[2][i], Panel[3][i])
            if i == 1:
                fdfString += 'end of panel {:d}\n'.format(iPanel + 1)
            else:
                fdfString += '\n'
    with open(fdffilename, 'w') as outputfile:
        outputfile.write(fdfString)
    outputfile.closed


def MakePanels(phiCount, IncludeLid, UseCosineSpacing):
    phi1D = numpy.linspace(0, 0.5 * pi, phiCount + 1)
    thetaCount = phiCount
    if UseCosineSpacing:
        # cosine spacing at waterline
        theta1D = 0.5 * pi + 0.5 * pi * (1 - numpy.cos(numpy.linspace(0, pi / 2, thetaCount + 1)))
    else:
        theta1D = numpy.linspace(0.5 * pi, pi, thetaCount + 1)  # uniform theta spacing

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
    if IncludeLid:

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


def CSPanels(phiCount):
    OuterRadius = 1.2
    WetPanels = numpy.array(MakePanels(phiCount, False, False)) * OuterRadius
    SurfacePanels = PZPanels(phiCount, 1.2)

    Panels = WetPanels
    for Panel in SurfacePanels:
        Panels = numpy.concatenate([Panels, [[Panel[0], Panel[3], Panel[2], Panel[1], ]]], axis=0)

    return Panels


def PZPanels(phiCount, RINNER):
    phi1D = numpy.linspace(0, 0.5 * pi, phiCount + 1)
    rCount = int(numpy.ceil((RINNER - 1.0) / (0.5 * pi / phiCount)))
    r1D = numpy.linspace(1, RINNER, rCount + 1)

    def Node(r, phi):
        return [r * cos(phi), r * sin(phi), 0]

    Panels = []
    for rIndex in range(rCount):
        for pIndex in range(phiCount):
            Panel = [
                Node(r1D[rIndex], phi1D[pIndex]),
                Node(r1D[rIndex], phi1D[pIndex + 1]),
                Node(r1D[rIndex + 1], phi1D[pIndex + 1]),
                Node(r1D[rIndex + 1], phi1D[pIndex]),
            ]
            Panels.append(Panel)

    return Panels


Panels = MakePanels(12, True, True)
SaveToGDF(Panels, 1, 1, r'OrcaWave v11.0 files\SphereWithLid.gdf')
SaveToGDF(Panels, 1, 1, r'Wamit v6.4S files\SphereWithLid.gdf')

RINNER, NTCL = 3, 20
Panels = PZPanels(12, RINNER)
SaveToFDF(Panels, r'OrcaWave v11.0 files\PanelledZone.fdf', RINNER, NTCL, 2, 2.0, 5, 10)
SaveToFDF(Panels, r'Wamit v6.4S files\PanelledZone.fdf', RINNER, NTCL, 2, 2.0, 5, 10)

Panels = CSPanels(8)
SaveToCSF(Panels, 1, 1, r'OrcaWave v11.0 files\SphereWithLid.csf')
SaveToCSF(Panels, 1, 1, r'Wamit v6.4S files\SphereWithLid.csf')
