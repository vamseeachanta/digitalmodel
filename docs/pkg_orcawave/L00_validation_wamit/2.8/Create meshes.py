from __future__ import print_function
from math import pi, sin, cos
import numpy

a, b, c, z0, CSfactor = 2.0, 1.0, 1.0, 0.2, 2.0


def EllipsoidPanels(thetaCount, phiCount):
    PanelCount = thetaCount * phiCount
    z1D = (z0 - c) * (1 - numpy.cos(numpy.linspace(0, pi, thetaCount + 1))) * 0.5  # double cosine spacing for z
    theta1D = numpy.arccos((z1D - z0) / c)
    phi1D = numpy.linspace(0, pi / 2, phiCount + 1)

    def Node(iTheta, iPhi):
        theta = theta1D[iTheta]
        phi = phi1D[iPhi]
        return [
            a * cos(phi) * sin(theta),
            b * sin(phi) * sin(theta),
            z0 + c * cos(theta)
        ]

    Panels = numpy.zeros(shape=(PanelCount, 4, 3))
    for iTheta, theta in enumerate(theta1D[:-1]):
        for iPhi, phi in enumerate(phi1D[:-1]):
            Panels[iTheta * phiCount + iPhi] = [
                Node(iTheta, iPhi),
                Node(iTheta + 1, iPhi),
                Node(iTheta + 1, iPhi + 1),
                Node(iTheta, iPhi + 1)
            ]
    return Panels


def EllipticalAnnulusPanels(rhoCount, phiCount):
    PanelCount = rhoCount * phiCount
    rho1D = numpy.sqrt(1 - (z0 / c)**2) * numpy.linspace(1, CSfactor, rhoCount + 1)
    phi1D = numpy.linspace(0, pi / 2, phiCount + 1)

    def Node(iRho, iPhi):
        rho = rho1D[iRho]
        phi = phi1D[iPhi]
        return [
            a * rho * cos(phi),
            b * rho * sin(phi),
            0
        ]

    Panels = numpy.zeros(shape=(PanelCount, 4, 3))
    for iRho, rho in enumerate(rho1D[:-1]):
        for iPhi, phi in enumerate(phi1D[:-1]):
            Panels[iRho * phiCount + iPhi] = [
                Node(iRho, iPhi),
                Node(iRho + 1, iPhi),
                Node(iRho + 1, iPhi + 1),
                Node(iRho, iPhi + 1)
            ]
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


def BuildGDF(thetaCount, phiCount):
    Panels = EllipsoidPanels(thetaCount, phiCount)
    PanelCount = len(Panels)
    gdffilename = 'Ellipsoid{:04d}.gdf'.format(PanelCount)
    SaveToGDF(Panels, 1, 1, r'OrcaWave v11.0 files\{:s}'.format(gdffilename))
    SaveToGDF(Panels, 1, 1, r'Wamit v7.3 files\{:04d}\{:s}'.format(PanelCount, gdffilename))
    print('Written ' + gdffilename)


def BuildCSF(rhoCount, thetaCount, phiCount):
    PanelsBelowSurface = EllipsoidPanels(thetaCount, phiCount) * CSfactor
    PanelsOnSurface = EllipticalAnnulusPanels(rhoCount, phiCount)
    Panels = numpy.append(PanelsOnSurface, PanelsBelowSurface, axis=0)
    PanelCount = len(Panels)
    csffilename = 'Ellipsoid{:04d}.csf'.format(PanelCount)
    SaveToCSF(Panels, 1, 1, r'OrcaWave v11.0 files\{:s}'.format(csffilename))
    SaveToCSF(Panels, 1, 1, r'Wamit v7.3 files\{:04d}\{:s}'.format(PanelCount, csffilename))
    print('Written ' + csffilename)


for rhoCount in [4, 6, 8, 10, 12, 15, 18, 22, 26]:
    BuildGDF(thetaCount=3 * rhoCount, phiCount=2 * rhoCount)
    BuildCSF(rhoCount, thetaCount=2 * rhoCount, phiCount=2 * rhoCount)
