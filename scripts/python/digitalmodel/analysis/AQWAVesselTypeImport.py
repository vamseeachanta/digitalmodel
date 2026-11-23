import argparse
import OrcFxAPI

parser = argparse.ArgumentParser()
parser.add_argument("filePath", help="AQWA results file to import.")
args = parser.parse_args()

importFilePath = args.filePath
importFileType = OrcFxAPI.iftAQWA
model = OrcFxAPI.Model(threadCount=1)
vt = model.CreateObject(OrcFxAPI.otVesselType)
bodyMaps = (OrcFxAPI.VesselTypeDataDiffractionImportBodyMap(vt.Name, "Draught1"),)
multibodyGroupName = ""
requestedData = (
    OrcFxAPI.vdtStructure,
    OrcFxAPI.vdtLoadRAOs,
    OrcFxAPI.vdtStiffnessAddedMassDamping,
    # OrcFxAPI.vdtOtherDamping,
    OrcFxAPI.vdtDisplacementRAOs,
    # OrcFxAPI.vdtSymmetry,
    # OrcFxAPI.vdtDrawing
)
calculationMethods = ("lmHaskind",)

success, messages = model.ImportVesselTypeData(
    importFilePath,
    importFileType,
    bodyMaps,
    calculationMethods,
    vt.Name,
    multibodyGroupName,
    requestedData,
    clearExistingData=True,
)

print(messages)

if success:
    model.SaveData(args.filePath.replace(".lis", ".dat"))
