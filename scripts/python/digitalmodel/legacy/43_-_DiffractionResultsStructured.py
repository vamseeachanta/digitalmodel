import OrcFxAPI

diff = OrcFxAPI.Diffraction("semi.owr")
hydrostaticResults = diff.hydrostaticResults
print(hydrostaticResults[0]["mass"])
print(hydrostaticResults[0]["volume"])
print(hydrostaticResults[0]["restoringMatrix"])
