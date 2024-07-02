import OrcFxAPI

model = OrcFxAPI.Model()
general = model.general

# two ways to obtain the count, we prefer the first option
print(f"(1) Stage count = {len(general.StageDuration)}")
print(f"(2) Stage count = {general.GetDataRowCount('StageDuration')}")

# modify the count
general.SetDataRowCount("StageDuration", 4)
print(f"(3) Stage count = {len(general.StageDuration)}")

# get and set the entire column of data
print(f"(4) Stage duration = {general.StageDuration}")
general.StageDuration = 8, 7, 12, 3, 29, 42 # sets count and values
print(f"(5) Stage duration = {general.StageDuration}")

# get items by index, note Python convention for negative indexing
print(f"(6) build-up duration = {general.StageDuration[0]}")
print(f"(7) final stage duration = {general.StageDuration[-1]}")

# delete and insert rows
general.StageDuration.DeleteRow(1)
general.StageDuration.InsertRow(3)
general.StageDuration[3] = 25
print(f"(8) Stage duration = {general.StageDuration}")

# iterate over the values
for index, value in enumerate(general.StageDuration):
    print(f"(9) Stage {index} duration = {value}")
