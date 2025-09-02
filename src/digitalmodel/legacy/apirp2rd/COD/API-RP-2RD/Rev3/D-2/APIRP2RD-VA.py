import yaml

with open("dataManager\\APIRP2RD.yml", 'r') as ymlfile:
    config = yaml.load(ymlfile)
  
for section in config:
    print(section)

print(config)