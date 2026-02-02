import yaml

with open("taper.yml", 'r') as ymlfile:
    cfg = yaml.load(ymlfile)

print(cfg)

print(cfg['material'])

print(cfg['material']['E'])
print(cfg['material']['rho'])
