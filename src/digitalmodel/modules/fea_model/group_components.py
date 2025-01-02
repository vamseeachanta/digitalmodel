class Group():

    def __init__(self, cfg):
        self.cfg = cfg

    def get_orcaflex_properties(self):
        group = {}

        structure = {
            "With Torsion": "Model",
            "Without Torsion": "Model",
            "CALM Buoy1": "Without Torsion",
            "Chain4": "Without Torsion",
            "Chain6": "Without Torsion",
            "Chain5": "Without Torsion",
            "Chain2": "Without Torsion",
            "Chain3": "Without Torsion",
            "Base1": "Without Torsion",
            "Inner String": "Model",
            "Outer String": "Model",
            "FloatingBuoy": "Model"
        }
        group.update({"Structure": structure})

        collapsed = ["Variable Data"]
        state = {"Collapsed": collapsed}
        group.update({"State": state})

        return group
