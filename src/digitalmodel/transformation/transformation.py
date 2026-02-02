class Transformation:
    def __init__(self):
        pass

    def router(self, cfg=None):
        self.cfg = cfg
        self.get_transformation_groups()
        return self.cfg
    
    def get_transformation_groups(self):
        """
        Get transformation groups from configuration
        """
        if self.cfg is None:
            self.cfg = {}
            
        # Initialize transformation groups if not present
        if "transformation_groups" not in self.cfg:
            self.cfg["transformation_groups"] = []
            
        # Add default transformation group if none exist
        if not self.cfg["transformation_groups"]:
            self.cfg["transformation_groups"] = [
                {
                    "name": "default_transformation",
                    "type": self.cfg.get("type", "eigen"),
                    "parameters": {}
                }
            ]
            
        return self.cfg["transformation_groups"]
