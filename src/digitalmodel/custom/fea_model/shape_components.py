class Shape():

    def __init__(self, cfg):
        self.cfg = cfg

    def get_orcaflex_properties(self):
        shape = {}

        general = {"Name": "Base1", "ShapeType": "Drawing", "Shape": "Block", "Connection": "Anchored"}
        shape.update(general)

        geometry = {"Origin": [-3, -3, 0], "Size": [6, 6, 3.622], "Orientation": [0, 0, 0]}
        shape.update(geometry)

        drawing = {"InsidePen": [1, "Solid", "Blue"], "NumberOfLines": 2}
        shape.update(drawing)

        shaded_drawing = {
            "ShadedDrawingFileName":
                "! '%ApplicationFolder%\Shaded Drawing Samples\Subsea Template\Subsea Template.x'",
            "ShadedDrawingDrawSize":
                11,
            "ShadedDrawingCullingMode":
                "Anticlockwise",
            "ShadedDrawingOrigin": [3, 3, 0]
        }
        shape.update(shaded_drawing)

        return shape
