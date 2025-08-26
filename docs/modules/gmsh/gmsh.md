
## Reverse Normals


Using the Command Line:
The command ReverseMesh Surface{-new_surfs()}; will reverse the normals of the specified surface.
The {-new_surfs()} part indicates that a new surface will be created with the reversed normals. This is useful for preserving the original mesh and creating a copy with reversed normals.
You can also use the API function mesh::setReverse to achieve the same result within a script. 
2. Using the Gmsh GUI:
Open your mesh file in Gmsh.
Go to "Tools" -> "Reverse shapes". This will reverse the normals of the selected surfaces. 

![Reversing normals](image.png)


3. Visualizing Normals:
To verify the change, you can enable "Backface Culling" in the Gmsh GUI (Options -> Mesh -> Color -> Backface Culling).
This will display one side of the mesh in a different color (e.g., gray), allowing you to visually check the normal direction. 


Before reversing:

![alt text](image-1.png)

After reversing:

![alt text](image-2.png)


References

https://gmsh.info/


Check mesh compatibility in open source software
https://prepomax.fs.um.si/

https://gitlab.com/MatejB/PrePoMax
https://gitlab.com/MatejB/PrePoMax-Models


https://grabcad.com/library/pneumatic-gripper-11

https://grabcad.com/dashboard


## Logging and Output Management

### Saving Logs from gmsh

There are several methods to capture and save logs from gmsh for debugging and analysis:

#### 1. GUI Method
- Open gmsh and run your operations
- Go to **Tools → Message Console** to view all messages
- Click **Save** button to manually save current session logs
- Alternatively, use **File → Save Messages As...** to export logs to a specific location

#### 2. Command Line Logging
Capture both standard output and error streams:

```bash
# Windows - Save all output to log file
gmsh.exe input.geo -o output.msh 2>&1 | tee gmsh.log

# Windows with verbose output (levels 0-5, higher = more detail)
gmsh.exe your_file.geo -v 5 > gmsh_output.log 2>&1

# Unix/Linux
gmsh input.geo -v 5 2>&1 | tee gmsh.log
```

#### 3. Default Log Locations

##### Windows
- Fatal errors: `%USERPROFILE%\.gmsh-errors`
- Session logs: Saved manually from GUI

##### Unix/Linux
- Fatal errors: `~/.gmsh-errors`
- Session logs: Saved manually from GUI

#### 4. Python API Logging
When using gmsh Python bindings:

```python
import gmsh

# Initialize with terminal output
gmsh.initialize()
gmsh.option.setNumber("General.Terminal", 1)  # Enable terminal output

# Use logger API
gmsh.logger.start()  # Start logging
# Your gmsh operations here
messages = gmsh.logger.get()  # Get log messages
gmsh.logger.stop()   # Stop logging

# Process messages
for message in messages:
    print(message)
    
gmsh.finalize()
```

#### 5. Verbose Levels
Control the amount of information logged:
- `0`: Minimal output (errors only)
- `1`: Warnings and errors
- `2`: Information messages (default)
- `3`: Detailed information
- `4`: Debug information
- `5`: Maximum verbosity

Example:
```bash
gmsh -v 3 model.geo  # Information level logging
```

#### 6. Important Notes
- **Automatic saving**: Full logs are automatically saved to disk only for fatal errors
- **Manual saving**: For regular warnings and information, manually save from the message console
- **Output redirection**: Most reliable method for capturing all output
- **Large models**: Consider log file size when using verbose mode with complex models

### References

https://www.manpagez.com/info/gmsh/gmsh-2.2.6/gmsh_63.php
https://victorsndvg.github.io/FEconv/formats/gmshmsh.xhtml
https://gmsh.info/doc/texinfo/gmsh.html

