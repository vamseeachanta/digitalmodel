import glob
import os


def fileList(data):
    fileList = []
    cwd = os.getcwd()
    os.chdir(cwd + "\\" + data['Folder'])
    for file in glob.glob(data['FileNameFilter']):
        fileList.append(os.path.join(os.getcwd(), file))
    os.chdir(cwd)

    return fileList
