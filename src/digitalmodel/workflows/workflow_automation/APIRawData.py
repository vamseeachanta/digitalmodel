def download_file_from_url(cfg):
    import os
    import time

    import wget

    url = cfg['url']
    filename = os.path.join(cfg['download_to'] + '/' + os.path.basename(url))

    if os.path.exists(filename):
        os.remove(filename)

    start_time = time.perf_counter()
    print("Dowloading file: {}".format(filename))
    wget.download(url, out=filename)
    end_time = time.perf_counter()
    print("Dowloading file: {} .... COMPLETE".format(filename))
    print("Time Taken to download: {} .... COMPLETE".format((end_time - start_time).__round__(3)))


if __name__ == '__main__':
    import os
    from pathlib import Path
    cfg = {
        'url': 'https://www.data.bsee.gov/Well/Files/APIRawData.zip',
        'download_to': os.path.abspath(Path("../../data_manager/data/bsee"))
    }

    download_file_from_url(cfg)
