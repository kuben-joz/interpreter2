import argparse
from pathlib import Path
from glob import glob
import subprocess
import sys


def main():
    path = Path(sys.argv[1])
    files = glob(str(path / "*.lat"))
    print(files)
    print('')
    for fn in files:
        p = subprocess.run(
            ["./latc", fn],
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
        )
        if p.stderr.decode("ascii")[:6] != "ERROR\n":
            print(Path(fn).stem)


if __name__ == "__main__":
    main()
