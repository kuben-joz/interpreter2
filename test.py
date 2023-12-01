import argparse
from pathlib import Path
from glob import glob
import subprocess
import sys


def main():
    path = Path(sys.argv[1])
    bad = len(sys.argv) > 2
    files = glob(str(path / "*.lat"))
    print(files)
    print("")
    for fn in files:
        p = subprocess.run(
            ["./latc", fn],
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
        )
        if (bad and p.stderr.decode("ascii")[:6] != "ERROR\n") or (
            not bad and p.stderr.decode("ascii")[:3] != "OK\n"
        ):
            print(Path(fn).stem)


if __name__ == "__main__":
    main()
