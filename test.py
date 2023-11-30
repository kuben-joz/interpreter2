import argparse
from pathlib import Path
from glob import glob
import subprocess


def main():
    files = glob("extra-tests/mrjp-tests-master/good/basic/*.lat")
    for fn in files:
        p = subprocess.run(
            ["./latc", fn],
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
        )
        if p.stderr.decode("ascii") != "ERROR\n":
            print(Path(fn).stem)


if __name__ == "__main__":
    main()
