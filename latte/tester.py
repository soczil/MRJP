import os
import argparse
import subprocess

parser = argparse.ArgumentParser()
parser.add_argument(
    '-d',
    '--directory',
    type=str,
    required=True,
    dest='dir'
)
parser.add_argument(
    '--good',
    dest='good',
    action='store_true'
)
parser.add_argument(
    '--bad',
    dest='good',
    action='store_false'
)
parser.set_defaults(good=True)

args = parser.parse_args()

if not args.dir.endswith('/'):
    args.dir += '/'

if args.good:
    dest = args.dir + 'good/'
else:
    dest = args.dir + 'bad/'

input_files = []
for file in os.listdir(dest):
    if file.endswith('.lat'):
        input_files.append(file)

input_files.sort()
for file in input_files:
    print(file[:-3], end=': ')
    p = subprocess.run(['./latc_llvm', dest + file], capture_output=True)
    if args.good:
        if (p.returncode != 0 
            or p.stderr.decode() != 'OK\n' 
            or p.stdout.decode() != ''):
            print('ERRORR')
        else:
            print('OKK')
    else:
        if (p.returncode != 1
            or not p.stderr.decode().startswith('ERROR\n')
            or p.stdout.decode() != ''):
            print('ERRORR')
        else:
            print('OKK')
