import os
import argparse
import subprocess
import filecmp

bad = 0

def check_output(file, dest):
    bc_file = file[:-3] + 'bc'
    output_file = file[:-3] + 'output'
    test_file = file[:-3] + 'test'
    f = open(test_file, 'w')
    if bc_file == dest + 'core018.bc':
        input_file = open(dest + 'core018.input', 'r')
        p = subprocess.run(['lli', bc_file], stdin=input_file , stdout=f)
        input_file.close()
    else:
        p = subprocess.run(['lli', bc_file], stdout=f)
    f.close()
    if p.returncode != 0 or not filecmp.cmp(test_file, output_file):
        print('BAD')
        bad += 1
    else:
        print('OKK')

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
parser.add_argument(
    '--clean',
    dest='clean',
    action='store_true'
)
parser.add_argument(
    '--no-clean',
    dest='clean',
    action='store_false'
)
parser.set_defaults(clean=True)

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
    print(file[:-4], end=': ')
    latc_file = dest + file
    p = subprocess.run(['./latc_llvm', latc_file], capture_output=True)
    if args.good:
        if (p.returncode != 0 
            or not p.stderr.decode().startswith('OK\n') 
            or p.stdout.decode() != ''):
            print('BAD')
            bad += 1
            print(p.returncode)
            print(p.stderr.decode())
        else:
            check_output(latc_file, dest)
    else:
        if (p.returncode != 1
            or not p.stderr.decode().startswith('ERROR\n')
            or p.stdout.decode() != ''):
            print('BAD')
            bad += 1
        else:
            print('OKK')
            # print(p.stderr.decode() + '\n')

if args.clean:
    for file in os.listdir(dest):
        if (file.endswith('.ll')
            or file.endswith('.bc')
            or file.endswith('.test')):
            os.remove(dest + file)

if bad == 0:
    print('\n\n\n\nKLASUNIA SWIATOWA\n\n\n\n')
