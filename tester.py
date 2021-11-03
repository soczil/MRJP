import os
import argparse
import subprocess
import filecmp

parser = argparse.ArgumentParser()
parser.add_argument(
    '-m',
    '--machine',
    type=str,
    required=True,
    dest='machine'
)
parser.add_argument(
    '-d',
    '--directory',
    type=str,
    required=True,
    dest='dir'
)
parser.add_argument(
    '-c',
    '--clean',
    type=bool,
    default=True,
    dest='clean'
)

args = parser.parse_args()

program = ""
if args.machine.lower() == 'jvm':
    program = 'insc_jvm'
elif args.machine.lower() == 'llvm':
    program = 'insc_llvm'
else:
    print('wrong machine', file=sys.stderr)
    exit(1)

if not args.dir.endswith('/'):
    args.dir += '/'

input_files = []
for file in os.listdir(args.dir):
    if file.endswith('.ins'):
        input_files.append(file)

input_files.sort()
for file in input_files:
    subprocess.run(['./' + program, args.dir + file])

os.chdir(args.dir)
for file in input_files:
    print(file, end=': ')
    test_file = file[:-4] + '.test'
    output_file = file[:-4] + '.output'
    f = open(test_file, 'w')
    if args.machine.lower() == 'jvm':
        subprocess.run(['java', file[:-4] + '.class'], stdout=f)
    else:
        subprocess.run(['lli', file[:-4] + '.bc'], stdout=f)
    f.close()
    
    if filecmp.cmp(test_file, output_file):
        print('OK')
    else:
        print('BAD')

if args.clean:
    for file in os.listdir('.'):
        if (file.endswith('.j') 
            or file.endswith('.class') 
            or file.endswith('.ll') 
            or file.endswith('.bc') 
            or file.endswith('.test')):
            os.remove(file)
