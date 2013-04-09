import subprocess
import unittest
import glob

def validateTestcase(self, input_file, result_file):
    prog = subprocess.Popen(['../322-interps/L1', input_file],stdout=subprocess.PIPE)
    with open(result_file) as results:
        self.assertEqual(prog.stdout.read(), results.read())

def testCompilerTestCase(self, input_file, result_file):
    subprocess.call(['./comp', input_file])
    prog = subprocess.Popen(['./a.out'],stdout=subprocess.PIPE)
    with open(result_file) as results:
        print(input_file)
        self.assertEqual(prog.stdout.read(), results.read())

class Tests(unittest.TestCase):

    '''
    def testValidateAll(self):
        all_inputs = sorted(glob.glob('tests/*.L1'))
        all_results = sorted(glob.glob('tests/*.res'))

        for i,r in zip(all_inputs, all_results):
            validateTestcase(self, i, r)
    '''

    def testCompilerAll(self):
        all_inputs = sorted(glob.glob('../322-interps/tests/L1/*.L1'))
        all_results = sorted(glob.glob('../322-interps/tests/L1/*.res'))

        for i,r in zip(all_inputs, all_results):
            testCompilerTestCase(self, i, r)


if __name__ == '__main__':
    unittest.main()
