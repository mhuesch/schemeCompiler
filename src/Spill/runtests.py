import subprocess
import unittest
import glob

def runTestcase(self, input_file, result_file):
    spillProg = subprocess.Popen(['./spill', input_file],stdout=subprocess.PIPE)
    idProg = subprocess.Popen(['./id', result_file],stdout=subprocess.PIPE)
    with open(result_file) as results:
        self.assertEqual(spillProg.stdout.read(), idProg.stdout.read())

class Tests(unittest.TestCase):

    def testValidateAll(self):
        all_inputs = sorted(glob.glob('../../322-interps/tests/mhuesch/spill-test/*.L2f'))
        all_results = sorted(glob.glob('../../322-interps/tests/mhuesch/spill-test/*.sres'))

        for i,r in zip(all_inputs, all_results):
            print i
            runTestcase(self, i, r)


if __name__ == '__main__':
    unittest.main()
