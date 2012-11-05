#!/usr/bin/python
 
import unittest
from subprocess import Popen, STDOUT, PIPE
from subprocess import call
import uuid
import os,sys,time
import argparse
import difflib

tmpdir = "/tmp/OPAM.UNITTEST"
results = "results"
opamcmd = "/tmp/OPAM.BIN/opam --yes --root /tmp/OPAM.ROOT"

verbose=0
FNULL=open(os.devnull, "w")

def diff(fromfile,tofile,verbose=0):
    fromdate = time.ctime(os.stat(fromfile).st_mtime)
    todate = time.ctime(os.stat(tofile).st_mtime)
    fromlines = open(fromfile, 'U').readlines()
    tolines = open(tofile, 'U').readlines()

    diff = difflib.unified_diff(fromlines, tolines, fromfile, tofile)#, fromdate, todate)
    l = list(diff)

    if l :
        print "File Differences : "
        print ''.join(l)
        return False
    else :
        return True

def opam(cmd, diffile=None,verbose=0):
    # first we exectute the action, then
    # we compare the output of list with
    # the diffile
    env = os.environ.copy()
    env['PATH'] = ':'.join(['/tmp/OPAM.BIN',env['PATH']])
    env['OPAM_ROOT'] = '/tmp/OPAM.ROOT'
    cmd = opamcmd.split() + cmd
    if verbose > 1 :
        print "Env\nPATH=%s\nOPAM_ROOT=%s" % (env['PATH'],env['OPAM_ROOT'])
        print "CMD=%s" % (' '.join(cmd))
    if verbose <= 1 :
        call(cmd, stdout=FNULL, env=env)
    else :
        call(cmd, env=env)

    if diffile :
        f = "%s/%s.opamtest" % (tmpdir,uuid.uuid1())
        output = open(f,'w')

        call(opamcmd.split() + ["list"], stdout=output, env=env)

        output.close()
        d = diff(f,diffile,verbose)
        os.remove(f)
        return d

    return None

def load_scenario(scenario,verbose=0):
    if verbose > 0 :
        print "Loading scenario %d" % scenario
    if verbose <= 1 :
        call(["./init-repo.sh", "-s", str(scenario)],stdout=FNULL)
    else :
        call(["./init-repo.sh", "-s", str(scenario)])

class OpamTests(unittest.TestCase):

    def setUp(self):
        if verbose > 0 : print "\nsetting up repository"
        call(["./init-repo.sh", "-i"], stdout=FNULL)
        if not os.path.exists(tmpdir):
            os.makedirs(tmpdir)

    def tearDown(self):
        if verbose > 0 : print "tearing down repository"
        call(["./init-repo.sh", "-c"], stdout=FNULL)

    def test_install(self):
        load_scenario(1,verbose)
        diffile="%s/install-P1" % results
        d = opam(["install", "P1"],diffile,verbose)
        self.assertTrue(d)

    def test_install_many(self):
        load_scenario(1,verbose)
        diffile="%s/install-P1-P2-P3-P4" % results
        opam(["install", "P1"])
        opam(["install", "P2"])
        opam(["install", "P3"])
        d = opam(["install", "P4"], diffile,verbose)
        self.assertTrue(d)

    def test_remove(self):
        load_scenario(1,verbose)
        diffile="%s/install-remove-P1" % results
        opam(["install", "P1"])
        d = opam(["remove", "P1"],diffile,verbose)
        self.assertTrue(d)

    def test_upgrade(self):
        load_scenario(1,verbose)
        diffile="%s/install-upgrade-P2" % results
        opam(["install", "P4"])
        d = opam(["upgrade", "P2"],diffile,verbose)
        self.assertTrue(d)

#    @unittest.skip("skipping")
    def test_reinstall(self):
        load_scenario(1,verbose)
        diffile="%s/reinstall-P2" % results
        opam(["install", "P2"])
        d = opam(["reinstall", "P2"],diffile,verbose)
        self.assertTrue(d)

    def test_install_opt(self):
        load_scenario(1,verbose)
        load_scenario(2,verbose)
        diffile="%s/install-opt" % results
        opam(["install", "P5"])
        opam(["install", "P2"])
        opam(["remove", "P5"])
        opam(["remove", "P2"])
        d = opam(["remove", "P1"],diffile,verbose)
        self.assertTrue(d)

def main():
    global verbose

    parser = argparse.ArgumentParser(description='description of you program')
    parser.add_argument('-v', '--verbose', action='store_true')
    parser.add_argument('-d', '--debug', action='store_true')
    args = parser.parse_args()

    verbosity=0
    if args.verbose == True :
        verbose = 1
        verbosity=2

    if args.debug == True :
        verbose = 2

    suite = unittest.TestLoader().loadTestsFromTestCase(OpamTests)
    unittest.TextTestRunner(verbosity=verbosity).run(suite)

if __name__ == '__main__':
    main()

