#!/usr/bin/env python

import sys
import json

def getFSM(entries):

    trans_for_state = {}

    for e in entries:
        s1, trans, s2 = None, None, None

        for f,v in e['match_fields'].iteritems():
            if f == 'meta.query.state':
                s1 = v[0]
            else:
                trans = (f, tuple(v))

        if 'action_name' in e:
            if e['action_name'].endswith('set_egress_port'):
                s2 = ('port', e['action_params']['port'])
            elif e['action_name'].endswith('set_mgid'):
                s2 = ('mgid', e['action_params']['mgid'])
            elif e['action_name'].endswith('set_next_state'):
                s2 = ('state', e['action_params']['next_state'])
            elif e['action_name'].endswith('query_drop'):
                s2 = ('drop', 0)
            else:
                raise Exception("Unrecognized action name: %s" % e['action_name'])

        assert s1 is not None, e

        if s1 not in trans_for_state: trans_for_state[s1] = {}
        assert trans not in trans_for_state[s1]
        trans_for_state[s1][trans] = s2

    return trans_for_state

def getMgidMap(mcast_str):
    groups = {}
    for l in mcast_str.split('\n'):
        mgid, ports = l.split(':')
        groups[int(mgid)] = set(map(int, ports.split()))
    return groups

def equivalentBDD(bdd1, bdd2, state1=0, state2=0):
    """ Assert that bdd1 and bdd2 are equivalent, starting from state1 and state2 respectively """
    fsm1, fsm2 = bdd1['fsm'], bdd2['fsm']
    trans1, trans2 = fsm1[state1], fsm2[state2]

    # Should have the same predicates
    preds1, preds2 = set(trans1.keys()), set(trans2.keys())
    assert preds1 == preds2, "%s != %s" % (preds1, preds2)

    for p in preds1:
        state_type1, state_val1 = trans1[p]
        state_type2, state_val2 = trans2[p]

        assert state_type1 == state_type2, "%s != %s" % (state_type1, state_type2)

        if state_type1 == 'state':
            equivalentBDD(bdd1, bdd2, state1=state_val1, state2=state_val2)
        elif state_type1 == 'mgid':
            assert bdd1['mgid_map'][state_val1] == bdd2['mgid_map'][state_val2]
        else:
            assert state_val1 == state_val2, "%s, %s, %s != %s" % (state_type1, state_type2, state_val1, state_val2)


filenames = sys.argv[1:]
#filenames = ["entries1.json", "mcast1.txt", "entries2.json", "mcast2.txt"]

if len(filenames) < 1:
    sys.stderr.write("Usage: %s ENTRIES1 MCAST1 ENTRIES2 MCAST2 ..." % sys.argv[0])
    sys.exit(1)

BDDs = []

for (entries_fn, mcast_fn) in zip(filenames[::2], filenames[1::2]):
    with open(entries_fn, 'r') as f:
        fsm = getFSM(json.load(f))
    with open(mcast_fn, 'r') as f:
        mgid_map = getMgidMap(f.read())
    BDDs.append(dict(fsm=fsm, mgid_map=mgid_map))

for bdd in BDDs[1:]:
    equivalentBDD(BDDs[0], bdd)
