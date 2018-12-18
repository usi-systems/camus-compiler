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
            elif e['action_name'].endswith('set_next_state'):
                s2 = ('state', e['action_params']['next_state'])
            elif e['action_name'].endswith('query_drop'):
                s2 = ('drop', 0)

        assert s1 is not None, e

        if s1 not in trans_for_state: trans_for_state[s1] = {}
        assert trans not in trans_for_state[s1]
        trans_for_state[s1][trans] = s2

    return trans_for_state

def equivalentFSM(fsm1, fsm2, state1=0, state2=0):
    """ Assert that fsm1 and fsm2 are equivalent, starting from state1 and state2 respectively """
    trans1, trans2 = fsm1[state1], fsm2[state2]

    # Should have the same predicates
    preds1, preds2 = set(trans1.keys()), set(trans2.keys())
    assert preds1 == preds2, "%s != %s" % (preds1, preds2)

    for p in preds1:
        state_type1, state_val1 = trans1[p]
        state_type2, state_val2 = trans2[p]

        assert state_type1 == state_type2, "%s != %s" % (state_type1, state_type2)

        if state_type1 == 'state':
            equivalentFSM(fsm1, fsm2, state1=state_val1, state2=state_val2)
        else:
            assert state_val1 == state_val2, "%s != %s" % (state_val1, state_val2)


filenames = sys.argv[1:]
#filenames = ["entries1.json", "entries1.json"]

if len(filenames) < 1:
    sys.stderr.write("Usage: %s JSON_ENTRIES1 JSON_ENTRIES2 ..." % sys.argv[0])
    sys.exit(1)


fsms = []
for fn in filenames:
    with open(fn, 'r') as f:
        entries = json.load(f)
        fsm = getFSM(entries)
        fsms.append(fsm)

for fsm in fsms:
    equivalentFSM(fsms[0], fsm)
