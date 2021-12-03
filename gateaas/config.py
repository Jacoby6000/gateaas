import os
import sys
import validators
from gates.And import And
from gates.Or import Or
from gates.NOr import NOr
from gates.Not import Not
from gates.NAnd import NAnd
from gates.XOr import XOr
from gates.XNOr import XNOr
from gates.NoOp import NoOp

failed_load = False
nop_validator = lambda x: True
def safe_get(key, validator=nop_validator):
    if key in os.environ:
        result = os.environ[key]
        if validator(result):
            return result
        else:
            print("Env var `" + key + "` is malformed")
    else:
        print("Env var `" + str(key) + "` does not exist.")

    failed_load = True
    return None


GATE_TYPE=safe_get("GATE_TYPE", lambda s: s in gate_options)
OUTPUTS=safe_get("OUTPUTS", lambda urls: all(validators.url(url) for url in urls.split(",")))
#INPUT_COUNT=safe_get("INPUT_COUNT", lambda s: s.isdigit() and int(s) > 0 and ((int(s) == 1 and GATE_TYPE=="NOT") or GATE_TYPE!="NOT"))
PORT=safe_get("PORT", lambda s: s.isdigit() and int(s) > 0 and int(s) < 65536)
BIND_ADDRESS=safe_get("BIND_ADDRESS", lambda s: all(part.isdigit() and int(part) >= 0 and int(part) <= 255 for part in s.split(".")) and len(s.split(".")) == 4 or s == "localhost")

gate_options = {
    "AND": And(),
    "OR": Or(),
    "NOR": NOr(),
    "NOT": Not(),
    "NAND": NAnd(),
    "XOR": XOr(),
    "XNOR": XNOr(),
    "NOOP": NoOp()
}

GATE=gate_options[GATE_TYPE]


if failed_load:
    sys.exit("Not all environment variables set properly")

OUTPUTS=OUTPUTS.split(",")
PORT=int(PORT)
