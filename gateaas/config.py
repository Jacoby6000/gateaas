import os
import sys
import logging
import logging.config

from gates import OR, XOR, XNOR, NOT, NOR, NOOP, NAND, AND

logging.config.fileConfig("logging_config.ini")

logger = logging.getLogger('gateaas')

failed_load = False
nop_validator = lambda x: True
def safe_get(key, validator=nop_validator):
    if key in os.environ:
        logger.debug(key + ": '" + os.environ[key] + "'")
        result = os.environ[key]
        if validator(result):
            return result
        else:
            logger.error("Env var `" + key + "` is malformed")
    else:
        logger.error("Env var `" + str(key) + "` does not exist.")

    failed_load = True
    return None

def validate_url(url):
    logger.debug(url)
    if url.startswith("https"):
        url = url[5:]
    elif url.startswith("http"):
        url = url[4:]
    else:
        return False

    if url.startswith("://"):
        url = url[3:]
    else:
        return False

    return True

gate_options = {
    "AND": AND(),
    "OR": OR(),
    "NOR": NOR(),
    "NOT": NOT(),
    "NAND": NAND(),
    "XOR": XOR(),
    "XNOR": XNOR(),
    "NOOP": NOOP()
}

GATE_TYPE=safe_get("GATE_TYPE", lambda s: s in gate_options)
OUTPUTS=safe_get("OUTPUTS", lambda urls: len(urls) == 0 or all(validate_url(url.strip()) for url in urls.split(",")))
#INPUT_COUNT=safe_get("INPUT_COUNT", lambda s: s.isdigit() and int(s) > 0 and ((int(s) == 1 and GATE_TYPE=="NOT") or GATE_TYPE!="NOT"))
PORT=safe_get("PORT", lambda s: s.isdigit() and int(s) > 0 and int(s) < 65536)
BIND_ADDRESS=safe_get("BIND_ADDRESS", lambda s: all(part.isdigit() and int(part) >= 0 and int(part) <= 255 for part in s.split(".")) and len(s.split(".")) == 4 or s == "localhost")

GATE=gate_options[GATE_TYPE]


if failed_load:
    logger.error("Not all environment variables are set properly")
    sys.exit("Not all environment variables are set properly")

if OUTPUTS and len(OUTPUTS) > 0:
    OUTPUTS=[url.strip() for url in OUTPUTS.split(",")]

PORT=int(PORT)
