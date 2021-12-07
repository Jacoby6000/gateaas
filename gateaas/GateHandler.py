import http.server
import requests
import json
from config import GATE, OUTPUTS, logger

class GateHandler(http.server.SimpleHTTPRequestHandler):

    def do_POST(self):
        content_len = int(self.headers.get('Content-Length'))
        post_body = self.rfile.read(content_len)

        path_parts = self.path.split("/")
        if len(path_parts) == 3 and path_parts[1] == "in" and path_parts[2].isdigit():
            self.send_response(200, "OK")
            self.send_header("Content-type", "text/plain")
            self.end_headers()
            print(bool(post_body))
            print(OUTPUTS)
            GATE.set(int(path_parts[2]), bool(post_body))
            if GATE.is_dirty():
                result = GATE.run()
                print(result)
                if OUTPUTS:
                    for output in OUTPUTS:
                        requests.post(output, data=str(int(result)))
        else:
            self.send_response(404, "Not Found")


    def do_GET(self):
        path_parts = self.path.split("/")
        if len(path_parts) == 2 and path_parts[1] == "probe":
            self.send_response(200, "OK")
            self.send_header("Content-type", "application/json")
            self.end_headers()
            state = GATE.probe()
            self.wfile.write(json.dumps(state).encode("UTF-8"))
        else:
            self.send_response(404, "Not Found")
