#!/bin/python3
import config
import socketserver


from GateHandler import GateHandler

def main():
    with socketserver.TCPServer((config.BIND_ADDRESS, config.PORT), GateHandler) as server:
        config.logger.info("Server initialized at " + config.BIND_ADDRESS + ":" + str(config.PORT))
        config.logger.info("Sending gate outputs to: " + str(config.OUTPUTS))
        try:
            server.serve_forever()
        except KeyboardInterrupt:
            server.socket.close()


if __name__ == "__main__":
    main()
