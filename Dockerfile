FROM python:3.9-alpine
ADD gateaas /opt/gateaas
WORKDIR /opt/gateaas
RUN pip install validators requests
CMD ["python3", "server.py"]
