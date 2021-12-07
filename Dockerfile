FROM python:3.9-alpine
ADD gateaas /opt/gateaas
ADD logging_config.ini /opt/gateaas/logging_config.ini
WORKDIR /opt/gateaas
RUN pip install requests
CMD ["python3", "-u", "server.py"]
