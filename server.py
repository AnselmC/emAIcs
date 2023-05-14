import logging

from jsonrpc import JSONRPCResponseManager, dispatcher
from werkzeug.serving import run_simple
from werkzeug.wrappers import Request, Response

from models.llama.model import get_response_for_prompt

logger = logging.getLogger("emaics-server")
logger.setLevel(logging.DEBUG)


@dispatcher.add_method
def execute_prompt(prompt: str, buffer: str) -> str:
    return get_response_for_prompt(prompt, buffer)


@Request.application
def application(request):
    logger.debug(f"Got request: {request.data}")
    response = JSONRPCResponseManager.handle(request.data, dispatcher)
    logger.debug(f"Resonse: {response.json}")
    return Response(response.json, mimetype="application/json")


if __name__ == "__main__":
    run_simple("localhost", 4000, application)
