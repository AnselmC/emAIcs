import argparse
import logging

from jsonrpc import JSONRPCResponseManager, dispatcher
from werkzeug.serving import run_simple
from werkzeug.wrappers import Request, Response

import backends.llama.api as llama_api
import backends.openai.api as openai_api

logger = logging.getLogger("emaics-server")
logger.setLevel(logging.DEBUG)

AVAILABLE_BACKENDS = dict(llama=llama_api, openai=openai_api)

BACKEND = None


@dispatcher.add_method
def execute_prompt(prompt: str, buffer: str) -> str:
    return BACKEND.get_response_for_prompt(prompt, buffer)


@Request.application
def application(request):
    logger.debug(f"Got request: {request.data}")
    response = JSONRPCResponseManager.handle(request.data, dispatcher)
    logger.debug(f"Resonse: {response.json}")
    return Response(response.json, mimetype="application/json")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="")
    parser.add_argument("-b", "--backend", choices=AVAILABLE_BACKENDS, default="openai")
    parser.add_argument("-s", "--server-address", default="localhost")
    parser.add_argument("-p", "--server-port", default=4000, type=str)
    parser.add_argument(
        "-k",
        "--api-key",
    )

    args = parser.parse_args()

    BACKEND = AVAILABLE_BACKENDS[args.backend]
    BACKEND.set_api_key(args.api_key)

    run_simple(args.server_address, args.server_port, application)
