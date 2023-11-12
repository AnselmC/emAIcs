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
def execute_prompt(prompt: str, buff: str) -> str:
    if BACKEND is None:
        raise RuntimeError(f"BACKENDS should be one of {AVAILABLE_BACKENDS}")
    return BACKEND.get_response_for_prompt(prompt, buff)


@Request.application
def application(request: Request) -> Response:
    logger.debug(f"Got request: {request.data}")
    try:
        response = JSONRPCResponseManager.handle(request.data, dispatcher)
    except Exception as exc:
        logger.exception(f"Error handling {request.data}")
        return exc
    logger.debug(f"Resonse: {response.json}")
    return Response(response.json, mimetype="application/json")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="")
    parser.add_argument("-b", "--backend", choices=AVAILABLE_BACKENDS, default="openai")
    parser.add_argument("-s", "--server-address", default="localhost")
    parser.add_argument("-p", "--server-port", default=4000, type=int)
    parser.add_argument(
        "-k",
        "--api-key",
    )

    args = parser.parse_args()

    BACKEND = AVAILABLE_BACKENDS[args.backend]
    BACKEND.set_api_key(args.api_key)

    logger.info(f"Starting *emAIcs* server with backend {BACKEND}")
    run_simple(args.server_address, args.server_port, application)
