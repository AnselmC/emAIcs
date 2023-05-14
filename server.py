import argparse
import logging

from jsonrpc import JSONRPCResponseManager, dispatcher
from werkzeug.serving import run_simple
from werkzeug.wrappers import Request, Response

import backends.llama.model as llama_model
import backends.openai.model as openai_model

logger = logging.getLogger("emaics-server")
logger.setLevel(logging.DEBUG)

AVAILABLE_BACKENDS = dict(llama=llama_model, openai=openai_model)

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
    parser.add_argument(
        "-b",
        "--backend",
        choices=AVAILABLE_BACKENDS,
        default=AVAILABLE_BACKENDS["openai"],
    )
    parser.add_argument(
        "-k", "--api-key", 
    )

    args = parser.parse_args()

    BACKEND = AVAILABLE_BACKENDS[args.backend]
    BACKEND.set_api_key(args.api_key)

    run_simple("localhost", 4000, application)
