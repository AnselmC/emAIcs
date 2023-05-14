import openai


def _get_model() -> openai.Model:
    return openai.Model.list()[-1]


def get_response_for_prompt(
    prompt: str,
    buffer: str,
) -> str:
    model = _get_model()
    max_tokens = 128
    temperature = 0.9
    return openai.Completion.create(
        model=model,
        prompt=f"{prompt}: {buffer}",
        max_tokens=max_tokens,
        temperature=temperature,
        stream=False,
    )


def _set_api_key(key: str) -> None:
    openai.api_key = key
