import openai


def get_response_for_prompt(
    prompt: str,
    buffer: str,
) -> str:
    max_tokens = 128
    temperature = 0.9
    resp = openai.Completion.create(
        model="text-davinci-003",
        prompt=f"{prompt}: {buffer}",
        max_tokens=max_tokens,
        temperature=temperature,
        stream=False,
    )
    if resp.choices:
        return resp.choices[0].text
    return ""


def set_api_key(key: str) -> None:
    openai.api_key = key
