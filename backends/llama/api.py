import os
import sys
from dataclasses import asdict, dataclass
from functools import lru_cache

import torch
from peft import PeftModel
from transformers import (GenerationConfig, LlamaForCausalLM, LlamaTokenizer,
                          StoppingCriteriaList)

DEFAULT_LLAMA_WEIGHTS = "huggyllama/llama-7b"
DEFAULT_LORA_WEIGHTS = "tloen/alpaca-lora-7b"


@dataclass
class User:
    first_name: str
    last_name: str


def print_username(user: User) -> str:
    pass


def _get_device() -> str:
    if hasattr(torch.backends, "mps") and torch.backends.mps.is_available():
        return "cpu"  # "mps"
    if torch.cuda.is_available():
        return "cuda"
    return "cpu"


@lru_cache(maxsize=1)
def _get_tokenizer_and_model(
    device: str,
    llama_weights: str = DEFAULT_LLAMA_WEIGHTS,
    lora_weights: str = DEFAULT_LORA_WEIGHTS,
) -> tuple[LlamaTokenizer, LlamaForCausalLM]:
    tokenizer = LlamaTokenizer.from_pretrained(llama_weights)

    match device:
        case "cuda":
            llama_model = LlamaForCausalLM.from_pretrained(
                llama_weights,
                load_in_8bit=True,
                torch_dtype=torch.float16,
                device_map="auto",
            )
            model = PeftModel.from_pretrained(
                llama_model,
                lora_weights,
                torch_dtype=torch.float16,
            )
        case "mps":
            llama_model = LlamaForCausalLM.from_pretrained(
                llama_weights,
                load_in_8bit=True,
                device_map={"": device},
                torch_dtype=torch.float16,
            )
            model = PeftModel.from_pretrained(
                llama_model,
                lora_weights,
                device_map={"": device},
                torch_dtype=torch.float16,
            )
        case _:
            llama_model = LlamaForCausalLM.from_pretrained(
                llama_weights, device_map={"": device}, low_cpu_mem_usage=True
            )
            model = PeftModel.from_pretrained(
                llama_model,
                lora_weights,
                device_map={"": device},
            )
    # unwind broken decapoda-research config
    model.config.pad_token_id = tokenizer.pad_token_id = 0  # unk
    model.config.bos_token_id = 1
    model.config.eos_token_id = 2

    model.eval()
    if torch.__version__ >= "2" and sys.platform != "win32":
        model = torch.compile(model)

    return (tokenizer, model)


@dataclass
class GenerationParams:
    temperature: float = 0.1
    top_p: float = 0.75
    top_k: float = 40
    num_beams: int = 4
    max_new_tokens: int = 128
    stream_response: bool = False


def get_response_for_prompt(
    prompt: str,
    buffer: str,
) -> str:
    generation_params: GenerationParams = GenerationParams()
    device = _get_device()
    tokenizer, model = _get_tokenizer_and_model(device=device)

    embellished_prompt = (
        "Below is an instruction that describes a task, paired with an input that provides further context. "
        "Write a response that appropriately completes the request.\n\n"
        f"### Instruction:\n{prompt}\n\n### "
        f"Input:\n{buffer}\n\n### Response:\n"
    )
    inputs = tokenizer(embellished_prompt, return_tensors="pt")
    input_ids = inputs["input_ids"].to(device)

    generation_config = GenerationConfig(**asdict(generation_params))

    generate_params = {
        "input_ids": input_ids,
        "generation_config": generation_config,
        "return_dict_in_generate": True,
        "output_scores": True,
        "max_new_tokens": generation_params.max_new_tokens,
    }

    if not generation_params.stream_response:
        with torch.no_grad():
            generation_output = model.generate(**generate_params)
        s = generation_output.sequences[0]
        output = tokenizer.decode(s)
        return output.split("### Response:\n")[-1].replace("</s>", "")
    raise NotImplementedError("")


def set_api_key(key: str) -> None:
    _ = key
