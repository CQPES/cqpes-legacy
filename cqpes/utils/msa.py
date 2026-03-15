import importlib.util
import sys
from types import ModuleType


def load_msa_so(
    msa_so_path: str,
) -> ModuleType:
    module_name = "msa"
    spec = importlib.util.spec_from_file_location(module_name, msa_so_path)

    if (spec is None) or (spec.loader is None):
        raise ImportError(f"Failed to create spec for msa {msa_so_path}.")

    module = importlib.util.module_from_spec(spec)
    sys.modules[module_name] = module
    spec.loader.exec_module(module)

    return module
