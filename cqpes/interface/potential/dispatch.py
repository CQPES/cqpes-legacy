import glob
import os
from typing import Literal, Optional

import cqpes  # noqa: F401

from .base import CQPESBasePot


class CQPESPot:
    def __new__(
        cls,
        workdir: str,
        backend: Optional[Literal["msa", "jaxpip"]] = None,
        **kwargs,
    ) -> CQPESBasePot:
        workdir = os.path.abspath(workdir)

        if backend is None:
            msa_artifacts = glob.glob(os.path.join(workdir, "*.so"))
            jaxpip_artifacts = glob.glob(os.path.join(workdir, "*.json.gz"))

            if jaxpip_artifacts:
                backend = "jaxpip"
            elif msa_artifacts:
                backend = "msa"
            else:
                raise FileNotFoundError(
                    f"[FATAL] Could not auto-detect backend in {workdir}.\n"
                    f"  - No *.json.gz found (required for jaxpip)\n"
                    f"  - No *.so found (required for msa)\n"
                    f"  Please check your 'prepare' step."
                )

        if backend == "msa":
            from .msa import CQPESMSAPot

            target_class = CQPESMSAPot
        elif backend == "jaxpip":
            from .jaxpip import CQPESJaxPIPPot

            target_class = CQPESJaxPIPPot
        else:
            raise ValueError(f"Unknown backend: {backend}")

        print(f"  [{'BACKEND':^10}] {backend.upper()}")

        instance = object.__new__(target_class)

        instance.__init__(workdir, **kwargs)

        return instance
