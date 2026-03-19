from typing import Optional, TextIO, Type, Union

__all__ = [
    "print_header",
    "custom_warning",
]

_LOG_HEADER = r"""
                      _____ ____  _____  ______  _____                          
                     / ____/ __ \|  __ \|  ____|/ ____|                         
                    | |   | |  | | |__) | |__  | (___                           
                    | |   | |  | |  ___/|  __|  \___ \                          
                    | |___| |__| | |    | |____ ____) |                         
                     \_____\___\_\_|    |______|_____/                          
"""


def print_header(
    module_name: str,
    no_header: bool = False,
) -> None:
    if not no_header:
        print(f"{_LOG_HEADER:^80}")
        print(f"{'CQPES: ChongQing Potential Energy Surface':^80}")

    print()
    print(f"{module_name.upper():^80}")
    print("=" * 80)


def custom_warning(
    message: Union[str, Warning],
    category: Type[Warning],
    filename: str,
    lineno: int,
    file: Optional[TextIO] = None,
    line: Optional[str] = None,
) -> None:
    print(f"  [{'WARN':^10}] {message}")
