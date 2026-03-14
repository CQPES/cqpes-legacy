__all__ = [
    "print_header",
]

_LOG_HEADER = r"""
                      _____ ____  _____  ______  _____                          
                     / ____/ __ \|  __ \|  ____|/ ____|                         
                    | |   | |  | | |__) | |__  | (___                           
                    | |   | |  | |  ___/|  __|  \___ \                          
                    | |___| |__| | |    | |____ ____) |                         
                     \_____\___\_\_|    |______|_____/                          
"""


def print_header(module_name: str) -> None:
    print(f"{_LOG_HEADER:^80}")
    print(f"{'CQPES: ChongQing Potential Energy Surface (legacy)':^80}")
    print()
    print(f"{module_name.upper():^80}")
    print("=" * 80)
