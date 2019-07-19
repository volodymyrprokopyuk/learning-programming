from pytest import mark
from fibonacci_sequence import fibonacci


fibonacci_test_data = [
    (-1, []),
    (0, []),
    (1, [0]),
    (2, [0, 1]),
    (3, [0, 1, 1]),
    (4, [0, 1, 1, 2]),
    (5, [0, 1, 1, 2, 3]),
    (6, [0, 1, 1, 2, 3, 5]),
    (7, [0, 1, 1, 2, 3, 5, 8]),
    (8, [0, 1, 1, 2, 3, 5, 8, 13]),
]


@mark.parametrize("size, expected_result", fibonacci_test_data)
def test_fibonacci(size, expected_result):
    result = fibonacci(size)
    assert result == expected_result
