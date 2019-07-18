from pytest import mark
from binary_search import binary_search


binary_search_test_data = [
    (None, None, None),
    ([], None, None),
    ([0], 1, None),
    ([0], 0, 0),
    ([0, 1], 1, 1),
    ([0, 1, 10, 20], 10, 2),
    ([0, 1, 10, 20], 100, None),
]


@mark.parametrize("sorted_list, search_item, expected_result", binary_search_test_data)
def test_binary_search(sorted_list, search_item, expected_result):
    result = binary_search(sorted_list, search_item)
    assert result == expected_result
