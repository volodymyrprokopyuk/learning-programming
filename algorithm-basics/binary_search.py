"""Binary search"""


def binary_search(sorted_list, search_item):
    """Binary search"""
    if not sorted_list:
        return None
    low_index = 0
    high_index = len(sorted_list) - 1
    while low_index <= high_index:
        middle_index = (low_index + high_index) // 2
        middle_value = sorted_list[middle_index]
        if middle_value == search_item:
            return middle_index
        if middle_value < search_item:
            low_index = middle_index + 1
        else:
            high_index = middle_index - 1
