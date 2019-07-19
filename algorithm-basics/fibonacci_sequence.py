"""Fibonacci sequence generator"""


def fibonacci(size):
    """Fibonacci sequence generator"""
    sequence = []
    if size <= 0:
        return sequence
    previous_element = -1
    current_element = 1
    while size > 0:
        next_element = previous_element + current_element
        sequence.append(next_element)
        previous_element = current_element
        current_element = next_element
        size -= 1
    return sequence
