from contextlib import contextmanager
from collections import namedtuple, defaultdict
from functools import partial, wraps
from operator import add


# ** BASICS **


# Iteration
name = "Vlad"
count = 0
for letter in name:
    print(letter, end=" ")
    count += 1
print(f"\nCoount: {count}")

index = 0
while index < count:
    letter = name[index]
    print(letter, end=" ")
    index += 1
print()


# Exception
try:
    with open("main.py", "r") as opened_file:
        number_of_lines = len(opened_file.readlines())
        print(f"Number of lines: {number_of_lines}")
except (IOError, TypeError) as error:
    print(f"ERROR: {error}")
finally:
    print("Finally end")


# Context manager
@contextmanager
def context_manager(name):
    print("Context manager create resource")
    try:
        yield name
    finally:
        print("Context manager clean up resource")


with context_manager("Vlad") as context_name:
    # raise Exception("oh")
    print(f"Context object: {context_name}")


# Conditional expression
number = 101
even_or_odd = "odd" if number % 2 else "even"
print(f"{number} is {even_or_odd}")


# Sequence unpacking
host = "www.github.com"
*domain, tld = host.split(".")
print(f"Domain: {domain}, TLD: {tld}")


# List comprehension (eager)
filtered_list = [item * 10 for item in range(10) if item > 5]
print(f"Filtered list: {filtered_list}")


# Generator expression (lazy)
filtered_generator = (item * 10 for item in range(10) if item > 5)
filtered_minimum = min(filtered_generator)
print(f"Filtered minimum: {filtered_minimum}")


# Set comprehension
filtered_set = {item * 10 for item in range(10) if item > 5}
print(f"Filtered set: {filtered_set}")


# Dictionary comprehension
filtered_dict = {str(item): item for item in range(10) if item > 5}
print(f"Filtered dict: {filtered_dict}")


# Named tuple (for efficiency)
Person = namedtuple("Person", "first_name last_name")
vlad = Person("Volodymyr", "Prokopyuk")
print(f"Named tuple Person: {vlad.first_name} {vlad.last_name}")


# Default dict
default_dict = defaultdict(int)
default_value = default_dict["key"]
print(f"Default dict value: {default_value}")


# ** FUNCTIONS **


# Functions: keyword arguments with default values
def add_refix(subject, prefix="Prefix "):
    return prefix + subject


prefixed_subject = add_refix(prefix="Vlad ", subject="Architect")
print(f"Prefixed subject: {prefixed_subject}")


# Function: variable list of positional arguments
def join_items(*items):
    return " ".join(items)


joined_items = join_items("one", "two", "three", "four")
print(f"Joined items: {joined_items}")


def join_with_prefix_and_suffix(prefix, *items, suffix):
    return " ".join([prefix, *items, suffix])


items_to_join = ["one", "two", "three", "four"]
joined_with_prefix_and_suffix = join_with_prefix_and_suffix(
    "Prefix", *items_to_join, suffix="suffix"
)
print(f"Joined with prefix and suffix: {joined_with_prefix_and_suffix}")


# Function: variable list of keword arguments
def print_kwargs(**kwargs):
    print(f"kwargs: {kwargs}")


print_kwargs(one=1, two=2, three=3, four=4)
kwargs_dict = {"ten": 10, "eleven": 11, "twelve": 12}
print_kwargs(**kwargs_dict)


# Funciton: partial application
add_one = partial(add, 1)
result = add_one(2)
print(f"Partial applicaiton of add: {result}")


# Decorator: without parameters
def decorator_without_params(original):
    @wraps(original)
    def decorated(*args, **kwargs):
        print("Decorator: before")
        result = original(*args, **kwargs)
        print("Decorator: after")
        return result

    return decorated


@decorator_without_params
def decorated_print(text):
    print(text)


decorated_print("Vlad")


# Decorator: with parameters
def decorator_with_params(param):
    def decorator(original):
        @wraps(original)
        def decorated(*args, **kwargs):
            print(f"Decorator: before {param}")
            result = original(*args, **kwargs)
            print(f"Decorator: after {param}")
            return result

        return decorated

    return decorator


@decorator_with_params("Svitlana")
def decorated_print2(text):
    print(text)


decorated_print2("Vlad")


# Generator: Fibonacci
def fibonacci(limit):
    prv, crr = -1, 1
    count = 0
    while count < limit:
        nxt = prv + crr
        yield nxt
        prv, crr = crr, nxt
        count += 1


fibonacci_sequence = fibonacci(10)
print(f"Generator: Fibonacci sequence: {list(fibonacci_sequence)}")


# Lambda expression
result = (lambda x, y: x + y)(1, 2)
print(f"Lambda: add_nums: {result}")


# ** Classes **


# Class: constructor, attributes/properties, and methods
# Class body is a normal code block like if, for, or while
class Person:

    # Class attribute
    status = "Happy"
    # Use expressions in Class definition
    general_greeting = f"Hello, {status}!" if status == "Happy" else "I'm sorry"

    # Class method
    @classmethod
    def general_greet(cls):
        return f"{cls.general_greeting}"

    # Constructor
    def __init__(self, first_name, last_name):
        # Instance attribute
        self.first_name = first_name
        self.last_name = last_name
        self._full_name = f"{self.first_name} {self.last_name}"

    # Read-only (computed) property
    @property
    def full_name(self):
        return self._full_name

    # Writable (validated) property
    @full_name.setter
    def full_name(self, value):
        self._full_name = value

    # Method
    def greet(self):
        return f"Hello, {self.full_name}!"


person1 = Person("Volodymyr", "Prokopyuk")
print(f"Class Person: {Person.status}, {person1.status}")
print(f"Class Person: {Person.general_greeting} {Person.general_greet()}")
print(f"Class Person: {person1.first_name} {person1.last_name}, {person1.full_name}")
person1.full_name = "Vlad Prokopyuk"
print(f"Class Person: {person1.full_name}")
print(f"Class Person: {person1.greet()}")


# Class mixin
class RecognitionMixing:
    def recognize(self):
        return f"Good job, {self.full_name}!"


# Class: (multiple) inheritance and mixin
class Employee(Person, RecognitionMixing):
    def __init__(self, first_name, last_name, position):
        # Call constructor/method of superclass
        super().__init__(first_name, last_name)
        self.position = position


empoloyee1 = Employee("Volodymyr", "Prokopyuk", "Architect")
print(f"Class Employee: {empoloyee1.full_name}. {empoloyee1.recognize()}")


# Class special methods
# __init__, __del__ > Initialization and descruction
#     - C()
#     - del c
# __hash__ > Hashing
#     - hash(c) in dict/set
# __str__, __repr__ > Representation
#     - str(c)
#     - f"{c}"
# __bool__ > Truthiness
#     - if/while c:
#     - c1 and/or/not c2:
#     - bool(c)
# ___len__ > Length:
#     - len(c)
# __call__ > Invocation
#     - c()
# __eq/ne__, __lt/le__, __gt/ge__ > Comparison
#     - if c1 </<=/>/>=/==/!= c2:
# __getitem__ > Sequence
#     - for i in c: -> IndexError
# __iter__ > Iteration
#     - for i in c: -> iter(c) -> __iter__, __next__, StopIteration
# __contains__ > Membership
#     - if i in c:
# _get/set/delitem__ > Indexing in list/dict
#     - c[0/"key"]
#     - c[0/"key"] = x
#     - del c[0/"key"]
# __get/set/delattr__, __dir__ > Attribute access in class/namedtuple
#     - c.attr
#     - c.attr = x
#     - del c.attr
# __add/sub__, __mul/div__, __neg/pos__, __mod/pow__ > Arithmetic operations
#     - c1 +/- c2
#     - c1 *// c2
#     - +/-c
#     - c %/** x
# __and/or/xor__ > Logical operations
#     - c1 &/|/^ c2
# __lshift/rshift__ > Bitwise operations
#     - c1 >>/<< c2
# __enter__, __exit__ > Context manager
#     - with c as x:
