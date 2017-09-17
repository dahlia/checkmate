# Sample Python code
# CHECK module-level check


def func():
    # CHECK function-level check
    pass


def other_func():
    # CHECK function-level check 2

    def inner_func():
        # CHECK nested function-level check
        pass


class some_cls:
    # CHECK class-level check

    def method(self):
        # CHECK method-level check
        pass
