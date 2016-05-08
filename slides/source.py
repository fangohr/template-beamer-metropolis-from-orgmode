def f(x):
    print("x = {}".format(x))
    return x**2, x**3

def test_f():
    assert f(0) == (0, 0)
