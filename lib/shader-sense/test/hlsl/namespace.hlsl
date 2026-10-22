
namespace Test {
    void test() {

    }
}

void main() {
    test();
}

namespace Test {
    // Namespace reopened, test is visible.
    void other() {
        test();
    }
}

void qualified() {
    Test::test();
}

namespace Outer {
    namespace Inner {
        float nested() { return 0.0; }
    }
    void outer() {
        Inner::nested();
    }
}

void qualifiedNested() {
    Outer::Inner::nested();
}
