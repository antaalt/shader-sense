
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
