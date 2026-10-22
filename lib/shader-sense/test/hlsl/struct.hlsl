struct Test {
    float oui;
};

struct Test2 {
    Test test;
    float non;
};

struct Container {
    Test test;
    Test2 test2;
    Test testArray[3];
    Container method(float a, float b) {
        return (Container)0;
    }
};

void main() {
    Container container;
    container.test.oui = 0.f;
    container.test2.non = 0.f;
    Test2 t2 = container.method(0.f, 1.f).test2;
    Test t = container.method(0.f, 1.f).test2.test;
    float value = container.testArray[0].oui;
}