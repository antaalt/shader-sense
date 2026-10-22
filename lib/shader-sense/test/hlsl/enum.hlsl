
enum EnumTest {
    ENUM_VALUE0 = 0,
    ENUM_VALUE1 = 1,
    ENUM_VALUE2 = 2,
    ENUM_VALUE3 = 3,
};

enum class EnumClassTest {
    VALUE0 = 0,
    VALUE1 = 1,
    VALUE2 = 2,
    VALUE3 = 3,
};

void main(float param) {
    uint value0 = ENUM_VALUE0;
    uint value1 = (uint)EnumClassTest::VALUE1;
    uint value2 = (uint)EnumClassTest::VALUE2;
}