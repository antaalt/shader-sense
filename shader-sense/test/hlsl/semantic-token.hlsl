#define MY_MACRO 1

enum MyEnum {
    Value0,
    Value1
};

void semanticTokens(float param0, uint param1) {
    float param0Copy = param0;
    uint value = param1 + MyEnum::Value0;
}