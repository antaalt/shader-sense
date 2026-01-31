#define PROCEDURAL_STRUCT(value) \
struct ProceduralTest##value { \
    float oui; \
} \

#define INLINE_PROCEDURAL_STRUCT() struct TestMacro{}

#define VALUE_POSITIVE 1
#define VALUE_NEGATIVE 1
#define VALUE_ADDITION (VALUE_POSITIVE - VALUE_NEGATIVE)

PROCEDURAL_STRUCT(Value0);
PROCEDURAL_STRUCT(Value1);

void main() {
#if VALUE_ADDITION
    float oui = 0.f
#else
    float oui = 0.f;
#endif
    INLINE_PROCEDURAL_STRUCT();
}