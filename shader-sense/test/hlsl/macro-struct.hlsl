#define PROCEDURAL_STRUCT(value) \
struct Test ##value## { \
    float oui; \
} \

#define VALUE_POSITIVE 1
#define VALUE_NEGATIVE 1
#define VALUE_ADDITION (VALUE_POSITIVE + VALUE_NEGATIVE)

void main() {
#if VALUE_ADDITION
    float oui = 0.f
#else
    float oui = 0.f
#endif
}