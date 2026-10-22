template <typename T, typename A = float>
struct TemplatedType {
    T type;
    float type2;
};

struct Type {
  float type;
};

RWStructuredBuffer<Type>	OutStreamingRequests; // ERROR

template<typename T>
void increment(inout TemplatedType<T, float> X, float value) {
  X.type += value;
}
TemplatedType<float> ty_template() {
  TemplatedType<float> ty;
  return ty;
}
float storage() {
  return 0.f;
}
Type ty() {
  Type ty;
  return ty;
}

template<typename T>
TemplatedType<T> get(float value) {
  TemplatedType<T> ty;
  ty.type = (T)value;
  ty.type2 = value;
  return ty;
}

void main() {
  float value = 1.0;
  TemplatedType<float> oui;
  increment<float>(oui, 2.0);
  increment(oui, 5.0);
  sizeof(float);
  sizeof(Type);
  sizeof(TemplatedType<float>);
}