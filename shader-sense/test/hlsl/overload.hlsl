float sum(float a)
{
    return a;
}

float sum(float a, float b)
{
    return a + b;
}

int sum(int a, int b)
{
    return a + b;
}

struct FloatData { float floatValue; };
struct IntData { int intValue; };

FloatData make(float v) { FloatData d; d.floatValue = v; return d; }
IntData make(int v) { IntData d; d.intValue = v; return d; }

void main()
{
    float r = sum(1.0, 2.0);
    float x = 1.0;
    int i = 1;
    float fx = make(x).floatValue;
    int ii = make(i).intValue;
    int si = sum(i, i);
}
