float4 main() : SV_Target
{
    return float4(PREAMBLE_VALUE, CUSTOM_MACRO, 0.0, 1.0);
}

float error() { return undefined_value; }
