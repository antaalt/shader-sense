void literals() {
    { // Float
        float a = 123.456e-67f;
        float b = 123.456;
        float c = 123.456f;
        float d = 123e-67f;
        float e = 123.f;
        float f = 123.0;
    }
    { // Integer
        uint a = 0xfff8000000u; // hexa unsigned
        uint b = 0xfff8000000; // hexa
        uint c = 0; // zero
        uint d = 0u; // zero unsigned
        uint e = 12;
        uint f = 012; // octal
    }
    { // Types
        bool a = false;
        bool b = true;
    }
}