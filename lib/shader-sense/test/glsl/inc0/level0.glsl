#include "./inc1/level1.glsl"

const float level0 = level1 + 4.0;

uint fibonacciLevel0(uint nthNumber) {
    int prevprevNumber, prevNumber = 0, currentNumber = 1;
    for (int i = 1; i < nthNumber ; i++) {
        prevprevNumber = prevNumber;
        prevNumber = currentNumber;
        currentNumber = prevprevNumber + prevNumber;
    }
    return currentNumber;
}