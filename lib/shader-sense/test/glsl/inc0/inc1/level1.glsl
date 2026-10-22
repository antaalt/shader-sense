
const float level1 = 43.0;

#define MACRO_TEST 42

struct Test {
    uint testData;
    float testData1;
};

uint fibonacciLevel1(uint nthNumber) {
    int prevprevNumber, prevNumber = 0, currentNumber = 1;
    for (int i = 1; i < nthNumber ; i++) {
        prevprevNumber = prevNumber;
        prevNumber = currentNumber;
        currentNumber = prevprevNumber + prevNumber;
    }
    return currentNumber;
}