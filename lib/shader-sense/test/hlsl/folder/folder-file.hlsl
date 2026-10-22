// Invalid path but accessible via custom includes.
#include "level-root.hlsl"
// This include will push level-root directory on stack, making level-root accessible.
#include "../inc0/level0.hlsl"

void main() {
    uint value = root;
}