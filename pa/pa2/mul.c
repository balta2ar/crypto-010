#include <stdio.h>

unsigned char gmul(unsigned char a, unsigned char b) {
    unsigned char p = 0;
    unsigned char counter;
    unsigned char hi_bit_set;
    for(counter = 0; counter < 8; counter++) {
        if((b & 1) == 1)
            p ^= a;
        hi_bit_set = (a & 0x80);
        a <<= 1;
        if(hi_bit_set == 0x80)
            a ^= 0x1b;
        b >>= 1;
    }
    return p;
}

int main() {
    printf("%u x %u = %u\n", 0, 0, gmul(0, 0));
    printf("%u x %u = %u\n", 0, 1, gmul(0, 1));
    printf("%u x %u = %u\n", 1, 0, gmul(1, 0));
    printf("%u x %u = %u\n", 1, 1, gmul(1, 1));
    printf("%u x %u = %u\n", 2, 2, gmul(2, 2));
    printf("%u x %u = %u\n", 5, 5, gmul(5, 5));
    printf("%u x %u = %u\n", 5, 10, gmul(5, 10));
    printf("%u x %u = %u\n", 13, 10, gmul(13, 10));
    printf("%u x %u = %u\n", 13, 42, gmul(13, 42));
    return 0;
}
