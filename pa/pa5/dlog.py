import gmpy2
from gmpy2 import mpz


P = mpz(13407807929942597099574024998205846127479365820592393377723561443721764030073546976801874298166903427690031858186486050853753882811946569946433649006084171)
G = mpz(11717829880366207009516117596335367088558084999998952205599979459063929499736583746670572176471460312928594829675428279466566527115212748467589894601965568)
H = mpz(3239475104050450443565264378728065788649097520952449527834792452971981976143292558073856937958553180532878928001494706097394108577585732452307673444020333)


if __name__ == '__main__':
    print('hello')

    table = dict()
    low, high = 0, 2 ** 20
    B = high

    # 1. populate table with values of the left part of equation
    for x1 in range(low, high + 1):
        left = gmpy2.t_mod(gmpy2.mul(H, gmpy2.invert(gmpy2.powmod(G, x1, P), P)), P)
        table[left] = x1
    print('step 1 done')

    # 2. check each value of the right part of equation
    for x0 in range(low, high + 1):
        right = gmpy2.powmod(gmpy2.powmod(G, B, P), x0, P)
        if right in table:
            x1 = table[right]
            x = x0 * B + x1
            print('solution: {0}'.format(x))
    print('step 2 done')
    print('done')
