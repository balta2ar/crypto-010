crypto-010
==========

### Coursera [Cryptography I](https://class.coursera.org/crypto-010)

This is a source code for optional programming assignments of Cryptography I
class on Coursera.

### Disclaimer

Even though assignments are optional, this code being publicly available might
still be a violation of [Honor Code](https://www.coursera.org/about/terms).
Thus, I have two requests:

1. If you are a student, please DO NOT CHEAT (naive but still).
2. If you are Coursera staff member and you think my code has any negative
   effect on study process of other students and the code should not be public,
   please contact me.

### Assignments

## PA1 Stream Ciphers

Many Time Pad.

Let us see what goes wrong when a stream cipher key is used more than once.
Below are eleven hex-encoded ciphertexts that are the result of encrypting
eleven plaintexts with a stream cipher, all with the same stream cipher key.
Your goal is to decrypt the last ciphertext, and submit the secret message
within it as solution.

Hint: XOR the ciphertexts together, and consider what happens when a space is
XORed with a character in [a-zA-Z].

Implemented in Haskell.

## PA2 Block Ciphers

In this project you will implement two encryption/decryption systems, one using
AES in CBC mode and another using AES in counter mode (CTR). In both cases the
16-byte encryption IV is chosen at random and is prepended to the ciphertext.
For CBC encryption we use the PKCS5 padding scheme discussed in class.

Implemented in Haskell, C.

## PA3 Message Integrity

Suppose a web site hosts large video file `F` that anyone can download. Browsers
who download the file need to make sure the file is authentic before displaying
the content to the user. One approach is to have the web site hash the contents
of `F` using a collision resistant hash and then distribute the resulting short
hash value `h = H(F)` to users via some authenticated channel (later on we will
use digital signatures for this). Browsers would download the entire file `F`,
check that `H(F)` is equal to the authentic hash value h and if so, display the
video to the user.

Unfortunately, this means that the video will only begin playing after the
*entire* file F has been downloaded. Our goal in this project is to build a file
authentication system that lets browsers authenticate and play video chunks as
they are downloaded without having to wait for the entire file.

Implemented in Haskell.

## PA4 Authenticated Encryption

In this project you will experiment with a padding oracle attack against a toy
web site hosted at [crypto-class.appspot.com](http://crypto-class.appspot.com/).
Padding oracle vulnerabilities affect a wide variety of products, including
[secure tokens](http://arstechnica.com/security/2012/06/securid-crypto-attack-steals-keys/).
This project will show how they can be exploited. We discussed
CBC padding oracle attacks in Lecture 7.6, but if you want to read more about
them, please see [Vaudenay's paper](http://www.iacr.org/archive/eurocrypt2002/23320530/cbc02_e02d.pdf).

Implemented in Haskell.

## PA5 Basic key exchange

Your goal this week is to write a program to compute discrete log modulo a prime
p. Let g be some element in `Z∗p` and suppose you are given `h` in `Z∗p` such
that `h = g^x` where `1≤x≤2^40`. Your goal is to find `x`. More precisely, the
input to your program is `p`, `g`, `h` and the output is `x`.

Implemented in python ([gmpy2](http://gmpy2.readthedocs.org/en/latest/)).

## PA 6 Public Key Encryption from trapdoor permutations

Your goal in this project is to break RSA when the public modulus N is generated
incorrectly. This should serve as yet another reminder not to implement crypto
primitives yourself.

I didn't solve this one :(
