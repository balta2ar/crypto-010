from Crypto.Cipher import AES

#key = '00000000000000000000000000000000'.decode('hex')
#plaintext = 'HelloWorld123456'
key = 'mysecretpassword'
plaintext = 'Secret Message A'

encobj = AES.new(key, AES.MODE_ECB)
ciphertext = encobj.encrypt(plaintext)

# Resulting ciphertext in hex
print(ciphertext.encode('hex'))
