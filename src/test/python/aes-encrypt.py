from Crypto.Cipher import AES

data = 0

key = 0x2b7e151628aed2a6abf7158809cf4f3c
cipher = AES.new(key, AES.MODE_CMAC)

nonce = cipher.nonce
ciphertext, tag = cipher.encrypt_and_digest(data)

print(ciphertext)