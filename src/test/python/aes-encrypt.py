from Crypto.Cipher import AES

data = 0

key = 0x123456789abcde
cipher = AES.new(key, AES.MODE_CMAC)

nonce = cipher.nonce
ciphertext, tag = cipher.encrypt_and_digest(data)

print(ciphertext)